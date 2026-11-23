// Verifica nel browser di Import / Export (24/10/2026): foglio CSV scaricato dall'app e reimportato nei quattro modi.
// Accoda (e Annulla), Pulisci e scarica (non scrive), Sostituisci questo fondo (con conferma), Importa nuovo fondo.
// Ogni passo si controlla nel DATABASE (sqlite3), non solo nei messaggi. Database di esempio TEMPORANEO.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');
const { apriBackup } = require('./aiuti-pagina');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' }).trim();

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

const CONTENUTO = 'SELECT id, busta, fascicolo, titolo, regesto, fondo FROM fascicoli ORDER BY id';

test('Import / Export: accoda, pulisci, sostituisci, nuovo fondo', async ({ page }) => {
  test.setTimeout(240000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  const db = (await page.evaluate(() => (document.querySelector('code.no-trad') || {}).textContent)).trim() + '/trama.sqlite';
  const pronto = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 });
  const modo = async nome => { await page.locator('#modo_import').getByText(nome, { exact: false }).first().click(); await pronto(); };
  const finestra = page.locator('.modal-dialog');
  const nelFinestra = async (bottone) => { await finestra.getByRole('button', { name: bottone }).click(); await pronto(); };

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-imp-'));
  // Le prove condividono un solo server e un solo database: questa li cambia (sostituzione, nuovo fondo), quindi a fine prova
  // si rimette tutto com'era con «Ripristino da JSON · Sostituisci tutto» (percorso già provato in verifica-ripristino).
  await apriBackup(page);
  await expect(page.locator('#scarica_backup')).toBeVisible();
  const [dj] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup:not(.disabled)').click()]);
  const jsonIniziale = path.join(dir, 'iniziale.json'); await dj.saveAs(jsonIniziale);
  try {
  const [dl] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_csv:not(.disabled)').click()]);
  const csv = path.join(dir, 'fondo.csv'); await dl.saveAs(csv);
  const carica = async file => {
    await page.locator('#file_import').setInputFiles(file);
    await expect(page.locator('#anteprima_import')).toContainText('24 righe', { timeout: 20000 });
    // Scelto il foglio non si apre niente (18/11/2026): Lite dice dove andrà e sceglie la modalità giusta.
    await expect(page.locator('#destinazione_import')).toContainText(/Fondo già presente|Fondo nuovo/, { timeout: 20000 });
    await expect(finestra).toBeHidden();
  };
  await carica(csv);
  const prima = sql(db, CONTENUTO);
  expect(sql(db, 'SELECT COUNT(*) FROM fascicoli')).toBe('24');

  // - Accoda: la finestra di controllo si può annullare (nessuna scrittura), poi si importa «com'è»: tutto già presente
  await page.locator('#esegui_import').click();
  await expect(finestra).toContainText('Controlla prima di importare', { timeout: 20000 });
  await nelFinestra('Annulla');
  await expect(finestra).toBeHidden();
  expect(sql(db, CONTENUTO)).toBe(prima);
  await page.locator('#esegui_import').click();
  await expect(finestra).toContainText('Controlla prima di importare', { timeout: 20000 });
  await nelFinestra(/Importa com.è, senza correggere/);
  await expect(finestra).toContainText('Import concluso', { timeout: 30000 }); await nelFinestra('Resta qui'); await expect(finestra).toBeHidden();
  await expect(page.locator('#esito_import')).toContainText('24 letti · 0 importati · 24 già presenti', { timeout: 30000 });
  expect(sql(db, CONTENUTO)).toBe(prima);
  await page.locator('#esito_import').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '23-import-accoda.png') });

  // - Pulisci e scarica: non scrive nel database, e il foglio pulito si può scaricare
  await page.locator('#avvia_pulito').click();
  await expect(finestra).toContainText('Controlla prima di pulire', { timeout: 20000 });
  await nelFinestra('Pulisci con le correzioni scelte');
  await expect(finestra).toBeHidden({ timeout: 30000 });
  await expect(page.locator('#scarica_pulito')).toBeVisible({ timeout: 60000 });
  await expect(page.locator('#esito_pulisci')).toContainText('24 lette · 24 pulite · 0 respinte');
  await pronto();
  await page.waitForTimeout(500);
  await page.locator('#esito_pulisci').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '24-pulisci-esito.png') });
  const [dp] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_pulito:not(.disabled)').click()]);
  const pulito = path.join(dir, 'pulito-' + dp.suggestedFilename()); await dp.saveAs(pulito);
  expect(fs.statSync(pulito).size).toBeGreaterThan(1000);
  expect(sql(db, CONTENUTO)).toBe(prima);

  // - Sostituisci questo fondo: un titolo cambiato nel foglio; prima la conferma si può rifiutare
  const modificato = path.join(dir, 'fondo-modificato.csv');
  const testo = fs.readFileSync(csv, 'utf8');
  expect(testo.includes('"Processo contro Lucia Ferrante";')).toBe(true);
  fs.writeFileSync(modificato, testo.replace('"Processo contro Lucia Ferrante";', '"TITOLO SOSTITUITO DALLA PROVA";'));
  await carica(modificato);
  await modo('Sostituisci questo fondo');
  await page.locator('#esegui_import').click();
  await expect(finestra).toContainText('Controlla prima di importare', { timeout: 20000 });
  await nelFinestra(/Importa com.è, senza correggere/);
  await expect(finestra).not.toContainText('Controlla prima di importare', { timeout: 20000 });
  await expect(finestra).toContainText('Sostituire tutte le schede?');
  await page.waitForTimeout(500);
  await page.screenshot({ path: path.join(FOTO, '25-sostituisci-conferma.png') });
  // rifiutata: niente cambia
  await finestra.getByText('Annulla', { exact: true }).click();
  await expect(finestra).toBeHidden();
  expect(sql(db, CONTENUTO)).toBe(prima);
  // confermata
  await page.locator('#esegui_import').click();
  await expect(finestra).toContainText('Controlla prima di importare', { timeout: 20000 });
  await nelFinestra(/Importa com.è, senza correggere/);
  await expect(finestra).toContainText('Sostituire tutte le schede?');
  await page.locator('#conferma_sostituzione').click();
  await pronto();
  await expect(finestra).toContainText('Import concluso', { timeout: 60000 }); await nelFinestra('Resta qui'); await expect(finestra).toBeHidden();
  await expect(page.locator('#esito_import')).toContainText('24 letti', { timeout: 60000 });
  expect(sql(db, 'SELECT COUNT(*) FROM fascicoli')).toBe('24');
  expect(sql(db, "SELECT titolo FROM fascicoli WHERE busta=1 AND fascicolo=1")).toBe('TITOLO SOSTITUITO DALLA PROVA');
  expect(sql(db, "SELECT COUNT(*) FROM fascicoli WHERE titolo LIKE '%Ferrante%' AND busta=1 AND fascicolo=1")).toBe('0');
  // una copia di sicurezza «prima di…» è stata messa da parte, e contiene il vecchio titolo
  const cartellaBackup = path.join(path.dirname(db), 'backup');
  const copie = fs.readdirSync(cartellaBackup).filter(f => /prima/.test(f) && f.endsWith('.sqlite'));
  expect(copie.length).toBeGreaterThan(0);
  expect(copie.some(f => sql(path.join(cartellaBackup, f), "SELECT titolo FROM fascicoli WHERE busta=1 AND fascicolo=1") === 'Processo contro Lucia Ferrante')).toBe(true);
  await page.locator('#esito_import').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '26-sostituito.png') });

  // - Importa nuovo fondo: 24 schede in un fondo nuovo; il fondo di prima non cambia
  const stato = sql(db, CONTENUTO);
  await carica(csv);
  await modo('Importa nuovo fondo');
  await page.locator('#import_archivio').fill('Archivio della prova');
  await page.locator('#import_fondo').fill('Fondo della prova');
  await page.locator('#esegui_import').click();
  await expect(finestra).toContainText('Controlla prima di importare', { timeout: 20000 });
  await nelFinestra(/Importa com.è, senza correggere/);
  await expect.poll(() => sql(db, 'SELECT COUNT(*) FROM fascicoli'), { timeout: 60000 }).toBe('48');   // l'esito di prima è ancora a video: si guarda il database
  await pronto();
  // Il fondo nuovo si vede subito: la finestra dell'esito lo dice e il fondo attivo è quello (18/11/2026).
  await expect(finestra).toContainText('Fondo nuovo importato'); await expect(finestra).toContainText('Fondo della prova');
  await expect(page.locator('#fondo_etichetta')).toContainText('Fondo della prova');
  await nelFinestra('Resta qui'); await expect(finestra).toBeHidden();
  await expect(page.locator('#esito_import')).toContainText('24 importati');
  expect(sql(db, "SELECT COUNT(*) FROM fascicoli WHERE fondo='Fondo della prova'")).toBe('24');
  const vecchi = sql(db, CONTENUTO).split('\n').filter(r => !r.includes('Fondo della prova'));
  expect(vecchi.join('\n')).toBe(stato);
  await page.locator('#esito_import').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '27-nuovo-fondo.png') });
  } catch (e) { console.log('ERRORE ORIGINALE:', String(e.message).split('\n').slice(0, 6).join(' | ')); throw e;
  } finally {
    await page.keyboard.press('Escape'); await page.waitForTimeout(600);
    await page.locator('#file_ripristino').setInputFiles(jsonIniziale);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
    await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
    await page.locator('#esegui_ripristino').click();
    await finestra.getByRole('button', { name: 'Sostituisci con il backup' }).click();
    await expect.poll(() => sql(db, 'SELECT COUNT(*) FROM fascicoli'), { timeout: 60000 }).toBe('24');
    await expect.poll(() => sql(db, "SELECT titolo FROM fascicoli WHERE busta=1 AND fascicolo=1"), { timeout: 30000 }).toBe('Processo contro Lucia Ferrante');
    expect(sql(db, 'SELECT COUNT(*) FROM fondi')).toBe('1');
  }
});
