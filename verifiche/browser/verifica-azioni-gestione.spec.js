// Verifica nel browser delle azioni di Gestione che SCRIVONO: genera fascicoli vuoti, note rapide, correzione di una segnatura
// (e il suo annullamento), spostamento nell'ordine di consultazione (e il suo annullamento). Ogni passo si controlla nel database.
// Ha un server suo (playwright.importa.config.js) e a fine prova rimette il database com'era con il ripristino da JSON.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');
const { apriBackup } = require('./aiuti-pagina');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' }).trim();

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

test('Gestione: vuoti, note rapide, correzione segnatura, ordine di consultazione', async ({ page }) => {
  test.setTimeout(240000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 });
  const db = (await page.evaluate(() => (document.querySelector('code.no-trad') || {}).textContent)).trim() + '/trama.sqlite';
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-gest-'));
  const conta = () => sql(db, 'SELECT COUNT(*) FROM fascicoli');
  const toast = () => page.locator('.shiny-notification');
  expect(conta()).toBe('24');

  // copia iniziale (JSON), per rimettere tutto a posto alla fine
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  await apriBackup(page);
  await expect(page.locator('#scarica_backup')).toBeVisible();
  const [dj] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup:not(.disabled)').click()]);
  const jsonIniziale = path.join(dir, 'iniziale.json'); await dj.saveAs(jsonIniziale);
  try {
    await page.getByRole('tab', { name: /Gestione/ }).click();
    await pronto();

    // - Genera fascicoli vuoti: busta 9, dal 1 al 3
    await page.locator('#vuoti_busta').fill('9');
    await page.locator('#vuoti_da').fill('1');
    await page.locator('#vuoti_a').fill('3');
    await expect(page.locator('#vuoti_genera')).toBeVisible();
    await page.locator('#vuoti_genera').click();
    await expect(toast()).toContainText('Create 3 schede vuote', { timeout: 20000 });
    expect(conta()).toBe('27');
    expect(sql(db, 'SELECT group_concat(fascicolo) FROM (SELECT fascicolo FROM fascicoli WHERE busta=9 ORDER BY fascicolo)')).toBe('1,2,3');
    // ripetuta: i numeri già presenti non si toccano
    await page.locator('.shiny-notification-close').first().click().catch(() => {});
    await page.locator('#vuoti_genera').click();
    await expect(toast().last()).toContainText(/Create 0 schede vuote.*saltati i già presenti/, { timeout: 20000 });
    expect(conta()).toBe('27');
    await page.screenshot({ path: path.join(FOTO, '29-gestione-vuoti.png') });

    // - Note rapide: la riga 1 aggiorna la scheda vuota b. 9 fasc. 1, la 7 ne crea una nuova
    await page.locator('#note_busta').fill('9');
    await page.locator('#note_testo').fill('1 | Mario Rossi | 1623 | Eresia | sì | no | nota della prova\n7 | | 1631 | Censura | n.d. | sì |');
    await expect(page.locator('#note_anteprima')).toContainText('Mario Rossi', { timeout: 15000 });
    await page.locator('#note_anteprima').scrollIntoViewIfNeeded();
    await page.screenshot({ path: path.join(FOTO, '30-gestione-note-anteprima.png') });
    await page.locator('#note_applica').click();
    await expect.poll(conta, { timeout: 30000 }).toBe('28');
    const f1 = sql(db, 'SELECT * FROM fascicoli WHERE busta=9 AND fascicolo=1');
    expect(f1).toContain('nota della prova');
    expect(f1).toContain('1623');
    expect(sql(db, 'SELECT COUNT(*) FROM fascicoli WHERE busta=9 AND fascicolo=7')).toBe('1');
    // una copia «prima di…» è stata messa da parte
    expect(fs.readdirSync(path.join(path.dirname(db), 'backup')).some(f => /note-rapide/.test(f))).toBe(true);
    await expect(page.locator('#note_testo')).toHaveValue('');

    // - Ordine e segnature: busta 9; spostare giù il primo (ordine di consultazione), annullare
    await page.getByRole('tab', { name: 'Ordine e segnature' }).click();
    await page.locator('#ord_busta + .selectize-control .selectize-input').click();   // menu «selectize»: non è un select vero
    await page.locator('.selectize-dropdown-content .option', { hasText: /^Busta 9$/ }).click();
    await pronto();
    const righe = page.locator('#ord_elenco tbody tr');
    await expect(righe).toHaveCount(4);   // fascicoli 1, 2, 3 e 7
    await righe.first().locator('button[data-azione="giu"]').click();
    await expect(page.locator('#ord_esito')).toContainText('Ordine di consultazione salvato', { timeout: 15000 });
    const posizioni = () => sql(db, 'SELECT group_concat(f.fascicolo || \':\' || o.posizione) FROM (SELECT * FROM ordine_fascicoli) o JOIN fascicoli f ON f.id=o.fascicolo_id WHERE f.busta=9');
    expect(posizioni()).not.toBe('');
    const dopoSposta = posizioni();
    await page.locator('#ord_annulla').click();
    await expect(page.locator('#ord_esito')).toContainText('Ultimo spostamento annullato', { timeout: 15000 });
    expect(posizioni()).not.toBe(dopoSposta);

    // - Correzione di una segnatura: b. 9 fasc. 3 → fasc. 30; poi «Annulla ultima correzione»
    await righe.nth(2).locator('button[data-azione="correggi"]').click();
    const finestra = page.locator('.modal-dialog');
    await expect(finestra).toContainText('Correggi segnatura');
    const fascOriginale = (await finestra.locator('#corr_primo').inputValue());
    await finestra.locator('#corr_primo').fill('30');
    await expect(finestra.locator('#corr_anteprima')).toContainText('fasc. 30', { timeout: 15000 });
    await page.waitForTimeout(500);
    await page.screenshot({ path: path.join(FOTO, '31-gestione-correzione.png') });
    await finestra.locator('#corr_applica').click();
    await expect(page.locator('#ord_esito')).toContainText('Corrette 1 segnature', { timeout: 15000 });
    expect(sql(db, 'SELECT COUNT(*) FROM fascicoli WHERE busta=9 AND fascicolo=30')).toBe('1');
    expect(sql(db, `SELECT COUNT(*) FROM fascicoli WHERE busta=9 AND fascicolo=${fascOriginale}`)).toBe('0');
    await page.locator('#ord_annulla_correzione').click();
    await expect(page.locator('#ord_esito')).toContainText('Ultima correzione annullata', { timeout: 15000 });
    expect(sql(db, 'SELECT COUNT(*) FROM fascicoli WHERE busta=9 AND fascicolo=30')).toBe('0');
    expect(sql(db, `SELECT COUNT(*) FROM fascicoli WHERE busta=9 AND fascicolo=${fascOriginale}`)).toBe('1');
    // le schede della busta 1 non sono mai state toccate
    expect(sql(db, "SELECT titolo FROM fascicoli WHERE busta=1 AND fascicolo=1")).toBe('Processo contro Lucia Ferrante');
  } finally {
    await page.getByRole('tab', { name: /Import \/ Export/ }).click();
    await page.locator('#file_ripristino').setInputFiles(jsonIniziale);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
    await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
    await page.locator('#esegui_ripristino').click();
    await page.locator('.modal-dialog').getByRole('button', { name: 'Sostituisci con il backup' }).click();
    await expect.poll(conta, { timeout: 60000 }).toBe('24');
    expect(sql(db, 'SELECT COUNT(*) FROM fascicoli WHERE busta=9')).toBe('0');
  }
});

test('Qualità: «Controlla e correggi» sulle schede del fondo cambia solo ciò che è proposto', async ({ page }) => {
  test.setTimeout(240000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 });
  const db = (await page.evaluate(() => (document.querySelector('code.no-trad') || {}).textContent)).trim() + '/trama.sqlite';
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-qual-'));
  const finestra = page.locator('.modal-dialog');
  const luoghi = (b, f) => sql(db, `SELECT group_concat(l.luogo, '|') FROM (SELECT l.luogo FROM luoghi l JOIN fascicoli x ON x.id=l.fascicolo_id WHERE x.busta=${b} AND x.fascicolo=${f} ORDER BY l.ordine) l`);
  const altre = () => sql(db, "SELECT id, busta, fascicolo, titolo, regesto FROM fascicoli WHERE NOT (busta=2 AND fascicolo=14) AND NOT (busta=3 AND fascicolo=24) ORDER BY id");

  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  await apriBackup(page);
  await expect(page.locator('#scarica_backup')).toBeVisible();
  const [dj] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup:not(.disabled)').click()]);
  const jsonIniziale = path.join(dir, 'iniziale.json'); await dj.saveAs(jsonIniziale);
  try {
    expect(luoghi(2, 14)).toBe('Venezia|Tunisi');
    expect(luoghi(3, 24)).toBe('Bologna|Ginevra');
    const prima = altre();

    await page.getByRole('tab', { name: /Fascicoli/ }).first().click();
    await pronto();
    // «Non ora»: niente cambia. La fascia con «Controlla e correggi» compare scegliendo il filtro «Con errori» (ridisegno di Fascicoli, 26/10).
    await page.locator('#solo_errori').click();
    await page.locator('#apri_anomalie').click();
    await expect(finestra).toContainText('Controlla le schede del fondo', { timeout: 30000 });
    await finestra.locator('#rev_annulla').click();
    await expect(finestra).toBeHidden();
    expect(luoghi(2, 14)).toBe('Venezia|Tunisi');
    // «Non applicare nessuna» + «Correggi le schede»: niente cambia
    await page.locator('#apri_anomalie').click();
    await expect(finestra).toContainText('Controlla le schede del fondo', { timeout: 30000 });
    await finestra.locator('.rev-batch[data-on="0"]').click();
    await finestra.locator('#rev_conferma').click();
    await pronto();
    expect(luoghi(2, 14)).toBe('Venezia|Tunisi');
    expect(luoghi(3, 24)).toBe('Bologna|Ginevra');
    await page.waitForTimeout(1500);

    // «Applica tutte»: si correggono i luoghi delle due schede, e solo quelli
    await page.locator('#apri_anomalie').click();
    await expect(finestra).toContainText('Controlla le schede del fondo', { timeout: 30000 });
    await page.waitForTimeout(500);
    await page.screenshot({ path: path.join(FOTO, '32-qualita-correggi-schede.png') });
    await finestra.locator('.rev-batch[data-on="1"]').click();
    await finestra.locator('#rev_conferma').click();
    await expect.poll(() => luoghi(2, 14), { timeout: 30000 }).toBe('Venezia|Tunis (Tunisi)');
    expect(luoghi(3, 24)).toBe('Bologna|Genève (Ginevra)');
    expect(altre()).toBe(prima);
    expect(fs.readdirSync(path.join(path.dirname(db), 'backup')).some(f => /correzione-schede/.test(f))).toBe(true);
  } finally {
    await page.getByRole('tab', { name: /Import \/ Export/ }).click();
    await page.locator('#file_ripristino').setInputFiles(jsonIniziale);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
    await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
    await page.locator('#esegui_ripristino').click();
    await finestra.getByRole('button', { name: 'Sostituisci con il backup' }).click();
    await expect.poll(() => luoghi(2, 14), { timeout: 60000 }).toBe('Venezia|Tunisi');
  }
});
