// Verifica nel browser di Qualità → Per colonna (decisioni sui valori): ammetti per il fondo, togli la regola, correggi in tutte le schede,
// «non è un valore: elimina». Ogni passo si controlla nel database. Database di esempio TEMPORANEO, server proprio
// (playwright.importa.config.js); a fine prova il database torna com'era con il ripristino da JSON.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');
const { apriBackup } = require('./aiuti-pagina');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' }).trim();

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

test('Qualità → Per colonna: ammetti, togli la regola, correggi, elimina', async ({ page }) => {
  test.setTimeout(240000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  const errori = [];
  page.on('pageerror', e => errori.push('JS: ' + e.message));
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 });
  const db = (await page.evaluate(() => (document.querySelector('code.no-trad') || {}).textContent)).trim() + '/trama.sqlite';
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-col-'));
  const finestra = page.locator('.modal-dialog');
  const reati = v => sql(db, `SELECT COUNT(*) FROM reati WHERE reato='${v}'`);
  const fascicoliDi = v => sql(db, `SELECT group_concat(fascicolo_id) FROM (SELECT fascicolo_id FROM reati WHERE reato='${v}' ORDER BY fascicolo_id)`);
  const riga = valore => page.locator('.colonne-tabella tr', { has: page.locator('td', { hasText: new RegExp('^' + valore + '$') }) });

  // copia iniziale (JSON) per rimettere tutto com'era
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  await apriBackup(page);
  await expect(page.locator('#scarica_backup')).toBeVisible();
  const [dj] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup:not(.disabled)').click()]);
  const jsonIniziale = path.join(dir, 'iniziale.json'); await dj.saveAs(jsonIniziale);
  const iniziale = sql(db, 'SELECT fascicolo_id, reato FROM reati ORDER BY fascicolo_id, reato');
  try {
    // dati di prova: due schede con «Eresìa» (vicino a Eresia), una con «magia» (solo maiuscola), una con «Stregoneria» (valore nuovo)
    sql(db, "UPDATE reati SET reato='Eresìa' WHERE rowid IN (SELECT rowid FROM reati WHERE reato='Eresia' ORDER BY fascicolo_id LIMIT 2)");
    sql(db, "UPDATE reati SET reato='magia' WHERE rowid IN (SELECT rowid FROM reati WHERE reato='Magia' ORDER BY fascicolo_id LIMIT 1)");
    sql(db, "UPDATE reati SET reato='Stregoneria' WHERE rowid IN (SELECT rowid FROM reati WHERE reato='Santità' LIMIT 1)");
    const eresia2 = fascicoliDi('Eresìa'), n_eresia = reati('Eresia');
    expect(reati('Eresìa')).toBe('2');
    await page.reload();
    await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });

    await pronto(); await page.waitForTimeout(1000);
    await page.getByRole('tab', { name: /Qualità/ }).first().click();
    await expect(page.getByRole('tab', { name: 'Per colonna' })).toBeVisible();
    await page.getByRole('tab', { name: 'Per colonna' }).click();
    await pronto();
    await expect(riga('Eresìa')).toBeVisible({ timeout: 20000 });
    await expect(riga('Stregoneria')).toBeVisible();
    await expect(riga('magia')).toBeVisible();
    await expect(riga('Eresìa')).toContainText('Eresia');                       // la forma giusta proposta
    await page.waitForTimeout(500);
    await page.screenshot({ path: path.join(FOTO, '33-colonne-valori.png') });

    // - Ammetti per il fondo: il database delle schede non cambia, il valore diventa «ammesso» e si può togliere
    await riga('Stregoneria').getByRole('button', { name: 'Ammetti per il fondo' }).click();
    await expect(page.locator('.shiny-notification', { hasText: 'Ammesso per il fondo' }).first()).toBeVisible({ timeout: 15000 });
    await expect(riga('Stregoneria').getByRole('button', { name: 'Togli la regola' })).toBeVisible();
    await expect(riga('Stregoneria').getByRole('button', { name: 'Ammetti per il fondo' })).toHaveCount(0);
    expect(reati('Stregoneria')).toBe('1');
    expect(sql(db, "SELECT COUNT(*) FROM vocabolario_fondo WHERE valore='Stregoneria'")).toBe('1');
    await riga('Stregoneria').getByRole('button', { name: 'Togli la regola' }).click();
    await expect(riga('Stregoneria').getByRole('button', { name: 'Ammetti per il fondo' })).toBeVisible({ timeout: 15000 });
    expect(sql(db, "SELECT COUNT(*) FROM vocabolario_fondo WHERE valore='Stregoneria'")).toBe('0');
    expect(reati('Stregoneria')).toBe('1');

    // - Correggi «Eresìa» → «Eresia»: anteprima, poi Applica; si riscrive nelle due schede, copia «valori» messa da parte
    await riga('Eresìa').getByRole('button', { name: 'Correggi…' }).click();
    await expect(finestra).toContainText('Correggi un valore');
    await expect(finestra.locator('#val_forma')).toHaveValue('Eresia');
    await expect(finestra).toContainText('Cambiano 2 schede', { timeout: 15000 });
    await page.screenshot({ path: path.join(FOTO, '34-colonne-correggi.png') });
    await finestra.getByRole('button', { name: 'Annulla' }).click();            // annullata: niente cambia
    await expect(finestra).toBeHidden();
    expect(reati('Eresìa')).toBe('2');
    await riga('Eresìa').getByRole('button', { name: 'Correggi…' }).click();
    await expect(finestra).toContainText('Cambiano 2 schede', { timeout: 15000 });
    await finestra.getByRole('button', { name: 'Applica' }).click();
    await expect(page.locator('.shiny-notification', { hasText: 'Corrette 2 schede' }).first()).toBeVisible({ timeout: 30000 });
    await pronto();
    expect(reati('Eresìa')).toBe('0');
    expect(reati('Eresia')).toBe(String(Number(n_eresia) + 2));
    expect(fascicoliDi('Eresia')).toContain(eresia2.split(',')[0]);
    expect(fs.readdirSync(path.join(path.dirname(db), 'backup')).some(f => /valori/.test(f))).toBe(true);
    await expect(riga('Eresìa')).toHaveCount(0);

    // - «magia» (solo maiuscola): la forma proposta è «Magia»
    await expect(riga('magia')).toContainText('Magia');
    await riga('magia').getByRole('button', { name: 'Correggi…' }).click();
    await expect(finestra.locator('#val_forma')).toHaveValue('Magia');
    await expect(finestra).toContainText('Cambia 1 scheda', { timeout: 15000 });
    await finestra.getByRole('button', { name: 'Applica' }).click();
    await expect.poll(() => reati('magia'), { timeout: 30000 }).toBe('0');
    await pronto();
    expect(reati('magia')).toBe('0');

    // - «Non è un valore: elimina»: Stregoneria sparisce dalla scheda, e solo da quella
    const totale = sql(db, 'SELECT COUNT(*) FROM reati');
    await riga('Stregoneria').getByRole('button', { name: /elimina/ }).click();
    await expect(finestra.locator('#val_forma')).not.toHaveValue('');
    await expect(finestra).toContainText('Cambia 1 scheda', { timeout: 15000 });
    await finestra.getByRole('button', { name: 'Applica' }).click();
    await expect.poll(() => reati('Stregoneria'), { timeout: 30000 }).toBe('0');
    await pronto();
    expect(reati('Stregoneria')).toBe('0');
    expect(sql(db, 'SELECT COUNT(*) FROM reati')).toBe(String(Number(totale) - 1));
    await page.screenshot({ path: path.join(FOTO, '35-colonne-dopo.png') });
    expect(errori).toEqual([]);
  } finally {
    await page.getByRole('tab', { name: /Import \/ Export/ }).click();
    if (!(await page.locator('#modo_ripristino').isVisible())) await apriBackup(page);
    await page.locator('#file_ripristino').setInputFiles(jsonIniziale);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
    await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
    await page.locator('#esegui_ripristino').click();
    await finestra.getByRole('button', { name: 'Sostituisci con il backup' }).click();
    await expect.poll(() => sql(db, 'SELECT fascicolo_id, reato FROM reati ORDER BY fascicolo_id, reato'), { timeout: 60000 }).toBe(iniziale);
  }
});
