// Macchina cancellazione (docs/STATI.md), il percorso intero (18/11/2026): pronta -> eseguita. «Elimina il fondo» cancella davvero, e PRIMA fa la copia di sicurezza
// (cartella backup, file «eliminazione-fondo»). Database di esempio TEMPORANEO, server proprio (playwright.importa.config.js); a fine prova ripristino da JSON.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');
const { apriBackup } = require('./aiuti-pagina');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' }).trim();

for (const caso of [
  { nome: 'Elimina un fondo', apri: '#canc_fondo_apri', titolo: 'Eliminare questo fondo?', pulsante: 'Elimina il fondo', avviso: 'Fondo eliminato', copia: /eliminazione-fondo/, fondi: false },
  { nome: 'Svuota tutto (tenendo le impostazioni)', apri: '#canc_tutto_apri', titolo: 'Svuotare tutto il database?', pulsante: 'Svuota tutto', avviso: 'Database svuotato', copia: /svuotamento/, fondi: true },
]) test(`${caso.nome} fino in fondo: copia di sicurezza prima, poi la cancellazione; il ripristino rimette tutto`, async ({ page }) => {
  test.setTimeout(240000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = async () => { await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 }); await page.waitForTimeout(400); };
  const casa = (await page.evaluate(() => document.querySelector('code.no-trad').textContent)).trim();
  const db = casa + '/trama.sqlite';
  const finestra = page.locator('.modal-dialog');
  const copie = () => fs.existsSync(path.join(casa, 'backup')) ? fs.readdirSync(path.join(casa, 'backup')).filter(f => caso.copia.test(f)) : [];
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-canc-'));

  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  await apriBackup(page);
  await expect(page.locator('#scarica_backup')).toBeVisible();
  const [dj] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup:not(.disabled)').click()]);
  const jsonIniziale = path.join(dir, 'iniziale.json'); await dj.saveAs(jsonIniziale);
  const n = sql(db, 'SELECT count(*) FROM fascicoli'), schede = sql(db, 'SELECT id, busta, fascicolo, regesto FROM fascicoli ORDER BY id');
  const copiePrima = copie().length, impostazioni = sql(db, "SELECT count(*) FROM impostazioni WHERE chiave <> 'fondo_attivo'");
  try {
    await page.getByRole('tab', { name: /Gestione/ }).click(); await pronto();
    await page.locator(caso.apri).evaluate(e => e.click());
    await expect(page.getByText(caso.titolo)).toBeVisible({ timeout: 10000 });
    await page.getByLabel('Non serve: mi basta la copia di sicurezza automatica').check();
    await page.locator('#canc_conferma').fill(n);
    await page.locator('#canc_applica').click();
    await expect(page.locator('.shiny-notification', { hasText: caso.avviso })).toBeVisible({ timeout: 30000 });
    expect(sql(db, 'SELECT count(*) FROM fascicoli'), 'le schede del fondo non ci sono più').toBe('0');
    expect(copie().length, 'la copia di sicurezza è stata fatta').toBe(copiePrima + 1);
    if (caso.fondi) {
      expect(sql(db, 'SELECT count(*) FROM fondi'), 'i fondi non ci sono più').toBe('0');
      expect(sql(db, "SELECT count(*) FROM impostazioni WHERE chiave <> 'fondo_attivo'"), 'le impostazioni sono rimaste').toBe(impostazioni);
    }
  } finally {
    await page.getByRole('tab', { name: /Import \/ Export/ }).click();
    if (!(await page.locator('#modo_ripristino').isVisible())) await apriBackup(page);
    await page.locator('#file_ripristino').setInputFiles(jsonIniziale);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
    await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
    await page.locator('#esegui_ripristino').click();
    await finestra.getByRole('button', { name: 'Sostituisci con il backup' }).click();
    await expect.poll(() => sql(db, 'SELECT id, busta, fascicolo, regesto FROM fascicoli ORDER BY id'), { timeout: 60000 }).toBe(schede);
  }
});

// Invariante: «La copia di sicurezza è fatta prima della cancellazione; se la copia fallisce, non si cancella.» Si rende la cartella dei backup non scrivibile.
test('Se la copia di sicurezza non riesce, il fondo non viene cancellato', async ({ page }) => {
  test.setTimeout(120000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = async () => { await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 }); await page.waitForTimeout(400); };
  const casa = (await page.evaluate(() => document.querySelector('code.no-trad').textContent)).trim();
  const db = casa + '/trama.sqlite', cartella = path.join(casa, 'backup');
  fs.mkdirSync(cartella, { recursive: true });
  const modo = fs.statSync(cartella).mode & 0o777;
  const n = sql(db, 'SELECT count(*) FROM fascicoli'), schede = sql(db, 'SELECT id, busta, fascicolo, regesto FROM fascicoli ORDER BY id');
  try {
    fs.chmodSync(cartella, 0o500);
    if (fs.accessSync && (() => { try { fs.accessSync(cartella, fs.constants.W_OK); return true; } catch (e) { return false; } })()) test.skip(true, 'la cartella resta scrivibile (utente con tutti i permessi)');
    await page.getByRole('tab', { name: /Gestione/ }).click(); await pronto();
    await page.locator('#canc_fondo_apri').evaluate(e => e.click());
    await expect(page.getByText('Eliminare questo fondo?')).toBeVisible({ timeout: 10000 });
    await page.getByLabel('Non serve: mi basta la copia di sicurezza automatica').check();
    await page.locator('#canc_conferma').fill(n);
    await page.locator('#canc_applica').click();
    await expect(page.locator('.shiny-notification-error'), 'l\'errore è detto a schermo').toBeVisible({ timeout: 15000 });
    await pronto(); await page.waitForTimeout(500);
    expect(sql(db, 'SELECT id, busta, fascicolo, regesto FROM fascicoli ORDER BY id'), 'senza copia di sicurezza non si cancella nulla').toBe(schede);
    await expect(page.getByText('Fondo eliminato')).toHaveCount(0);
  } finally {
    fs.chmodSync(cartella, modo);
  }
});
