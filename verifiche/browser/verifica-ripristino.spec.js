// Verifica nel browser del ripristino da JSON (Import / Export): accoda, sostituisci con conferma, Annulla.
// Database di esempio TEMPORANEO (vedi playwright.config.js); gli screenshot vanno in SCREENSHOT_DIR.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { apriBackup } = require('./aiuti-pagina');

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

test('ripristino da JSON: accoda, annulla, sostituisci', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();

  await apriBackup(page);
  const bottone = page.locator('#scarica_backup:not(.disabled)');
  const [dl] = await Promise.all([page.waitForEvent('download'), bottone.click()]);
  const json = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'trama-rip-')), 'backup.json');
  await dl.saveAs(json);
  expect(fs.statSync(json).size).toBeGreaterThan(100);

  const carica = async () => {
    await page.locator('#file_ripristino').setInputFiles(json);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
  };

  // senza file
  await page.locator('#esegui_ripristino').click();
  await expect(page.locator('.shiny-notification')).toContainText('Nessun file scelto');
  await page.locator('.shiny-notification-close').click();
  await expect(page.locator('.shiny-notification')).toHaveCount(0);

  // accoda: tutto già presente
  await carica();
  await page.locator('#modo_ripristino').getByLabel('Accoda ai fascicoli esistenti').check();
  await page.locator('#esegui_ripristino').click();
  const esito = page.locator('#esito_ripristino .alert');
  await expect(esito).toContainText('0 importati', { timeout: 30000 });
  await expect(esito).toContainText('Copia di sicurezza');
  await expect(esito).toBeInViewport();            // l'esito si porta in vista da solo
  await expect(page.locator('.shiny-notification')).toHaveCount(0);   // niente avviso che ripete l'esito
  await esito.scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '5-ripristino-accoda.png') });

  // sostituisci + Annulla: nessuna modifica
  await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
  await page.locator('#esegui_ripristino').click();
  const finestra = page.locator('.modal-dialog');
  await expect(finestra).toContainText('Sostituire i dati con il backup?');
  await expect(finestra).toContainText('backup.json');
  await page.screenshot({ path: path.join(FOTO, '6-ripristino-conferma.png') });
  await finestra.getByRole('button', { name: 'Annulla' }).click();
  await expect(finestra).toBeHidden();

  // sostituisci + conferma
  await page.locator('#esegui_ripristino').click();
  await finestra.getByRole('button', { name: 'Sostituisci con il backup' }).click();
  await expect(finestra).toBeHidden();
  await expect(page.locator('#esito_ripristino .alert')).not.toContainText('0 importati', { timeout: 30000 });
  await expect(page.locator('#esito_ripristino .alert')).toContainText('0 già presenti');
  await page.locator('#esito_ripristino').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '7-ripristino-sostituito.png') });

  // dopo la sostituzione l'app è ancora viva
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click();
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i);
});
