// Verifica nel browser vero (24/10/2026): la finestra del fondo e il pulsante «Usa <anno>».
//   cd verifiche/browser && npm run prove
// L'app parte (config accanto) su un database di esempio TEMPORANEO; qui si provano i gesti e si lasciano gli screenshot in
// SCREENSHOT_DIR (predefinito: verifiche/browser/schermate, ignorata da git).
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

test('la finestra del fondo non si sovrappone alla barra della scheda', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.screenshot({ path: path.join(FOTO, '1-barra-con-fondo.png') });

  await page.locator('#fondo_apri').click();
  const finestra = page.locator('.modal-dialog');
  await expect(finestra).toBeVisible();
  await expect(finestra).toContainText('Scegli il fondo');
  await expect(finestra.locator('.riga-fondo').first()).toBeVisible();
  await page.screenshot({ path: path.join(FOTO, '2-finestra-fondo.png') });

  // si sceglie «(tutti i fondi)»: la finestra si chiude e la barra lo dice
  await finestra.locator('.riga-fondo', { hasText: 'tutti i fondi' }).click();
  await expect(finestra).toBeHidden();
  await expect(page.locator('#fondo_etichetta')).toContainText('tutti i fondi');
  await page.screenshot({ path: path.join(FOTO, '3-dopo-la-scelta.png') });
});

test('«Usa <anno>» sta in una riga sua, sotto i campi', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('button', { name: /Nuovo fascicolo/ }).first().click();
  await expect(page.getByRole('heading', { name: 'Nuovo fascicolo' })).toBeVisible();
  // l'anno si propone in base alla scheda precedente: servono busta e fascicolo (nel fondo di esempio la b. 3 ha già schede)
  await page.locator('#busta').fill('3'); await page.locator('#fascicolo').fill('99');
  await page.locator('#fascicolo').blur();
  await expect(page.locator('.proposta-anno a.pillola')).toBeVisible({ timeout: 15000 });
  const usa = page.locator('.proposta-anno a.pillola');
  await page.locator('.proposta-anno').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '4-scheda-anno.png') });
  const riga = await page.locator('.proposta-anno').boundingBox();
  const mese = page.getByText(/Aggiungi mese e giorno/i).first();
  const m = await mese.boundingBox();
  expect(riga.y >= m.y + m.height || riga.y + riga.height <= m.y).toBeTruthy();     // non si sovrappongono
  await usa.click();
  await expect(page.locator('#estremo_remoto')).not.toHaveValue('');                 // il pulsante riempie l'anno
});
