// Verifica nel browser dei termini del tesauro dei reati (18/11/2026, D33, passo 3d): nella Scheda i termini compaiono solo sotto le classi scelte
// nel Soggettario, si spuntano, si salvano e tornano alla riapertura; la ricerca `reato:` li trova. Scrive nel database di esempio TEMPORANEO
// e alla fine lo rimette com'era (server proprio: playwright.importa.config.js).
const { test, expect } = require('@playwright/test');
const fs = require('fs'), path = require('path');
const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
    !document.querySelector('.shiny-notification .progress'), null, { timeout: 30000 });
  await page.waitForTimeout(400);
};
const cerca = async (page, testo) => {
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
  await page.locator('#cerca_fascicoli').fill(testo); await pronto(page);
  return page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta):not(:has(td.dataTables_empty))');
};
const apriScheda = async (page, testo) => {
  const righe = await cerca(page, testo);
  await expect(righe.first()).toBeVisible();
  await righe.first().locator('td').nth(2).click();
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 });
  await pronto(page);
};
const pillola = (page, classe) => page.locator('.campo-pillole[data-per="reato"] .pillola[data-valore="' + classe + '"]');
const termine = (page, nome) => page.locator('.campo-termini .checkbox, .campo-termini .form-check').filter({ hasText: nome });

test('I termini del tesauro: si vedono sotto la classe, si salvano, la ricerca li trova', async ({ page }) => {
  test.setTimeout(180000);
  const errori = []; page.on('pageerror', e => errori.push(String(e)));
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await apriScheda(page, 'Elisabetta Conti');

  // La scheda ha «Crimini sessuali»: i suoi termini si vedono, quelli di «Magia» no.
  await expect(pillola(page, 'Crimini sessuali')).toHaveClass(/scelta/);
  await expect(termine(page, 'Bigamia')).toBeVisible();
  await expect(termine(page, 'Astrologia')).toBeHidden();
  await page.locator('.campo-termini').scrollIntoViewIfNeeded(); await page.waitForTimeout(800);
  await page.screenshot({ path: path.join(FOTO, '40-termini-scheda.png') });

  // Una classe in più mostra i suoi termini; tolta, li nasconde.
  await pillola(page, 'Magia').click(); await pronto(page);
  await expect(termine(page, 'Astrologia')).toBeVisible();
  await pillola(page, 'Magia').click(); await pronto(page);
  await expect(termine(page, 'Astrologia')).toBeHidden();

  // Si spunta «Bigamia» e si salva: la scheda ha una modifica da salvare, poi non più.
  await termine(page, 'Bigamia').locator('input').check(); await pronto(page);
  await page.locator('#salva').click(); await pronto(page);
  await expect(page.locator('.shiny-notification-error')).toHaveCount(0);

  // Alla riapertura il termine è ancora spuntato; l'elenco lo trova con reato:bigamia (e non con un termine che la scheda non ha).
  await apriScheda(page, 'Elisabetta Conti');
  await expect(termine(page, 'Bigamia').locator('input')).toBeChecked();
  let righe = await cerca(page, 'reato:bigamia');
  await expect(righe).toHaveCount(1);
  await page.screenshot({ path: path.join(FOTO, '41-termini-ricerca.png') });
  righe = await cerca(page, 'reato:astrologia');
  await expect(righe).toHaveCount(0);

  // Si rimette il database com'era.
  await apriScheda(page, 'Elisabetta Conti');
  await termine(page, 'Bigamia').locator('input').uncheck(); await pronto(page);
  await page.locator('#salva').click(); await pronto(page);
  righe = await cerca(page, 'reato:bigamia');
  await expect(righe).toHaveCount(0);
  expect(errori, 'errori JS nella pagina').toEqual([]);
});
