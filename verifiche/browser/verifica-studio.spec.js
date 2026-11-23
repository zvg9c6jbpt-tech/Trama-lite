// Studio contemporaneo: la navigazione resta raggiungibile su ogni schermo;
// ridimensionamento e cambio tema/lingua conservano campi e bozza della Scheda.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), path = require('path');
const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');
const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'));
};
test.beforeEach(async ({ page }) => {
  await page.setViewportSize({ width: 1440, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText('Tribunale di esempio', { timeout: 60000 });
  await pronto(page);
  fs.mkdirSync(FOTO, { recursive: true });
});

test('Studio: navigazione e contenuti accessibili su desktop e finestra stretta', async ({ page }) => {
  test.setTimeout(120000);
  for (const width of [1440, 1024, 390]) {
    await page.setViewportSize({ width, height: 1000 });
    for (const value of ['Home', 'Fascicoli', 'Luoghi citati', 'Qualità', 'Gestione', 'Import / Export']) {
      const tab = page.locator(`.navbar .nav-link[data-value="${value}"]`);
      await tab.scrollIntoViewIfNeeded();
      await tab.click();
      await expect(page.locator(`.tab-pane.active[data-value="${value}"]`)).toBeVisible();
      await pronto(page);
      expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1), `${value}: pagina larga ${width}px`).toBe(true);
    }
  }
});

test('Studio: tutti i campi e la bozza sopravvivono a schermo, tema e lingua', async ({ page }) => {
  test.setTimeout(120000);
  await page.locator('.navbar .nav-link[data-value="Fascicoli"]').click();
  await expect(page.locator('#cerca_fascicoli')).toBeVisible();
  await page.locator('#cerca_fascicoli').fill('Marco Fontana');
  // La ricerca parte con un ritardo: si aspetta che l'elenco sia già filtrato (nessuna scheda senza il nome cercato; le righe di gruppo, con una sola cella, non contano), non che Shiny sia libero.
  const righe = page.locator('.tab-pane[data-value="Fascicoli"] tbody tr');
  await expect.poll(() => righe.evaluateAll(rs => rs.filter(r => r.querySelectorAll('td').length > 2 && !/Marco Fontana/.test(r.textContent)).length), { timeout: 30000 }).toBe(0);
  const row = righe.filter({ hasText: 'Marco Fontana' }).first();
  await expect(row).toBeVisible();
  // Sotto carico (suite intera) il primo clic può perdersi: si riclicca solo finché la Scheda non è aperta, quindi mai due volte su una Scheda già aperta.
  await expect(async () => {
    if (!(await page.locator('#regesto').isVisible())) await row.locator('td').nth(2).click({ timeout: 5000 });
    await expect(page.locator('#regesto')).toBeVisible({ timeout: 5000 });
  }).toPass({ timeout: 60000 });
  await pronto(page);
  const campi = () => page.locator('#modulo input[id], #modulo select[id], #modulo textarea[id]').evaluateAll(es => es.map(e => e.id).sort());
  const prima = await campi();
  expect(prima.length).toBeGreaterThan(30);
  await page.locator('#regesto').focus();
  await expect(page.locator('#regesto')).toHaveCSS('outline-style', 'solid');
  await expect(page.locator('#regesto')).toHaveCSS('outline-width', '3px');
  const nota = 'Bozza conservata durante il restyling: à è ì ò ù.';
  await page.locator('#note').fill(nota);
  await expect(page.locator('#salva_succ')).toContainText('Salva e prossimo');
  for (const width of [1440, 1280, 1024, 820, 390]) {
    await page.setViewportSize({ width, height: 1000 });
    await expect(page.locator('#note')).toHaveValue(nota);
    expect(await campi()).toEqual(prima);
    await page.evaluate(() => window.scrollTo(0, 0));
    await page.screenshot({ path: path.join(FOTO, `studio-scheda-${width}.png`) });
  }
  await page.setViewportSize({ width: 1440, height: 1000 });
  for (const [id, nome] of [['scuro', 'scuro'], ['inglese', 'inglese']]) {
    await page.locator('#menu_preferenze').click();
    await page.locator(`#${id}`).evaluate(e => e.click());
    await page.locator('#menu_preferenze').click();
    await pronto(page);
    if (id === 'inglese') await expect(page.locator('html')).toHaveAttribute('lang', 'en');
    else await expect.poll(() => page.locator('body').evaluate(e => getComputedStyle(e).backgroundColor)).toBe('rgb(22, 18, 20)');
    await expect(page.locator('#note')).toHaveValue(nota);
    expect(await campi()).toEqual(prima);
    await page.evaluate(() => window.scrollTo(0, 0));
    await page.screenshot({ path: path.join(FOTO, `studio-scheda-${nome}.png`) });
    await page.locator('#menu_preferenze').click();
    await page.locator(`#${id}`).evaluate(e => e.click());
    await page.locator('#menu_preferenze').click();
    await pronto(page);
    if (id === 'scuro') await expect.poll(() => page.locator('body').evaluate(e => getComputedStyle(e).backgroundColor)).toBe('rgb(239, 237, 235)');
    else await expect(page.locator('html')).toHaveAttribute('lang', 'it');
  }
  await page.locator('#annulla_scheda').evaluate(e => e.click());
});
