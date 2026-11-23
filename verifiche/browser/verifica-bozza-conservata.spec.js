// Macchina scheda_aperta (docs/STATI.md), ramo «I dati sono cambiati altrove» (18/11/2026): con una bozza in corso, un'azione che ricarica i dati
// (qui «Ammetti per il fondo» in Qualità → Per colonna) non sovrascrive la bozza: compare l'avviso, tornando alla Scheda la bozza c'è ancora,
// e «Annulla» la butta. Database di esempio TEMPORANEO, server proprio (playwright.importa.config.js); a fine prova si rimette com'era.
const { test, expect } = require('@playwright/test');
const { execFileSync } = require('child_process');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' }).trim();

test('Bozza in corso e dati ricaricati altrove: la bozza resta, con l\'avviso', async ({ page }) => {
  test.setTimeout(180000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = async () => { await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 }); await page.waitForTimeout(400); };
  const db = (await page.evaluate(() => document.querySelector('code.no-trad').textContent)).trim() + '/trama.sqlite';
  const riga = v => page.locator('.colonne-tabella tr', { has: page.locator('td', { hasText: new RegExp('^' + v + '$') }) });
  const iniziale = sql(db, 'SELECT fascicolo_id, reato FROM reati ORDER BY fascicolo_id, reato');
  try {
    sql(db, "UPDATE reati SET reato='Stregoneria' WHERE rowid IN (SELECT rowid FROM reati WHERE reato='Santità' LIMIT 1)");
    await page.reload();
    await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
    await pronto(); await page.waitForTimeout(1500);

    // una scheda aperta, con una bozza
    await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto();
    const righe = page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
    await expect(righe.first()).toBeVisible({ timeout: 20000 });
    await righe.first().locator('td').nth(2).click();
    await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 });
    await pronto(); await page.waitForTimeout(2500);
    const originale = await page.locator('#note').inputValue();
    await page.locator('#note').fill('BOZZA CHE DEVE RESTARE');
    await expect(page.locator('#salva_succ')).toContainText(/Salva e prossimo/, { timeout: 10000 });

    // un'azione altrove che ricarica i dati
    await page.getByRole('tab', { name: /Qualità/ }).first().click();
    await page.getByRole('tab', { name: 'Per colonna' }).click(); await pronto();
    await expect(riga('Stregoneria')).toBeVisible({ timeout: 20000 });
    await riga('Stregoneria').getByRole('button', { name: 'Ammetti per il fondo' }).click();
    await expect(page.locator('.shiny-notification', { hasText: 'I dati sono cambiati altrove' })).toBeVisible({ timeout: 15000 });
    await riga('Stregoneria').getByRole('button', { name: 'Togli la regola' }).click();
    await expect(riga('Stregoneria').getByRole('button', { name: 'Ammetti per il fondo' })).toBeVisible({ timeout: 15000 });

    // tornando alla scheda la bozza c'è ancora; Annulla la butta (passa dalla guardia)
    for (let i = 0; i < 5 && !(await page.locator('#regesto').isVisible()); i++) { await page.goBack(); await pronto(); }   // la Scheda non è nella barra: ci si torna con Indietro
    await expect(page.locator('#regesto')).toBeVisible({ timeout: 10000 });
    expect(await page.locator('#note').inputValue(), 'la bozza non è stata sovrascritta').toBe('BOZZA CHE DEVE RESTARE');
    await page.locator('#annulla_scheda').evaluate(e => e.click());
    await page.getByRole('button', { name: /Scarta e continua/ }).click({ timeout: 10000 });
    await pronto();
    await expect(page.locator('#note')).toHaveValue(originale, { timeout: 10000 });
  } finally {
    sql(db, "UPDATE reati SET reato='Santità' WHERE reato='Stregoneria'");
    sql(db, "DELETE FROM vocabolario_fondo WHERE valore='Stregoneria'");
  }
  expect(sql(db, 'SELECT fascicolo_id, reato FROM reati ORDER BY fascicolo_id, reato'), 'i dati tornano com\'erano').toBe(iniziale);
});
