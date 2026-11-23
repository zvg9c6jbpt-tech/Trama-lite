// Ricerca e clic su una riga (24/10/2026). Si scrive nella ricerca di Fascicoli e si clicca SUBITO una riga, prima che l'elenco
// si sia ridisegnato col filtro: si apre la scheda della riga cliccata (quella vista in quel momento) e intestazione, campi,
// avviso di salvataggio e scheda scritta sono la stessa; nel database cambia soltanto quella. Database di esempio TEMPORANEO.
// (Sospetto del 24/10 su un'incoerenza: era un errore di misura della prova, non dell'app; vedi l'handoff.)
const { test, expect } = require('@playwright/test');
const { execFileSync } = require('child_process');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' });

test('clic su una riga subito dopo la ricerca: la scheda aperta è una sola e il salvataggio scrive solo quella', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click();
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'));
  await expect(page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)').first()).toBeVisible({ timeout: 20000 });   // è il primo test: l'app parte a freddo
  const casa = (await page.evaluate(() => document.querySelector('code.no-trad').innerText)).trim();
  const db = casa + '/trama.sqlite';
  const prima = sql(db, "SELECT id, busta, fascicolo, titolo, regesto FROM fascicoli ORDER BY id");

  // niente attese tra la ricerca e il clic: è proprio la situazione da provare
  await page.locator('#cerca_fascicoli').fill('Francesca Pini');
  await page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)').first().locator('td').nth(2).dispatchEvent('click');   // sull'elemento, non su un punto: una riga che si sposta mentre la tabella si ridisegna non deve far cadere il clic (la geometria cambia con font e spaziature)
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 });
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'));
  await page.waitForTimeout(2500);   // tutto si è assestato: da qui in poi niente più «corsa»

  const campi = (await page.locator('#busta').inputValue()) + '/' + (await page.locator('#fascicolo').inputValue());
  const testata = await page.locator('.testa-titolo').innerText();   // NON un testo qualunque «b. N, fasc. M»: anche la tabella nascosta ne ha
  const [, b, f] = testata.match(/b\. (\d+), fasc\. (\d+)/);
  expect(`${b}/${f}`, 'intestazione e campi della scheda devono dire lo stesso fascicolo').toBe(campi);

  await page.locator('#regesto').fill('PROVA CLIC ANTICIPATO');
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.shiny-notification')).toContainText('Salvata', { timeout: 10000 });
  await expect(page.locator('.shiny-notification')).toContainText(`b. ${b}, fasc. ${f}`);   // l'avviso dice la scheda mostrata

  const scritti = sql(db, "SELECT busta||'/'||fascicolo FROM fascicoli WHERE regesto = 'PROVA CLIC ANTICIPATO'").trim();
  expect(scritti, 'il regesto nuovo sta solo nella scheda mostrata').toBe(campi);

  // tutte le altre schede, identiche
  const dopo = sql(db, "SELECT id, busta, fascicolo, titolo, regesto FROM fascicoli ORDER BY id");
  const righePrima = prima.trim().split('\n'), righeDopo = dopo.trim().split('\n');
  const cambiate = righeDopo.filter((r, i) => r !== righePrima[i]);
  expect(cambiate.length, 'schede cambiate nel database').toBe(1);
  expect(cambiate[0]).toContain('PROVA CLIC ANTICIPATO');
});
