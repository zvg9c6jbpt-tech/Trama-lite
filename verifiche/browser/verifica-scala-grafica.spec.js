// Verifica nel browser della revisione grafica del 29/10/2026: una scala di testo, un titolo per pagina, la voce attiva del menu
// sempre riempita, un'altezza di pulsante (44 px), il passo spento leggibile, i luoghi più citati come barre. Su database di esempio TEMPORANEO.
const { test, expect } = require('@playwright/test');

const SCALA = [24, 18, 16, 14, 13];   // titoli di pagina e numeri, di blocco, di riquadro, testo corrente, micro-etichette
const PAGINE = ['Home', 'Fascicoli', 'Luoghi', 'Import', 'Qualità', 'Gestione'];

let errori;
test.beforeEach(async ({ page }) => {
  page.setDefaultTimeout(10000);
  errori = [];
  page.on('pageerror', e => errori.push('JS: ' + e.message));
  page.on('console', m => { if (m.type() === 'error' && !/favicon|Failed to load resource/.test(m.text())) errori.push('console: ' + m.text()); });
  await page.setViewportSize({ width: 1440, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await pronto(page);
});
test.afterEach(async ({ page }) => {
  expect(await page.locator('.shiny-output-error').count(), 'uscite di Shiny in errore').toBe(0);
  expect(errori).toEqual([]);
});
const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') && !document.querySelector('.shiny-notification .progress'), null, { timeout: 30000 });
  await page.waitForTimeout(300);
};
const pagina = async (page, nome) => { await page.getByRole('tab', { name: new RegExp(nome) }).first().click(); await pronto(page); };

// i testi che si leggono davvero: salta il testo di misura di selectize e il sigillo del marchio (nascosto di proposito)
const fuoriScala = page => page.evaluate(scala => {
  const fuori = [];
  document.querySelectorAll('body *').forEach(e => {
    if (e.offsetParent === null || e instanceof SVGElement || e.closest('.selectize-input, [style*="visibility: hidden"]') && !e.classList.contains('item')) return;
    if (![...e.childNodes].some(c => c.nodeType === 3 && c.textContent.trim())) return;
    if (e.tagName === 'SPAN' && e.parentElement.tagName === 'DIV' && !e.className && getComputedStyle(e).position === 'absolute') return;
    const px = parseFloat(getComputedStyle(e).fontSize);
    if (!scala.includes(px)) fuori.push(px + 'px <' + e.tagName.toLowerCase() + ' ' + String(e.className).slice(0, 40) + '> ' + e.textContent.trim().slice(0, 30));
  });
  return fuori;
}, SCALA);

test('Scala: in ogni pagina il testo sta sulle cinque misure, in chiaro e in scuro', async ({ page }) => {
  for (const scuro of [false, true]) {
    await page.locator('#menu_preferenze').click();
    await page.locator('#scuro').evaluate((e, v) => { if (e.checked !== v) e.click(); }, scuro);
    await page.locator('#menu_preferenze').click(); await pronto(page); await page.waitForTimeout(1200);
    for (const n of PAGINE) {
      await pagina(page, n);
      const f = (await fuoriScala(page)).filter(x => !/selectize|\bitem\b.*scegli o aggiungi/.test(x));
      expect(f, `${n} (${scuro ? 'scuro' : 'chiaro'}) fuori scala`).toEqual([]);
    }
  }
  await page.locator('#menu_preferenze').click();
  await page.locator('#scuro').evaluate(e => { if (e.checked) e.click(); });   // si rimette com'era
  await page.locator('#menu_preferenze').click(); await pronto(page);
});

test('Titoli: Fascicoli, Luoghi, Import, Qualità e Gestione hanno lo stesso titolo (24 px, stesso margine)', async ({ page }) => {
  const visto = [];
  for (const n of PAGINE.slice(1)) {
    await pagina(page, n);
    const t = page.locator('h1.fasc-titolo:visible');
    await expect(t).toHaveCount(1);
    visto.push(await t.evaluate(e => [getComputedStyle(e).fontSize, getComputedStyle(e).fontWeight, Math.round(e.getBoundingClientRect().left)].join('|')));
  }
  expect(new Set(visto).size, visto.join(' / ')).toBe(1);
});

test('Home: allineata alle altre pagine, nessuna cornice più stretta', async ({ page }) => {
  const x = n => pagina(page, n).then(() => page.locator('.tab-pane.active h1.fasc-titolo, .tab-pane.active .home').first().evaluate(e => Math.round(e.getBoundingClientRect().left)));
  expect(await x('Home')).toBe(await x('Fascicoli'));
});

test('Menu: la voce attiva è una sola, in grassetto e sottolineata, anche nella Scheda', async ({ page }) => {
  const attiva = async () => { await page.mouse.move(700, 500); return page.evaluate(() => {
    const a = [...document.querySelectorAll('.navbar-nav .nav-link')].filter(e => e.offsetParent !== null && getComputedStyle(e).borderBottomColor !== 'rgba(0, 0, 0, 0)');
    return a.map(e => e.dataset.value + ':' + getComputedStyle(e).borderBottomWidth + ':' + getComputedStyle(e).fontWeight);
  }); };
  for (const n of PAGINE) { await pagina(page, n); expect((await attiva()).length, `voci attive in ${n}`).toBe(1); expect((await attiva())[0]).toMatch(/:2px:600$/); }
  await pagina(page, 'Home'); await page.getByText(/Continua:/).first().click(); await pronto(page);
  expect(await attiva()).toEqual([expect.stringMatching(/^Fascicoli:2px:600$/)]);
});

test('Pulsanti: un\'altezza sola, 44 px, fuori dai due blocchi di servizio della Home', async ({ page }) => {
  for (const n of PAGINE) {
    await pagina(page, n);
    const alti = await page.evaluate(() => [...document.querySelectorAll('.tab-pane.active .btn')]
      .filter(e => e.offsetParent && !e.closest('.dropdown-menu, .home-piede')).map(e => Math.round(e.getBoundingClientRect().height)));
    expect(alti.filter(h => h !== 44), `${n}: altezze dei pulsanti`).toEqual([]);
  }
});

test('Passi spenti: il titolo ha contrasto pieno, non un\'opacità', async ({ page }) => {
  await pagina(page, 'Import');
  const r = await page.locator('.passo-attesa .passo-titolo').first().evaluate(e => ({ op: getComputedStyle(e.closest('.passo-attesa')).opacity, c: getComputedStyle(e).color }));
  expect(r.op).toBe('1');
  const v = r.c.match(/[\d.]+/g).slice(0, 3).map(Number).map(x => { x /= 255; return x <= 0.03928 ? x / 12.92 : ((x + 0.055) / 1.055) ** 2.4; });
  const l = 0.2126 * v[0] + 0.7152 * v[1] + 0.0722 * v[2];
  expect((1.05) / (l + 0.05), 'contrasto del titolo del passo spento su bianco').toBeGreaterThanOrEqual(4.5);
});

test('Luoghi: ogni riga dell\'elenco ha la barra in proporzione alle citazioni', async ({ page }) => {
  await pagina(page, 'Luoghi');
  await page.locator('.ordine-voci [data-valore="citazioni"]').click(); await pronto(page);
  const p = await page.locator('.lista-voci .voce-lista').evaluateAll(l => l.map(e => parseFloat(e.style.getPropertyValue('--p'))));
  expect(p[0]).toBe(100);
  expect(p.every((x, i) => i === 0 || x <= p[i - 1])).toBe(true);   // dal più citato al meno citato
  expect(new Set(p).size).toBeGreaterThan(2);
  await expect(page.locator('.lista-riepilogo')).toHaveCount(0);   // la seconda lista «I più citati» non c'è più
});
