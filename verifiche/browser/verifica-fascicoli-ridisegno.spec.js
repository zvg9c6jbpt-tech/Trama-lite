// Fascicoli ridisegnato (26/10/2026): buste come gruppi, casella per busta e in testa alla colonna, filtri a pillola con «Con errori»,
// barra delle azioni che compare solo con una selezione, elenco piatto se si ordina per un'altra colonna.
const { test, expect } = require('@playwright/test');

const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'));
};

test.beforeEach(async ({ page }) => {
  await page.setViewportSize({ width: 1440, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText('Tribunale di esempio', { timeout: 60000 });
  await page.locator('.navbar .nav-link[data-value="Fascicoli"]').click();
  await expect(page.locator('.tab-pane[data-value="Fascicoli"] tr.gruppo-busta').first()).toBeVisible({ timeout: 30000 });
  await pronto(page);
});

const spuntati = page => page.evaluate(() => window.spuntati.size);
const righe = page => page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');

test('Fascicoli: una testa per busta, con quanti fascicoli e a che punto è', async ({ page }) => {
  const teste = page.locator('tr.gruppo-busta');
  await expect(teste).toHaveCount(3);
  await expect(teste.first()).toContainText('8 fascicoli');
  await expect(teste.first()).not.toContainText('completa');   // una busta a posto non lo dice: si segnala solo quel che manca
  await expect(page.locator('.busta-ok')).toHaveCount(0);
  await expect(page.locator('.busta-barra').first()).toBeVisible();   // la busta 3 ha schede da completare: barra e percentuale, non la parola
  await expect(righe(page).first().locator('.cella-sub')).toBeHidden();   // nella vista compatta (quella di partenza) le persone non stanno sotto il titolo
  await expect(righe(page).first().locator('.segn-breve')).toHaveText('fasc. 1');   // dentro la busta la segnatura è abbreviata
  await expect(righe(page).first().locator('.segn-piena')).toBeHidden();
});

test('Fascicoli: con le righe compatte il secondo reato diventa «+1»', async ({ page }) => {
  await page.evaluate(() => window.impostaDensita('compatta', false));
  const riga = righe(page).nth(2);   // b. 1, fasc. 3: Magia e Bestemmia
  await expect(riga.locator('.reato-chip:visible')).toHaveCount(1);
  await expect(riga.locator('.reato-piu')).toHaveText('+1');
  await page.evaluate(() => window.impostaDensita('leggibile', false));
  await expect(riga.locator('.reato-chip:visible')).toHaveCount(2);
  await expect(riga.locator('.reato-piu')).toBeHidden();
});

test('Fascicoli: la fascia del fondo sta su una riga, le caselle sulla stessa verticale, lo stato accanto alla segnatura', async ({ page }) => {
  const alto = sel => page.locator(sel).evaluate(e => Math.round(e.getBoundingClientRect().top));
  expect(Math.abs((await alto('.btn-indietro')) - (await alto('#menu_preferenze')))).toBeLessThanOrEqual(2);   // «Preferenze» non va a capo
  const x = await page.evaluate(() => ['#spunta_tutte', '.spunta-busta', '.spunta-riga'].map(s => { const r = document.querySelector(s).getBoundingClientRect(); return r.x + r.width / 2; }));
  expect(Math.max(...x) - Math.min(...x)).toBeLessThanOrEqual(2);
  await expect(page.locator('th', { hasText: /^Stato$/ })).toHaveCount(0);   // la colonna a parte non c'è più
  await expect(page.locator('td .stato-core').first()).toBeVisible();   // le schede da completare lo portano accanto alla segnatura
  await expect(righe(page).first().locator('.stato-core')).toHaveCount(0);   // quelle a posto no
});

test('Fascicoli: sul telefono Indietro e Preferenze stanno sulla stessa riga', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 900 });
  await pronto(page);
  const alto = sel => page.locator(sel).evaluate(e => Math.round(e.getBoundingClientRect().top));
  expect(Math.abs((await alto('.btn-indietro')) - (await alto('#menu_preferenze')))).toBeLessThanOrEqual(2);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
});

test('Fascicoli: i testi non scendono sotto i 13 px', async ({ page }) => {
  const minimo = await page.evaluate(() => Math.min(...[...document.querySelectorAll('.tab-pane[data-value="Fascicoli"] table th, .tab-pane[data-value="Fascicoli"] .cella-sub, .tab-pane[data-value="Fascicoli"] .busta-n, .tab-pane[data-value="Fascicoli"] .reato-chip')]
    .filter(e => e.offsetParent).map(e => parseFloat(getComputedStyle(e).fontSize))));
  expect(minimo).toBeGreaterThanOrEqual(13);
});

test('Fascicoli: si spunta per riga, per busta e per tutte; la barra compare solo con una selezione', async ({ page }) => {
  const barra = page.locator('.barra-selezione');
  await expect(barra).toBeHidden();
  await righe(page).nth(1).locator('.spunta-riga').check();
  await expect(barra).toBeVisible();
  await expect(barra).toContainText('1 fascicolo spuntato');
  await page.locator('.spunta-busta').first().check();
  expect(await spuntati(page)).toBe(8);
  await page.locator('#spunta_tutte').check();
  await expect.poll(() => spuntati(page)).toBe(24);
  await expect(barra.getByRole('button', { name: /Imposta un campo/ })).toBeVisible();
  await page.locator('#spunta_tutte').uncheck();
  await expect.poll(() => spuntati(page)).toBe(0);
  await expect(barra).toBeHidden();
});

test('Fascicoli: «Con errori» mostra le schede segnalate e la fascia per correggerle', async ({ page }) => {
  await expect(page.locator('.banner-anomalie')).toHaveCount(0);   // la fascia non c'è finché non si sceglie il filtro
  await expect(page.locator('#solo_errori .n')).toHaveText('2', { timeout: 20000 });
  await page.locator('#solo_errori').click();
  await expect(righe(page)).toHaveCount(2);
  await expect(page.locator('.banner-anomalie')).toContainText('2 schede');
  await page.locator('#filtri_tutti').click();
  await expect(righe(page)).toHaveCount(24);
  await expect(page.locator('.banner-anomalie')).toHaveCount(0);
});

test('Fascicoli: «Da completare» e «Con errori» si escludono a vicenda', async ({ page }) => {
  await expect(page.locator('#solo_errori .n')).toHaveText('2', { timeout: 20000 });
  await page.locator('#solo_errori').click();
  await expect(page.locator('#solo_errori')).toHaveClass(/attiva/);
  await page.locator('#solo_incompleti').click();
  await expect(page.locator('#solo_incompleti')).toHaveClass(/attiva/);
  await expect(page.locator('#solo_errori')).not.toHaveClass(/attiva/);
  await page.locator('#solo_errori').click();
  await expect(page.locator('#solo_errori')).toHaveClass(/attiva/);
  await expect(page.locator('#solo_incompleti')).not.toHaveClass(/attiva/);
});

test('Fascicoli: ordinando per anno l\'elenco è piatto, per segnatura torna per buste', async ({ page }) => {
  await page.locator('th', { hasText: 'Anno' }).click();
  await expect(page.locator('tr.gruppo-busta:visible')).toHaveCount(0);
  await expect(righe(page)).toHaveCount(24);
  await expect(righe(page).first().locator('.segn-piena')).toBeVisible();   // fuori dai gruppi la segnatura è intera
  await expect(righe(page).first().locator('.segn-breve')).toBeHidden();
  await page.locator('th', { hasText: 'Segnatura' }).click();
  await expect(page.locator('tr.gruppo-busta:visible')).toHaveCount(3);
});

test('Fascicoli: a 390 px la pagina non è più larga dello schermo', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 900 });
  // Un telefono apre la pagina già a quella larghezza: DataTables fissa le colonne al primo disegno.
  await page.goto('/#Fascicoli');
  await page.locator('.navbar .nav-link[data-value="Fascicoli"]').click();
  await expect(page.locator('.tab-pane[data-value="Fascicoli"] tr.gruppo-busta').first()).toBeVisible({ timeout: 30000 });
  await pronto(page);
  expect(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth + 1)).toBe(true);
  // Anno, reati e luoghi non escono a destra: stanno in una riga sotto il titolo, e la tabella sta nello schermo.
  const r = await page.evaluate(() => {
    const t = document.querySelector('.tab-pane[data-value="Fascicoli"] table.dataTable');
    const w = t.closest('.dataTables_wrapper');
    return { tab: Math.round(t.getBoundingClientRect().right), wrap: w.scrollWidth <= w.clientWidth + 1, alto: innerWidth };
  });
  expect(r.tab).toBeLessThanOrEqual(r.alto);
  expect(r.wrap, 'la tabella non scorre di lato').toBe(true);
  const meta = righe(page).first().locator('.cella-meta');
  await expect(meta).toBeVisible();
  await expect(meta).toContainText(/\d{4}/);
  await expect(page.locator('.tab-pane[data-value="Fascicoli"] th.col-dati:visible')).toHaveCount(0);
  await page.setViewportSize({ width: 1440, height: 1000 });
  await expect(meta).toBeHidden();
});

test('Fascicoli: due righe di comandi, colonne con la loro misura, intestazione che resta in vista', async ({ page }) => {
  const r = await page.evaluate(() => {
    const b = s => document.querySelector('.tab-pane[data-value="Fascicoli"] ' + s).getBoundingClientRect();
    const t = b('h1.fasc-titolo'), c = b('.cerca-fascicoli'), n = b('#nuovo_da_elenco'), v = b('.fasc-vista'), f = b('.fasc-chips'), th = b('table thead th');
    const w = s => Math.round(b('table thead th.' + s).width);
    return { stessaRiga: Math.abs(t.top - c.top) < 30 && Math.abs(n.top - c.top) < 30, vistaConFiltri: Math.abs(v.top - f.top) < 30, intestazioneAlta: Math.round(th.top),
             anno: w('col-anno'), reato: w('col-reato'), titolo: w('col-titolo') };
  });
  expect(r.stessaRiga, 'titolo, ricerca e «Nuovo fascicolo» sulla stessa riga').toBe(true);
  expect(r.vistaConFiltri, 'vista e densità accanto ai filtri').toBe(true);
  expect(r.intestazioneAlta, 'la tabella parte in alto, non a un terzo di pagina').toBeLessThan(270);
  expect(r.titolo, 'il titolo prende lo spazio che resta').toBeGreaterThan(r.reato);
  expect(r.anno, 'l\'anno non si allarga').toBeLessThan(150);
  // scorrendo, l'intestazione resta attaccata in alto
  await page.evaluate(() => { document.querySelectorAll('*').forEach(e => { if (e.scrollHeight > e.clientHeight + 50 && /auto|scroll/.test(getComputedStyle(e).overflowY)) e.scrollTop = 600; }); window.scrollTo(0, 600); });
  await page.waitForTimeout(300);
  const top = await page.locator('.tab-pane[data-value="Fascicoli"] table thead th').first().evaluate(e => Math.round(e.getBoundingClientRect().top));
  expect(top, 'intestazione in vista').toBeLessThan(80);
  expect(top, 'intestazione in vista').toBeGreaterThanOrEqual(-1);
});
