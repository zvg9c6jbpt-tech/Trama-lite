// Seconda revisione del 29/10/2026, aggiornata il 19/11/2026 alla barra scura in alto (voci su una riga, gruppi, badge, fondo e backup nella striscia del contesto, guida), contrasti,
// pesi, un solo pulsante pieno in Home, sottonavigazione a sinistra, Luoghi senza doppia lista, testata della Scheda su due righe.
const { test, expect } = require('@playwright/test');

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
const scuro = async (page, on) => {
  await page.locator('#menu_preferenze').click();
  await page.locator('#scuro').evaluate((e, v) => { if (e.checked !== v) e.click(); }, on);
  await page.locator('#menu_preferenze').click(); await pronto(page); await page.waitForTimeout(1500);
};
// contrasto fra il colore del testo e lo sfondo (composto risalendo gli antenati), con il canvas per leggere qualunque sintassi di colore
const contrasto = (page, selettore) => page.evaluate(sel => {
  const cv = document.createElement('canvas'); cv.width = cv.height = 1; const cx = cv.getContext('2d', { willReadFrequently: true });
  const rgb = c => { cx.clearRect(0, 0, 1, 1); cx.fillStyle = '#000'; cx.fillStyle = c; cx.fillRect(0, 0, 1, 1); const d = cx.getImageData(0, 0, 1, 1).data; return [d[0], d[1], d[2], d[3] / 255]; };
  const L = c => { const t = c.slice(0, 3).map(v => { v /= 255; return v <= .03928 ? v / 12.92 : Math.pow((v + .055) / 1.055, 2.4); }); return .2126 * t[0] + .7152 * t[1] + .0722 * t[2]; };
  const e = [...document.querySelectorAll(sel)].find(x => x.offsetParent !== null); if (!e) return null;
  const pila = []; for (let n = e; n; n = n.parentElement) { const c = rgb(getComputedStyle(n).backgroundColor); if (c[3] > 0) pila.push(c); if (c[3] === 1) break; }
  let bg = [255, 255, 255, 1]; for (let i = pila.length - 1; i >= 0; i--) { const c = pila[i]; bg = [0, 1, 2].map(k => c[k] * c[3] + bg[k] * (1 - c[3])).concat(1); }
  let fg = rgb(getComputedStyle(e).color); fg = [0, 1, 2].map(k => fg[k] * fg[3] + bg[k] * (1 - fg[3])).concat(1);
  const x = L(fg), y = L(bg); return (Math.max(x, y) + .05) / (Math.min(x, y) + .05);
}, selettore);

test('menu in alto: le sette voci stanno su una riga e il contatore sta dentro la sua voce', async ({ page }) => {
  const r = await page.evaluate(() => {
    const voci = [...document.querySelectorAll('.navbar-nav .nav-link')].filter(e => e.offsetParent !== null);
    const alti = voci.map(e => Math.round(e.getBoundingClientRect().top));
    const bad = [...document.querySelectorAll('.navbar-nav .nav-badge')].filter(e => e.offsetParent !== null).map(b => { const l = b.closest('.nav-link').getBoundingClientRect(), x = b.getBoundingClientRect(); return { dentro: x.left >= l.left && x.right <= l.right, destra: Math.round(l.right - x.right) }; });
    return { n: voci.length, alti, bad };
  });
  expect(r.n).toBe(6);   // senza la Presentazione, tolta il 23/11/2026
  for (const t of r.alti) expect(Math.abs(t - r.alti[0]), 'tutte le voci sulla stessa riga').toBeLessThanOrEqual(1);
  expect(r.bad.length, 'i numeri di Fascicoli e Qualità').toBe(2);
  for (const q of r.bad) { expect(q.dentro, 'il numero sta dentro la sua voce').toBe(true); expect(q.destra).toBeLessThanOrEqual(24); }
});

test('menu: due gruppi separati, i numeri di Fascicoli e Qualità, guida raggiungibile da ogni pagina', async ({ page }) => {
  await expect(page.locator('.navbar .nav-sep')).toHaveCount(1);
  const inc = await page.locator('.navbar .nav-link[data-value="Fascicoli"] .nav-badge').innerText();
  await pagina(page, 'Fascicoli');
  await expect(page.locator('#solo_incompleti .n')).toHaveText(inc.trim());   // il numero del menu è quello di «Da completare»
  await expect(page.locator('.navbar .nav-link[data-value="Qualità"] .nav-badge')).toHaveText(/^\d+$/);
  for (const n of ['Luoghi', 'Qualità', 'Gestione']) {
    await pagina(page, n);
    await expect(page.locator('.navbar .nav-aiuto')).toBeVisible();
    await expect(page.locator('.navbar .nav-aiuto')).toHaveAttribute('href', /guida-trama-lite\.html/);
  }
});

test('fondo attivo e backup stanno nella striscia del contesto, a ogni larghezza', async ({ page }) => {
  for (const [w, h] of [[1440, 900], [1000, 800]]) {
    await page.setViewportSize({ width: w, height: h }); await page.waitForTimeout(400);
    await expect(page.locator('.barra-contesto #fondo_etichetta')).toHaveCount(1);
    await expect(page.locator('.barra-contesto #banner_backup')).toHaveCount(1);
    await expect(page.locator('#fondo_etichetta')).toBeVisible();
    expect(await page.locator('#fondo_etichetta').innerText(), 'il nome del fondo non è troncato').not.toMatch(/…|\.\.\.$/);
  }
});

test('menu in alto (sotto 1150 px): la pillola attiva non è tagliata', async ({ page }) => {
  await page.setViewportSize({ width: 1000, height: 800 }); await page.waitForTimeout(500);
  const r = await page.evaluate(() => { const a = document.querySelector('.navbar-nav .nav-link.active').getBoundingClientRect(), c = document.querySelector('.navbar-collapse').getBoundingClientRect(); return { sotto: a.bottom - c.bottom, sopra: c.top - a.top }; });
  expect(r.sotto).toBeLessThanOrEqual(0.5);
  expect(r.sopra).toBeLessThanOrEqual(0.5);
});

test('contrasti: voce attiva e link in chiaro, pulsanti di pericolo in scuro', async ({ page }) => {
  expect(await contrasto(page, '.navbar-nav .nav-link.active')).toBeGreaterThanOrEqual(4.5);
  expect(await contrasto(page, '.tab-pane.active a.azione')).toBeGreaterThanOrEqual(4.5);
  await scuro(page, true);
  await pagina(page, 'Gestione');
  expect(await contrasto(page, '.btn-outline-danger')).toBeGreaterThanOrEqual(4.5);
  await pagina(page, 'Import');
  expect(await contrasto(page, '.btn-outline-danger')).toBeGreaterThanOrEqual(4.5);
  await scuro(page, false);
});

test('Home: un solo pulsante pieno, «Indietro» non si vede spento', async ({ page }) => {
  await expect(page.locator('.tab-pane.active[data-value="Home"] .btn-primary:visible').first()).toBeVisible();   // l'indirizzo #Home arriva a connessione fatta
  const pieni = await page.locator('.tab-pane.active .btn-primary:visible:not([disabled])').count();
  expect(pieni).toBe(1);
  await expect(page.locator('.btn-indietro[disabled]:visible')).toHaveCount(0);   // arrivando da #Home il passo indietro esiste: basta che non sia spento
});

test('caratteri: solo i pesi 400, 500, 600 e 700, in ogni pagina', async ({ page }) => {
  for (const n of ['Home', 'Fascicoli', 'Luoghi', 'Import', 'Qualità', 'Gestione']) {
    await pagina(page, n);
    const pesi = await page.evaluate(() => { const s = new Set(); document.querySelectorAll('body *').forEach(e => { if (e.offsetParent === null || ![...e.childNodes].some(c => c.nodeType === 3 && c.textContent.trim())) return; s.add(getComputedStyle(e).fontWeight); }); return [...s]; });
    for (const w of pesi) expect(['400', '500', '600', '700'], `peso ${w} in ${n}`).toContain(w);
  }
});

test('sottonavigazione: Qualità e Gestione a sinistra, allineate al titolo, righe di testo non troppo lunghe', async ({ page }) => {
  for (const n of ['Qualità', 'Gestione']) {
    await pagina(page, n);
    const r = await page.evaluate(() => { const t = document.querySelector('.tab-pane.active h1').getBoundingClientRect(), p = document.querySelector('.tab-pane.active .nav-pills .nav-link').getBoundingClientRect(), c = document.querySelector('.tab-pane.active .tab-pane.active .card, .tab-pane.active .tab-pane.active .riquadro-scheda, .tab-pane.active .tab-pane.active > *').getBoundingClientRect(); return { titolo: Math.round(t.left), pill: Math.round(p.left), contenuto: Math.round(c.left) }; });
    expect(Math.abs(r.pill - r.titolo), `pillola ${n}`).toBeLessThanOrEqual(2);
    expect(Math.abs(r.contenuto - r.titolo), `contenuto ${n}`).toBeLessThanOrEqual(2);
  }
  await pagina(page, 'Gestione');
  const largo = await page.evaluate(() => Math.max(0, ...[...document.querySelectorAll('.tab-pane.active .card-body > p')].filter(e => e.offsetParent).map(e => e.getBoundingClientRect().width)));
  expect(largo, 'larghezza dei paragrafi').toBeLessThanOrEqual(800);
});

test('Luoghi: un solo elenco, con la barra in ogni riga, e il riepilogo a destra senza seconda lista', async ({ page }) => {
  await pagina(page, 'Luoghi');
  await expect(page.locator('.lista-voci')).toHaveCount(1);
  await expect(page.locator('.riepilogo-voci')).toBeVisible();
  await expect(page.getByText('I più citati')).toHaveCount(0);
  await page.locator('.lista-voci .voce-lista').first().click(); await pronto(page);
  await expect(page.locator('.scheda-voce')).toBeVisible();
  await expect(page.locator('.lista-voci .voce-lista.attiva')).toHaveCount(1);
});

test('Scheda: testata su due righe, titolo e regesto composti non grigi', async ({ page }) => {
  await page.getByText(/Continua:/).first().click(); await page.waitForTimeout(3500); await pronto(page);
  const r = await page.evaluate(() => { const t = document.querySelector('.tab-pane.active .testa-titolo').getBoundingClientRect(), s = document.querySelector('.tab-pane.active .testa-stato').getBoundingClientRect(), n = document.querySelector('.tab-pane.active .testa-crea').getBoundingClientRect(); return { sotto: s.top >= t.bottom - 2, creaAlLato: Math.abs(n.top - document.querySelector('#torna_elenco').getBoundingClientRect().top) < 12 }; });
  expect(r.sotto, 'la seconda riga sta sotto il titolo').toBe(true);
  expect(r.creaAlLato, '«Nuovo fascicolo» resta sulla riga del titolo').toBe(true);
  const c = await page.locator('.campo-composto.e-composto input, .campo-composto.e-composto textarea').first().evaluate(e => { const s = getComputedStyle(e); return { stile: s.borderTopStyle, bg: s.backgroundColor }; });
  expect(c.stile).toBe('dashed');
  // Palette grafite (01/11/2026): i toni sono neutri, quindi «non grigio» diventa «diverso da un campo normale».
  const normale = await page.locator('.tab-pane.active input.form-control:not([readonly])').first().evaluate(e => getComputedStyle(e).backgroundColor);
  const m = c.bg.match(/[\d.]+/g).map(Number), n = normale.match(/[\d.]+/g).map(Number);
  expect(Math.abs(m[0] - n[0]) + Math.abs(m[1] - n[1]) + Math.abs(m[2] - n[2]), 'diverso da un campo normale').toBeGreaterThan(8);
});

test('Scheda compatta: anni stretti con «n.d.» sulla stessa riga, etichette vicine al proprio campo, Altri campi in poche righe', async ({ page }) => {
  await page.getByText(/Continua:/).first().click(); await page.waitForTimeout(3500); await pronto(page);
  const r = await page.evaluate(() => {
    const b = s => document.querySelector(s).getBoundingClientRect();
    const rem = b('#estremo_remoto'), rec = b('#estremo_recente'), nd = b('#data_nd');
    const fondo = getComputedStyle(document.querySelector('.colonna-principale')).backgroundColor;
    const campo = getComputedStyle(document.querySelector('#estremo_remoto')).backgroundColor;
    return { larghezza: Math.round(rem.width), stessaRiga: Math.abs(rem.top - rec.top) < 4 && nd.top < rem.bottom + 40, fondo, campo };
  });
  expect(r.larghezza, 'un anno non occupa mezza colonna').toBeLessThanOrEqual(160);
  expect(r.stessaRiga, 'remoto e recente stanno sulla stessa riga, «n.d.» accanto').toBe(true);
  expect(r.fondo, 'il riquadro non è bianco puro').not.toBe(r.campo);
  // Altri campi aperto: i campi del profilo stanno in poche righe e ogni etichetta è più vicina al proprio campo che a quello sopra
  await page.evaluate(() => { document.querySelector('.colonna-altri details').open = true; });
  await page.waitForTimeout(400);
  const a = await page.evaluate(() => {
    const g = document.querySelector('.colonna-altri'), campi = [...g.querySelectorAll('.shiny-input-container')].filter(e => e.offsetParent && e.querySelector('input,textarea,select'));
    const tops = new Set(campi.map(e => Math.round(e.getBoundingClientRect().top / 10)));
    const c = campi.find(e => e.querySelector('label'));
    const l = c.querySelector('label').getBoundingClientRect(), i = c.querySelector('input').getBoundingClientRect();
    return { alto: Math.round(g.getBoundingClientRect().height), righe: tops.size, gap: Math.round(i.top - l.bottom), margine: parseFloat(getComputedStyle(c).marginBottom) };
  });
  expect(a.alto, 'Altri campi aperto sta in meno di una schermata').toBeLessThan(620);
  expect(a.gap, 'etichetta e campo vicini').toBeLessThan(a.margine + 1);
});

test('telefono: il menu sfuma finché ha altre voci, e la voce attiva resta in vista', async ({ page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto('/');
  await pronto(page);
  const stato = () => page.evaluate(() => {
    const n = document.querySelector('.navbar .navbar-nav'), a = n.querySelector('.nav-link.active'), r = a.getBoundingClientRect(), c = n.getBoundingClientRect();
    return { sfuma: n.classList.contains('scorre-ancora'), attiva: a.getAttribute('data-value'), inVista: r.left >= c.left - 1 && r.right <= c.right + 1 };
  });
  expect((await stato()).sfuma, 'da Home ci sono altre voci: il bordo destro sfuma').toBe(true);
  await pagina(page, 'Qualità');
  await page.waitForTimeout(500);
  const q = await stato();
  expect(q.attiva).toBe('Qualità');
  expect(q.inVista, 'la voce attiva si porta in vista').toBe(true);
  await pagina(page, 'Gestione');
  await page.waitForTimeout(500);
  const g = await stato();
  expect(g.inVista).toBe(true);
  expect(g.sfuma, 'in fondo alla barra non sfuma più').toBe(false);
});

test('lingua dichiarata e Fascicoli senza scorrimento laterale tra 640 e 960 px', async ({ page }) => {
  await page.setViewportSize({ width: 800, height: 600 });
  await page.goto('/');
  await pronto(page);
  expect(await page.evaluate(() => document.documentElement.lang), '<html lang> dichiara l\'italiano').toBe('it');
  await pagina(page, 'Fascicoli');
  for (const larghezza of [640, 720, 800, 900, 960]) {
    await page.setViewportSize({ width: larghezza, height: 600 });
    await page.waitForTimeout(300);
    expect(await page.evaluate(() => document.documentElement.scrollWidth), `a ${larghezza} px la pagina non scorre di lato`).toBeLessThanOrEqual(larghezza);
  }
});
