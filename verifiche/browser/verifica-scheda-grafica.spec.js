// Verifica grafica della Scheda (26/10/2026): schermo intero a più larghezze, in più stati (scheda piena, vuota, con modifiche non salvate,
// tema scuro, inglese). Oltre agli screenshot (schermate/scheda-*.png) controlla in modo meccanico i difetti che si vedono a occhio:
// scorrimento orizzontale della pagina, testo che sporge o è tagliato, elementi che escono dalla loro colonna (a destra e a sinistra),
// pulsanti sovrapposti, menu fuori dallo schermo, campo in uso coperto dalla barra di salvataggio o dalla testata.
// Le sezioni chiuse (<details>) si aprono prima di misurare: ciò che sta dentro si vede solo da aperto, ma deve stare a posto.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), path = require('path');

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

const LARGHEZZE = [1440, 1280, 1024, 820, 390];
test.describe.configure({ timeout: 300000 });   // cinque larghezze, con attese per lo scorrimento morbido

const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
    !document.querySelector('.shiny-notification .progress'), null, { timeout: 30000 });
  await page.waitForTimeout(400);
};

const apriSezioni = page => page.evaluate(() => document.querySelectorAll('.carta-scheda details').forEach(d => { d.open = true; }));

// Difetti grafici nella Scheda aperta. Restituisce un elenco di frasi (vuoto = nessun difetto).
const difetti = page => page.evaluate(() => {
  const out = [];
  const de = document.documentElement;
  if (de.scrollWidth > de.clientWidth + 1) out.push(`la pagina scorre in orizzontale (${de.scrollWidth} > ${de.clientWidth})`);
  const radice = document.querySelector('#busta')?.closest('.tab-pane') || document.body;
  const visibile = el => { const r = el.getBoundingClientRect(); const s = getComputedStyle(el);
    return r.width > 0 && r.height > 0 && s.visibility !== 'hidden' && s.display !== 'none' && !el.closest('.campo-nascosto, [hidden]'); };
  const nome = el => (el.id ? '#' + el.id : el.tagName.toLowerCase() + (el.className && typeof el.className === 'string' ? '.' + el.className.trim().split(/\s+/).slice(0, 2).join('.') : '')) +
    ' «' + (el.innerText || el.value || el.getAttribute('aria-label') || '').trim().replace(/\s+/g, ' ').slice(0, 30) + '»';
  // testo che sporge dalla sua scatola o tagliato: pulsanti, pastiglie, etichette, intestazioni, riepilogo (anche con overflow visibile)
  radice.querySelectorAll('button, .btn, .pillola, .etichetta-sezione, label, .colonna-titolo, summary, .home-tag, .completezza-scheda, .testa-posizione, dt, dd').forEach(el => {
    if (!visibile(el)) return;
    const s = getComputedStyle(el);
    if (s.overflowX === 'auto' || s.overflowX === 'scroll') return;
    if (el.classList.contains('da-salvare')) return;   // il puntino «da salvare» sta fuori dal bordo di proposito
    if (el.scrollWidth > el.clientWidth + 1 && s.textOverflow !== 'ellipsis') out.push('testo che sporge o tagliato: ' + nome(el));
    if (el.scrollHeight > el.clientHeight + 2 && s.overflowY === 'hidden') out.push('testo tagliato in altezza: ' + nome(el));
  });
  // elementi che escono dalla colonna (riquadro) che li contiene, a destra o a sinistra, o dallo schermo
  const colonne = [...radice.querySelectorAll('.colonna-scheda')];
  radice.querySelectorAll('input, select, textarea, .selectize-control, .shiny-input-container, .btn, .pillola, table, img').forEach(el => {
    if (!visibile(el)) return;
    const r = el.getBoundingClientRect();
    if (r.right > innerWidth + 1 || r.left < -1) out.push('esce dallo schermo: ' + nome(el));
    const c = colonne.find(x => x.contains(el));
    if (c) { const rc = c.getBoundingClientRect(); if (r.right > rc.right + 1 || r.left < rc.left - 1) out.push('esce dal riquadro: ' + nome(el)); }
  });
  // pulsanti o pastiglie sovrapposti (la barra fissa sta sopra ciò che scorre: non è un difetto)
  const mobili = [...radice.querySelectorAll('button, .btn, .pillola')].filter(visibile);
  for (let i = 0; i < mobili.length; i++) for (let j = i + 1; j < mobili.length; j++) {
    if (mobili[i].contains(mobili[j]) || mobili[j].contains(mobili[i])) continue;
    if (mobili[i].closest('.barra-fondo-scheda') || mobili[j].closest('.barra-fondo-scheda')) continue;
    const a = mobili[i].getBoundingClientRect(), b = mobili[j].getBoundingClientRect();
    const w = Math.min(a.right, b.right) - Math.max(a.left, b.left), h = Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top);
    if (w > 3 && h > 3) out.push('sovrapposti: ' + nome(mobili[i]) + ' e ' + nome(mobili[j]));
  }
  return [...new Set(out)];
});

// Il campo che si sta usando non finisce sotto la barra di salvataggio né sotto la testata fissa (con Tab).
const campoInVista = async page => {
  const problemi = [];
  for (const id of ['#estremo_remoto', '#estremo_recente', '#regesto', '#note', '#serie', '#altra_segnatura']) {
    const el = page.locator(id).first();
    if (!(await el.count()) || !(await el.isVisible())) continue;
    await el.focus(); await page.waitForTimeout(700);   // Bootstrap scorre in modo morbido
    const r = await el.evaluate(e => { const b = e.getBoundingClientRect(), bar = document.querySelector('.barra-fondo-scheda'), t = document.querySelector('.testa-scheda');
      return { top: b.top, bottom: b.bottom, barra: bar.getBoundingClientRect().top, testa: t.getBoundingClientRect().bottom, alto: innerHeight }; });
    if (r.bottom > r.barra + 1 && r.barra < r.alto) problemi.push(`la barra di salvataggio copre ${id}`);
    if (r.top < r.testa - 1) problemi.push(`la testata copre ${id}`);
  }
  return problemi;
};

// I due menu (Nuovo fascicolo, Altro) si aprono e stanno dentro lo schermo.
const menuInVista = async page => {
  const problemi = [];
  for (const [contenitore, nome] of [['.testa-crea', 'Nuovo fascicolo'], ['.barra-fondo-scheda .barra-altro', 'Altro']]) {
    await page.locator(contenitore + ' > button').first().click(); await page.waitForTimeout(300);
    const r = await page.evaluate(sel => { const m = document.querySelector(sel + ' .dropdown-menu.show');
      if (!m) return null; const b = m.getBoundingClientRect(); return { l: b.left, r: b.right, t: b.top, b: b.bottom, w: innerWidth, h: innerHeight }; }, contenitore);
    if (!r) problemi.push(`il menu «${nome}» non si apre`);
    else if (r.l < -1 || r.r > r.w + 1 || r.t < -1 || r.b > r.h + 1) problemi.push(`il menu «${nome}» esce dallo schermo`);
    await page.keyboard.press('Escape'); await page.waitForTimeout(150);
  }
  return problemi;
};

const apriScheda = async (page, ricerca) => {
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
  await page.locator('#cerca_fascicoli').fill(ricerca); await pronto(page);
  const righe = page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
  await expect(righe.first()).toBeVisible();
  await righe.first().locator('td').nth(2).click();
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 });
  await pronto(page);
};

let errori;
test.beforeEach(async ({ page }) => {
  page.setDefaultTimeout(10000);
  errori = [];
  page.on('pageerror', e => errori.push('JS: ' + e.message));
  page.on('console', m => { if (m.type() === 'error' && !/favicon|Failed to load resource/.test(m.text())) errori.push('console: ' + m.text()); });
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await pronto(page);
});
test.afterEach(async ({ page }) => {
  expect(await page.locator('.shiny-output-error').count(), 'uscite di Shiny in errore').toBe(0);
  expect(errori).toEqual([]);
});

const giro = async (page, stato) => {
  const tutti = [];
  for (const w of LARGHEZZE) {
    await page.setViewportSize({ width: w, height: w < 600 ? 800 : 900 });
    await page.waitForTimeout(500);
    await apriSezioni(page);
    await page.evaluate(() => window.scrollTo(0, 0));
    await page.screenshot({ path: path.join(FOTO, `scheda-${stato}-${w}.png`), fullPage: true });
    for (const d of await difetti(page)) tutti.push(`${w}px: ${d}`);
    for (const d of await campoInVista(page)) tutti.push(`${w}px: ${d}`);
    await page.evaluate(() => window.scrollTo(0, 0)); await page.waitForTimeout(500);
    for (const d of await menuInVista(page)) tutti.push(`${w}px: ${d}`);
  }
  await page.setViewportSize({ width: 1280, height: 900 });
  expect(tutti, 'difetti grafici').toEqual([]);
};

test('Scheda piena: nessun difetto grafico a nessuna larghezza', async ({ page }) => {
  await apriScheda(page, 'Marco Fontana');
  await giro(page, 'piena');
});

test('Scheda vuota (nuovo fascicolo): nessun difetto grafico a nessuna larghezza', async ({ page }) => {
  await page.locator('#home_porta_nuova').click();
  await expect(page.getByRole('heading', { name: 'Nuovo fascicolo' })).toBeVisible();
  await pronto(page);
  await giro(page, 'vuota');
});

test('Scheda con modifiche non salvate (etichette lunghe della barra): nessun difetto grafico', async ({ page }) => {
  await apriScheda(page, 'Marco Fontana');
  await page.locator('#note').fill('Una nota scritta dalla prova: la scheda ha modifiche da salvare e le etichette della barra si allungano.');
  await expect(page.locator('#salva_succ')).toContainText(/Salva e prossimo/);
  await giro(page, 'modificata');
  await page.locator('#annulla_scheda').evaluate(e => e.click());   // niente resta scritto
});

// Tema scuro e interfaccia in inglese (i testi inglesi sono più lunghi: il rischio di uscire dai riquadri è maggiore). Alla fine si rimette tutto com'era.
const interruttore = async (page, id, acceso) => {
  await page.locator('#menu_preferenze').click();
  const el = page.locator('#' + id);
  if ((await el.isChecked()) !== acceso) await el.evaluate(e => e.click());
  await page.locator('#menu_preferenze').click();
  await pronto(page);
  await page.waitForTimeout(1200);   // il tema si ricarica
};

test('Scheda in tema scuro e in inglese: nessun difetto grafico', async ({ page }) => {
  await apriScheda(page, 'Marco Fontana');
  try {
    await interruttore(page, 'scuro', true);
    await giro(page, 'scura');
    await interruttore(page, 'scuro', false);
    await interruttore(page, 'inglese', true);
    await expect(page.getByRole('button', { name: /New case file/ }).first()).toBeVisible();
    await giro(page, 'inglese');
  } finally {
    await interruttore(page, 'scuro', false).catch(() => {});
    await interruttore(page, 'inglese', false).catch(() => {});
  }
});
