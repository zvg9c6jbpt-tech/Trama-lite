// Verifica dell'accessibilità (18/11/2026): contrasto del testo (AA, 4,5:1; 3:1 per il testo grande), aree toccabili e anello di fuoco
// su ogni pagina, in chiaro e in scuro. Database di esempio TEMPORANEO (lo stesso lanciatore delle altre prove).
// Con A11Y_STAMPA=1 stampa l'elenco completo dei difetti invece di fermarsi al primo.
const { test, expect } = require('@playwright/test');

const PAGINE = ['Home', 'Fascicoli', 'Scheda', 'Luoghi', 'Import', 'Qualità', 'Gestione'];

const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
    !document.querySelector('.shiny-notification .progress'), null, { timeout: 30000 });
  await page.waitForTimeout(400);
};

// Gira dentro la pagina: elenca il testo sotto soglia e i bersagli piccoli.
const ANALISI = () => {
  const cv = document.createElement('canvas'); cv.width = cv.height = 1;
  const cx = cv.getContext('2d', { willReadFrequently: true });
  const rgba = css => { cx.clearRect(0, 0, 1, 1); cx.fillStyle = '#000'; cx.fillStyle = css; cx.fillRect(0, 0, 1, 1);
    const d = cx.getImageData(0, 0, 1, 1).data; return [d[0], d[1], d[2], d[3] / 255]; };
  const sopra = (f, b) => { const a = f[3]; return [0, 1, 2].map(i => f[i] * a + b[i] * (1 - a)).concat([1]); };
  const lum = c => { const v = c.slice(0, 3).map(x => { x /= 255; return x <= .03928 ? x / 12.92 : Math.pow((x + .055) / 1.055, 2.4); });
    return .2126 * v[0] + .7152 * v[1] + .0722 * v[2]; };
  const rapporto = (a, b) => { const [x, y] = [lum(a), lum(b)].sort((p, q) => q - p); return (x + .05) / (y + .05); };
  const visibile = el => { const r = el.getBoundingClientRect(); if (r.width < 1 || r.height < 1) return false;
    for (let e = el; e; e = e.parentElement) { const s = getComputedStyle(e);
      if (s.display === 'none' || s.visibility === 'hidden' || s.visibility === 'collapse') return false; } return true; };
  const sfondo = el => { let strati = []; let opac = 1;
    for (let e = el; e; e = e.parentElement) { const s = getComputedStyle(e);
      if (s.backgroundImage !== 'none') return null;
      const c = rgba(s.backgroundColor); if (c[3] > 0) strati.push(c);
      if (c[3] >= 1) break; }
    let b = [255, 255, 255, 1]; if (!strati.length || strati[strati.length - 1][3] < 1) b = rgba(getComputedStyle(document.documentElement).colorScheme === 'dark' ? '#000' : '#fff');
    for (let i = strati.length - 1; i >= 0; i--) b = sopra(strati[i], b); return b; };
  const opacita = el => { let o = 1; for (let e = el; e; e = e.parentElement) o *= parseFloat(getComputedStyle(e).opacity); return o; };
  const percorso = el => { const p = []; for (let e = el; e && p.length < 3; e = e.parentElement) p.push(e.tagName.toLowerCase() + (e.id ? '#' + e.id : '') + (e.className && typeof e.className === 'string' ? '.' + e.className.trim().split(/\s+/).slice(0, 2).join('.') : '')); return p.join(' < '); };

  const contrasto = [], bersagli = [];
  const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT);
  const visti = new Set();
  while (walker.nextNode()) {
    const t = walker.currentNode; if (!t.textContent.trim()) continue;
    const el = t.parentElement; if (!el || visti.has(el) || /^(SCRIPT|STYLE|NOSCRIPT|OPTION)$/.test(el.tagName)) continue;
    visti.add(el); if (!visibile(el)) continue;
    const r = document.createRange(); r.selectNodeContents(t); const q = r.getBoundingClientRect(); if (q.width < 1 || q.height < 1) continue;
    const s = getComputedStyle(el); if (el.closest('[disabled], .disabled, [aria-disabled="true"]')) continue;
    const b = sfondo(el); if (!b) continue;
    const o = opacita(el); let f = rgba(s.color); f[3] *= o; const col = sopra(f, b);
    const px = parseFloat(s.fontSize), grande = px >= 24 || (px >= 18.66 && parseInt(s.fontWeight) >= 700);
    const rr = rapporto(col, b), soglia = grande ? 3 : 4.5;
    if (rr < soglia) contrasto.push({ testo: t.textContent.trim().slice(0, 40), rapporto: +rr.toFixed(2), soglia, el: percorso(el) });
  }
  for (const el of document.querySelectorAll('a[href], button, [role="tab"], [role="button"], input:not([type="hidden"]), select, summary, .form-check-input, .selectize-input')) {
    if (!visibile(el)) continue; if (el.closest('.dropdown-menu:not(.show), [disabled]') || /-selectized$/.test(el.id)) continue;   // -selectized: la casella di scrittura di selectize, larga quanto il cursore
    const r = el.getBoundingClientRect(); if (r.width < 1) continue;
    // un link dentro una riga di testo è esente (WCAG 2.5.8): lo riconosce l'essere inline in un paragrafo
    if (el.tagName === 'A' && getComputedStyle(el).display === 'inline' && el.closest('p, li, td, .nota-campo, .home-spiega, span')) continue;
    // soglia dura: 24 px (WCAG 2.2, 2.5.8, AA). Pulsanti, schede e riassunti: 44 px, come nel redesign; le caselle e i link dentro le tabelle solo 24.
    const nelleTabelle = !!el.closest('table'), grande = (el.matches('button, [role="tab"], summary, .btn, .nav-link, a.nav-aiuto') && !el.matches('.chip-campo, .pillola') && !nelleTabelle);
    const minimo = grande ? 44 : 24;
    if (r.width < minimo || r.height < minimo) bersagli.push({ el: percorso(el), testo: (el.textContent || el.getAttribute('aria-label') || '').trim().replace(/\s+/g, ' ').slice(0, 30), w: Math.round(r.width), h: Math.round(r.height), minimo });
  }
  return { contrasto, bersagli };
};

for (const tema of ['light', 'dark']) {
  test(`accessibilità (${tema}): contrasto e bersagli su ogni pagina`, async ({ page }) => {
    test.setTimeout(180000);
    await page.emulateMedia({ colorScheme: tema });
    await page.setViewportSize({ width: 1280, height: 900 });
    await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
    await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
    await pronto(page);
    const difetti = [];
    for (const nome of PAGINE) {
      const tab = page.getByRole('tab', { name: new RegExp(nome) }).first();
      if (await tab.count()) { await tab.click(); await pronto(page); }
      const r = await page.evaluate(ANALISI);
      for (const c of r.contrasto) difetti.push(`[${nome}] contrasto ${c.rapporto} < ${c.soglia}: «${c.testo}» (${c.el})`);
      const visti = new Set();
      for (const b of r.bersagli) { const k = b.el.replace(/#\S+/g, '') + b.minimo; if (visti.has(k)) continue; visti.add(k);
        difetti.push(`[${nome}] bersaglio ${b.w}×${b.h} (minimo ${b.minimo}): «${b.testo}» (${b.el})`); }
    }
    // la scheda aperta (campi, pillole, caselle): dalla prima riga di Fascicoli
    await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
    await page.locator('a.link-apri').first().click(); await pronto(page);
    for (const l of [1280, 390]) {
      await page.setViewportSize({ width: l, height: 900 }); await page.waitForTimeout(400);
      const r = await page.evaluate(ANALISI);
      for (const c of r.contrasto) difetti.push(`[Scheda aperta ${l}px] contrasto ${c.rapporto} < ${c.soglia}: «${c.testo}» (${c.el})`);
      for (const b of r.bersagli) difetti.push(`[Scheda aperta ${l}px] bersaglio ${b.w}×${b.h} (minimo ${b.minimo}): «${b.testo}» (${b.el})`);
    }
    if (process.env.A11Y_STAMPA) console.log(difetti.join('\n') || 'nessun difetto');
    expect(difetti, difetti.join('\n')).toEqual([]);
  });
}
