// Verifica nel browser della scelta «omonimo» (02/11/2026): un foglio con «Castro» (Lecce o Bergamo) e «PATERNÒ» in maiuscolo,
// «Pulisci e scarica» (non scrive nel database). Nella finestra gli omonimi hanno «Lascia com'è» e nessuna possibilità spuntata; la scelta non cambia
// la cella, va nel foglio «Luoghi scelti»; il file si apre con un lettore rigoroso (nessuna relazione verso parti assenti).
// Il foglio si ricava dall'export del fondo di esempio (database TEMPORANEO): niente file fuori dal progetto.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');
const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));
const unzip = (file, parte) => { try { return execFileSync('unzip', ['-p', file, parte], { encoding: 'utf8', maxBuffer: 1 << 26 }); } catch (e) { return ''; } };

// CSV con «;» e virgolette: si legge e si riscrive, cambiando solo le celle dei luoghi.
const leggiCsv = testo => {
  const righe = []; let r = [], c = '', q = false;
  for (let i = 0; i < testo.length; i++) {
    const ch = testo[i];
    if (q) { if (ch === '"' && testo[i + 1] === '"') { c += '"'; i++; } else if (ch === '"') q = false; else c += ch; }
    else if (ch === '"') q = true;
    else if (ch === ';') { r.push(c); c = ''; }
    else if (ch === '\n') { r.push(c.replace(/\r$/, '')); righe.push(r); r = []; c = ''; }
    else c += ch;
  }
  if (c || r.length) { r.push(c); righe.push(r); }
  return righe;
};
const scriviCsv = righe => '﻿' + righe.map(r => r.map(c => /[";\n\r]/.test(c) ? '"' + c.replace(/"/g, '""') + '"' : c).join(';')).join('\r\n') + '\r\n';

test('Pulisci: gli omonimi si scelgono nella finestra e la scelta finisce solo in «Luoghi scelti»', async ({ page }) => {
  test.setTimeout(240000);
  await page.setViewportSize({ width: 1280, height: 1000 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  const pronto = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 });
  const finestra = page.locator('.modal-dialog');
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-omonimi-'));

  // Il foglio: l'export del fondo di esempio con tre celle dei luoghi cambiate.
  const [dl] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_csv:not(.disabled)').click()]);
  const originale = path.join(dir, 'fondo.csv'); await dl.saveAs(originale);
  const tab = leggiCsv(fs.readFileSync(originale, 'utf8').replace(/^﻿/, ''));
  const j = tab[0].indexOf('Luoghi citati o indicizzati'); expect(j).toBeGreaterThan(-1);
  tab[1][j] = 'Lecce|Castro'; tab[2][j] = 'Bergamo|Castro'; tab[3][j] = 'PATERNÒ';
  const foglio = path.join(dir, 'omonimi.csv'); fs.writeFileSync(foglio, scriviCsv(tab));
  await page.locator('#file_import').setInputFiles(foglio);
  await expect(page.locator('#anteprima_import')).toContainText(`${tab.length - 1} righe`, { timeout: 30000 });
  await page.locator('#avvia_pulito').click();
  await expect(finestra).toContainText('Controlla prima di pulire', { timeout: 60000 });
  await page.screenshot({ path: path.join(FOTO, '30-omonimi-finestra.png') });

  // «PATERNÒ» non diventa da solo un altro comune: è una proposta fra «Paterno» e «Paternò».
  await expect(finestra).toContainText(/«PATERNÒ»: forse «Paterno» o «Paternò»/);

  // Gli omonimi: «Lascia com'è» è il predefinito (valore vuoto), nessuna possibilità vera è spuntata, niente «Preselezionata».
  const blocchi = finestra.locator('.rev-decisione').filter({ hasText: 'esiste in più luoghi' });
  const n = await blocchi.count();
  expect(n).toBeGreaterThanOrEqual(2);
  for (let i = 0; i < n; i++) {
    const b = blocchi.nth(i);
    expect(await b.locator('input:checked').evaluateAll(l => l.map(x => x.value))).toEqual(['']);
    await expect(b).not.toHaveClass(/rev-presel/);
  }
  // Nessuna possibilità mostra pezzi di chiave: solo «comune», «frazione», «comune in provincia di X», «frazione di X».
  const etichette = (await blocchi.allInnerTexts()).join('\n').match(/«Castro \([^)]*\)»/g) || [];
  expect(etichette.length).toBeGreaterThan(0);
  for (const e of etichette) expect(e).toMatch(/^«Castro \((comune|frazione|comune in provincia di [^)]+|frazione di [^)]+)\)»$/);

  // Si sceglie il comune per la prima riga (Lecce): la seconda (Bergamo) si lascia com'è.
  const primo = blocchi.first();
  await primo.scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '31-omonimi-scelta.png') });
  const voce = primo.locator('label').filter({ hasText: /Castro \(comune in provincia di Lecce\)/ }).first();
  await expect(voce).toBeVisible();
  await voce.click(); await pronto();
  expect((await primo.locator('input:checked').evaluateAll(l => l.map(x => x.value)))[0]).not.toBe('');
  await finestra.getByRole('button', { name: 'Pulisci con le correzioni scelte' }).click();
  await expect(finestra).toBeHidden({ timeout: 60000 });
  await expect(page.locator('#scarica_pulito')).toBeVisible({ timeout: 60000 });
  await pronto();
  await page.locator('#esito_pulisci').scrollIntoViewIfNeeded();
  await page.screenshot({ path: path.join(FOTO, '32-omonimi-esito.png') });
  const [dp] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_pulito:not(.disabled)').click()]);
  const pulito = path.join(dir, 'pulito.xlsx'); await dp.saveAs(pulito);

  // Il file: il foglio «Luoghi scelti» c'è, con la scelta; la cella dei luoghi resta com'era.
  expect(unzip(pulito, 'xl/workbook.xml')).toContain('name="Luoghi scelti"');
  const sst = unzip(pulito, 'xl/sharedStrings.xml');
  expect(sst).toContain('Castro (comune in provincia di Lecce)');
  expect(sst).toContain('Lecce|Castro');                      // la cella non è cambiata
  expect(sst).toContain('PATERNÒ');                           // né la maiuscola è diventata un altro luogo

  // Nessun foglio ha relazioni verso parti assenti dal file (un lettore rigoroso, come openpyxl, lo rifiuterebbe).
  const elenco = execFileSync('unzip', ['-Z1', pulito], { encoding: 'utf8' }).split('\n').filter(Boolean);
  for (const rels of elenco.filter(f => /^xl\/worksheets\/_rels\/.+\.rels$/.test(f))) {
    for (const [, t] of unzip(pulito, rels).matchAll(/Target="([^"]+)"/g)) {
      const risolto = path.posix.normalize(path.posix.join('xl/worksheets', t));
      expect(elenco, `${rels} → ${t}`).toContain(risolto);
    }
  }
});
