// Verifica nel browser delle pagine: Home, Fascicoli, Luoghi, Qualità, Gestione. Le azioni principali, su database di esempio TEMPORANEO.
// In ogni passo: nessun errore JavaScript, nessuna uscita di Shiny in errore (.shiny-output-error).
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

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
  const rotte = await page.locator('.shiny-output-error').count();
  expect(rotte, 'uscite di Shiny in errore').toBe(0);
  expect(errori).toEqual([]);
});
const pagina = async (page, nome) => { await page.getByRole('tab', { name: new RegExp(nome) }).first().click(); await pronto(page); };
// attende che Shiny abbia finito (niente barra «Controllo le schede…», niente calcoli in corso)
const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
    !document.querySelector('.shiny-notification .progress'), null, { timeout: 30000 });
  await page.waitForTimeout(300);
};
const foto = async (page, nome) => { await pronto(page); await page.screenshot({ path: path.join(FOTO, nome) }); };

test('Home: i pulsanti portano dove dicono', async ({ page }) => {
  await foto(page, '10-home.png');
  await page.locator('#home_vai_fascicoli').click();
  await expect(page.locator('#cerca_fascicoli')).toBeVisible();
  await page.locator('a.navbar-brand, .navbar-brand').first().click();
  await page.locator('#home_vai_export').click();
  await expect(page.getByText("Importa nell'app")).toBeVisible();
  await page.locator('.navbar-brand').first().click();
  await page.locator('#home_porta_nuova').click();
  await expect(page.getByRole('heading', { name: 'Nuovo fascicolo' })).toBeVisible();
  await page.locator('.navbar-brand').first().click();
  await page.locator('#home_continua').click();
  await expect(page.locator('#busta')).toBeVisible();     // la scheda di «Continua»
  await page.locator('.navbar-brand').first().click();
  await page.getByText('Altre funzioni').click();
  await page.locator('#home_vai_qualita').click();
  await expect(page.getByRole('tab', { name: 'Controlli' })).toBeVisible();
});

test('Fascicoli: cerca, filtra, cambia vista, apre una scheda', async ({ page }) => {
  await pagina(page, 'Fascicoli');
  await expect(page.locator('#cerca_fascicoli')).toBeVisible();
  await foto(page, '11-fascicoli.png');
  const pannello = page.locator('.tab-pane[data-value="Fascicoli"]');
  const righe = pannello.locator('table tbody tr:not(.gruppo-busta)');
  const tutte = await righe.count();
  expect(tutte).toBeGreaterThan(5);

  await page.locator('#cerca_fascicoli').fill('zzzzzz-non-esiste');
  await pronto(page);
  await foto(page, '11b-fascicoli-nessun-risultato.png');
  await expect(pannello.locator('table tbody tr td:not(.dataTables_empty)')).toHaveCount(0);
  await page.locator('#cerca_fascicoli').fill('');
  await expect(righe).toHaveCount(tutte, { timeout: 10000 });

  await page.locator('#vista_scaffale').click();
  await foto(page, '12-fascicoli-scaffale.png');
  await page.locator('#vista_tabella').click();
  await page.locator('#densita_compatta').click();
  await page.locator('#densita_leggibile').click();
  await page.locator('#solo_incompleti').click();
  await foto(page, '13-fascicoli-incompleti.png');
  await page.locator('#solo_incompleti').click();

  // si apre una scheda, si cambia il titolo manualmente, si salva, si ritrova
  await righe.first().locator('td').nth(2).click();
  await expect(page.locator('#busta')).toBeVisible({ timeout: 15000 });
  await foto(page, '14-scheda.png');
});

test('Luoghi: cerca e apre un luogo', async ({ page }) => {
  await pagina(page, 'Luoghi');
  await expect(page.locator('#luoghi_cerca')).toBeVisible();
  await foto(page, '15-luoghi.png');
  await page.locator('#luoghi_cerca').fill('Bologna');
  const lista = page.locator('#luoghi_lista');
  await expect(lista.getByRole('button', { name: /Bologna/ })).toBeVisible();
  await expect(lista.getByRole('button', { name: /Ferrara/ })).toHaveCount(0);
  await expect(page.locator('#luoghi_conta')).toContainText('su 12');
  await lista.getByRole('button', { name: /Bologna/ }).click();
  await foto(page, '16-luogo-aperto.png');
  await page.locator('#luoghi_cerca').fill('');
  await page.getByRole('button', { name: 'Z–A' }).click();
  await page.getByRole('button', { name: 'Per citazioni' }).click();
  await expect(lista.getByRole('button').first()).toContainText('Bologna');   // il più citato (10)
});

test('Qualità: Controlli e Per colonna', async ({ page }) => {
  await pagina(page, 'Qualità');
  await foto(page, '17-qualita.png');
  await page.getByRole('tab', { name: 'Per colonna' }).click();
  await page.waitForTimeout(1500);
  await foto(page, '18-per-colonna.png');
  await page.getByRole('tab', { name: 'Controlli' }).click();
  await page.waitForTimeout(500);
});

test('Gestione: duplicati, e le azioni distruttive si fermano alla conferma', async ({ page }) => {
  await pagina(page, 'Gestione');
  await foto(page, '19-gestione.png');
  await page.locator('#dup_analizza').click();
  await page.waitForTimeout(2500);
  await foto(page, '20-duplicati.png');
  for (const [id, titolo] of [['canc_fondo_apri', /Elimina|fondo/i], ['canc_tutto_apri', /Svuota|tutto/i]]) {
    await page.locator('#' + id).click();
    const finestra = page.locator('.modal-dialog');
    await expect(finestra).toContainText(titolo);
    await page.waitForTimeout(500);
    await foto(page, `21-${id}.png`);
    await finestra.getByRole('button', { name: /Annulla/ }).click();
    await expect(finestra).toBeHidden();
  }
  // nulla è stato toccato
  await expect(page.locator('#fondo_etichetta')).toContainText(/\(24\)/);
});

test('Scheda: si modifica il regesto, si salva, si ritrova; dal luogo si apre la scheda', async ({ page }) => {
  await pagina(page, 'Fascicoli');
  await page.locator('#cerca_fascicoli').fill('Francesca Pini');
  await pronto(page);
  const righeF = page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
  await expect(righeF).toHaveCount(1);   // il filtro ha finito di ridisegnare l'elenco
  const riga = righeF.first();
  await riga.locator('td').nth(2).click();
  const regesto = page.locator('#regesto');
  await expect(regesto).toBeVisible({ timeout: 15000 });
  const segnatura = (await page.locator('#busta').inputValue()) + '/' + (await page.locator('#fascicolo').inputValue());
  const nuovo = 'Regesto riscritto dalla prova nel browser.';
  await regesto.fill(nuovo);
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.shiny-notification')).toContainText('Salvata', { timeout: 10000 });
  await foto(page, '22-scheda-salvata.png');

  // fuori dalla scheda e di nuovo dentro: il testo c'è ancora
  await page.getByRole('button', { name: /Indietro ai Fascicoli/ }).first().click();
  await expect(page.locator('#cerca_fascicoli')).toBeVisible();
  await pronto(page);
  await page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)').first().locator('td').nth(2).click();
  await expect(page.locator('#regesto')).toHaveValue(nuovo, { timeout: 15000 });
  expect((await page.locator('#busta').inputValue()) + '/' + (await page.locator('#fascicolo').inputValue())).toBe(segnatura);
  // la prova vera di «ritrovato» è nel database (il campo, subito dopo l'apertura, potrebbe ancora avere il valore di prima)
  const casa = (await page.locator('code.no-trad').first().innerText()).trim();
  const [bb, ff] = segnatura.split('/');
  expect(execFileSync('sqlite3', [casa + '/trama.sqlite', `SELECT regesto FROM fascicoli WHERE busta=${bb} AND fascicolo=${ff}`], { encoding: 'utf8' }).trim()).toBe(nuovo);

  // dal luogo, il collegamento alla scheda
  await pagina(page, 'Luoghi');
  await page.locator('#luoghi_cerca').fill('Bologna');
  await pronto(page);
  await page.locator('#luoghi_lista').getByRole('button', { name: /Bologna/ }).click();
  await page.getByRole('link', { name: 'b.1, f.1' }).click();
  await expect(page.locator('#busta')).toHaveValue('1', { timeout: 15000 });
  await expect(page.locator('#fascicolo')).toHaveValue('1');
});

// D30 nella Scheda (26/10/2026): estremi uguali → al salvataggio una finestra chiede Sì / No. «No» salva com'è e non lo richiede; «Sì» toglie il recente.
// dopo un salvataggio il modulo si riaggiorna con un attimo di ritardo: prima di digitare ancora si lascia assestare
const assesta = async page => { await pronto(page); await page.waitForTimeout(1500); };

test('Scheda: estremi uguali, la finestra chiede; No tiene tutti e due, Sì toglie il recente', async ({ page }) => {
  await pagina(page, 'Fascicoli');
  await page.locator('#cerca_fascicoli').fill('Informazioni circa Pietro Galli');   // b. 1, fasc. 4: 1562–1563
  await pronto(page);
  const righe = page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
  await expect(righe).toHaveCount(1);   // l'elenco ha finito di filtrare
  await righe.first().locator('td').nth(2).click();
  await expect(page.locator('#estremo_recente')).toBeVisible({ timeout: 15000 });
  await pronto(page);
  const casa = (await page.locator('code.no-trad').first().innerText()).trim();
  const db = casa + '/trama.sqlite';
  const b = await page.locator('#busta').inputValue(), f = await page.locator('#fascicolo').inputValue();
  const recente = () => execFileSync('sqlite3', [db, `SELECT IFNULL(estremo_recente,'-') FROM fascicoli WHERE busta=${b} AND fascicolo=${f}`], { encoding: 'utf8' }).trim();
  const originale = (await page.locator('#estremo_recente').inputValue());
  expect(originale).not.toBe('');
  const remoto = await page.locator('#estremo_remoto').inputValue();

  // anni diversi: nessuna finestra
  await page.locator('#note').fill('prova D30');
  await page.waitForTimeout(700);   // Shiny riceve il valore digitato
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.shiny-notification')).toContainText('Salvata', { timeout: 10000 });
  await expect(page.locator('.modal-dialog')).toHaveCount(0);
  await assesta(page);

  // estremi uguali: la finestra compare e non scrive niente finché non si risponde
  await page.locator('#estremo_recente').fill(remoto);
  await page.waitForTimeout(700);   // Shiny riceve il valore digitato
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.modal-dialog')).toContainText('Due estremi uguali');
  await expect(page.locator('.modal-dialog')).toContainText(remoto);
  expect(recente()).toBe(originale);
  await foto(page, '42-d30-finestra.png');

  // No: salva com'è
  await page.getByRole('button', { name: /No, tieni tutti e due/ }).click();
  await expect(page.locator('.modal-dialog')).toHaveCount(0);
  await expect.poll(recente, { timeout: 10000 }).toBe(remoto);
  await assesta(page);
  // un secondo salvataggio con la stessa data non lo richiede
  await page.locator('#note').fill('prova D30, seconda');
  await page.waitForTimeout(700);   // Shiny riceve il valore digitato
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.shiny-notification').filter({ hasText: 'Salvata' }).last()).toBeVisible({ timeout: 10000 });
  await expect(page.locator('.modal-dialog')).toHaveCount(0);
  await assesta(page);

  // Sì (con un'altra data: la coppia del «No» non si richiede): si toglie il recente, anche a schermo
  await page.locator('#estremo_recente').fill(originale);
  await page.waitForTimeout(700);   // Shiny riceve il valore digitato
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect.poll(recente, { timeout: 10000 }).toBe(originale);
  await assesta(page);
  const altra = String(Number(remoto) + 1);
  await page.locator('#estremo_remoto').fill(altra);
  await page.locator('#estremo_recente').fill(altra);
  await page.waitForTimeout(700);   // Shiny riceve il valore digitato
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.modal-dialog')).toContainText('Due estremi uguali');
  await page.getByRole('button', { name: /Sì, togli il recente/ }).click();
  await expect.poll(recente, { timeout: 10000 }).toBe('-');
  await expect(page.locator('#estremo_recente')).toHaveValue('', { timeout: 10000 });

  // si rimette com'era
  await page.locator('#estremo_remoto').fill(remoto);
  await page.locator('#estremo_recente').fill(originale);
  await page.locator('#note').fill('');
  await page.waitForTimeout(700);   // Shiny riceve il valore digitato
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect.poll(recente, { timeout: 10000 }).toBe(originale);
});
