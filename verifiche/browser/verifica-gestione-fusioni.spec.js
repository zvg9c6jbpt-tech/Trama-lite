// Verifica nel browser (24/10/2026) di ciò che scrive e ancora non era provato: «Rinumera nell'ordine mostrato» (e il suo annullamento),
// «Ricomponi titolo e regesto» dalle spunte, fusione dei luoghi (pagina Luoghi e pulsante di Qualità); poi, con DUE fondi,
// la Home (cambio di fondo) e «Rimuovi duplicati» con le spunte. Ogni passo si controlla nel database. Database di esempio
// TEMPORANEO, server proprio (playwright.importa.config.js); a fine prova il database torna com'era con il ripristino da JSON.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync } = require('child_process');
const { apriBackup } = require('./aiuti-pagina');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' }).trim();

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));
const TUTTO = 'SELECT id, fondo, busta, fascicolo, titolo, regesto FROM fascicoli ORDER BY id';

async function apri(page) {
  await page.setViewportSize({ width: 1280, height: 1000 });
  const errori = [];
  page.on('pageerror', e => errori.push('JS: ' + e.message));
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const pronto = () => page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy'), null, { timeout: 60000 });
  const db = (await page.evaluate(() => (document.querySelector('code.no-trad') || {}).textContent)).trim() + '/trama.sqlite';
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-gf-'));
  const finestra = page.locator('.modal-dialog');
  const sezione = async () => { if (!(await page.locator('#modo_ripristino').isVisible())) await apriBackup(page); };
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  await sezione();
  await expect(page.locator('#scarica_backup')).toBeVisible();
  const [dj] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup:not(.disabled)').click()]);
  const jsonIniziale = path.join(dir, 'iniziale.json'); await dj.saveAs(jsonIniziale);
  const ripristina = async () => {
    await page.getByRole('tab', { name: /Import \/ Export/ }).click();
    await sezione();
    await page.locator('#file_ripristino').setInputFiles(jsonIniziale);
    await expect(page.locator('#file_ripristino_progress')).toContainText(/Upload complete|completato/, { timeout: 15000 });
    await page.locator('#modo_ripristino').getByLabel('Sostituisci tutto').check();
    await page.locator('#esegui_ripristino').click();
    await finestra.getByRole('button', { name: 'Sostituisci con il backup' }).click();
  };
  const ricarica = async () => {
    await page.reload();
    await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempi/i, { timeout: 60000 });
    await pronto(); await page.waitForTimeout(1000);
  };
  const avviso = (testo, t = 30000) => expect(page.locator('.shiny-notification', { hasText: testo }).first()).toBeVisible({ timeout: t });
  return { db, dir, finestra, pronto, jsonIniziale, ripristina, ricarica, avviso, errori };
}

test('Gestione e Qualità: rinumera e annulla, ricomponi dalle spunte, fusione dei luoghi', async ({ page }) => {
  test.setTimeout(300000);
  const { db, finestra, pronto, ripristina, ricarica, avviso, errori } = await apri(page);
  const prima = sql(db, TUTTO);
  const luoghiTutti = sql(db, 'SELECT fascicolo_id, ordine, luogo FROM luoghi ORDER BY fascicolo_id, ordine');
  try {
    // dati di prova per le fusioni: «bologna» (minuscolo) e «Módena» (accento) accanto alle grafie giuste
    sql(db, "UPDATE luoghi SET luogo='bologna' WHERE rowid IN (SELECT rowid FROM luoghi WHERE luogo='Bologna' ORDER BY fascicolo_id LIMIT 1)");
    sql(db, "UPDATE luoghi SET luogo='Módena' WHERE rowid IN (SELECT rowid FROM luoghi WHERE luogo='Modena' ORDER BY fascicolo_id LIMIT 1)");
    await ricarica();

    // - Fusione dalla pagina Luoghi: si sceglie la grafia da tenere per gruppo, poi si applicano le scelte
    await page.getByRole('tab', { name: /Luoghi/ }).first().click();
    await pronto();
    await expect(page.locator('.riquadro-doppioni')).toContainText('Possibili doppioni', { timeout: 20000 });
    await page.screenshot({ path: path.join(FOTO, '36-luoghi-doppioni.png') });
    await page.locator('.riquadro-doppioni #luoghi_dop_applica').click();         // senza scelte: avviso, niente cambia
    await avviso('Scegli prima');
    expect(sql(db, "SELECT COUNT(*) FROM luoghi WHERE luogo='bologna'")).toBe('1');
    await page.locator('.riquadro-doppioni').locator('input[type=radio][value="Bologna"]').check();
    await page.locator('.riquadro-doppioni #luoghi_dop_applica').click();
    await avviso('1 gruppo corretto');
    await expect.poll(() => sql(db, "SELECT COUNT(*) FROM luoghi WHERE luogo='bologna'"), { timeout: 20000 }).toBe('0');
    expect(sql(db, "SELECT COUNT(*) FROM luoghi WHERE luogo='Módena'")).toBe('1');   // l'altro gruppo non si è mosso
    expect(sql(db, "SELECT COUNT(*) FROM luoghi WHERE luogo='Bologna'")).toBe('10');

    // - Fusione dal pulsante di Qualità: «Fondi 1 gruppo proposto»
    await page.getByRole('tab', { name: /Qualità/ }).first().click();
    await pronto();
    const fondi = page.locator('#fondi_luoghi');
    await expect(fondi).toContainText('Fondi 1 gruppo proposto', { timeout: 20000 });
    await page.screenshot({ path: path.join(FOTO, '37-qualita-fusione.png') });
    await fondi.click();
    await avviso('Fuso 1 gruppo');
    await expect.poll(() => sql(db, "SELECT COUNT(*) FROM luoghi WHERE luogo='Módena'"), { timeout: 20000 }).toBe('0');
    expect(sql(db, "SELECT COUNT(*) FROM luoghi WHERE luogo='Modena'")).toBe('3');
    await expect(fondi).toHaveCount(0);

    // - Rinumera nell'ordine mostrato: busta 1, primo numero 50; poi «Annulla ultima correzione»
    const busta1 = () => sql(db, "SELECT group_concat(id || ':' || fascicolo) FROM (SELECT id, fascicolo FROM fascicoli WHERE busta=1 ORDER BY id)");
    const originale = busta1();
    const quanti = sql(db, 'SELECT COUNT(*) FROM fascicoli WHERE busta=1');
    await page.getByRole('tab', { name: /Gestione/ }).click();
    await page.getByRole('tab', { name: 'Ordine e segnature' }).click();
    await page.locator('#ord_busta + .selectize-control .selectize-input').click();
    await page.locator('.selectize-dropdown-content .option', { hasText: /^Busta 1$/ }).click();
    await pronto();
    await page.locator('#ord_rinumera').click();
    await expect(finestra).toContainText("Rinumera i fascicoli nell'ordine mostrato");
    await finestra.locator('#corr_primo').fill('50');
    await expect(finestra).toContainText('fasc. 50', { timeout: 15000 });
    await page.screenshot({ path: path.join(FOTO, '38-rinumera-anteprima.png') });
    await finestra.getByRole('button', { name: 'Annulla' }).click();            // annullata: niente cambia
    await expect(finestra).toBeHidden();
    expect(busta1()).toBe(originale);
    await page.locator('#ord_rinumera').click();
    await finestra.locator('#corr_primo').fill('50');
    await expect(finestra).toContainText('fasc. 50', { timeout: 15000 });
    await finestra.getByRole('button', { name: 'Applica correzione' }).click();
    await expect(page.locator('#ord_esito')).toContainText(`Corrette ${quanti} segnature`, { timeout: 20000 });
    expect(sql(db, 'SELECT MIN(fascicolo) || \'-\' || MAX(fascicolo) FROM fascicoli WHERE busta=1')).toBe(`50-${49 + Number(quanti)}`);
    expect(sql(db, 'SELECT COUNT(*) FROM fascicoli WHERE busta=1')).toBe(quanti);       // nessuna scheda persa, stessi id
    await page.locator('#ord_annulla_correzione').click();
    await expect(page.locator('#ord_esito')).toContainText('Ultima correzione annullata', { timeout: 20000 });
    expect(busta1()).toBe(originale);

    // - Ricomponi dalle spunte: busta 1 fasc. 1 (titolo scritto a mano nella forma di prima)
    const id1 = sql(db, "SELECT id FROM fascicoli WHERE busta=1 AND fascicolo=1");
    const titolo1 = () => sql(db, `SELECT titolo FROM fascicoli WHERE id='${id1}'`);
    const resto = () => sql(db, `SELECT id, titolo, regesto FROM fascicoli WHERE id<>'${id1}' ORDER BY id`);
    const restoPrima = resto();
    expect(titolo1()).toBe('Processo contro Lucia Ferrante');
    await page.getByRole('tab', { name: /Fascicoli/ }).first().click();
    await pronto();
    await page.locator(`#tabella .spunta-riga[data-id="${id1}"]`).check();
    await expect(page.locator('#sel_ricomponi')).toBeVisible();
    await page.locator('#sel_ricomponi').click();
    await expect(finestra).toContainText('Ricomponi titolo e regesto di 1 fascicoli');
    await finestra.getByLabel('Anche i titoli scritti a mano').check();
    await expect(finestra).toContainText('Sostituirebbe testi scritti a mano', { timeout: 15000 });
    await page.screenshot({ path: path.join(FOTO, '39-ricomponi-anteprima.png') });
    await finestra.locator('#sel_ricomponi_applica').click();                  // senza la conferma: rifiutato, niente cambia
    await avviso('Spunta la conferma');
    expect(titolo1()).toBe('Processo contro Lucia Ferrante');
    await finestra.getByLabel(/Ho guardato le differenze/).check();
    await finestra.locator('#sel_ricomponi_applica').click();
    await avviso('Ricomposto 1 fascicolo');
    await expect.poll(titolo1, { timeout: 20000 }).not.toBe('Processo contro Lucia Ferrante');
    expect(resto()).toBe(restoPrima);                                           // solo la scheda spuntata
    const backup = path.join(path.dirname(db), 'backup');
    const copia = fs.readdirSync(backup).filter(f => /ricomposizione/.test(f) && f.endsWith('.sqlite'));
    expect(copia.length).toBeGreaterThan(0);
    expect(copia.some(f => sql(path.join(backup, f), `SELECT titolo FROM fascicoli WHERE id='${id1}'`) === 'Processo contro Lucia Ferrante')).toBe(true);
    expect(errori).toEqual([]);
  } finally {
    await ripristina();
    await expect.poll(() => sql(db, TUTTO), { timeout: 60000 }).toBe(prima);
    const dopo = () => sql(db, 'SELECT fascicolo_id, ordine, luogo FROM luoghi ORDER BY fascicolo_id, ordine');
    await expect.poll(dopo, { timeout: 30000 }).toBe(luoghiTutti);   // il ripristino riscrive le tabelle figlie dopo la scheda
  }
});

test('Home con due fondi e «Rimuovi duplicati» con le spunte', async ({ page }) => {
  test.setTimeout(300000);
  const { db, dir, finestra, pronto, ripristina, avviso, errori } = await apri(page);
  const prima = sql(db, TUTTO);
  try {
    // secondo fondo: il foglio del primo, importato come «Tribunale di esempi» nello stesso archivio (nome quasi uguale: stesse segnature)
    await page.getByRole('tab', { name: /Import \/ Export/ }).click();
    const [dl] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_csv:not(.disabled)').click()]);
    const csv = path.join(dir, 'fondo.csv'); await dl.saveAs(csv);
    await page.locator('#file_import').setInputFiles(csv);
    await expect(page.locator('#anteprima_import')).toContainText('24 righe', { timeout: 20000 });
    await page.locator('#modo_import').getByText('Importa nuovo fondo', { exact: false }).first().click();
    await pronto();
    await page.locator('#import_archivio').fill('Archivio di Esempio');
    await page.locator('#import_fondo').fill('Tribunale di esempi');
    await page.locator('#esegui_import').click();
    await expect(finestra).toContainText('Controlla prima di importare', { timeout: 20000 });
    await finestra.getByRole('button', { name: /Importa com.è, senza correggere/ }).click();
    await expect.poll(() => sql(db, 'SELECT COUNT(*) FROM fascicoli'), { timeout: 60000 }).toBe('48');
    await pronto();
    expect(sql(db, 'SELECT COUNT(*) FROM fondi')).toBe('2');
    await expect(finestra).toContainText('Fondo nuovo importato', { timeout: 20000 });
    await finestra.getByRole('button', { name: 'Apri in Home' }).click(); await pronto();

    // - Home: il fondo in uso in evidenza, l'altro a un clic; «Lavora su questo fondo» lo rende attivo
    await page.locator('.navbar-brand').first().click();
    await pronto();
    const altri = page.locator('details.home-altri-fondi');
    await expect(altri.locator('summary')).toContainText('Cambia fondo (1 altri)', { timeout: 20000 });
    await altri.evaluate(d => { d.open = true; });   // dal 22/11 è già aperto con pochi fondi: un clic lo chiuderebbe
    await expect(page.locator('.home-uno')).toContainText(/Tribunale di esempi(?!o)/);   // dopo l'import il fondo nuovo è quello in uso
    await expect(altri).toContainText(/Tribunale di esempio/);
    await expect(altri).toContainText('24');
    await page.screenshot({ path: path.join(FOTO, '40-home-due-fondi.png') });
    const attivo = () => page.locator('#fondo_etichetta').innerText();
    const prima_etichetta = await attivo();
    await altri.getByRole('button', { name: /Lavora su questo fondo/ }).click();
    await expect.poll(attivo, { timeout: 20000 }).not.toBe(prima_etichetta);
    await pronto();
    await expect(page.locator('.home-uno')).toContainText(/Tribunale di esempio/, { timeout: 20000 });
    await expect(page.locator('details.home-altri-fondi')).toContainText(/Tribunale di esempi(?!o)/, { timeout: 20000 });

    // - Rimuovi duplicati: 24 gruppi (stesso archivio, stessa segnatura, fondo scritto in due modi)
    await page.getByRole('tab', { name: /Gestione/ }).click();
    await page.locator('#dup_analizza').click();
    await expect(page.locator('#dup_esito')).toContainText('24 gruppi di possibili duplicati', { timeout: 30000 });
    await page.locator('#dup_esito').scrollIntoViewIfNeeded();
    await page.screenshot({ path: path.join(FOTO, '41-duplicati-gruppi.png') });
    // senza spunte: avviso, niente cambia
    await page.locator('#dup_rimuovi').click();
    await avviso('Spunta almeno un gruppo');
    expect(sql(db, 'SELECT COUNT(*) FROM fascicoli')).toBe('48');
    // gruppo 1: tengo la copia che NON è proposta (la seconda), e lo spunto
    const gruppo1 = page.locator('#dup_esito .border').first();
    const scelte = gruppo1.locator('input[type=radio]');
    const proposta = await scelte.evaluateAll(l => l.findIndex(x => x.checked));
    await scelte.nth(proposta === 0 ? 1 : 0).check();
    const tenuto = await gruppo1.locator('input[type=radio]:checked').getAttribute('value');
    const ids1 = await scelte.evaluateAll(l => l.map(x => x.value));
    await gruppo1.locator('input[type=checkbox]').check();
    await page.locator('#dup_rimuovi').click();
    await avviso('Eliminata 1 copia');
    await expect.poll(() => sql(db, 'SELECT COUNT(*) FROM fascicoli'), { timeout: 30000 }).toBe('47');
    expect(sql(db, `SELECT COUNT(*) FROM fascicoli WHERE id='${tenuto}'`)).toBe('1');             // resta quella scelta
    expect(ids1.filter(i => i !== tenuto).map(i => sql(db, `SELECT COUNT(*) FROM fascicoli WHERE id='${i}'`))).toEqual(['0']);
    await expect(page.locator('#dup_esito')).toContainText('23 gruppi di possibili duplicati', { timeout: 20000 });
    const backup = path.join(path.dirname(db), 'backup');
    expect(fs.readdirSync(backup).some(f => /duplicati/.test(f))).toBe(true);
    // gli altri 23 gruppi non si sono mossi: ogni segnatura c'è ancora due volte
    expect(sql(db, 'SELECT COUNT(*) FROM (SELECT busta, fascicolo FROM fascicoli GROUP BY 1, 2 HAVING COUNT(*)=2)')).toBe('23');
    expect(errori).toEqual([]);
  } finally {
    await ripristina();
    await expect.poll(() => sql(db, TUTTO), { timeout: 60000 }).toBe(prima);
    expect(sql(db, 'SELECT COUNT(*) FROM fondi')).toBe('1');
  }
});
