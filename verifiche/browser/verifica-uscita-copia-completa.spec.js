// Verifica nel browser del ripristino da copia completa (.zip). Va per ULTIMA (il nome la mette in coda): a ripristino
// riuscito Trama si chiude da sola e il server di prova si ferma. Database di esempio TEMPORANEO.
const { test, expect } = require('@playwright/test');
const fs = require('fs'), os = require('os'), path = require('path');
const { execFileSync, spawn } = require('child_process');
const { apriBackup } = require('./aiuti-pagina');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' });

const FOTO = process.env.SCREENSHOT_DIR || path.join(__dirname, 'schermate');   // cartella ignorata da git
test.beforeAll(() => fs.mkdirSync(FOTO, { recursive: true }));

test('ripristino da copia completa: file sbagliato, annulla, conferma e chiusura', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Import \/ Export/ }).click();
  await apriBackup(page);

  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-zip-'));
  const [dl] = await Promise.all([page.waitForEvent('download'), page.locator('#scarica_backup_completo:not(.disabled)').click()]);
  const zip = path.join(dir, 'copia.zip');
  await dl.saveAs(zip);
  expect(fs.statSync(zip).size).toBeGreaterThan(1000);

  // Lo stato cambia DOPO la copia (titolo di una scheda e lista dei luoghi): il ripristino deve riportare la copia, non lasciare il nuovo.
  const casa = (await page.locator('code.no-trad').first().innerText()).trim();
  const dbVivo = path.join(casa, 'trama.sqlite');
  const luoghiVivo = path.join(casa, 'luoghi', 'luoghi.csv');
  expect(fs.existsSync(dbVivo)).toBe(true);
  sql(dbVivo, "PRAGMA busy_timeout=5000; UPDATE fascicoli SET titolo = 'MODIFICATO DOPO LA COPIA' WHERE id = (SELECT id FROM fascicoli LIMIT 1);");
  expect(sql(dbVivo, "SELECT COUNT(*) FROM fascicoli WHERE titolo = 'MODIFICATO DOPO LA COPIA'").trim()).toBe('1');
  if (fs.existsSync(luoghiVivo)) fs.appendFileSync(luoghiVivo, '\nriga-aggiunta-dopo-la-copia\n');

  // senza file: avviso
  await page.locator('#esegui_completo').click();
  await expect(page.locator('.shiny-notification')).toContainText('Scegli prima il file .zip');
  await page.locator('.shiny-notification-close').click();

  // un .zip che non è una copia di Trama: errore leggibile, nessuna finestra di conferma
  const falso = path.join(dir, 'falso.zip');
  fs.writeFileSync(falso, 'non sono uno zip');
  await page.locator('#file_completo').setInputFiles(falso);
  await expect(page.locator('#anteprima_completo .alert-danger')).toBeVisible({ timeout: 15000 });
  await page.locator('#esegui_completo').click();
  await expect(page.locator('.modal-dialog')).toHaveCount(0);
  await page.locator('.shiny-notification-close').first().click();

  // la copia vera: anteprima con i fascicoli
  for (const x of await page.locator('.shiny-notification-close').all()) await x.click();
  await page.locator('#file_completo').setInputFiles(zip);
  await expect(page.locator('#anteprima_completo')).toContainText('Nella copia: 24 fascicoli', { timeout: 15000 });
  await expect(page.locator('#anteprima_completo')).toContainText('Ora nel database: 24 fascicoli');
  await page.locator('#esegui_completo').click();
  const finestra = page.locator('.modal-dialog');
  await expect(finestra).toContainText('Ripristinare la copia completa?');
  await expect(page.locator('.modal.show')).toBeVisible();
  await page.waitForTimeout(500);   // fine dell'animazione di apertura
  await page.screenshot({ path: path.join(FOTO, '8-copia-completa-conferma.png') });

  // Annulla: l'app resta in vita
  await finestra.getByRole('button', { name: 'Annulla' }).click();
  await expect(finestra).toBeHidden();
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i);

  // conferma: ripristina e dice di riaprire
  await page.locator('#esegui_completo').click();
  await finestra.getByRole('button', { name: 'Sostituisci e chiudi Trama' }).click();
  await expect(finestra).toContainText('Copia completa ripristinata', { timeout: 30000 });
  await expect(finestra).toContainText('Ripristinati 24 fascicoli');
  await expect(finestra).toContainText('Trama si è chiusa');
  await page.waitForTimeout(500);
  await page.screenshot({ path: path.join(FOTO, '9-copia-completa-fatto.png') });

  // Il confronto decisivo, a server fermo: ciò che sta ora nella cartella dati è identico a ciò che c'era nella copia.
  await page.waitForTimeout(3500);
  const estratti = fs.mkdtempSync(path.join(os.tmpdir(), 'trama-zip-estratti-'));
  execFileSync('unzip', ['-q', zip, '-d', estratti]);
  const dump = db => sql(db, '.dump');
  expect(sql(dbVivo, 'PRAGMA integrity_check;').trim()).toBe('ok');
  expect(dump(dbVivo)).toBe(dump(path.join(estratti, 'trama.sqlite')));
  expect(sql(dbVivo, "SELECT COUNT(*) FROM fascicoli WHERE titolo = 'MODIFICATO DOPO LA COPIA'").trim()).toBe('0');
  if (fs.existsSync(path.join(estratti, 'luoghi', 'luoghi.csv')))
    expect(fs.readFileSync(luoghiVivo, 'utf8')).toBe(fs.readFileSync(path.join(estratti, 'luoghi', 'luoghi.csv'), 'utf8'));
  // le copie «di prima» (con la modifica) sono state messe da parte
  const prima = fs.readdirSync(path.join(casa, 'backup')).filter(f => f.startsWith('trama-prima-ripristino-completo'));
  expect(prima.length).toBeGreaterThan(0);
  expect(sql(path.join(casa, 'backup', prima[0]), "SELECT COUNT(*) FROM fascicoli WHERE titolo = 'MODIFICATO DOPO LA COPIA'").trim()).toBe('1');

  // Riapertura: Trama riparte proprio sulla cartella ripristinata (nessun dato di esempio ricreato) e le schede
  // hanno i contenuti della copia, non quelli cambiati dopo.
  const progetto = path.resolve(__dirname, '..', '..');
  const porta = 4331;
  const app = spawn('Rscript', ['-e', `shiny::runApp(".", port = ${porta}, host = "127.0.0.1", launch.browser = FALSE)`],
    { cwd: progetto, env: { ...process.env, TRAMA_DB: dbVivo, TRAMA_HOME: casa, TRAMA_COLLAUDO: '1' }, stdio: 'ignore' });
  try {
    await expect.poll(async () => { try { return (await fetch(`http://127.0.0.1:${porta}/`)).status; } catch { return 0; } },
      { timeout: 120000, intervals: [1000] }).toBe(200);
    const nuova = await page.context().newPage();
    await nuova.setViewportSize({ width: 1280, height: 900 });
    await nuova.goto(`http://127.0.0.1:${porta}/`);
    await expect(nuova.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio.*\(24\)/i, { timeout: 60000 });
    await nuova.getByRole('tab', { name: /Fascicoli/ }).first().click();
    for (const chi of ['Hassan da Tunisi', 'Francesca Pini', 'Vittoria Belli']) {
      const [titolo, regesto] = sql(path.join(estratti, 'trama.sqlite'),
        `SELECT titolo || char(9) || regesto FROM fascicoli WHERE titolo LIKE '%${chi}%'`).replace(/\n$/, '').split('\t');
      await expect(nuova.locator('#cerca_fascicoli')).toBeVisible();   // si è tornati all'elenco
      await nuova.locator('#cerca_fascicoli').fill(chi);
      const righe = nuova.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
      await expect(righe).toHaveCount(1, { timeout: 15000 });
      await expect(righe.first()).toContainText(chi);   // l'elenco ricorda l'ultima ricerca: si attende quella nuova
      await righe.first().locator('td').nth(2).click();
      await expect(nuova.locator('#titolo')).toHaveValue(titolo, { timeout: 15000 });
      await expect(nuova.locator('#regesto')).toHaveValue(regesto);
      await nuova.getByRole('button', { name: /Indietro ai Fascicoli/ }).first().click();
    }
    await nuova.screenshot({ path: path.join(FOTO, '28-riaperta-dopo-ripristino.png') });
  } finally { app.kill(); }
});
