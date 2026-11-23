// Lanciatore delle prove nel browser (24/10/2026): avvia Trama Lite su un database di esempio TEMPORANEO, mai quello vero.
// Uso (lo chiamano i file playwright*.config.js):  node avvia-app.js <porta>
// Il progetto è due cartelle sopra questa; cartella dati e database stanno in una cartella temporanea che si cancella alla chiusura.
const { spawnSync, spawn } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const porta = process.argv[2] || '4321';
const progetto = path.resolve(__dirname, '..', '..');
const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'e2e-trama-lite-'));
const db = path.join(temp, 'trama.sqlite');
const env = { ...process.env, TRAMA_DB: db, TRAMA_HOME: temp, TRAMA_COLLAUDO: '1' };
// Come avvia.command: in una copia nuova la libreria di renv non c'è (o punta alla cache di un altro Mac): si usa quella di R.
const libreriaRenv = path.join(progetto, 'renv', 'library');
const usabile = fs.existsSync(libreriaRenv) && fs.readdirSync(libreriaRenv, { recursive: true }).some(f => /(^|[\\/])shiny$/.test(String(f)));
if (!usabile) env.RENV_CONFIG_AUTOLOADER_ENABLED = 'FALSE';

const prepara = spawnSync('Rscript', ['-e',
  `suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)); ` +
  `con <- apri_db(Sys.getenv("TRAMA_DB")); invisible(carica_esempio(con)); DBI::dbDisconnect(con)`],
  { cwd: progetto, env, stdio: 'inherit' });
if (prepara.status !== 0) process.exit(prepara.status || 1);

const app = spawn('Rscript', ['-e',
  `shiny::runApp(".", port = ${porta}, host = "127.0.0.1", launch.browser = FALSE)`],
  { cwd: progetto, env, stdio: 'inherit' });
const chiudi = () => { app.kill(); fs.rmSync(temp, { recursive: true, force: true }); };
process.on('SIGTERM', () => { chiudi(); process.exit(0); });
process.on('SIGINT', () => { chiudi(); process.exit(0); });
// Se Trama si chiude da sola (ripristino da copia completa) la cartella resta: le prove ne leggono i file; la toglie la chiusura di Playwright.
app.on('exit', code => process.exit(code ?? 0));
