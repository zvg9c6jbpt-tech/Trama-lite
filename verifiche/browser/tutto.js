// Tutte le verifiche con un comando solo (04/11/2026):  cd verifiche/browser && npm run tutto
// Lancia insieme la suite R (verifiche/tutte.R), le prove nel browser in due gruppi (porte 4321 e 4322)
// e le prove che scrivono (Import / Export, Gestione: porta 4323). Ogni server ha il suo database temporaneo.
// Alla fine un riepilogo con durata ed esito di ciascuna parte; stato 0 solo se tutte vanno.
// L'output completo di chi fallisce resta nei file indicati (cartella temporanea); `TRAMA_TUTTO_SERIALE=1` le lancia una alla volta.
const { spawn } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const progetto = path.resolve(__dirname, '..', '..');
const log = fs.mkdtempSync(path.join(os.tmpdir(), 'tutto-trama-lite-'));
const npx = process.platform === 'win32' ? 'npx.cmd' : 'npx';
const playwright = config => ({ cmd: npx, args: ['playwright', 'test', '-c', config, '--reporter=line'], cwd: __dirname });

const parti = [
  { nome: 'suite R', cmd: 'Rscript', args: ['verifiche/tutte.R'], cwd: progetto, env: {} },
  { nome: 'prove browser 1', ...playwright('playwright.config.js'), env: { TRAMA_GRUPPO: '1', TRAMA_PORTA: '4321' } },
  { nome: 'prove browser 2', ...playwright('playwright.config.js'), env: { TRAMA_GRUPPO: '2', TRAMA_PORTA: '4322' } },
  { nome: 'prove import/gestione', ...playwright('playwright.importa.config.js'), env: { TRAMA_PORTA: '4323' } },
];

const lancia = p => new Promise(fine => {
  const file = path.join(log, p.nome.replace(/[^a-z0-9]+/gi, '-') + '.txt');
  const inizio = Date.now();
  const out = fs.openSync(file, 'w');
  const figlio = spawn(p.cmd, p.args, { cwd: p.cwd, env: { ...process.env, ...p.env }, stdio: ['ignore', out, out] });
  figlio.on('error', e => { fs.writeSync(out, String(e)); fine({ ...p, file, stato: 127, s: 0 }); });
  figlio.on('close', stato => { fs.closeSync(out); fine({ ...p, file, stato, s: Math.round((Date.now() - inizio) / 1000) }); });
});

(async () => {
  const inizio = Date.now();
  let esiti;
  if (process.env.TRAMA_TUTTO_SERIALE === '1') { esiti = []; for (const p of parti) esiti.push(await lancia(p)); }
  else esiti = await Promise.all(parti.map(lancia));
  console.log('\nRIEPILOGO');
  for (const e of esiti) {
    const coda = fs.readFileSync(e.file, 'utf8').trim().split('\n').filter(Boolean).slice(-1)[0] || '';
    console.log(`${e.stato === 0 ? 'ok    ' : 'FALLITO'}  ${e.nome.padEnd(22)} ${String(e.s).padStart(4)} s   ${coda.slice(0, 90)}`);
    if (e.stato !== 0) console.log(`        output completo: ${e.file}`);
  }
  const tutti = esiti.every(e => e.stato === 0);
  console.log(`\nESITO: ${tutti ? 'ok' : 'NON TORNA'}  (${Math.round((Date.now() - inizio) / 1000)} s in tutto)`);
  if (tutti) fs.rmSync(log, { recursive: true, force: true });
  process.exit(tutti ? 0 : 1);
})();
