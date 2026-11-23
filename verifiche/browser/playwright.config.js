// Verifica nel browser di Trama Lite (24/10/2026). Lanciatore proprio (avvia-app.js, accanto): database di esempio TEMPORANEO.
//   cd verifiche/browser && npm install && npm run installa-browser    (una volta)
//   cd verifiche/browser && npm run prove      (oppure: npm run prova-importa)
const { defineConfig } = require('@playwright/test');
const path = require('path');
// Due gruppi bilanciati (misurati il 04/11/2026: la scheda grafica da sola dura ~215 s, il resto ~220 s), su due porte e due database:
// TRAMA_GRUPPO=1|2 e TRAMA_PORTA (npm run tutto li imposta). Senza TRAMA_GRUPPO girano tutte, come prima, sulla 4321.
const porta = process.env.TRAMA_PORTA || '4321';
const gruppo = process.env.TRAMA_GRUPPO;
const LENTE = '**/verifica-scheda-grafica.spec.js';
module.exports = defineConfig({
  testDir: __dirname,
  testMatch: gruppo === '1' ? [LENTE] : '**/*.spec.js',
  testIgnore: [...(gruppo === '2' ? [LENTE] : []), '**/verifica-omonimi.spec.js', '**/verifica-importa-esporta.spec.js', '**/verifica-azioni-gestione.spec.js', '**/verifica-qualita-colonne.spec.js', '**/verifica-gestione-fusioni.spec.js', '**/verifica-termini.spec.js', '**/verifica-bozza-conservata.spec.js', '**/verifica-cancellazione-completa.spec.js'],   // cambiano fondi e schede: hanno un server loro (playwright.importa.config.js); gli omonimi girano solo lì
  timeout: 60000,
  workers: 1,
  use: { headless: true, baseURL: `http://127.0.0.1:${porta}`, actionTimeout: 20000 },
  webServer: {
    command: `node "${path.resolve(__dirname, 'avvia-app.js')}" ${porta}`,
    url: `http://127.0.0.1:${porta}`, timeout: 180000, reuseExistingServer: false,
  },
});
