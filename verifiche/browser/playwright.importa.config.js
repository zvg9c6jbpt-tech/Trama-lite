// Verifica nel browser di Trama Lite (24/10/2026). Lanciatore proprio (avvia-app.js, accanto): database di esempio TEMPORANEO.
//   cd verifiche/browser && npm install && npm run installa-browser    (una volta)
//   cd verifiche/browser && npm run prove      (oppure: npm run prova-importa)
// Le prove che scrivono davvero (Import / Export; Gestione), su un server loro: ognuna rimette il database com'era.
const { defineConfig } = require('@playwright/test');
const path = require('path');
const porta = process.env.TRAMA_PORTA || '4321';   // npm run tutto la sposta
module.exports = defineConfig({
  testDir: __dirname,
  testMatch: ['**/verifica-importa-esporta.spec.js', '**/verifica-omonimi.spec.js','**/verifica-azioni-gestione.spec.js', '**/verifica-qualita-colonne.spec.js', '**/verifica-gestione-fusioni.spec.js', '**/verifica-bozza-conservata.spec.js', '**/verifica-cancellazione-completa.spec.js', '**/verifica-termini.spec.js'],
  timeout: 60000,
  workers: 1,
  use: { headless: true, baseURL: `http://127.0.0.1:${porta}`, actionTimeout: 20000 },
  webServer: {
    command: `node "${path.resolve(__dirname, 'avvia-app.js')}" ${porta}`,
    url: `http://127.0.0.1:${porta}`, timeout: 180000, reuseExistingServer: false,
  },
});
