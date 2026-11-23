// Prova nel browser contro l'app che gira in una macchina virtuale Windows (UTM): URL in TRAMA_URL, nessun server locale.
const { defineConfig } = require('@playwright/test');
module.exports = defineConfig({
  testDir: __dirname,
  testMatch: ['**/verifica-menu-e-usabilita.spec.js', '**/verifica-studio.spec.js', '**/verifica-scheda-grafica.spec.js', '**/verifica-scala-grafica.spec.js', '**/verifica-fondo-anno.spec.js', '**/verifica-fascicoli-ridisegno.spec.js'],
  timeout: 90000, workers: 1,
  use: { headless: true, baseURL: process.env.TRAMA_URL, actionTimeout: 30000 },
});
