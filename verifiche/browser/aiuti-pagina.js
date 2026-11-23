// Aiuti comuni alle prove nel browser.

// «Backup e ripristino» (details.altro-export) sta in una sezione chiusa. Si apre dal DOM, non con un clic sul titolo: durante il cambio
// di scheda il clic a volte non arriva, e quello che le prove controllano sono i pulsanti dentro, non il titolo. È idempotente.
async function apriBackup(page) {
  await page.evaluate(() => { document.querySelector('details.altro-export').open = true; });
}

module.exports = { apriBackup };
