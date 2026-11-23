// Le transizioni delle macchine a stati che non avevano un collaudo (18/11/2026; vedi docs/STATI.md, macchine scheda_aperta, guardia_modifiche e cancellazione).
// Niente resta scritto: «Salva» senza modifiche non scrive, «Scarta» e «Resta» non toccano il database, la finestra di cancellazione si chiude con «Annulla».
// Database di esempio TEMPORANEO (avvia-app.js).
const { test, expect } = require('@playwright/test');
const { execFileSync } = require('child_process');
const sql = (db, q) => execFileSync('sqlite3', [db, q], { encoding: 'utf8' });
const pronto = async page => {
  await page.waitForFunction(() => !document.documentElement.classList.contains('shiny-busy') &&
    !document.querySelector('.shiny-notification .progress'), null, { timeout: 30000 });
  await page.waitForTimeout(400);
};
const casa = page => page.evaluate(() => document.querySelector('code.no-trad').innerText.trim());

test('Guardia delle modifiche: Resta tiene la bozza, Scarta la butta; Salva senza modifiche non scrive', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
  await page.locator('#cerca_fascicoli').fill('Marco Fontana'); await pronto(page);
  await page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)').first().locator('td').nth(2).click();
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 });
  await pronto(page); await page.waitForTimeout(2500);
  const db = (await casa(page)) + '/trama.sqlite';
  const prima = sql(db, 'SELECT id, busta, fascicolo, titolo, regesto, note FROM fascicoli ORDER BY id');
  const originale = await page.locator('#note').inputValue();

  // scheda_aperta: pronta -> pronta, «Nessuna modifica»
  await page.getByRole('button', { name: 'Salva', exact: true }).click();
  await expect(page.locator('.shiny-notification')).toContainText(/Nessuna modifica/i, { timeout: 10000 });

  // pronta -> modificata; modificata -> guardia (in attesa di scelta) -> «Resta»: la bozza resta
  await page.locator('#note').fill('BOZZA DELLA PROVA');
  await expect(page.locator('#salva_succ')).toContainText(/Salva e prossimo/, { timeout: 10000 });   // il server ha visto la bozza
  await page.locator('#annulla_scheda').evaluate(e => e.click());
  await expect(page.getByText('Modifiche non salvate')).toBeVisible({ timeout: 10000 });
  await page.getByRole('button', { name: /Resta sulla scheda/ }).click(); await pronto(page);
  await expect(page.getByText('Modifiche non salvate')).toHaveCount(0);
  expect(await page.locator('#note').inputValue(), 'dopo «Resta» la bozza è ancora lì').toBe('BOZZA DELLA PROVA');

  // guardia -> «Scarta e continua»: l'azione parte (la scheda si ricarica dal database) e la bozza sparisce
  await page.locator('#annulla_scheda').evaluate(e => e.click());
  await expect(page.getByText('Modifiche non salvate')).toBeVisible({ timeout: 10000 });
  await page.getByRole('button', { name: /Scarta e continua/ }).click(); await pronto(page);
  await expect(page.getByText('Modifiche non salvate')).toHaveCount(0);
  await expect(page.locator('#note')).toHaveValue(originale, { timeout: 10000 });

  // dopo lo scarto nessuna guardia: l'azione parte subito
  await page.locator('#annulla_scheda').evaluate(e => e.click()); await pronto(page);
  await expect(page.getByText('Modifiche non salvate')).toHaveCount(0);

  expect(sql(db, 'SELECT id, busta, fascicolo, titolo, regesto, note FROM fascicoli ORDER BY id'), 'nessuna scheda è cambiata nel database').toBe(prima);
});

test('Cancellazione di un fondo: il pulsante resta spento finché la scelta e il numero non sono giusti; Annulla non cancella', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  const db = (await casa(page)) + '/trama.sqlite';
  const n = parseInt(sql(db, 'SELECT count(*) FROM fascicoli').trim(), 10);
  const fondi = sql(db, 'SELECT count(*) FROM fondi').trim();
  await page.getByRole('tab', { name: /Gestione/ }).click(); await pronto(page);
  await page.locator('#canc_fondo_apri').evaluate(e => e.click());
  await expect(page.getByText('Eliminare questo fondo?')).toBeVisible({ timeout: 10000 });
  const elimina = page.getByRole('button', { name: 'Elimina il fondo' });
  await expect(elimina, 'finestra_aperta: spento').toBeDisabled();
  await page.getByLabel('Ho esportato quello che mi serve').check();
  await expect(elimina, 'solo la scelta, senza numero: ancora spento').toBeDisabled();
  await page.locator('#canc_conferma').fill(String(n + 1));
  await expect(elimina, 'numero sbagliato: spento').toBeDisabled();
  await page.locator('#canc_conferma').fill(String(n));
  await expect(elimina, 'pronta: acceso').toBeEnabled();
  await page.locator('#canc_conferma').fill(String(n + 1));
  await expect(elimina, 'pronta -> finestra_aperta: si rispegne').toBeDisabled();
  await page.getByRole('button', { name: 'Annulla' }).click();
  await expect(page.getByText('Eliminare questo fondo?')).toHaveCount(0);
  expect(sql(db, 'SELECT count(*) FROM fascicoli').trim(), 'nessuna scheda cancellata').toBe(String(n));
  expect(sql(db, 'SELECT count(*) FROM fondi').trim(), 'nessun fondo cancellato').toBe(fondi);
});

// Indietro verso l'elenco non cambia fascicolo: niente guardia (cronologia.R, `vai`), e la bozza resta nella scheda; Avanti la ritrova.
test('Indietro e Avanti con una bozza: tornare all\'elenco non chiede nulla e non perde la bozza', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
  await page.locator('#cerca_fascicoli').fill('Marco Fontana'); await pronto(page);
  await page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)').first().locator('td').nth(2).click();
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 });
  await pronto(page); await page.waitForTimeout(2500);
  const indirizzo = page.url();
  await page.locator('#note').fill('BOZZA PER INDIETRO');
  await expect(page.locator('#salva_succ')).toContainText(/Salva e prossimo/, { timeout: 10000 });

  await page.goBack(); await pronto(page);
  await expect(page.getByText('Modifiche non salvate'), 'stesso fascicolo: nessuna guardia').toHaveCount(0);
  await expect(page.locator('#regesto'), 'si è tornati all\'elenco').toBeHidden({ timeout: 10000 });

  await page.goForward(); await pronto(page);
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 10000 });
  expect(page.url(), 'l\'indirizzo è tornato quello della scheda').toBe(indirizzo);
  expect(await page.locator('#note').inputValue(), 'la bozza è ancora lì').toBe('BOZZA PER INDIETRO');
  await page.locator('#annulla_scheda').evaluate(e => e.click());   // butta la bozza (passa dalla guardia)
  await page.getByRole('button', { name: /Scarta e continua/ }).click({ timeout: 10000 }).catch(() => {});
});

// La guardia raggiunta da un cambio di scheda (clic su un'altra riga). Indietro che cambia fascicolo non si raggiunge dal solo elenco: la cronologia sostituisce le voci fra schede.
test('Bozza e cambio di scheda: «Resta» e «Scarta» dal clic su un\'altra riga', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
  const righe = page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
  await expect(righe.first()).toBeVisible({ timeout: 20000 });
  const titolo = async () => (await page.locator('.testa-titolo').innerText()).match(/b\. \d+, fasc\. \d+/)[0];
  const apri = async k => { await righe.nth(k).locator('td').nth(2).click(); await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 }); await pronto(page); await page.waitForTimeout(2000); };
  const bozza = async t => { await page.locator('#note').fill(t); await expect(page.locator('#salva_succ')).toContainText(/Salva e prossimo/, { timeout: 10000 }); };
  const torna = async () => { await page.goBack(); await pronto(page); await expect(page.locator('#regesto')).toBeHidden({ timeout: 10000 }); };

  await apri(0); const A = await titolo();
  await bozza('BOZZA A'); await torna();                      // l'elenco: niente guardia

  // clic su un'altra riga con la bozza ancora nella scheda A: apre la stessa A (stesso fascicolo) senza guardia
  await apri(0); expect(await titolo()).toBe(A);
  expect(await page.locator('#note').inputValue(), 'la bozza è ancora lì').toBe('BOZZA A'); await torna();

  // un altro fascicolo: la guardia; «Resta» annulla lo spostamento (si resta nell'elenco) e la bozza di A è ancora nella scheda
  await righe.nth(1).locator('td').nth(2).click();
  await expect(page.getByText('Modifiche non salvate')).toBeVisible({ timeout: 10000 });
  await page.getByRole('button', { name: /Resta sulla scheda/ }).click(); await pronto(page);
  await expect(page.getByText('Modifiche non salvate')).toHaveCount(0);
  await expect(page.locator('#regesto'), 'dopo «Resta» lo spostamento non è avvenuto').toBeHidden();
  await apri(0); expect(await titolo()).toBe(A);
  expect(await page.locator('#note').inputValue(), 'la bozza di A c\'è ancora').toBe('BOZZA A');

  // «Scarta e continua»: si apre B e la bozza di A sparisce
  await torna();
  await righe.nth(1).locator('td').nth(2).click();
  await expect(page.getByText('Modifiche non salvate')).toBeVisible({ timeout: 10000 });
  await page.getByRole('button', { name: /Scarta e continua/ }).click(); await pronto(page);
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 10000 }); await page.waitForTimeout(2000);
  const B = await titolo(); expect(B, 'si è aperta un\'altra scheda').not.toBe(A);
  expect(await page.locator('#note').inputValue()).not.toBe('BOZZA A');

});

// La guardia raggiunta dal cambio di fondo (finestra «Scegli il fondo»): «Resta» lascia fondo e bozza, «Scarta e continua» cambia fondo.
// L'impostazione del fondo attivo si scrive nel database di prova: a fine prova si rimette il fondo di esempio.
test('Bozza e cambio di fondo: «Resta» tiene fondo e bozza, «Scarta» cambia fondo', async ({ page }) => {
  await page.setViewportSize({ width: 1280, height: 900 });
  await page.goto('/#Home'); await page.locator('.tab-pane.active[data-value="Home"]').waitFor({ timeout: 60000 });   // l'indirizzo #Home si applica a connessione fatta
  await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 60000 });
  await page.getByRole('tab', { name: /Fascicoli/ }).first().click(); await pronto(page);
  const righe = page.locator('.tab-pane[data-value="Fascicoli"] table tbody tr:not(.gruppo-busta)');
  await expect(righe.first()).toBeVisible({ timeout: 20000 });
  await righe.first().locator('td').nth(2).click();
  await expect(page.locator('#regesto')).toBeVisible({ timeout: 15000 }); await pronto(page); await page.waitForTimeout(2000);
  await page.locator('#note').fill('BOZZA PER IL FONDO');
  await expect(page.locator('#salva_succ')).toContainText(/Salva e prossimo/, { timeout: 10000 });
  const scegli = async testo => { await page.locator('#fondo_apri').click(); await page.locator('.modal-dialog .riga-fondo', { hasText: testo }).first().click(); };
  try {
    await scegli('tutti i fondi');
    await expect(page.getByText('Modifiche non salvate')).toBeVisible({ timeout: 10000 });
    await page.getByRole('button', { name: /Resta sulla scheda/ }).click(); await pronto(page);
    await expect(page.locator('#fondo_etichetta'), 'dopo «Resta» il fondo è lo stesso').toContainText(/Tribunale di esempio/i);
    expect(await page.locator('#note').inputValue(), 'la bozza c\'è ancora').toBe('BOZZA PER IL FONDO');

    await scegli('tutti i fondi');
    await expect(page.getByText('Modifiche non salvate')).toBeVisible({ timeout: 10000 });
    await page.getByRole('button', { name: /Scarta e continua/ }).click(); await pronto(page);
    await expect(page.locator('#fondo_etichetta'), 'dopo «Scarta» il fondo è cambiato').toContainText('tutti i fondi', { timeout: 10000 });
  } finally {
    if (!/Tribunale di esempio/i.test(await page.locator('#fondo_etichetta').innerText())) {
      await scegli('Tribunale di esempio'); await pronto(page);
      await expect(page.locator('#fondo_etichetta')).toContainText(/Tribunale di esempio/i, { timeout: 10000 });
    }
  }
});
