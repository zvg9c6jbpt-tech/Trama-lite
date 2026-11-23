# Lacune aperte: registro

Una lacuna è **innocua** quando, anche se succede, (1) non si perdono né si falsificano dati, (2) chi usa il programma lo vede e sa che cosa fare, (3) un collaudo lo prova. Si chiude una lacuna per volta; ogni passaggio finisce in un commit.

## Come si chiude una lacuna

1. **Descrivere** il caso in una riga: che cosa succede, a chi, con quali dati.
2. **Riprodurlo** con uno script su un database di prova (mai il database vero). Se non si riproduce, non è ancora una lacuna.
3. **Scegliere la difesa**, in quest'ordine: impedirlo (vincolo, controllo) → renderlo reversibile (copia prima, annulla) → renderlo visibile (avviso, registro, Qualità) → documentarlo.
4. **Scrivere il collaudo** in `verifiche/` (di norma in `correzioni_critica.R` o in un file del tema) che fallisce prima e passa dopo.
5. **Aggiornare** guida HTML, traduzione inglese se c'è un testo nuovo.
6. `Rscript verifiche/tutte.R` con `TRAMA_DB_COLLAUDI=esempio` → `ESITO: ok`; commit; riga sotto da «aperta» a «chiusa» con data e commit.

## Registro

| # | Lacuna | Rischio se succede | Difesa prevista | Stato |
|---|--------|--------------------|-----------------|-------|
| L6 | `avvia.bat` e il pacchetto Windows non sono mai stati provati su un PC | Su un PC d'archivio non parte, o parte con la libreria sbagliata | Prova su un PC vero (checklist tolta con il pacchetto Windows, il 24/10/2026: è nella storia di git) | **chiusa per decisione (23/10/2026)**: niente build per Windows |

## Chiuse

Le correzioni del 22/10 (commit `e5a2b48`) sono nel `git log`.

- **L2**: segnature doppie senza numero già nel database. Difesa: riga di Qualità «Segnature doppie senza numero» (clic → le schede), che la Home già conta in «Da guardare»; l'indice si riprova a ogni apertura e dopo un ripristino (`.indice_senza_numeri`). Collaudo: `correzioni_critica.R`.
- **L3**: backup JSON con doppioni senza numero. Difesa: «Sostituisci» li riporta tutti com'erano e lo scrive nei dettagli; «Accoda» respinge il secondo con «Segnatura già occupata». Collaudo: `correzioni_critica.R`.
- **L1**: «Rimuovi duplicati». Difesa: `.fondi_vicini` (numeri diversi = fondi diversi); `rimuovi_duplicati(con, elimina)` vuole gli id e rifiuta ciò che non è una copia di adesso o che svuoterebbe un gruppo; la finestra ha una spunta spenta per gruppo e la scelta di quale resta; titoli diversi segnalati. Collaudo: `correzioni_critica.R`, `funzioni.R`.
- **L7**: «Sostituisci» e le schede che perdono dati. Difesa: `schede_senza_corrispondenza` le elenca nella conferma, che vuole «Ho capito». Collaudo: `correzioni_critica.R`.
- **L4**: duplicati esatti in Luoghi, Soggettario, Lingua, Produttori. Difesa: decisione nella finestra di revisione come per le Persone (mai preselezionata, «Lascia com'è» sempre possibile). Collaudo: `correzioni_critica.R`.
- **L5**: cartella dati sincronizzata. Dal 26/10/2026 l'app non la rileva più né avvisa (decisione di Stefano: la sincronizzazione cloud non serve adesso). Il rischio resta: tenere la cartella dei dati in un posto locale.
- **L8**: finestra di revisione lenta. Misura: 24 ms per una cella di 150 parole, fino a ~60 ms al tetto di 90.000 confronti; con migliaia di celle lunghe, minuti. Difesa: tetto sulla somma (`DIFF_BUDGET_TOTALE`, ~2 s); oltre, le celle si mostrano intere senza evidenziare. Collaudo: `correzioni_critica.R` (400 celle lunghe in 2,2 s). Misura del 18/11/2026 sui tab veri: la revisione dell'import è lineare, 7–12 ms a riga (Torino 186 righe 2,3 s; Napoli 367 righe 2,7 s; Napoli replicata fino a 5.505 righe 40 s). I fondi veri visti sono di 110–367 righe: nessun problema; un fondo di 5.000 righe richiede circa 40 s.

### L6: cosa è fatto e cosa no
Fatto: file Windows in CRLF e solo ASCII (`.gitattributes`), lanciatori che spengono renv se la libreria non è usabile, controlli statici nel collaudo. **Decisione di Stefano (23/10/2026): la build per Windows non si fa; ripresa il 28-29/10/2026** perché i colleghi usano Windows: `distribuzione/windows/costruisci.sh` produce lo zip senza R dentro (vedi README). **Resta aperto: non provato su un PC Windows** con R 4.6 e senza i pacchetti.
