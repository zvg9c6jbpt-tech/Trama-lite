# Contratto per colonna: Trama Lite

*Generato da `verifiche/contratto_colonne.R` (22/10/2026). Non si modifica a mano: le regole sono la tabella di `NORMALIZZAZIONE_LITE.md` §4, gli esempi i casi del corpus provati da `corpus_normalizzazione.R`.*

**A** = si corregge da solo (e ogni correzione è tracciata: cella, prima, dopo, regola) · **P** = si propone, decide chi schedatura · **N** = non si tocca. Ogni colonna passa prima da L1 (Unicode, invisibili, spazi). Ogni regola è idempotente e neutra sul dato giusto.

## 1. Segnatura

- **Separatore:** -
- **A (da solo):** scomposizione dalle etichette («b.», «f.», sigla propria), spazi
- **P (proposta):** parti che non sa leggere («b. 12A», «aa. IV»); sequenza di busta e fascicolo fuori ordine (avviso)
- **N (intoccabile):** -
- **Nota:** Un nome solo non dice il fondo: **riga respinta** (il fondo è obbligatorio). Righe senza segnatura = note, non fascicoli. In vigore.
- **Esempi:** `b.  12, fasc. 1` → `Archivio Storico Diocesano di Napoli, Fondo di Prova, b.12, f.1` (A, caso 51) · `b. 12A, fasc. 1` → `<respinta>` (P, caso 32)
- **Verifica:** corpus (casi 32, 51, 52) · `normalizzazione_prova.R`, `pulisci.R`, `formati_vecchi.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 2. Altra segnatura

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** il testo
- **Nota:** In vigore.
- **Esempi:** `Ms.  12 ;  vedi b. 3` → `Ms. 12 ; vedi b. 3` (A, caso 57)
- **Verifica:** corpus (casi 57) · `electron.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 3. Titolo

- **Separatore:** -
- **A (da solo):** spazi, connettore con refuso («cicra»→«circa»), maiuscole dei nomi; «S.» sciolto sempre (D23)
- **P (proposta):** **ogni nome che non concorda con Persone o Regesto** (D11)
- **N (intoccabile):** titoli latini, con qualifiche («Giordani, capitano»), testo scritto a mano
- **Nota:** Testo libero (D16): nessuna grammatica obbligata. Il titolo composto dal programma è fatto dei soli nomi (1–2 nomi; tre o più «<Genere> per diversi»). **Modificata:** niente correzione per voti.
- **Esempi:** `Atti cicra Paolo Rossi` → `Atti circa Paolo Rossi` (A, caso 33) · `Atti circa Nicola tre` → `Atti circa Nicola Tre` (A, caso 48) · `Atti circa Paolo Rosi` → resta com'è (P, caso 34) · `Processus contra infrascriptos blasphemos` → resta com'è (N, caso 35)
- **Verifica:** corpus (casi 33, 34, 35, 48, 53, 54, 55, 56, 77, 78, 79, 80) · `normalizzazione_prova.R`, `guida_nomi.R`, `coerenza_fondo.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 4. Estremo remoto

- **Separatore:** -
- **A (da solo):** «1555.0» → 1555; «n.d.» → `data_nd`; **D18**: «1625-1-9» → «1625-01-09», «Marzo 1644» e «1644-03» → «marzo 1644»
- **P (proposta):** non è un anno; fuori intervallo (D6); **datazione per secolo della Guida al Tab (D21): né errore né anno, resta com'è scritta e si conta**; «s.d.» e simili; candidato togliendo una cifra
- **N (intoccabile):** il valore se «lascia com'è»
- **Nota:** Default 1200–1899, parametro per fondo.
- **Esempi:** `1555.0` → `1555` (A, caso 26) · `n.d.` → `n.d.` (A, caso 28) · `1600.9` → resta com'è (P, caso 27)
- **Verifica:** corpus (casi 26, 27, 28, 29, 30, 31) · `date_precise.R`, `normalizzazione_prova.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 5. Estremo recente

- **Separatore:** -
- **A (da solo):** come sopra; **D30**: uguale al remoto → si toglie
- **P (proposta):** recente prima del remoto: scambia / tieni uno solo
- **N (intoccabile):** idem
- **Nota:** Il recente non viene mai prima del remoto.
- **Esempi:** `1625-1-9` → `1625-01-09` (A, caso 58) · `1544` → (vuoto) (A, caso 85) · `1644` → resta com'è (N, caso 59)
- **Verifica:** corpus (casi 58, 59, 85, 86, 87, 88) · `date_precise.R`, `normalizzazione_prova.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 6. definizione.tipologia

- **Separatore:** -
- **A (da solo):** variante registrata del fondo
- **P (proposta):** refuso vicino (se non ha prova); valore nuovo
- **N (intoccabile):** maiuscole (convenzione del foglio)
- **Nota:** Vocabolario **per fondo**.
- **Esempi:** `Fascicolo` → `fascicolo` (A, caso 60)
- **Verifica:** corpus (casi 60) · `funzioni.R`, `roundtrip.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 7. Supporto (materiale)

- **Separatore:** -
- **A (da solo):** variante registrata del fondo
- **P (proposta):** valore nuovo
- **N (intoccabile):** -
- **Nota:** Come Definizione. Vocabolario: cartaceo, papiraceo, pergamenaceo, membranaceo (Guida al Tab, 22/10/2026).
- **Esempi:** `Cartaceo` → `cartaceo` (A, caso 61) · `carta` → resta com'è (P, caso 62)
- **Verifica:** corpus (casi 61, 62) · `colonne.R`, `funzioni.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 8. dimensioni/altezza

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `300` → resta com'è (N, caso 63)
- **Verifica:** corpus (casi 63) · `funzioni.R`, `modello_compilabile.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 9. dimensioni/larghezza

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `210` → resta com'è (N, caso 64)
- **Verifica:** corpus (casi 64) · `funzioni.R`, `modello_compilabile.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 10. tipo di numerazione

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `per  carte` → `per carte` (A, caso 101) · `cartulazione` → resta com'è (N, caso 65)
- **Verifica:** corpus (casi 65, 101) · `funzioni.R`, `modello_compilabile.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 11. Numerazione

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `1-120` → resta com'è (N, caso 66)
- **Verifica:** corpus (casi 66) · `funzioni.R`, `pulisci.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 12. epoca numerazione

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `moderna` → resta com'è (N, caso 67)
- **Verifica:** corpus (casi 67) · `funzioni.R`, `modello_compilabile.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 13. consistenza.quantità

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `120` → resta com'è (N, caso 68)
- **Verifica:** corpus (casi 68) · `funzioni.R`, `layout_xlsx.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 14. consistenza.unitàMisura

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri).
- **Esempi:** `carte` → resta com'è (N, caso 69)
- **Verifica:** corpus (casi 69) · `funzioni.R`, `layout_xlsx.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 15. Tipologia documentaria

- **Separatore:** `; `
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** il valore del foglio
- **Nota:** Atti, Lettere, Decreti (Guida al Tab) escono come arrivano, anche più d'una con «; »; ogni altro valore esce «Atti», dichiarato (decisione 5 del 22/10/2026: già così dal 14/10).
- **Esempi:** `Processo` → `Atti` (A, caso 71) · (vuoto) → `Atti` (A, caso 84) · `Lettere; Decreti` → resta com'è (N, caso 70)
- **Verifica:** corpus (casi 70, 71, 84) · `normalizzazione_prova.R`, `formati_vecchi.R`, `pulisci.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 16. Regesto o descrizione sintetica

- **Separatore:** -
- **A (da solo):** (composto) spazi, connettore con refuso, maiuscole dei nomi; «S.» sciolto sempre (D23)
- **P (proposta):** **ogni nome che non concorda** (D11); reati discordi dal Soggettario (di default il Soggettario, da verificare)
- **N (intoccabile):** regesti strutturati dei cataloghi; prosa scritta a mano; il «;» della colonna V di ASMo
- **Nota:** Punto finale in uscita. Max 3 nomi poi «diversi soggetti».
- **Esempi:** `Luogo di consumazione del reato: Fiumalbo↵Tipo di reato: Eresia↵Applicazione della tortura: sì.` → resta com'è (N, caso 36)
- **Verifica:** corpus (casi 36) · `normalizzazione_prova.R`, `guida_nomi.R`, `coerenza_fondo.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 17. Persone citate o indicizzate

- **Separatore:** `|`
- **A (da solo):** spazi, maiuscole («PAolo»→«Paolo»), «;»→«|», «:»→«|», particella in lettura, `’`→`'`; «S.» sciolto sempre (D23)
- **P (proposta):** **duplicato esatto (D4)**; **grafia diversa fra colonne (D11)**; senza cognome certo (spunta spenta); segnaposto; più candidati vicini; più di una virgola (avviso)
- **N (intoccabile):** «d'Auria», «de' Rossi»; qualifiche fra parentesi
- **Nota:** Vedi §5. **Modificata:** tolti «grafia che vince per voti» e «duplicato esatto» dalle automatiche.
- **Esempi:** `Rossi, Paolo ; Bianchi, Maria` → `Rossi, Paolo|Bianchi, Maria` (A, caso 1) · `Rossi, Paolo | Bianchi, Maria` → `Rossi, Paolo|Bianchi, Maria` (A, caso 2) · `Rossi, Paolo|Rossi, Paolo` → `Rossi, Paolo` (P, caso 3) · `d'Auria, Tommaso` → resta com'è (N, caso 5)
- **Verifica:** corpus (casi 1, 2, 3, 4, 5, 6, 7, 8, 9, 38, 49, 81, 83, 89, 90, 98, 99, 100, 102) · `guida_nomi.R`, `normalizzazione_prova.R`, `pulisci.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 18. Luogo di conservazione

- **Separatore:** -
- **A (da solo):** spazi; la città dell'archivio → l'ente; «S.» sciolto sempre (D23)
- **P (proposta):** -
- **N (intoccabile):** il valore, vuoto resta vuoto
- **Nota:** L'ente, mai una città (regola di Stefano del 12/10, confermata il 22/10/2026 contro la Guida al Tab, che dice «la città»).
- **Esempi:** `Napoli` → `Archivio Storico Diocesano di Napoli` (A, caso 40)
- **Verifica:** corpus (casi 40) · `funzioni.R`, `roundtrip.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 19. Luogo di produzione (data topica)

- **Separatore:** `|`
- **A (da solo):** come Luoghi; «S.» sciolto sempre (D23)
- **P (proposta):** come Luoghi
- **N (intoccabile):** parentesi
- **Nota:** Segue le regole dei luoghi (§6).
- **Esempi:** `S. Severo` → `San Severo` (A, caso 72) · `Roma|Roma` → `Roma` (P, caso 104) · `Modena` → resta com'è (N, caso 73)
- **Verifica:** corpus (casi 72, 73, 104) · `guida_nomi.R`, `normalizzazione_prova.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 20. Luoghi citati o indicizzati

- **Separatore:** `|`
- **A (da solo):** vedi §6; «S.» sciolto sempre (D23)
- **P (proposta):** vedi §6
- **N (intoccabile):** ciò che sta fra parentesi
- **Nota:** §6.
- **Esempi:** `TRapani` → `Trapani` (A, caso 10) · `Avignon` → `Avignon (Avignone)` (A, caso 11) · `Avignone` → resta com'è (P, caso 13) · `Avignon (Avignone)` → resta com'è (N, caso 12)
- **Verifica:** corpus (casi 10, 11, 12, 13, 14, 15, 16, 17, 41, 42, 43, 44, 45, 82, 103) · `normalizzazione_prova.R`, `autocorrezione.R`, `guida_nomi.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 21. Soggetto produttore

- **Separatore:** `|`
- **A (da solo):** forma canonica «Inquisizione di \<Città\>»; solo Roma = «Sant'Uffizio»; `’`→`'`
- **P (proposta):** -
- **N (intoccabile):** i nomi dei fondi
- **Nota:** I 15 «Sant’Uffizio» esistenti migrano a «Sant'Uffizio» (D13).
- **Esempi:** `Tribunale dell'Inquisizione di Napoli` → `Inquisizione di Napoli` (A, caso 24) · `Sant’Uffizio` → `Sant'Uffizio` (A, caso 25)
- **Verifica:** corpus (casi 24, 25) · `correzioni_critica.R`, `formati_vecchi.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 22. Soggetti citati o indicizzati

- **Separatore:** `|`
- **A (da solo):** «;»→«|», spazi; «S.» sciolto sempre (D23)
- **P (proposta):** **nome di persona dentro: errore proposto (D12)**
- **N (intoccabile):** il testo
- **Nota:** Fonte di nomi se compilata.
- **Esempi:** `Rossi, Paolo` → resta com'è (P, caso 46)
- **Verifica:** corpus (casi 46) · `normalizzazione_prova.R`, `guida_nomi.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 23. Soggettario

- **Separatore:** `|`
- **A (da solo):** maiuscola iniziale, refuso vicino a una classe (con segnalazione), **decisioni già registrate del fondo (es. «Furto» → «Giurisdizione»)**, «Modena» fra i reati eliminato, **equivalenze della Guida al Tab (D20)**, **termini del tesauro (D33)**
- **P (proposta):** valore che non assomiglia a nessuna classe e senza decisione registrata
- **N (intoccabile):** le classi
- **Nota:** Vocabolario **chiuso**, 19 classi; i termini del tesauro (D33) rientrano nella loro classe e restano registrati nel database.
- **Esempi:** `eresia` → `Eresia` (A, caso 18) · `Eresa` → `Eresia` (A, caso 19) · `Furto` → resta com'è (P, caso 47)
- **Verifica:** corpus (casi 18, 19, 20, 21, 47, 91, 92, 93, 94, 95) · `colonne.R`, `pulisci.R`, `normalizzazione_prova.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 24. Eventi

- **Separatore:** `|`
- **A (da solo):** solo L1 + separatore
- **P (proposta):** -
- **N (intoccabile):** il resto
- **Nota:** Fedeli.
- **Esempi:** `Battesimo; abiura` → resta com'è (N, caso 74)
- **Verifica:** corpus (casi 74) · `pulisci.R`, `layout_xlsx.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 25. Note libere

- **Separatore:** -
- **A (da solo):** spazi (non a capo)
- **P (proposta):** -
- **N (intoccabile):** il testo
- **Nota:** Punto finale in uscita.
- **Esempi:** `Santità.` → `Santità.` (A, caso 37) · `Prima parte; seconda parte.↵Terza riga.` → resta com'è (N, caso 39)
- **Verifica:** corpus (casi 37, 39) · `normalizzazione_prova.R`, `guida_nomi.R`, `export_convenzioni.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 26. Lingua

- **Separatore:** `; `
- **A (da solo):** minuscole, `|`, `;`, virgola → «; »; refuso vicino
- **P (proposta):** lingua nuova
- **N (intoccabile):** -
- **Nota:** Minuscole nel foglio, «Italiano» nel database. Per fondo. Vocabolario di base dal 22/10/2026: Italiano, Latino, Francese, Slavo.
- **Esempi:** `Italiano|Latino` → `italiano; latino` (A, caso 22) · `italiano;latino` → `italiano; latino` (A, caso 23) · `italiano; latino` → resta com'è (N, caso 50)
- **Verifica:** corpus (casi 22, 23, 50) · `colonne.R`, `pulisci.R`, `normalizzazione_prova.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 27. Manifest IIIF

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedele.
- **Esempi:** `https://esempio.org/manifest.json ` → `https://esempio.org/manifest.json` (A, caso 75)
- **Verifica:** corpus (casi 75) · `roundtrip.R`, `layout_xlsx.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

## 28. Identificativo immagine o cartella

- **Separatore:** -
- **A (da solo):** solo L1
- **P (proposta):** -
- **N (intoccabile):** tutto
- **Nota:** Fedele.
- **Esempi:** `  img_001 ` → `img_001` (A, caso 76)
- **Verifica:** corpus (casi 76) · `roundtrip.R`, `layout_xlsx.R` · sempre: `roundtrip.R`, `salvataggio_neutro.R`, `tab_reali.R`

