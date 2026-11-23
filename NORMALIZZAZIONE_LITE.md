# Trama Lite: le regole di normalizzazione (versione 4, 21 ottobre 2026)

Autore delle decisioni: Stefano Colavecchia. Stato: **le regole attive sono tutte nel codice e nel corpus d'oro** (`R/concordanza.R`, `R/arianna.R`, `verifiche/corpus/normalizzazione.csv`, `verifiche/corpus_normalizzazione.R`: 80 casi, tutti verdi; il contratto per colonna, con gli esempi, è in `CONTRATTO_COLONNE.md`). Versione 4 (21/10): Lite è un progetto autonomo, fuori dalla cartella di Trama, con la sua cartella dei dati e la **lista dei luoghi** propria al posto del gazetteer. Versione 3: toglie il toponimo antico dalle parentesi (D8) e riporta ciò che il corpus ha mostrato. La versione 2 incorporava i verdetti di Stefano sui 40 casi e sulle cinque questioni. Le regole «in vigore» sono quelle che il codice applica (`dati/regole_normalizzazione_per_colonna.xlsx`); quelle marcate **nuova** o **modificata** erano le decisioni da implementare: oggi sono nel codice, salvo dove il §9 dice altro.

Ambito di Lite: normalizzazione, schedatura semplice, controlli di qualità. **Niente gazetteer con coordinate, mappe, statistiche, schede persona, RDF.** Lite è un progetto a sé (`~/Desktop/Dh Progetti/Lite/trama-lite`), con un database suo (`~/Documents/Trama Lite/trama.sqlite`) e le stesse 28 colonne del modello di Trama: con Trama si scambia con i fogli Excel, i file JSON e le copie, non condividendo il database.

---

## 1. Il contratto

Ogni regola deve rispettare tutti e sette i punti. Una regola che ne viola uno non entra.

1. **Determinismo.** Lo stesso input dà lo stesso output, byte per byte, su PC e Mac.
2. **Idempotenza.** `norm(norm(x)) == norm(x)`.
3. **Neutra sul corretto.** Una cella già giusta non cambia mai; un export pulito attraversa la normalizzazione con **0 celle cambiate**.
4. **Tracciata.** Ogni cambiamento ha cella, valore prima, valore dopo e regola. Nessuna correzione anonima.
5. **Senza perdite e senza invenzioni.** Non sparisce nessun valore **se non per scelta esplicita di chi schedatura**; non si attribuisce alla fonte ciò che non dice (`n.d.` ≠ `NA` ≠ «no»; nessun default ereditato in import).
6. **Ogni regola ha uno stato solo:**
   - **A, automatica.** Si applica da sola; compare fra le «Correzioni», con la spunta, e si può togliere prima di scrivere. Vale solo per ciò che è deterministico, **non dipende da una scelta fra più identità possibili** e ha una prova.
   - **P, proposta.** Compare fra le «Da decidere», nessuna scelta preselezionata (o spunta spenta). Mai scritta in silenzio.
   - **N, intoccabile.** Testo libero, parentesi, regesti strutturati o scritti a mano, titoli latini, valori fedeli.
7. **Una regola entra solo con la prova sui fondi veri**, guardando le **celle**, non i conteggi (la prima versione della concordanza cambiava 7.500 celle di Modena che sembravano «pochi»).

La finestra «Controlla prima di importare» resta il luogo in cui chi schedatura decide. «Lascia com'è» resta sempre possibile.

---

## 2. Decisioni di Stefano (20/10/2026)

| # | Decisione |
|---|---|
| D1 | **Unicode NFC sempre, in lettura, nessuna eccezione**, anche nei testi liberi (è un'equivalenza di forma, non di contenuto). |
| D2 | **Apostrofo salvato: `'` dritto** in nomi, luoghi, produttori e testi composti. Il `’` si accetta in lettura e si porta a `'`. |
| D3 | **Segnaposto** (`-`, `?`, `s.d.`, `n.a.`…): sempre **P**, mai A. `n.d.` resta valido solo dove è ammesso. |
| D4 | **Liste multi-valore: mai riordinare** (l'ordine è semantico). Un **duplicato esatto è una proposta (P), non si toglie da solo**: potrebbero essere due omonimi (verdetto sul caso 3). **Limite del modello (22/10):** il database tiene una voce una volta per scheda (chiave scheda + voce), quindi un duplicato lasciato com'è si fonde in una sola; la proposta con scelta si offre per le **Persone**, dove il duplicato può essere un omonimo. L'ordine delle voci è quello del foglio in tutti gli elenchi (anche Lingua e Produttori, con la colonna `ordine` dal 22/10). |
| D5 | **Luogo con refuso senza prova**: **P**, mai correzione automatica. Un nome straniero valido non è un refuso (D7). |
| D6 | **Anni: 1200–1899 come default, con un parametro per fondo.** |
| D7 | **Forma locale.** «Avignon» è il nome in francese, non un refuso di «Avignone»: la forma normalizzata è **«Avignon (Avignone)»**. |
| D8 | **Forma locale per tutte le forme locali** (Q1): sempre `Locale (Italiano)`, **senza toponimo antico**: **«Liège (Liegi)»** (Stefano, 20/10: «regola standard, fai fuori il toponimo antico»). La parentesi contiene **una sola forma**, quindi non ci sono separatori dentro le parentesi. Nessuna parentesi quando la forma italiana è uguale alla locale a meno dei segni diacritici («Embrun», «Quimper», «Bistrița»). |
| D9 | **Luogo ignoto:** per ogni luogo una spunta con le azioni **«Aggiungi alla lista»** o **«Elimina»** (toglie il luogo dalla scheda, scelta esplicita). Se non spunti niente resta **«Lascia com'è»**; non blocca l'import. |
| D10 | **Vocabolari chiusi (reati e simili):** prima si cerca fra le **decisioni già registrate** per il fondo (variante registrata → A); solo se non c'è si propone (P). **Registrata ora: «Furto» → «Giurisdizione»** (regola dei reati comuni degli inventari a stampa). |
| D11 | **Nomi discordi fra Titolo, Regesto e Persone: sempre una decisione, mai per voti.** Il programma mostra la grafia più scritta come suggerimento (prima fra le scelte), senza preselezionarla; non la applica mai da solo. **Fatto il 20/10 in `concordanza.R`.** Resta automatica **solo la differenza di maiuscole fra le stesse lettere** («Nicola tre» / «Nicola Tre», «PAolo» → «Paolo»): lì non si sceglie fra due persone (interpretazione mia del verdetto sul caso 34: confermala). Una differenza di accento, di spazio o di una lettera è invece una decisione. |
| D12 | **Nome di persona in «Soggetti citati»:** errore proposto (P) (Q2). |
| D13 | **I 15 «Sant’Uffizio» già salvati si migrano a «Sant'Uffizio»**, come migrazione dichiarata con copia di sicurezza (Q3). |
| D14 | **La lista dei luoghi di Lite è il gazetteer di Trama senza coordinate** (nomi, forme locali, omonimi) (Q4), **di proprietà di Lite**: `dati/luoghi.csv` (chiave, nome, paese), copia personale in `~/Documents/Trama Lite/luoghi/luoghi.csv`. È **unica** per tutta la copia di Lite (non per fondo). Cresce con «Aggiungi alla lista» (D9) e non ha un editor. |
| D15 | **Il foglio `regole_normalizzazione_per_colonna.xlsx` si aggiorna e resta la fonte** (Q5): vedi §9 per l'allineamento fra i due documenti. |
| D16 | **Testo libero = solo controlli formali (22/10/2026).** Il **Titolo** ammette qualunque costrutto («Paolo Rossi», «Processo contro X», «Processo per eresia a X», titoli latini o di altri schedatori): niente grammatica obbligata, niente vocabolario dei reati, nessun errore per la forma. Si controllano solo NFC, spazi doppi e ai bordi, apostrofo, separatori, maiuscole dei nomi, refusi con un riferimento (nomi di Persone, connettore «cicra») e la concordanza dei nomi (D11, sempre decisione). **Il vocabolario controllato dei reati vale solo per Regesto e Soggettario.** Scopo: un file «pulito» (0 celle cambiate e 0 decisioni al secondo passaggio) è formalmente coerente, non verificato nel merito. Casi 53–56. |
| D17 | **Check di base sui refusi nel testo libero (22/10/2026), dal lessico del corpus.** In Titolo e Regesto una parola minuscola di almeno 6 lettere scritta **una sola volta** nel corpus (foglio + database) e a **una lettera** da una parola scritta almeno **3 volte** è un sospetto refuso («causaraum» → «causarum»). È una **proposta (P) con la spunta spenta**, mai automatica. Prudenza: la differenza non può cadere sull'ultima lettera (desinenze latine valide); nomi propri e inizi di frase restano fuori; con due candidate alla pari si tace; con un corpus piccolo si tace. Limite dichiarato: non vede i refusi su parole rare né quelli sull'ultima lettera, e la precisione sui fondi veri non è ancora misurata. `R/refusi_lessico.R`, blocco 3j di `proposte_import`. |
| D19 | **I fogli nel formato vecchio si normalizzano come quelli nel modello (22/10/2026, dal parere di Codex e dalla prova sui sei tab veri).** (a) I controlli cercano le colonne per nome del modello: per un foglio ASDNa la mappa è completata con quei nomi (`.con_nomi_modello`), altrimenti saltavano in silenzio (Torino: da 15 a 11 decisioni utili e 41→273 celle visibili). (b) **Una qualifica fra busta e fascicolo** («m.1, inv., f.2», «m.1, non inv., f.2») fa parte della segnatura e sta nel suffisso della busta (`, inv.`): senza, le due serie erano la stessa scheda (82 righe su 186 respinte) e «inv.» spariva dal foglio. (c) Il regesto è confrontato col vocabolario dei reati solo se il tratto dopo «per» somiglia a un elenco di reati (una o due parole, senza articoli né apostrofi): la prosa («per l'arresto di X») non dà avvisi (`.sembra_un_reato`). (d) Una persona «Cognome Nome» senza virgola in una cella dove le altre l'hanno è una decisione (proposta: «Cognome, Nome»), mai girata da sola. (e) Ciò che l'export riscriveva in silenzio è ora una cella visibile: particella dopo il nome, «Sant'Uffizio», lingue in minuscolo con «; », tipologia fuori dalla guida → «Atti». (f) Un indirizzo in fondo alle note non riceve il punto finale. (g) «Lascia com'è» su un duplicato esatto si chiama «Tieni una voce sola»: il database ne tiene una. |
| D18 | **Giorno e mese facoltativi negli estremi (22/10/2026, Guida al Tab).** Le forme ammesse sono «aaaa», «mese aaaa» (minuscolo) e «aaaa-mm-gg». L'anno resta in `estremo_remoto`/`estremo_recente` (numerico: ordinamento, controlli, intervallo D6); la data intera sta nell'extra «Estremo remoto/recente (data precisa)» e **esce nel foglio solo se il suo anno è quello del campo** (se l'anno cambia a mano, esce l'anno). Riscrittura sicura (A): «1625-1-9», «Marzo 1644», «1644-03». Una data che non esiste (30 febbraio) resta una decisione. Remoto/recente: mese e giorno si confrontano solo se li hanno entrambi («marzo 1625» non contraddice «1625-03-05»). Il secolo («sec. XV») e le scansioni della Guida non sono ancora previsti. |
| D20 | **Equivalenze fuori vocabolario (22/10/2026).** Tre voci che la Guida al Tab colloca sotto una classe entrano in quella classe da sole (A), per ogni fondo e anche scritte in minuscolo: «Bigamia» → «Crimini sessuali», «Apostasia» → «Eresia», «Ebrei» → «Ebraismo». Si vedono in «Correzioni» («equivalenza della Guida al Tab»); una decisione registrata del fondo per la stessa voce vince (`EQUIVALENZE`, `profili.R`). |
| D22 | **Nomi dei sovrani (22/10/2026, Guida al Tab).** Papi, re, imperatori e consorti di re e imperatori hanno la formula «Nome (forma italiana; titolo)» senza virgola: `.formula_sovrano` la riconosce dal titolo (papa, antipapa, re, regina, imperatore, imperatrice) e l'export non la riscrive in «Cognome, Nome». Prima «Marie-Antoinette d'Autriche (…)» usciva «d'Autriche (…), Marie-Antoinette». I titoli nobiliari fra parentesi dopo «Cognome, Nome» passano senza controlli. |
| D23 | **«S.» si scioglie sempre (22/10/2026, Stefano; sostituisce la proposta con prova nella lista del 14/10).** In Luoghi citati, Luogo di produzione, Luogo di conservazione, Persone, Soggetti citati, Titolo e Regesto; non in Note né in Segnatura. Forma: Sant' davanti a vocale, Santo davanti a s+consonante, Santa davanti a un nome femminile (finisce in -a, salvo Luca, Andrea, Nicola, Mattia, Barnaba…; più Croce), San negli altri casi; «S.ta» Santa, «S.to» Santo, «S.ti» Santi. Eccezioni: «S. Uffizio/Officio» → «Sant'Uffizio/Sant'Officio», «S. Sede» → «Santa Sede», «S. Padre» → «Santo Padre». **Non si tocca** dove «S.» è «Sacra» (S. Congregazione, S. Consulta, S. Rota, S. Inquisizione, S. Penitenzieria) né «SS.». **Dai tab veri (22/10):** le sigle non si toccano (S.M = Sua Maestà, S.A.R., S.U = Sant'Uffizio, S.S) né le formule «Sua» (S. Santità, S. Eccellenza, S. Pontefice…); «S.t Officio» → «Sant'Officio». **Il dubbio «Nome S. Cognome»** (iniziale o santo?): in persone, soggetti, titolo e regesto, se «S.» segue una parola maiuscola che non nomina un luogo o un ente (Convento, Chiesa, Borgo, Castel…), la cella resta com'è e lo scioglimento è una **proposta** non spuntata di partenza («decidi tu»); nei luoghi si scioglie sempre. Automatica e tracciata (`sciogli_santi`, `luoghi.R`). |
| D24 | **Ogni modifica fra il foglio originale e il pulito è dichiarata (22/10/2026, dal parere di Codex).** (a) «Pulisci» confronta cella per cella l'originale con il pulito e scrive il foglio **Registro modifiche** (riga, segnatura, colonna, prima, dopo, regola, dichiarata); le correzioni accettate in finestra entrano anche in «Correzioni». (b) «Dichiarata»: **sì** = proposta accettata; **convenzione** = regola fissa di uscita contata in anteprima (punto finale, città→ente, particella dopo il nome, duplicato esatto tolto dal database, «Atti» se la tipologia è vuota, spazi, forma della segnatura, archivio e fondo scritti); **no** = mai annunciata, e il collaudo sui tab veri fallisce (`verifiche/tab_reali.R`, `verifiche/registro_modifiche.R`). (c) **Il nome senza virgola** («Mario Rossi») si indicizza «Rossi, Mario» in anteprima, come correzione visibile e rifiutabile, solo se il cognome è certo (2–4 parole di lettere, senza «di/da», «alias», virgolette, iniziali, parentesi, e senza altri nomi con la virgola nella cella: lì è una decisione); se la correzione è rifiutata l'uscita di «Pulisci» non la applica. «Anna di Mosca» non diventa più «Mosca, Anna di». **L'export dal database non gira mai un nome senza virgola (23/10/2026, scelta A di Stefano):** lo decide l'anteprima, una volta; il database tiene la scelta, e un rifiuto sopravvive a salvataggio, riapertura ed esportazione (prima l'export lo rigirava di nuovo: `verifiche/registro_modifiche.R`). Il controllo delle anomalie nel database legge la scheda com'è: un nome senza virgola già salvato è una correzione proposta (spuntata, rifiutabile) e, se accettata, si scrive «Cognome, Nome» con la sua coppia persona–reato (`verifiche/anomalie.R`). **Righe e fondi, decisioni (22/10, secondo parere):** l'import dice in quale fondo è entrata ogni riga sorgente (`riga_fondo`) e il registro di ogni fondo confronta solo le sue righe, con il numero di riga del foglio originale; le celle cambiate da una decisione presa in finestra (estremi scambiati, cognome scelto, luogo eliminato…) entrano come «decisione presa nella finestra», dichiarata «sì»; due decisioni sulla stessa cella non spezzano più il nome scritto a mano con la virgola. **Da decidere con Stefano:** duplicati esatti (D4 dice proposta, ma il database non ammette duplicati: rifiutarla non basta) e «Atti» scritto in una tipologia vuota (contro «il vuoto resta vuoto») - **deciso**: «Atti» è il default d'uscita, applicato solo se il campo è vuoto o fuori guida (decisione 5 del 22/10, già così dal 14/10; Stefano il 23/10: «default postumo»), mai memorizzato dall'import. **Confermato il 23/10/2026 (Stefano: «il nome senza virgola deve seguire le regole»):** annullato il commit `6c910d8`, che lo lasciava com'era. |
| D25 | **Ritirata (23/10/2026).** Il pacchetto per il coordinatore (zip con foglio, manifesto, impronte, segnature «inviato»/«ricevuto», confronto fra pacchetti) è stato **tolto** da Lite per decisione di Stefano: Lite è autonomo e gratuito, senza ruoli, blocchi o protocolli di coordinamento; l'organizzazione del lavoro e lo scambio via email restano esterni. Il codice e la guida non lo contengono più (si recupera dalla storia di git, fino al commit `16bdede` (23/10/2026), file `R/consegna.R`). |
| D26 | **«S.» e il dizionario dei santi (22/10/2026, parere di Codex).** Il genere e l'elisione vengono da `dati/santi.csv` (nome;genere), non dalla desinenza: «S. Scolastica» → «Santa Scolastica» (prima «Santo»), «S. Beatrice» → «Santa Beatrice» (prima «San»). Un nome fuori dal dizionario si indovina ancora nei luoghi, nel titolo e nel regesto (ipotesi); in **Soggetti** resta com'è e si propone, mai spuntato (`ignoti = "lascia"`); in **Persone** nessun «S.» si scioglie da solo, nemmeno un santo del dizionario (`ignoti = "persone"`, secondo parere 22/10: «S. Giovanni» può essere un'iniziale), si propone e lo accetta chi guarda. Il dizionario si amplia con una riga nel file. |
| D27 | **Completezza con «sec. XV» e «Continua» a piano concluso (22/10/2026, parere di Codex).** (a) La datazione per secolo (D6) è presente, non assente: per i quattro campi fondamentali (`campi_core`) la scheda non manca dell'anno; nel campo numerico non c'è nessun anno inventato; `schede_solo_secolo` le conta. Una data non risolta che non è un secolo («marzo 1644») resta una lacuna. (b) Con un piano tutto schedato «Continua» non propone un numero oltre il piano: la Home dice «Piano completato» e offre «Aggiungi fuori piano» (`piano_concluso`, `prossimo_fuori_piano`). Senza piano «Continua» funziona come prima. **Precisazione (23/10):** se l'ultima scheda toccata sta in una busta che il piano non copre non è «piano completato» e «Continua» propone il numero che segue in quella busta; «S. Zeno» e «S. Zaccaria» sono «San», non «Santo». |
| D28 | **Segnatura: spazi doppi o ai bordi (23/10/2026, Stefano: «x» alla proposta).** Archiginnasio scrive «b.  1860»: prima (regola del 20/10) la riga non entrava; ora è una correzione **proposta con anteprima** e spuntata (`proposte_import`, 3k), come ogni correzione sicura. Se due righe del foglio diventerebbero la stessa segnatura la correzione non è spuntata e nomina l'altra riga («collisione»); La collisione si legge come la legge il database (stesso fondo, busta, fascicolo e suffissi: «f. 1» e «fasc. 1» sono la stessa), non come testo. In ogni caso l'import non fonde mai due schede: la seconda è respinta come «già occupata» in «accoda»; in «nuovo archivio» un fondo entra tutto o niente (collaudo `import_nuovo_archivio.R`), e il doppione lo ferma con un messaggio in italiano, non con l'errore del database (corretto il 23/10, dalla critica). Poiché la correzione è spenta quando collide, ciò accade solo se chi guarda la spunta lo stesso. Le altre forme sbagliate («;»/«\|» al posto della virgola, virgola senza spazio, numero illeggibile) restano respinte. Caso 51 del corpus: da P/«respinta» ad A; Archiginnasio entra con 12 righe su 15 (le altre 3 sono note senza segnatura). |
| D29 | **Nomi senza cognome non si girano (23/10/2026, Stefano: «fra Paolo, non avendo il cognome, NON viene censita nelle persone citate: simple as that»).** Un nome fatto di un titolo e un nome («fra Paolo», «suor Maria», «don Carlo», «padre Pio») o di un solo nome non ha un cognome da mettere per primo: `senza_cognome()` lo riconosce e l'export lo lascia com'è, anche dal database («Paolo, fra» non esce più). Vale la regola di sempre: non è una persona da censire, e l'anteprima propone di toglierlo dalle Persone con la spunta spenta (caso 8, verdetto del 20/10: non si cancella un dato da soli). Un nome con titolo e cognome («Fra Paolo Sarpi») si gira come ogni nome. Rimane la scelta A sui nomi senza virgola: «Ferrari Antonio» diventa «Antonio, Ferrari» (spuntato, rifiutabile in anteprima); se rifiutato resta com'è anche nell'export dal database, che non riordina più nulla. |
| D30 | **Estremo recente uguale all'estremo remoto: si toglie (24/10/2026, Stefano).** Se i due estremi sono la stessa data («1544» e «1544», «1544-03-12» e «1544-03-12») la data è una sola scritta due volte: il recente si svuota da solo. È una correzione di tipo A, visibile in anteprima (spuntata, rifiutabile) e dichiarata nel registro («estremo recente uguale al remoto»); nel database toglie anche la data precisa del recente. Un anno e una data dentro quell'anno («1544» e «1544-05-10») non sono la stessa data: restano. **Nella Scheda (26/10/2026, Stefano: «avvisare al salvataggio»)** non si toglie in silenzio: al salvataggio una finestra chiede «Sì, togli il recente» / «No, tieni tutti e due» (`estremi_uguali()` in `R/anomalie.R`; il «No» vale per quella scheda e quella data). Casi 85–88 del corpus; `verifiche/registro_modifiche.R`, `verifiche/anomalie.R`. |
| D31 | **Maiuscola dopo l'apostrofo (24/10/2026, Stefano).** Dopo una particella con l'apostrofo (d', de', dell', dall', nell', all', sull', l') la lettera seguente è maiuscola: «De'cefali» → «De'Cefali», «d'africa» → «d'Africa». Correzione di tipo A, visibile in anteprima e rifiutabile, dichiarata nel registro; vale per Persone, Soggetti, Titolo e regesto composto e nel controllo anomalie. La maiuscola o minuscola della particella (D'Auria / d'Auria) non si tocca. Casi 89–90 del corpus; `verifiche/registro_modifiche.R`. |
| D32 | **Tre lacune chiuse dalla prova «asp 2» (30/10/2026, Stefano: «sistema quelle lacune»).** (a) **Suffisso della segnatura**: «7 bis», «7ter», «4-ter.», «20-Bis», «232 bis» si scrivono «-bis», «-ter»… (bis, ter, quater, quinquies, sexies, septies, octies, novies, decies, sempre minuscoli, con il trattino); gli altri suffissi («-a», «-2») restano com'erano; sostituisce la conservazione del suffisso «com'è scritto» (prima « bis» e «-Bis» restavano diversi). Correzione A, dichiarata («segnatura: il suffisso bis, ter… si scrive «-bis», «-ter»»). (b) **Refusi nei nomi di titolo e regesto**: il titolo di soli nomi entra nel confronto anche se non è uguale a una persona ma le somiglia (una o due lettere); il regesto che Trama compone, «<tipo> per <reati> circa|contro <nomi>», si legge (prima solo «<tipo> circa|contro <nomi>»). Un refuso resta una decisione (D11): finisce in «Da decidere» con la forma più scritta come suggerimento. Le particelle non contano nella «forma meglio scritta»: «Antonio De Santis» non batte «Antonio de Santis». (c) **Nome senza virgola**: se il titolo o il regesto della riga scrivono le stesse parole in un altro ordine, l'ordine lo dà il testo («Gallo Matteo» con «Matteo Gallo» → «Gallo, Matteo»); senza testo vale l'ultima parola come cognome (D-22/10). (d) **Seconda tornata (stesso giorno):** il produttore «inquisizione di benevento» → «Inquisizione di Benevento» (A); il regesto composto che comincia in minuscolo prende la maiuscola (A); un luogo di conservazione raro che somiglia (≤ 2 lettere) a uno scritto almeno tre volte e tre volte più spesso va in «Da decidere» («forse …»), mai corretto da solo; due voci di persona con lo stesso cognome e nomi simili nella stessa cella («Giovan Battista» / «Gian Battista») sono un avviso; in Qualità la riga **«Lingua assente»** (la lingua non si inventa). Le date invertite, gli anni «1555?» / «ca. 1557» e le persone senza cognome erano già decisioni della finestra «Controlla prima di importare». Collaudo: `verifiche/nomi_e_suffissi.R`. |
| D33 | **Soggettario: la classe resta in alto, i termini specifici stanno sotto (17/11/2026, Stefano).** Le classi della Guida al Tab restano il livello alto (19: «Amministrazione» e «Tribunali locali», classi della Guida, diventano termini sotto Giurisdizione; «Clero» resta classe); i termini specifici (Bigamia, Apostasia, Astrologia, le 20 sigle di Trenti) sono sotto una classe e il foglio esportato riceve la classe (D20 non cambia). Corrispondenza decisa: Apostasia → Eresia, Astrologia → Magia, Inosservanza (IP) e Ossessione (OS) → Bestemmia e miscredenza; le altre seguono la regola già presente nel tab di Modena (verificata su 3.709 schede con una sola sigla). Dove il tab di Modena dice altro è dichiarato nell'origine del termine. **Astrologia sta sempre sotto Magia, anche sul tab di Modena** (decisione di Stefano, 17/11/2026): nel tab di Modena solo 2 schede su 8 con la sigla AS hanno «Magia», e a quelle 6 la classe non è stata aggiunta (nessuna invenzione): è un intervento da decidere sul tab, non sull'import. Nel database il termine sta in `reato_termini` (fascicolo, classe, termine); l'import lo registra prima di riportare il nome alla classe, «Sostituisci» lo riporta, le copie di sicurezza lo contengono; i valori già presenti fra i capi (1 scheda con «Amministrazione») si riportano alla classe con una migrazione che conserva il termine. **Interfaccia (18/11/2026):** nella Scheda, sotto il Soggettario, una casella per ogni termine delle classi scelte (facoltative, solo nella maschera: il foglio esce con la classe); `reato:<termine>` e il testo libero li trovano; Qualità ha il controllo «Termini senza la loro classe» (anomalia, da salvare di nuovo la scheda). |
| D34 | **Anagrafiche: gli archivi hanno un'anagrafica nel profilo, con identificativi esterni verificati (17/11/2026).** Sezione `anagrafiche.archivi` del profilo: nome, varianti, luogo, identificativo Wikidata, ISIL dove Wikidata lo riporta. Gli identificativi si verificano su Wikidata e non si inventano: un archivio senza identificativo verificato non entra; un nome dei tab non si attribuisce a un archivio se non è certo che sia quello (esempio: il tab di prova «ap 2», dati fittizi, è stato ignorato). Il validatore ferma l'avvio per nome o variante ripetuti, Wikidata o ISIL malformati o ripetuti. **Alias dei luoghi** (`anagrafiche.luoghi_alias`): come un luogo è scritto in un archivio e quale luogo della lista è, con la ragione; la concordanza lo applica prima di ogni confronto con la lista, lo registra («alias del luogo nel profilo: …») e non chiede nulla. Primo alias (Stefano, 17/11/2026): nel tab di Torino «Casale» è Casale Monferrato (20 celle). **Enti produttori** (`anagrafiche.enti`, Stefano, 17/11/2026): gli enti non inquisitoriali di Torino («Senato di Nizza», «Senato di Casale», «Parlamento di Torino», «Marchesato di Saluzzo», le due forme «Inquisizione della Lombardia…») sono enti noti e non si toccano; una variante si riscrive nel nome e si registra («Conseil del Commis» → «Conseil des Commis»). Un produttore che è una frase (6 parole o più, nessun ente noto, non nella forma «Inquisizione di <città>») è una decisione proposta di partenza come «sposta»: il produttore diventa «Sant'Uffizio» se la frase nomina l'ente con un suo sinonimo, e la frase originale va in «Note libere» («Produttore indicato nel foglio: …»), senza perdere niente; se la frase non dice di chi si tratta resta un avviso. Nessun identificativo Wikidata per gli enti: per i tribunali locali e i senati non ne esiste uno verificato. **Luoghi** (17/11/2026): la lista ha già Wikidata sul 99,4% delle voci; i luoghi dei tab fuori lista sono soprattutto frazioni scritte «Frazione (Comune)», la forma della guida (Modena: 311 forme, 169 con parentesi). Wikidata non le chiude: di 140 frazioni il cui comune è in lista, solo 8 hanno una voce verificabile (un solo risultato, nome uguale, «frazione/località», situata nel comune) e sono state aggiunte alla lista (chiave = frazione + comune, come le altre); per le altre non esiste una voce, e non se ne inventa. **Frazioni confermate da una fonte (18/11/2026, deciso da Stefano):** basta una fonte indipendente (ISTAT località 2021, toponomastica IGM 2026 o OpenStreetMap entro 12 km dal comune, anche togliendo «di <comune>») e il comune dev'essere un comune vero dell'elenco ISTAT; ne sono entrate 54 (Modena), senza Wikidata. Le coordinate vengono solo da ISTAT o IGM (CC BY 4.0, da citare), mai da OpenStreetMap (ODbL, licenza del progetto non decisa), e per IGM solo entro 6 km dal comune: le 18 confermate solo da OpenStreetMap, o da IGM oltre i 6 km, restano senza coordinate. Un nome uguale a quello di un comune diventa un omonimo da decidere (es. «Cassano»). Fuori restano le forme con un comune non italiano, le regioni scritte come comune e le frazioni senza conferma: coda di lavoro di Qualità, non un difetto. |
| D35 | **Lingua: codici ISO 639 nel profilo (18/11/2026, punto 3 dell'handoff).** Ogni lingua del vocabolario ha il suo codice (Italiano `ita`, Latino `lat`, Francese `fra`, Slavo `sla`) in `tesauro.lingua` del profilo; il foglio e il database restano con i nomi italiani (nessuna colonna nuova nel modello a 28). «Slavo» è un gruppo di lingue (ISO 639-5), non una lingua: lo dice la nota del profilo, e una lingua precisa si aggiunge al vocabolario solo se un fondo la indica. Il caricatore ferma l'avvio per codici mancanti, doppi o malformati. |
| D36 | **Audit di affidabilità (18/11/2026, Stefano: «analiticità massima»; doppio spazio = errore).** Prova per mutazione su 5.584 errori iniettati nei tab veri: nessun buco sul foglio pulito; chiuse le lacune trovate. (a) **Spazi doppi** dentro la cella anche in «tipo di numerazione», «Numerazione», «epoca numerazione»: portati a uno (A). (b) **Persone**, tre domande nuove, mai applicate da sole (P): cognome tutto minuscolo senza particella («rossi, mario»); nome tutto maiuscolo con qualifica fra parentesi; nome senza virgola con particella in fondo o numerale («Savoia Carlo Emanuele I di»). Restano intoccati «d'Auria», «de' Rossi», «Anna di Mosca». (c) Limiti dichiarati: una persona senza virgola con la qualifica fra parentesi («Marchi Camillo (canonico)») non si segnala, perché non si distingue da un nome senza cognome («Francesco Antonio (sacerdote)», D29) e dalla formula dei papi (D22); inoltre se una cella di luoghi ha insieme una correzione sicura e una proposta (es. «Bologna:Siviglia»), tutta la cella è proposta da confermare. (d) Perdite note per convenzione: Tipologia → «Atti» (Napoli), particella dopo il nome, «Casale» → «Casale Monferrato». (e) Dalla prova per mutazione, altre due lacune chiuse: parola con maiuscole in mezzo («VALle», domanda); luogo ignoto in una cella che ha altre voci con parentesi (si salta la voce, non la cella). I dubbi sulle persone si guardano sulla cella com'è scritta, anche se un'altra proposta la tocca già. La prova è un collaudo permanente: `verifiche/mutazioni.R` (seme fisso, circa 1 minuto e mezzo, salta se mancano i tab veri). Il luogo di produzione con voce doppia («Roma|Roma») ora è segnalato e ridotto a una voce, come i luoghi citati. Corpus: casi 98–104. |
| D21 | **Datazione per secolo (22/10/2026, Guida al Tab).** «sec. XV», «prima/seconda metà del sec. XV», «primo/secondo/terzo/ultimo quarto del sec. XV», «inizio/fine sec. XVI», dal XIII al XIX, si riconoscono (`.datazione_secolo`) senza riscriverle: non diventano un anno (come per ogni data non risolta, nel database il campo numerico resta vuoto e il testo sta in «Estremo remoto/recente non risolto», da cui esce com'era), non si chiede di decidere e l'anteprima le conta. Ogni altra forma («circa 1600», «sec. XX») resta una decisione. |

---

## 3. Gli strati, in ordine fisso

Ogni strato lavora sull'output del precedente. L'ordine fa parte del contratto: cambiarlo cambia i risultati.

| Strato | Che cosa fa | Stato |
|---|---|---|
| **L0 Lettura** | Intestazioni tolleranti (`.chiave()`), righe senza segnatura fuori, numeri tipo «1555.0» → interi, più fogli di una cartella (Numbers) | A |
| **L1 Carattere** | NFC; NBSP → spazio; caratteri invisibili tolti; `’` → `'` (nelle colonne di nomi, luoghi, produttori, titolo); spazi doppi e ai bordi | A |
| **L2 Celle multi-valore** | Separatore giusto per colonna; spazi attorno; «;», «:» e «!» sbagliati **fuori dalle parentesi** («Giurisdizione!Eresia» → «Giurisdizione|Eresia», 22/10); duplicati esatti segnalati | A (duplicati: P) |
| **L3 Forma del valore** | Maiuscole, particelle, punto finale (solo in uscita), forma canonica del produttore, ente conservatore, forma locale dei luoghi | A |
| **L4 Vocabolario** | Valore noto o variante registrata (A); refuso vicino con prova (A); valore nuovo senza decisione registrata (P) | A / P |
| **L5 Coerenza fra colonne** | Nomi di Titolo, Regesto, Persone; reati di Regesto e Soggettario | **P sempre** (D11) |
| **L6 Coerenza di riga** | Anni nell'intervallo, remoto ≤ recente, sequenza di busta e fascicolo | solo segnalazione |
| **L7 Composizione** | Titolo (max 2 nomi) e regesto (max 3) dai campi; i testi scritti a mano non si toccano | - |

### Regole nuove di L1 e L2

| Regola | Esempio | Stato |
|---|---|---|
| NFC su ogni cella di testo | «Santità» (a + accento combinante) → «Santità» | A, **nuova** |
| NBSP → spazio normale | «Rossi, Paolo» → «Rossi, Paolo» | A, estende `concordanza.R:34` |
| Spazio a larghezza zero, BOM, trattino morbido: tolti | «Ros​si» → «Rossi» | A, **nuova** |
| `’` → `'` in nomi, luoghi, produttori, titolo (parte dei nomi) | «d’Auria, Tommaso» → «d'Auria, Tommaso» | A, **nuova** |
| Ritorno a capo dentro una cella di colonna **non** di testo libero | «Napoli\nRoma» | P, **nuova** |
| Segnaposto in colonne diverse da quelle con `n.d.` ammesso | «?» in Persone; «s.d.» in estremo remoto (candidato `n.d.`) | P, **nuova** |
| Duplicato esatto in una lista | «Rossi, Paolo\|Rossi, Paolo»: proposta di toglierne uno, spunta spenta, **non** si toglie da solo | **P**, **nuova** (D4) |
| Ordine degli elementi di una lista | mai cambiato | - |
| Separatori **dentro le parentesi** | non servono più (D8: una sola forma nella parentesi); «Liège (Liegi)\|Napoli» = **due** luoghi, caso 44, già verde | A, in vigore |

L'apostrofo non si tocca in **Note libere**, **Altra segnatura**, nei regesti strutturati o scritti a mano e in ogni colonna di testo libero (N).

---

## 4. Le colonne

Colonne del modello a 28 colonne (`dati/modello_export.xlsx`). A = automatica, P = proposta, N = intoccabile; «in vigore» = già nel codice. Tutte le colonne ricevono L1 (NFC, invisibili, spazi) tranne dove è scritto.

| # | Colonna | Separatore | A | P | N | Note |
|---|---|---|---|---|---|---|
| 1 | Segnatura | - | scomposizione dalle etichette («b.», «f.», sigla propria), spazi | parti che non sa leggere («b. 12A», «aa. IV»); sequenza di busta e fascicolo fuori ordine (avviso) | - | Un nome solo non dice il fondo: **riga respinta** (il fondo è obbligatorio). Righe senza segnatura = note, non fascicoli. In vigore. |
| 2 | Altra segnatura | - | solo L1 | - | il testo | In vigore. |
| 3 | Titolo | - | spazi, connettore con refuso («cicra»→«circa»), maiuscole dei nomi; «S.» sciolto sempre (D23) | **ogni nome che non concorda con Persone o Regesto** (D11) | titoli latini, con qualifiche («Giordani, capitano»), testo scritto a mano | Testo libero (D16): nessuna grammatica obbligata. Il titolo composto dal programma è fatto dei soli nomi (1–2 nomi; tre o più «<Genere> per diversi»). **Modificata:** niente correzione per voti. |
| 4 | Estremo remoto | - | «1555.0» → 1555; «n.d.» → `data_nd`; **D18**: «1625-1-9» → «1625-01-09», «Marzo 1644» e «1644-03» → «marzo 1644» | non è un anno; fuori intervallo (D6); **datazione per secolo della Guida al Tab (D21): né errore né anno, resta com'è scritta e si conta**; «s.d.» e simili; candidato togliendo una cifra | il valore se «lascia com'è» | Default 1200–1899, parametro per fondo. |
| 5 | Estremo recente | - | come sopra; **D30**: uguale al remoto → si toglie | recente prima del remoto: scambia / tieni uno solo | idem | Il recente non viene mai prima del remoto. |
| 6 | Definizione | - | variante registrata del fondo | refuso vicino (se non ha prova); valore nuovo | maiuscole (convenzione del foglio) | Vocabolario **per fondo**. |
| 7 | Supporto | - | variante registrata del fondo | valore nuovo | - | Come Definizione. Vocabolario: cartaceo, papiraceo, pergamenaceo, membranaceo (Guida al Tab, 22/10/2026). |
| 8–14 | Dimensioni, numerazione, consistenza | - | solo L1 | - | tutto | Fedeli. Altezza e larghezza sono in **millimetri**, il solo numero (Guida al Tab; decisione del 22/10/2026: la guida HTML parlava di centimetri). |
| 15 | Tipologia documentaria | `; ` | solo L1 | - | il valore del foglio | Atti, Lettere, Decreti (Guida al Tab) escono come arrivano, anche più d'una con «; »; ogni altro valore esce «Atti», dichiarato (decisione 5 del 22/10/2026: già così dal 14/10). |
| 16 | Regesto | - | (composto) spazi, connettore con refuso, maiuscole dei nomi; «S.» sciolto sempre (D23) | **ogni nome che non concorda** (D11); reati discordi dal Soggettario (di default il Soggettario, da verificare) | regesti strutturati dei cataloghi; prosa scritta a mano; il «;» della colonna V di ASMo | Punto finale in uscita. Max 3 nomi poi «diversi soggetti». |
| 17 | Persone citate | `\|` | spazi, maiuscole («PAolo»→«Paolo»), «;»→«\|», «:»→«\|», particella in lettura, `’`→`'`; «S.» sciolto sempre (D23) | **duplicato esatto (D4)**; **grafia diversa fra colonne (D11)**; senza cognome certo (spunta spenta); segnaposto; più candidati vicini; più di una virgola (avviso) | «d'Auria», «de' Rossi»; qualifiche fra parentesi | Vedi §5. **Modificata:** tolti «grafia che vince per voti» e «duplicato esatto» dalle automatiche. |
| 18 | Luogo di conservazione | - | spazi; la città dell'archivio → l'ente; «S.» sciolto sempre (D23) | - | il valore, vuoto resta vuoto | L'ente, mai una città (regola di Stefano del 12/10, confermata il 22/10/2026 contro la Guida al Tab, che dice «la città»). |
| 19 | Luogo di produzione | `\|` | come Luoghi; «S.» sciolto sempre (D23) | come Luoghi | parentesi | Segue le regole dei luoghi (§6). |
| 20 | Luoghi citati | `\|` | vedi §6; «S.» sciolto sempre (D23) | vedi §6 | ciò che sta fra parentesi | §6. |
| 21 | Soggetto produttore | `\|` | forma canonica «Inquisizione di \<Città\>»; solo Roma = «Sant'Uffizio»; `’`→`'` | - | i nomi dei fondi | I 15 «Sant’Uffizio» esistenti migrano a «Sant'Uffizio» (D13). |
| 22 | Soggetti citati | `\|` | «;»→«\|», spazi; «S.» sciolto sempre (D23) | **nome di persona dentro: errore proposto (D12)** | il testo | Fonte di nomi se compilata. |
| 23 | Soggettario | `\|` | maiuscola iniziale, refuso vicino a una classe (con segnalazione), **decisioni già registrate del fondo (es. «Furto» → «Giurisdizione»)**, «Modena» fra i reati eliminato, **equivalenze della Guida al Tab (D20)**, **termini del tesauro (D33)** | valore che non assomiglia a nessuna classe e senza decisione registrata | le classi | Vocabolario **chiuso**, 19 classi; i termini del tesauro (D33) rientrano nella loro classe e restano registrati nel database. |
| 24 | Eventi | `\|` | solo L1 + separatore | - | il resto | Fedeli. |
| 25 | Note libere | - | spazi (non a capo) | - | il testo | Punto finale in uscita. |
| 26 | Lingua | `; ` | minuscole, `\|`, `;`, virgola → «; »; refuso vicino | lingua nuova | - | Minuscole nel foglio, «Italiano» nel database. Per fondo. Vocabolario di base dal 22/10/2026: Italiano, Latino, Francese, Slavo. |
| 27 | Manifest IIIF | - | solo L1 | - | tutto | Fedele. |
| 28 | Identificativo immagine | - | solo L1 | - | tutto | Fedele. |

Spaziatura dei separatori (in vigore): **«\|» mai con spazi prima né dopo; «;» mai prima, sempre dopo; la virgola come il «;»**. «;» e «:» non sono separatori fra persone, luoghi, reati o soggetti: si leggono come «\|» e si segnalano fra le correzioni, **ma mai dentro le parentesi**. La virgola è ammessa solo nel testo semplice e dentro un nome.

---

## 5. Nomi di persona

- **Forma nel database:** «de Santis, Antonio» (particella in testa, minuscola). **Forma nel foglio:** particella **dopo il nome**: «Santis, Antonio de» (Stefano, 14/10). L'import legge **entrambe**.
- **Con l'apostrofo il cognome è uno solo e non si tocca:** «d'Auria», «de' Rossi», «d'Africa» (verdetto sui casi 5 e 6). Il `’` si porta a `'` (caso 4).
- **Senza cognome certo** («Paolo», «Paolo da Roma», «Nicola da Isernia»): il titolo lo riporta, le Persone no; in import è una proposta **con la spunta spenta**.
- **Concordanza (L5), modificata (D11):** due lettere di differenza e stesse iniziali segnalano che **forse** è la stessa persona. Il programma mostra le grafie in conflitto e quella più scritta come **suggerimento con la spunta spenta**, e **decide sempre chi schedatura**. Mai un abbinamento se ci sono più candidati vicini. **Fatto il 20/10:** `concordanza.R` non corregge più per voti.
- **Un duplicato esatto** («Rossi, Paolo|Rossi, Paolo») può essere un omonimo: proposta, mai tolto da solo.
- **Un nome in una sola delle tre fonti** (Titolo, Regesto, Persone) è plausibile e non si segnala.

**Incongruenza trovata (da correggere nel foglio delle regole):** `dati/regole_normalizzazione_per_colonna.xlsx`, riga 17, scrive «de Santis, Antonio» → «Santis de, Antonio»; il codice (14/10) scrive «Santis, Antonio de». Vale il codice.

---

## 6. La lista dei luoghi (senza gazetteer)

**Lite non ha un gazetteer.** Ha una **lista piatta di nomi ammessi** (D14), senza coordinate: `dati/luoghi.csv` con `chiave`, `nome`, `paese`; più le forme locali (`dati/luoghi_forma_locale.csv`). Niente mappa, editor, Wikidata. La lista è **una per copia di Lite** (non per fondo) e cresce solo con «Aggiungi alla lista» (D9); l'aggiunta è atomica, con una copia prima in `backup-luoghi/`.

**La forma locale (D7, D8).** Per ogni luogo straniero la tabella dà la forma locale e la forma italiana. La forma normalizzata è `Locale (Italiano)`, mai con altri nomi dentro la parentesi. Nessuna parentesi se la forma italiana coincide con la locale a meno dei segni diacritici.

| Caso | Regola | Stato |
|---|---|---|
| Maiuscole, accenti, spazi diversi dalla lista («TRapani») | → forma della lista | A |
| **Forma locale** («Avignon», «Alger», «London») | non è mai un refuso: si scrive la forma completa della tabella: «Avignon» → **«Avignon (Avignone)»**, «London» → **«London (Londra)»** | A (D7, D8) |
| Forma già completa («Avignon (Avignone)») | invariata (idempotenza) | A |
| Forma italiana di un luogo straniero («Avignone», «Algeri») | proposta della forma completa | P |
| «Liegi (Leodien)» | → **«Liège (Liegi)»** (il toponimo antico «Leodien» si perde, per scelta: D8) | A |
| Una lettera in più o in meno **con prova** (maiuscola dentro la parola, lettera raddoppiata) | → forma della lista | A |
| Una lettera in più o in meno **senza prova** | proposta | P (D5) |
| Luogo assente dalla lista | luogo ignoto: spunta per luogo, **Aggiungi alla lista** / **Elimina**; senza spunte «Lascia com'è»; non blocca l'import | P (D9) |
| «S. Severo» | «San/Santa/Santo/Sant'» **sempre** (D23) | A |
| Omonimo di provincia («Livo») | la Scheda chiede la provincia: «Livo (Trento)» | P |
| Ciò che sta fra parentesi | mai toccato dalle correzioni di separatore | N |
| «Santa Maria la Nova» | non è un luogo (zona di Napoli) | - |

**Effetto sui dati veri (misurato il 20/10 su una copia, solo lettura): 15 celle in 12 luoghi cambierebbero** per D8, più **1 cella «Liège (Leodien)» → «Liège (Liegi)»** (l'unica con un toponimo antico diverso dalla forma italiana: «Leodien» si perde per scelta), da migrare con copia di sicurezza e dichiarazione: Alger, Antwerpen, Barcelona (2 celle), Durrës, Lisboa, London (2), Marseille, Sevilla (2), Tanger, Toulon, Tunis, Zaragoza. (Un primo conteggio di 17 contava due volte Antwerpen, che ha due righe nella tabella, e includeva Bistrița.) **Bistrița** non cambia (la forma italiana è «Bistrita», uguale a meno dei diacritici). «Zagreb (Zagabria)» ha già la forma giusta. La tabella `dati/luoghi_forma_locale.csv` ha 34 righe, **23 senza parentesi** con forma italiana diversa: vanno riviste una per una prima di applicare D8.

Senza la lista dei luoghi il confronto sul nome non esiste: Lite perde la sola correzione di un refuso scritto sempre uguale in tutto il foglio. La coerenza interna (la grafia più frequente) resta solo come **suggerimento**, mai come correzione (D11).

---

## 7. Come si prova

1. **Corpus d'oro** (`verifiche/corpus/normalizzazione.csv`): una riga per caso, `id; colonna; strato; input; output; stato; regola; verdetto di Stefano (sì/no, data)`. Ogni regola nuova entra con almeno un caso positivo e uno che **non deve scattare**.
   I casi 57–83 (proposti il 22/10) sono **confermati il 23/10/2026 su delega di Stefano** («il lavoro devi farlo tu»): ho controllato uno per uno che ogni esito segua una decisione già scritta (D18 date, D23 e D26 santi, L1 spazi, decisione 5 del 22/10 sulla tipologia, convenzioni su maiuscole di lingua, supporto e definizione), senza trovare contraddizioni; se un caso non ti convince, riscrivi il verdetto in quella riga.
2. **Per ogni riga del corpus:** `norm(input) == output` e `norm(output) == output` (idempotenza).
3. **Neutralità sui dati veri:** export pulito → 0 celle cambiate, per ogni fondo e formato (xlsx, CSV, JSON, SQL, SQLite). Si guardano le celle.
4. **Iniezione di errori:** si parte da un export pulito, si introducono errori **noti di classe A** (maiuscole, spazi, separatori, apostrofo, NFD) e si controlla che la normalizzazione restituisca **esattamente l'originale**; quelli di classe P devono comparire fra le «Da decidere» e non essere riscritti.
5. **Separatori dentro le parentesi: non più un prerequisito** (D8: la parentesi ha una sola forma). Il caso 44 («Liège (Liegi)\|Napoli» = due luoghi) resta nel corpus come prova di regressione. Gli split che non guardano le parentesi (`arianna.R:397`, `:598`, `:903`, `compose.R:170`) restano un rischio generico per ogni nome con un «\|» o un «;» fra parentesi, ma non bloccano più D8.
6. **Round-trip esistenti** (`roundtrip.R`, `salvataggio_neutro.R`, `backup_json.R`, `export_convenzioni.R`, `normalizzazione_prova.R`, `interfaccia.R`) restano verdi: valori attesi aggiornati solo per regole dichiarate qui.
7. **Nessun commit di una regola nuova senza la copia di sicurezza e l'impronta del database vero identica prima e dopo.**

### I 40 casi del corpus, con i verdetti di Stefano (20/10)

Legenda: ✔ = confermato così com'era; ✎ = confermato con modifica o sostituito dal verdetto di Stefano.

| # | Colonna | Input | Output | Stato | Verdetto |
|---|---|---|---|---|---|
| 1 | Persone | `Rossi, Paolo ; Bianchi, Maria` | `Rossi, Paolo\|Bianchi, Maria` | A | ✔ |
| 2 | Persone | `Rossi, Paolo \| Bianchi, Maria` | `Rossi, Paolo\|Bianchi, Maria` | A | ✔ |
| 3 | Persone | `Rossi, Paolo\|Rossi, Paolo` | **proposta** di toglierne uno, spunta spenta (possono essere omonimi) | **P** | ✎ |
| 4 | Persone | `d’Auria, Tommaso` | `d'Auria, Tommaso` | A | ✔ |
| 5 | Persone | `d'Auria, Tommaso` | invariato | N | ✔ |
| 6 | Persone | `de' Rossi, Mario` | invariato | N | ✔ |
| 7 | Persone | `Rossi, Paolo:Bianchi, Maria` | `Rossi, Paolo\|Bianchi, Maria` | A | ✔ |
| 8 | Persone | `Isernia, Nicola da` (forma del foglio) | proposta di non inserirlo, spunta spenta | P | ✔ |
| 9 | Persone | `?` | proposta | P | ✔ |
| 10 | Luoghi | `TRapani` | `Trapani` | A | ✔ |
| 11 | Luoghi | `Avignon` | `Avignon (Avignone)` | A | ✔ |
| 12 | Luoghi | `Avignon (Avignone)` | invariato | A | ✔ |
| 13 | Luoghi | `Avignone` | proposta `Avignon (Avignone)` | P | ✔ |
| 14 | Luoghi | `Napoli; Roma` | `Napoli\|Roma` | A | ✔ |
| 15 | Luoghi | `Liegi (Leodien)` | **`Liège (Liegi)`** | A | ✎ |
| 16 | Luoghi | `S. Prospero` | `San Prospero` (sempre, D23) | A | ✎ |
| 17 | Luoghi | `Xyzzy` | luogo ignoto: spunta **Aggiungi** / **Elimina**; senza spunte «Lascia com'è» | P | ✎ |
| 18 | Soggettario | `eresia` | `Eresia` | A | ✔ |
| 19 | Soggettario | `Eresa` | `Eresia` (segnalato) | A | ✔ |
| 20 | Soggettario | `Furto` | **`Giurisdizione`** (decisione registrata, D10); un valore nuovo senza decisione = proposta | A / P | ✎ |
| 21 | Soggettario | `Eresia \| Magia` | `Eresia\|Magia` | A | ✔ |
| 22 | Lingua | `Italiano\|Latino` | `italiano; latino` | A | ✔ |
| 23 | Lingua | `italiano;latino` | `italiano; latino` | A | ✔ |
| 24 | Produttore | `Tribunale dell'Inquisizione di Napoli` | `Inquisizione di Napoli` | A | ✔ |
| 25 | Produttore | `Sant’Uffizio` | `Sant'Uffizio` | A | ✔ |
| 26 | Estremo remoto | `1555.0` | `1555` | A | ✔ |
| 27 | Estremo remoto | `1600.9` | errore; **la cella del foglio pulito resta vuota** finché non si decide (il valore sta in «Da decidere») | P | ✔ |
| 28 | Estremo remoto | `n.d.` | `n.d.` (`data_nd`) | A | ✔ |
| 29 | Estremo remoto | `s.d.` | proposta `n.d.`; cella vuota finché non si decide | P | ✔ |
| 30 | Estremi | remoto `1514`, recente `1512` | scambia / tieni uno | P | ✔ |
| 31 | Estremo remoto | `1950` | fuori intervallo, decisione; cella vuota finché non si decide | P | ✔ |
| 32 | Segnatura | `b. 12A` | **la riga non deve entrare finché non si decide, e il testo non si perde**. **CORRETTO il 20/10:** `segnatura_non_ammessa` la respinge con il motivo (prima usciva «…, b.NA, f.1»). | P | ✔ |
| 33 | Titolo | `Atti cicra Paolo Rossi` | `Atti circa Paolo Rossi` | A | ✔ |
| 34 | Titolo | `Atti circa Paolo Rosi` + Persone `Rossi, Paolo` | **sempre decisione**, mai per voti | **P** | ✎ |
| 35 | Titolo | titolo in latino | invariato | N | ✔ |
| 36 | Regesto | regesto strutturato | invariato (in uscita si aggiunge il punto finale, regola in vigore) | N | ✔ |
| 37 | Tutte | «Santità» NFD | «Santità» NFC | A | ✔ |
| 38 | Tutte | `Rossi,` + NBSP + `Paolo` | `Rossi, Paolo` | A | ✔ |
| 39 | Note | nota libera con «;» e a capo | invariata | N | ✔ |
| 40 | Luogo di conservazione | città dell'archivio | ente | A | ✔ |

**Casi nuovi da aggiungere al corpus (dalle decisioni):**

| # | Colonna | Input | Output | Stato |
|---|---|---|---|---|
| 41 | Luoghi | `London` | `London (Londra)` | A |
| 42 | Luoghi | `Embrun` | invariato (forma italiana uguale) | A |
| 43 | Luoghi | `Bistrița` | invariato (uguale a meno dei diacritici) | A |
| 44 | Luoghi | `Liège (Liegi)\|Napoli` | **due** luoghi, invariato | N |
| 45 | Luoghi | `Xyzzy`, con «Elimina» spuntato | il luogo è tolto dalla scheda, registrato con cella, prima e dopo | A su scelta |
| 46 | Soggetti citati | `Rossi, Paolo` | proposta: errore, è un nome di persona | P |
| 47 | Soggettario | `Furto` senza decisione registrata nel fondo | valore nuovo, decisione | P |

---

## 8. Misure sui dati (20/10/2026, solo lettura)

Fatte su una **copia** del database vero di Trama (`~/Documents/Trama/trama.sqlite`, impronta identica a quella dell'originale prima e dopo), con `PRAGMA query_only`. Il database di oggi ha **505 schede in 5 fondi** (Napoli e altri; **Modena non c'è**). Sono dati già importati, non i fogli d'ingresso originali: non provano che i fogli futuri siano puliti.

| Misura | Esito |
|---|---|
| Apostrofo dritto `'` | persone 26, luoghi 3, titolo 10, regesto 11, schede persona 17, gazetteer 15 |
| Apostrofo tipografico `’` | **solo `produttori.soggetto`: 15 celle** («Sant’Uffizio») |
| Accenti scomposti (NFD) | **0** in ogni colonna |
| NBSP, spazio a larghezza zero, BOM, trattino morbido | **0** |
| Spazi doppi / ai bordi | 1 / 0 |
| A capo nelle celle | 8 (note libere e record JSON originale: testo libero, N) |
| Trattini tipografici `–` `—` | 49, tutti nel record JSON originale dei campi extra (testo generato) |
| Luoghi: celle / distinti | 758 / 161 |
| Luoghi con parentesi | 7 distinti (Chalkida (Negroponte), Heraklion (Candia), Istanbul (Costantinopoli), Liège (Leodien), Nafpaktos (Lepanto), Zagreb (Zagabria), Zakynthos (Zante)) |
| `\|` dentro parentesi nei luoghi | **0** |
| Luoghi che D8 cambierebbe | **15 celle in 12 luoghi** |

Lettura: la forma dritta prevale ovunque (D2 confermata); gli accenti scomposti, gli invisibili e i NBSP non compaiono nei dati attuali, quindi le regole L1 sono **prevenzione**. Modificare «Sant’Uffizio» (15 celle) e i luoghi di D8 (15 celle) sono **due migrazioni dichiarate**, da eseguire con copia di sicurezza, anteprima della differenza cella per cella, e non per effetto collaudo.

---

## 9. Che cosa resta prima del codice

1. ~~Riscrivere la concordanza dei nomi (D11)~~ **fatto il 20/10:** `concordanza.R` non corregge più per voti; i collaudi `normalizzazione_prova`, `anomalie` e `interfaccia` sono stati aggiornati con l'aspettativa nuova.
2. ~~Il difetto del caso 32 (segnatura con busta non leggibile → «b.NA»)~~ **fatto il 20/10:** la riga è respinta con il motivo (`segnatura_non_ammessa`); il valore «12A» resta nel foglio, non si perde.
3. **Rivedere la tabella `luoghi_forma_locale.csv`** (23 righe senza parentesi) e decidere riga per riga la forma italiana e l'eventuale toponimo antico (D8).
4. **Registrare «Furto» → «Giurisdizione»** nelle decisioni del fondo (D10) e rimuovere dai regesti di Sant'Officio le 5 segnalazioni che ne dipendono, solo dopo aver guardato le celle.
5. **Le due migrazioni** (D13, D8): copia di sicurezza, anteprima, conferma.
6. **Modena:** non è nel database di oggi; le misure sui caratteri (casi 4, 25, 37, 38) vanno rifatte sul suo foglio prima di dare le regole L1 per sicure.
7. **Il foglio `regole_normalizzazione_per_colonna.xlsx`** è la fonte (D15): aggiornato il 20/10 con la riga 17 corretta e un foglio «Lite 20-10». Le due fonti non devono divergere: ogni regola nuova si scrive in tutte e due, o si decide di tenerne una sola.
8. ~~Il corpus d'oro~~ **fatto:** `verifiche/corpus/normalizzazione.csv` (80 casi, tutti attivi e verdi: ogni colonna ne ha almeno uno) e `verifiche/corpus_normalizzazione.R` (parte dentro `tutte.R`). Ogni regola nuova si implementa accendendo il suo caso (`attivo` da «no» a «si»).
9. **Fatto il 21/10, D9:** le azioni «Aggiungi alla lista» / «Elimina» sui luoghi ignoti, con la sostituzione atomica del fondo (`importa_arianna(..., senza_segnatura_ok)`); il caso 45 è nel corpus.
