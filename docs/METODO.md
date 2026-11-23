# Il metodo: normalizzare dati d'archivio senza falsificarli

Questo documento descrive il **metodo** su cui poggia Trama Lite, separato dal materiale su cui oggi è applicato (gli inventari dei tribunali dell'Inquisizione, vedi `PROFILO.md`) e dal programma che lo esegue (vedi `README.md`). Le regole di dettaglio e le decisioni una per una restano in `NORMALIZZAZIONE_LITE.md`; qui c'è l'idea, in una forma che si possa discutere e riusare.

## Il problema

Un inventario archivistico nasce a mano, in anni diversi, da persone diverse. Quando i fogli di più archivi si mettono insieme, le stesse cose sono scritte in modi diversi: `Rossi, Paolo` e `Paolo Rossi`, `S. Severo` e `San Severo`, `Avignon` e `Avignone`, un separatore sbagliato, una maiuscola mancante, un refuso nel titolo.

Un correttore automatico generico risolve questi casi, ma ne crea di peggiori: tratta come errore ciò che è una scelta della fonte, sceglie fra due persone omonime, riempie con un valore ciò che la fonte non dice. In un dato di ricerca questi sono i danni da evitare, perché non si vedono più dopo.

Il metodo parte da qui: **correggere solo ciò che si può correggere senza decidere al posto di chi schedatura, e rendere visibile tutto il resto.**

## Il contratto

Ogni regola di normalizzazione deve rispettare tutti e sette i punti. Una regola che ne viola uno non entra.

- **Deterministica.** Lo stesso input dà lo stesso output, byte per byte, su qualunque macchina.
- **Idempotente.** Normalizzare un risultato già normalizzato non lo cambia.
- **Neutra sul corretto.** Una cella già giusta non si tocca mai. Un export pulito, rimesso nel programma, deve dare zero celle cambiate. È la prova che il programma non «ritocca».
- **Tracciata.** Ogni cambiamento ha cella, valore prima, valore dopo e regola che l'ha prodotto. Nessuna correzione anonima.
- **Senza perdite e senza invenzioni.** Un valore sparisce solo per scelta esplicita di chi schedatura. Ciò che la fonte non dice non lo dice nemmeno il programma: «non disponibile», «vuoto» e «no» sono tre cose diverse e restano tali.
- **Ogni regola ha uno stato solo** (vedi sotto).
- **Una regola entra solo con la prova sui dati veri**, guardando le celle e non i conteggi. Un conteggio piccolo può nascondere migliaia di cambiamenti sbagliati (una prima versione di una regola sembrava toccare «pochi» valori e ne cambiava 7.500 in un solo fondo).

## Tre stati per ogni regola

- **A, automatica.** Si applica da sola, ma compare fra le correzioni con la sua spunta e si può togliere prima di scrivere. Vale solo per ciò che è deterministico, non dipende da una scelta fra più identità possibili e ha una prova. Esempio: normalizzazione Unicode, spazi doppi, `’` → `'`.
- **P, proposta.** Compare fra le cose da decidere, senza scelta preselezionata o con la spunta spenta. Non viene mai scritta in silenzio. Esempio: due nomi che potrebbero essere la stessa persona o due omonimi.
- **N, intoccabile.** Testo libero, parentesi, regesti scritti a mano, titoli in latino, valori fedeli alla fonte. Si controllano solo gli aspetti formali.

La distinzione A/P non è di difficoltà tecnica: è **di responsabilità**. Se una correzione richiede di scegliere fra più possibilità, è una proposta anche quando l'algoritmo sarebbe sicuro. Chi schedatura ha sempre l'opzione «Lascia com'è».

## Gli strati, in ordine fisso

Ogni strato lavora sul risultato del precedente. L'ordine fa parte del contratto: cambiarlo cambia i risultati.

- **Lettura.** Intestazioni tollerante, righe senza identificativo escluse, numeri letti come numeri.
- **Carattere.** Unicode in forma unica, spazi e caratteri invisibili, apostrofo.
- **Celle con più valori.** Separatore giusto per colonna, spazi attorno, mai riordinare (l'ordine può essere significativo), duplicato esatto segnalato e non tolto.
- **Forma del valore.** Maiuscole, particelle, forma canonica di enti e luoghi.
- **Vocabolario.** Valore noto o variante già registrata: automatica. Refuso vicino con prova: automatica. Valore nuovo: proposta.
- **Coerenza fra colonne.** Lo stesso nome scritto in modi diversi in colonne diverse della stessa riga. Sempre una decisione.
- **Coerenza di riga.** Date nell'intervallo, ordine delle date, sequenza dei numeri. Solo segnalazione.
- **Composizione.** Titolo e regesto generati dai campi; i testi scritti a mano non si toccano.

## Decidere, non votare

Quando due colonne della stessa riga scrivono lo stesso nome in modo diverso, il programma non sceglie la grafia più frequente. Mostra le alternative, suggerisce la più scritta come prima voce e lascia scegliere. Il principio, nato in questo progetto, è che **la frequenza non è un criterio di verità**: un errore ripetuto resta un errore.

Ogni decisione di principio presa da chi cura i dati è registrata con data e motivo (nel progetto sono più di trenta, `D1…`). Il registro è parte del metodo: chi adotta il metodo su altro materiale scrive il suo, e le differenze fra registri sono un risultato di ricerca, non un difetto.

## Riconosci tollerante, riscrivi fedele

In lettura il programma accetta intestazioni con spazi o maiuscole diverse, formati vecchi, varianti. In scrittura restituisce la grafia del foglio di partenza (o quella del modello, dichiarato) e non quella che gli fa comodo. Una conversione di formato è sempre dichiarata.

## Controllare prima di scrivere

Nessuna scrittura avviene prima che chi schedatura abbia visto che cosa cambierà. Le correzioni sicure sono spuntate, le cose da decidere no, e l'opzione di non fare niente c'è sempre. Un'azione che scrive, distrugge o sostituisce dice che cosa perderebbe e vuole una conferma esplicita. Niente si cancella a tappeto.

## Come si prova

- **Corpus d'oro.** Un file di casi (input, output atteso, stato, regola, verdetto di chi decide). Ogni regola entra con almeno un caso che deve scattare e uno che non deve scattare. Oggi sono circa 90 casi. È anche un **benchmark**: un'altra implementazione del motore si può confrontare con quel file.
- **Idempotenza e neutralità** su ogni caso e su dati veri in tutti i formati di scarico.
- **Iniezione di errori noti.** Si parte da un export pulito, si introducono errori di classe A e si controlla che il risultato sia esattamente l'originale; quelli di classe P devono comparire fra le cose da decidere e non essere risolti in silenzio.
- **Secondo corpus di fogli veri**, letto da fuori del progetto: nessuna riga sparisce, il foglio pulito è neutro, i conteggi per regola sono confrontati con quelli registrati.
- **Il confronto cella per cella** fra foglio originale e foglio pulito produce un registro di modifiche, dichiarate come decisione, convenzione o correzione.

## Che cosa il metodo non fa

- Non identifica persone o luoghi: non dice chi sia «Paolo Rossi» né dove sia «Livo». Lo chiede.
- Non migliora il testo libero: lo controlla nelle sue parti formali.
- Non sostituisce un sistema di descrizione archivistica: lavora prima, sul foglio, e dopo restituisce un foglio.
- Non pretende di valere ovunque: le regole sono le decisioni di un gruppo di lavoro su un tipo di materiale.

## Limiti dichiarati e lavoro aperto

- Le decisioni sono finora di una persona sola e su un solo dominio. Il passo successivo, per essere un metodo condiviso, è metterle a confronto con quelle di altri archivi.
- Il motore è separato dal profilo per vocabolari, convenzioni e parametri (`profili/<nome>/profilo.json`), ma non del tutto: restano nel codice le regole italiane dell'abbreviazione «S.» e alcuni testi dell'interfaccia (vedi `PROFILO.md`, sezione «Che cosa è ancora cablato»). Un secondo profilo di prova (un epistolario inventato) fa girare il programma senza toccare il codice (`verifiche/secondo_profilo.R`); non è ancora un materiale vero.
- Il confronto con strumenti esistenti (per esempio OpenRefine, o i controlli interni dei sistemi archivistici) è da fare con una prova su un caso comune, non per descrizione.

## Dove sta ogni cosa nel progetto

- Il contratto e le decisioni: `NORMALIZZAZIONE_LITE.md` (§1 contratto, §2 decisioni, §3 strati, §4 regole per colonna, §7 prove).
- La tabella per colonna, generata: `CONTRATTO_COLONNE.md`.
- Il corpus d'oro: `verifiche/corpus/normalizzazione.csv`, eseguito da `verifiche/corpus_normalizzazione.R`.
- Il motore delle proposte: `R/concordanza.R`, `R/revisione_import.R`, `R/arianna.R`.
- Il registro delle modifiche: `R/registro_modifiche.R`.
- Le lacune note e come si chiudono: `LACUNE.md`.
