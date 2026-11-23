# Il profilo: tutto ciò che il metodo deve sapere sul materiale

Il metodo (`METODO.md`) è indipendente dal dominio. Per funzionare su un materiale concreto ha bisogno di un **profilo**: l'insieme delle colonne, dei vocabolari, degli elenchi di riferimento e delle convenzioni che dicono che cosa è «corretto» per quel materiale.

Il profilo attuale è quello dei **tab di inventario dei tribunali dell'Inquisizione moderna** (Napoli, Modena, Bologna, Torino, Dublino, Reggio Emilia e altri), nel modello a 28 colonne.

## Come è fatto

Un profilo è una cartella: `profili/<nome>/profilo.json`. Il programma lo legge all'avvio (`R/profilo_dominio.R`), controlla che ci sia tutto ciò che gli serve e, se manca qualcosa, **non parte** e dice che cosa. Non ci sono valori di ripiego, perché un vocabolario sbagliato in silenzio falsificherebbe i dati. Il profilo si sceglie con la variabile `TRAMA_PROFILO` (di partenza `inquisizione`).

Le costanti che il resto del codice usa (`VOCABOLARI`, `EQUIVALENZE`, `ANNI_AMMESSI`, `TIPI_TITOLO`, `CORREZIONI_SEGNATURA`…) hanno gli stessi nomi di prima: nascono dal profilo invece che dal codice. Una prova (`verifiche/profilo_dominio.R`) controlla i valori e che un profilo guasto fermi l'avvio.

## Che cosa contiene il profilo attuale

Nel file `profili/inquisizione/profilo.json`:

- **I vocabolari**: soggettario (19 classi, dalla Guida al Tab, con una nota sulle frequenze), supporti, lingue, tipologie documentarie, soggetti produttori, definizione.
- **Le equivalenze e le varianti**: voci che la guida colloca sotto una classe («Bigamia» → «Crimini sessuali») e varianti da ricondurre («Eresia*» → «Eresia»).
- **Il parametro per fondo di partenza**: l'intervallo degli anni ammessi (1200–1899).
- **Il produttore**: la forma «Inquisizione di \<Città\>», il prefisso lungo da togliere («Tribunale dell'»), i sinonimi riconosciuti e l'eccezione di Roma («Sant'Uffizio»).
- **La grammatica del titolo**: tipi, connettori, plurali, parola della tipologia («Processi» → «Processo»).
- **I titoli dei sovrani** che fanno scattare la formula «Nome (forma italiana; titolo)».
- **L'abbreviazione «S.»** nell'ambiente ecclesiastico: «S. Uffizio» → «Sant'Uffizio», «S. Sede» → «Santa Sede», «S. Padre» → «Santo Padre», e le parole che non sono santi (Congregazione, Consulta, Rota…).
- **Le correzioni di segnatura** dichiarate, valide per segnatura e titolo precisi.
- **Il tesauro dei reati** (`tesauro.reato`, D33): 22 termini specifici (Bigamia, Apostasia, Astrologia… dalle sigle della «Casistica di perseguibilità» di Trenti, più Amministrazione e Tribunali locali, che la Guida elencava fra le classi), ciascuno con identificativo, classe della Guida sotto cui sta e origine della corrispondenza. Il caricatore ferma l'avvio se una classe non esiste, se un identificativo o un termine sono ripetuti, o se un termine ha il nome di una classe. L'import lo usa: un termine (o una sua variante) rientra nella classe e resta registrato nel database (tabella `reato_termini`); il foglio esce con la classe. Un profilo senza tesauro (`"termini": []`) funziona come prima.
- **I codici delle lingue** (`tesauro.lingua`, D35): ogni lingua del vocabolario ha il codice ISO 639 (639-3 per una lingua, 639-5 per un gruppo) e la norma da cui viene; il foglio resta con i nomi italiani. Il caricatore ferma l'avvio se una lingua non ha codice, se un codice non è di tre lettere o è dato a due lingue, se la norma è sconosciuta o se un gruppo non ha la nota che lo dice. «Slavo» è `sla` (639-5): un gruppo, non una lingua. Accessorio: `codice_lingua()`.

Restano **fuori dal file di profilo** perché sono altro:

- **Le 28 colonne**: sono il modello di scambio (`R/profili.R`, `PROFILO_MODELLO`, e `dati/modello_export.xlsx`). Un altro materiale con colonne diverse è un modello diverso, non un profilo diverso dello stesso modello: per ora il progetto ne ha uno solo e lo dichiara.
- **I formati di sola lettura** (ASDNa, ASMo), anch'essi in `R/profili.R`.
- **Le risorse italiane condivise**, in `dati/`: la lista dei luoghi (`luoghi.csv`, circa 11.600 righe, con identificativo Wikidata e coordinate come attributi: Lite non disegna mappe), le forme locali dei luoghi stranieri (circa 90), le regioni, le correzioni e il dizionario dei santi (circa 270 nomi con il genere). Servono a qualunque materiale italiano.
- **Le decisioni registrate per fondo**: stanno nel database dell'utente (`vocabolario_fondo`).
- **L'esempio inventato per provare**: `dati/esempio/`.

### Anagrafiche (`anagrafiche`)

`anagrafiche.archivi` è l'elenco degli archivi con nome, varianti, luogo, identificativo Wikidata (`Q` e cifre) e, se Wikidata lo riporta, ISIL (`IT-MO0172`). L'identificativo si verifica su Wikidata e non si inventa. Un profilo senza archivi scrive `"archivi": []`. `anagrafica_archivio(nome)` cerca per nome o variante.

`anagrafiche.luoghi_alias` dice come un luogo è scritto in un archivio e quale luogo della lista è: `scritto`, `luogo` (nome esatto della lista), `archivio` (facoltativo, uno dell'anagrafica: l'alias vale solo per le righe la cui segnatura comincia con quell'archivio) e `perche` (obbligatorio, finisce nel registro delle modifiche). `alias_luogo(x, segnatura)` lo cerca.

`anagrafiche.enti` elenca gli enti produttori che non seguono la regola del profilo: `nome`, `varianti` (una variante si riscrive nel nome), `perche` (obbligatorio), `wikidata` solo se verificato. `anagrafica_ente(x)` li cerca; `produttore_descrizione(x)` riconosce un produttore che è una frase.

## Che cosa è ancora cablato nel codice

Il lavoro di separazione non è finito. Restano nel codice, e si sistemano solo se serve un secondo profilo:

- le regole italiane dell'abbreviazione «S.» (elisione, genere dai nomi in -a, parole che precedono un luogo come «Convento»): sono di lingua e non di materiale, ma stanno in `R/luoghi.R`;
- i testi dell'interfaccia con esempi dell'Inquisizione («es. Inquisizione», il messaggio sul produttore, l'esempio nella pagina Gestione): restano frasi intere nel codice perché il sistema di traduzione le estrae come stringhe; per spostarle serve un dizionario per profilo.

Sono già nel profilo: la variante «S.t Officio» di Torino (`abbreviazione_santo.s_t_prima_di`), le traduzioni inglesi dei vocabolari (`traduzioni_en`) e la scheda di prova dei collaudi (`prova`).

Il secondo profilo esiste, di prova (`verifiche/profili_prova/epistolario`, un epistolario inventato): `verifiche/secondo_profilo.R` avvia il programma con `TRAMA_PROFILI` e `TRAMA_PROFILO` e controlla che vocabolari e convenzioni siano quelli suoi. Ha trovato e fatto correggere due difetti (profilo senza equivalenze, elenco «S.t» vuoto).

## Che cosa serve per un secondo profilo

Per applicare il metodo a un altro materiale (altri archivi, altro tipo di fonte):

- le colonne del foglio di partenza e una riga di `R/profili.R` per ciascuna;
- i vocabolari chiusi e le equivalenze;
- una lista di riferimento per luoghi e, se servono, persone o enti;
- le convenzioni scritte come decisioni, con data e motivo;
- un corpus d'oro con casi che devono scattare e casi che non devono scattare;
- almeno un foglio vero su cui provare la neutralità (zero celle cambiate su un export pulito).

Il lavoro di codice che resta è quello della sezione precedente. Il resto è un secondo `profili/<nome>/profilo.json` con le stesse sezioni del primo (il programma dice quale manca).

## Quando un profilo si può dire pronto

- Il corpus d'oro del profilo gira tutto verde e prova anche l'idempotenza.
- Un export pulito attraversa il programma con zero celle cambiate.
- Le decisioni sono scritte e datate, con il nome di chi le ha prese.
- I fogli veri sono stati letti da fuori dal progetto, senza che sparisca nessuna riga.
