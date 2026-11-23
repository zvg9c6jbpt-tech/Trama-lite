# Trama Lite

**Pulisce e controlla gli inventari d'archivio prima che entrino in un sistema di descrizione, senza falsificarli.**

Gli inventari arrivano da archivi e persone diverse, con le stesse cose scritte in modi diversi: nomi in ordine diverso, luoghi in forma locale o italiana, separatori sbagliati, refusi. Trama Lite li porta in un unico modello a 28 colonne, corregge ciò che si può correggere con certezza e **mostra a chi schedatura tutto il resto da decidere**. Ogni cambiamento è tracciato (cella, prima, dopo, regola) e un foglio già pulito attraversa il programma senza cambiare una cella.

È un'app Shiny (R), funziona senza rete, ed è pensata per più persone che lavorano ciascuna su fondi diversi.

## Come è fatto: tre strati

- **Il metodo** (`docs/METODO.md`): il contratto di normalizzazione, i tre stati delle regole (automatica, proposta, intoccabile), il principio «decidere, non votare», il modo di provarlo. Non dipende dal materiale.
- **Il profilo** (`profili/inquisizione/profilo.json`, spiegato in `docs/PROFILO.md`): vocabolari, convenzioni e parametri del materiale. Quello attuale riguarda gli inventari dei tribunali dell'Inquisizione moderna. Se il profilo manca o è incompleto il programma non parte. Il documento dice anche che cosa è ancora scritto nel codice e che cosa servirebbe per un secondo profilo.
- **L'applicazione** (questa cartella): import con finestra «Controlla prima di importare», scheda per fascicolo, qualità dei dati, export in xlsx, CSV, JSON, SQL e SQLite, copie di sicurezza.

Le regole di dettaglio e le decisioni una per una sono in `NORMALIZZAZIONE_LITE.md`; la tabella per colonna, generata, in `CONTRATTO_COLONNE.md`.

## Avvio

- Mac: doppio clic su `avvia.command` (serve R) oppure `Rscript -e 'shiny::runApp(".")'`.
- Windows: serve R 4.6. Si usa lo zip costruito con `bash distribuzione/windows/costruisci.sh` (programma, `avvia.bat` e una cartella `libreria` con i pacchetti già pronti). Provato in una macchina virtuale; non ancora su un PC Windows reale.
- Pacchetto Mac con R incluso: `bash distribuzione/mac/costruisci.sh`.

I dati stanno in `~/Documents/Trama Lite` (database, copie di sicurezza, esportazioni, lista dei luoghi personale). Il programma può stare in una cartella di sola lettura.

Per provarlo senza dati propri c'è un fondo inventato: `dati/esempio/esempio.xlsx` (nel repository ne porta 6 fascicoli; i 24 completi, che servono ai collaudi, sono un dataset di prova da scaricare a parte e mettere in `dati/esempio/`, oppure da rigenerare con `Rscript dati/esempio/genera_esempio.R`).

## Documenti

- `docs/METODO.md`: l'idea e il contratto.
- `docs/PROFILO.md`: il materiale a cui è applicato e come si cambierebbe.
- `profili/inquisizione/profilo.json`: il profilo in uso.
- `pubblicazione/guida/guida-trama-lite.html`: come si usa (un file solo, senza rete).
- `NORMALIZZAZIONE_LITE.md`: le regole di normalizzazione e le decisioni.
- `CONTRATTO_COLONNE.md`: il contratto per colonna.
- `LACUNE.md`: i rischi noti e come si chiudono.

## Collaudi

    Rscript verifiche/tutte.R
    TRAMA_DB_COLLAUDI=esempio Rscript verifiche/tutte.R

Con l'esempio minimo `tutte.R` si ferma subito e lo dice (`ESITO: INCOMPLETO`): i collaudi vogliono i 24 fascicoli completi.

Il riepilogo conta separatamente riusciti, falliti e saltati (i collaudi che richiedono i tab privati o un browser si saltano e lo dicono). Con `TRAMA_VERIFICA_COMPLETA=1` un salto rende la verifica incompleta. Le prove nel browser e quelle di import/export sono descritte in `verifiche/browser/`.

## Stato

Beta. Il motore e il corpus di prova (102 casi, più i tab veri come secondo corpus) sono stabili; la distribuzione per Windows non è ancora provata su un PC reale.

## Licenza e citazione

Il codice è sotto licenza MIT (`LICENSE`). Per citarlo c'è `CITATION.cff`.

I dati in `dati/` sono risorse di lavoro: la lista dei luoghi (`luoghi.csv`) porta identificativi Wikidata e coordinate, e prima di riusarla altrove va verificata la fonte delle coordinate. I tab d'archivio usati come secondo corpus (`verifiche/tab_reali.R`) **non** sono nel repository: stanno fuori dal progetto e qui ci sono solo i conteggi. Senza quella cartella il collaudo stampa `SALTATO`. Il fondo di esempio (`dati/esempio/`) è inventato.

Trama Lite è un progetto a sé: non dipende da Trama.
