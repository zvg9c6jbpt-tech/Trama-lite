# Aggiungere le frazioni di una zona alla lista dei luoghi

Le frazioni entrano a zone, man mano che si lavora su un archivio. Sono già dentro: Campania, Molise, Puglia, Basilicata, Calabria, Lazio (02/11/2026).

Serve una cartella di lavoro (`LAVORO`) con, già scaricati: `istat.csv` (codice, nome, bilingue, altra, provincia, sigla, regione, dall'elenco ISTAT dei comuni),
`scarico-wikidata/comuni.json` e `comuni2.json` (comuni con codice ISTAT, ID e coordinate), `scarico-geonames/IT.txt`.

1. `LAVORO=… python3 -I comuni_per_regione.py` scrive `comuni_per_regione.json` (comuni di ogni regione con il loro Q di Wikidata).
2. `cd LAVORO && node …/frazioni-wikidata.cjs Sicilia Abruzzo` interroga Wikidata comune per comune (a gruppi di 40: la regione intera va in timeout) e scrive `scarico-wikidata/frazioni_<Regione>.json`.
3. `LAVORO=… python3 -I aggiungi_frazioni.py Sicilia Abruzzo` aggiunge a `dati/luoghi.csv` le frazioni nuove: chiave semplice se il nome è unico, altrimenti frazione+comune; salta nomi di meno di 4 lettere e nomi con cifre.
4. Collaudi: `verifiche/tab_reali.R` va lanciato con `TRAMA_HOME` su una cartella vuota, altrimenti legge la copia personale e non prova la lista nuova.

Comuni ISTAT che mancano o senza identificativo: `LAVORO=… python3 -I comuni_mancanti.py` (stessi dati scaricati di `base.py`). Fra più elementi con lo stesso codice ISTAT sceglie quello con più collegamenti Wikimedia, e si può rieseguire: non riscrive le correzioni già presenti. **Dopo ogni esecuzione** controllare che gli identificativi dei comuni siano comuni e non frazioni omonime (il collaudo di `normalizzazione_prova.R` non lo vede) e che «Aggiungi alla lista» e le correzioni delle copie personali (`dati/luoghi_correzioni.csv`, colonne `wikidata_prima`, `wikidata`, `coordinate`) restino coerenti.
