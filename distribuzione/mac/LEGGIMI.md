# Trama-Lite.app per Mac

`bash distribuzione/mac/costruisci.sh [cartella]` → `build/Trama-Lite.app`:
R e i pacchetti che Trama Lite chiede (`R/richiesti.R`, con le loro dipendenze, nelle versioni di `renv.lock`)
copiati dentro il .app e resi spostabili (`install_name_tool`), firma ad-hoc, lanciatore in bash. Doppio clic: parte
R incluso su 127.0.0.1 (porta libera), si apre il browser, si ferma da solo 8 secondi dopo la chiusura
dell'ultima scheda. I dati stanno in `~/Documents/Trama Lite` (con `trama.log`), mai dentro il .app, e non sono
quelli di Trama.

Il programma è quello dell'elenco `distribuzione/programma.txt` (app, `R/*.R`, `www/`, la lista dei luoghi, il
layout dell'export, le forme locali, il fondo di esempio, la guida). Lo stesso elenco lo usa
`verifiche/programma_minimo.R`, che prova il programma in sola lettura.

**Provato il 21/10/2026** (R 4.6.1, arm64; 162 MB, zip di 81 MB): costruzione, avvio con `env -i` e dati in una cartella vuota,
pagina servita («Trama Lite», con la pagina «Luoghi citati»), nessuna libreria caricata fuori dal .app (lsof); nella cartella
dati compaiono `trama.sqlite`, `luoghi/`, `backup/` e `trama.log`.
**Non provato:** un Mac pulito senza R né rete (serve un altro computer); doppio clic da Finder; il primo
avvio di un .app scaricato.

Limiti noti:
- Architettura: il .app vale per l'architettura di chi lo costruisce (arm64 qui).
- Gatekeeper: la firma ad-hoc non basta per un .app scaricato o mandato per mail: macOS lo blocca (clic destro →
  Apri, oppure `xattr -dr com.apple.quarantine Trama-Lite.app`). Per evitarlo servono un account sviluppatore Apple
  (99 $/anno), firma e notarizzazione.
- Tolti dal bundle `cairo.so`, `R_de.so` e il pacchetto `tcltk` (legano librerie X11 che un Mac pulito non ha).
- Il codice del programma è copiato nel .app al momento della costruzione: una modifica a `R/` richiede di rifarlo.
