# ──────────────────────────────────────────────
# verifiche/prestazioni.R: quanto costano le operazioni pesanti (13/10)
#
# METODO (annotato qui, perché la misura non sia occasionale):
#  - su una COPIA del database di prova (`database_di_prova()`, mai il vero), nel fondo più grande;
#  - ogni operazione si misura due volte: «a freddo» = la PRIMA chiamata di questo processo (R appena
#    avviato, connessione nuova), «a caldo» = la mediana di altre 3 chiamate di fila;
#  - orologio: `system.time()[["elapsed"]]`, in secondi;
#  - SOGLIE (a freddo), larghe perché la macchina non è sempre la stessa: 3 s per Qualità, omonimi, riempimento
#    delle colonne, valori da decidere, mappa, località; 8 s per l'export di tutto il fondo nel modello.
#    Se una soglia salta il collaudo dice quale operazione e quanto: non si alza la soglia, si guarda il perché.
#  - la tabella di ogni giro si stampa (e con TRAMA_PRESTAZIONI_SALVA=1 si aggiunge a
#    verifiche/esiti/prestazioni.md), così le misure si confrontano nel tempo.
#
#     Rscript verifiche/prestazioni.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
con <- apri_db(database_di_prova()); on.exit(dbDisconnect(con), add = TRUE)
g <- elenco_fondi(con); fondo <- g$id[which.max(g$n)]  # per id, non per nome
cat(sprintf("Fondo misurato: %s (%d schede) · %s\n", g$fondo[which.max(g$n)], max(g$n), format(Sys.time(), "%Y-%m-%d %H:%M")))

OPERAZIONI <- list(
  list("elenco dei fascicoli",        3, function() elenco_fascicoli(con, fondo = fondo)),
  list("Qualità (tutti i controlli)", 3, function() qualita(con, fondo)),
  list("grafie vicine (persone)",     3, function() grafie_vicine(con, "persone", fondo = fondo)),
  list("riempimento delle colonne",   3, function() riempimento_colonne(con, fondo)),
  list("valori da decidere",          3, function() valori_da_decidere(con, fondo)),
  list("completezza (Home)",          3, function() sum(completezza_core(elenco_fascicoli(con, fondo = fondo)) == 4L)),
  list("tabella nel modello a 28 colonne", 8, function() tabella_arianna(con, fondo = fondo)),
  # La revisione dell'import è lineare nelle righe (~12 ms a riga): il foglio di esempio replicato 20 volte (480 righe) prende ~7 s. Prima del 04/11/2026
  # ricalcolava le chiavi della lista dei luoghi per ogni nome e ne prendeva ~21: la soglia sta fra i due.
  list("revisione dell'import (480 righe)", 14, function() proposte_import(foglio_480, NULL, NULL)))
foglio_480 <- local({ a <- leggi_arianna(FOGLIO_ESEMPIO); a$dati <- a$dati[rep(seq_len(nrow(a$dati)), 20), , drop = FALSE]; a })

t <- function(f) round(system.time(f())[["elapsed"]], 2)
righe <- lapply(OPERAZIONI, function(o) {
  freddo <- t(o[[3]]); caldo <- stats::median(vapply(1:3, function(i) t(o[[3]]), 0))
  data.frame(operazione = o[[1]], freddo = freddo, caldo = caldo, soglia = o[[2]], stringsAsFactors = FALSE)
})
r <- do.call(rbind, righe)
cat(sprintf("%-36s %8s %8s %8s\n", "operazione", "freddo", "caldo", "soglia"))
for (i in seq_len(nrow(r))) cat(sprintf("%-36s %7.2fs %7.2fs %7ds %s\n", r$operazione[i], r$freddo[i], r$caldo[i], r$soglia[i],
                                        if (r$freddo[i] > r$soglia[i]) "  <-- SOPRA LA SOGLIA" else ""))
if (nzchar(Sys.getenv("TRAMA_PRESTAZIONI_SALVA", ""))) {
  dir.create("verifiche/esiti", showWarnings = FALSE)
  cat(sprintf("\n### %s · %s (%d schede)\n\n| operazione | freddo | caldo |\n|---|---|---|\n%s\n", format(Sys.time(), "%Y-%m-%d %H:%M"), fondo, max(g$n),
              paste(sprintf("| %s | %.2f s | %.2f s |", r$operazione, r$freddo, r$caldo), collapse = "\n")),
      file = "verifiche/esiti/prestazioni.md", append = TRUE)
}
sopra <- r$operazione[r$freddo > r$soglia]
cat(if (length(sopra)) sprintf("ESITO: NON TORNA (sopra la soglia: %s)\n", paste(sopra, collapse = "; ")) else "ESITO: ok\n")
if (length(sopra)) quit(status = 1)
