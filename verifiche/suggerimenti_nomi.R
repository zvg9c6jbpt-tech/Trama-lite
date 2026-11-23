# ──────────────────────────────────────────────
# verifiche/suggerimenti_nomi.R: i nomi suggeriti nel campo Persone sono normalizzati (24/10/2026, Stefano)
# Il database può avere «Paolo Rossi» e «maria bianchi» (importati senza la correzione): il campo suggerisce «Rossi, Paolo» e «Bianchi, Maria»,
# una voce sola per ciascuna. Il database non cambia. Il cognome incerto e le formule restano come sono. L'anno proposto sta in una riga sua.
#     Rscript verifiche/suggerimenti_nomi.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
con <- apri_db(":memory:")
nuova <- function(n, persone) { f <- fascicolo_vuoto(FALSE); f$archivio <- "A"; f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- n; f$persone <- persone; salva_fascicolo(con, f) }
nuova(1L, c("Paolo Rossi", "maria bianchi")); nuova(2L, c("Rossi, Paolo", "Bianchi, Maria", "Anna di Mosca", "fra Paolo")); nuova(3L, "Louis XIV (Luigi XIV; re di Francia)")
s <- suggerimenti_da_db(con, "F")$persona
v("«Paolo Rossi» e «Rossi, Paolo» sono una voce sola, «Rossi, Paolo»", sum(s == "Rossi, Paolo") == 1L && !"Paolo Rossi" %in% s)
v("«maria bianchi» e «Bianchi, Maria» sono una voce sola, con le maiuscole", sum(s == "Bianchi, Maria") == 1L && !"maria bianchi" %in% s)
v("il cognome incerto, il nome senza cognome e la formula di un sovrano restano come sono", all(c("Anna di Mosca", "fra Paolo", "Louis XIV (Luigi XIV; re di Francia)") %in% s))
v("il database non è cambiato (restano le due grafie scritte)", all(c("Paolo Rossi", "maria bianchi") %in% dbGetQuery(con, "SELECT persona FROM persone")$persona))
v("senza persone l'elenco è vuoto", length(suggerimenti_da_db(apri_db(":memory:"))$persona) == 0L)
tema <- paste(unlist(lapply(c("R/tema.R", sort(list.files("tema", "[.]css$", full.names = TRUE))), readLines, encoding = "UTF-8", warn = FALSE)), collapse = "\n")   # il CSS sta in tema/*.css
v("«Usa <anno>» sta in una riga sua, senza il margine negativo che lo sovrapponeva a «Aggiungi mese e giorno»",
  grepl("\\.proposta-anno \\{[^}]*padding-top", tema) && !grepl("\\.proposta-anno \\{[^}]*margin: *-", tema))
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
