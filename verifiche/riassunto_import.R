# verifiche/riassunto_import.R: le righe non entrate dopo un import si dicono come nel ripristino da JSON (24/10/2026):
# «già presenti» (segnatura già occupata) separato dai «respinti» veri. Niente database.
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
n <- 0L
verifica <- function(x, testo = "") { if (!isTRUE(x)) stop("FALLITO: ", testo, call. = FALSE); n <<- n + 1L }
occ <- "Segnatura già occupata: lo stesso fondo, busta, fascicolo e suffisso compaiono due volte (o sono già nel database)."
altro <- "La segnatura non dice il fondo."
r <- function(...) { m <- c(...); data.frame(riga = seq_along(m), segnatura = rep("x", length(m)), motivo = m, stringsAsFactors = FALSE) }

verifica(identical(riassunto_import(24, 0, r(rep(occ, 24))), "24 letti · 0 importati · 24 già presenti"), "tutte già presenti")
verifica(identical(riassunto_import(24, 20, r(rep(occ, 4))), "24 letti · 20 importati · 4 già presenti"), "alcune già presenti")
verifica(identical(riassunto_import(5, 3, r(occ, altro)), "5 letti · 3 importati · 1 già presenti · 1 respinti"), "miste")
verifica(identical(riassunto_import(5, 3, r(altro, altro)), "5 letti · 3 importati · 2 respinti"), "solo respinte")
verifica(identical(riassunto_import(24, 24, r()), "24 letti · 24 importati · 0 respinti"), "nessuna: resta «0 respinti»")
verifica(identical(riassunto_import(1, 1, NULL), "1 letti · 1 importati · 0 respinti"), "senza tabella")
verifica(conta_non_entrate(r(occ, altro, occ))$presenti == 2 && conta_non_entrate(r(occ, altro, occ))$respinte == 1, "conteggio")
cat(sprintf("ESITO: ok - %d verifiche sul riassunto dell'import.\n", n))
