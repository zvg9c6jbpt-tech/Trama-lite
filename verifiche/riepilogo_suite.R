# ──────────────────────────────────────────────
# verifiche/riepilogo_suite.R: il riepilogo di tutte.R distingue riusciti, falliti e saltati (23/10/2026)
# Tre collaudi finti (uno riesce, uno cade, uno si salta con `SALTATO: …`) passano per la vera tutte.R:
# un salto non deve finire fra i riusciti, il motivo deve leggersi, e con TRAMA_VERIFICA_COMPLETA=1 un salto rende la verifica incompleta.
#     Rscript verifiche/riepilogo_suite.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
fix <- tempfile("collaudi_finti"); dir.create(fix)
writeLines('cat("ESITO: ok\\n")', file.path(fix, "riesce.R"))
writeLines('stop("guasto di prova")', file.path(fix, "cade.R"))
writeLines('cat("SALTATO: manca la cartella di prova\\nESITO: ok - saltato\\n")', file.path(fix, "salta.R"))
suite <- function(scelti = character(), completa = FALSE) {
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c(shQuote("verifiche/tutte.R"), scelti), stdout = TRUE, stderr = TRUE,
                                  env = c(paste0("TRAMA_COLLAUDI_CARTELLA=", fix), paste0("TRAMA_VERIFICA_COMPLETA=", if (completa) "1" else "0"))))
  list(testo = paste(out, collapse = "\n"), stato = { s <- attr(out, "status"); if (is.null(s)) 0L else as.integer(s) })
}
tutti <- suite()
v("tutti e tre: 1 riuscito, 1 fallito, 1 saltato", grepl("3 collaudi: 1 riusciti, 1 falliti, 1 saltati", tutti$testo, fixed = TRUE))
v("il salto dice il motivo accanto al nome", grepl("saltato +salta +[0-9]+ s +manca la cartella di prova", tutti$testo))
v("il guasto fa fallire la suite (stato 1)", tutti$stato == 1L && grepl("ESITO: NON TORNA (cade)", tutti$testo, fixed = TRUE))
senza_guasto <- suite(c("riesce", "salta"))
v("senza guasti: 1 riuscito e 1 saltato, mai 2 riusciti", grepl("2 collaudi: 1 riusciti, 0 falliti, 1 saltati", senza_guasto$testo, fixed = TRUE))
v("senza guasti la suite passa (stato 0) ma dice che uno non è stato eseguito", senza_guasto$stato == 0L && grepl("ESITO: ok - ma 1 non eseguiti: salta", senza_guasto$testo, fixed = TRUE))
completa <- suite(c("riesce", "salta"), completa = TRUE)
v("verifica completa: un salto la rende INCOMPLETA (stato 2)", completa$stato == 2L && grepl("ESITO: INCOMPLETO (non eseguiti: salta)", completa$testo, fixed = TRUE))
pulita <- suite("riesce", completa = TRUE)
v("verifica completa senza salti: ok (stato 0)", pulita$stato == 0L && grepl("1 collaudi: 1 riusciti, 0 falliti, 0 saltati", pulita$testo, fixed = TRUE) && grepl("ESITO: ok\n", pulita$testo, fixed = TRUE))
# I collaudi veri che si saltano usano la stessa riga
for (f in c("tab_reali", "revisione_browser", "autocorrezione"))
  v(sprintf("%s.R dichiara il salto con «SALTATO: »", f), any(grepl('SALTATO: ', readLines(file.path("verifiche", paste0(f, ".R")), warn = FALSE), fixed = TRUE)))
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
