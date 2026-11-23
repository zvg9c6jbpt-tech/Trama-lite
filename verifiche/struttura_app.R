# ──────────────────────────────────────────────
# verifiche/struttura_app.R: la divisione di app.R in moduli non cambia l'app, e Trama Lite resta autonoma (20-21/10/2026)
#   1. L'inventario dell'interfaccia (ogni `input$…`, ogni `output$… <-`, ogni `observe`/`observeEvent` di app.R e di R/) è quello
#      del file `verifiche/struttura_app.txt`: spostare codice da un file all'altro non lo cambia. Chi AGGIUNGE o TOGLIE davvero un
#      comando lo dichiara rigenerando il file: `TRAMA_STRUTTURA_SALVA=1 Rscript verifiche/struttura_app.R`.
#   2. Trama Lite non è legata alle parti di Trama (dashboard, mappe, gazetteer con coordinate, RDF, schede persona): lo si prova su un
#      database e una cartella dati NUOVI e temporanei (mai quelli veri).
#     Rscript verifiche/struttura_app.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
estrai <- function(file) {
  t <- paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  c(regmatches(t, gregexpr("input\\$[A-Za-z0-9_.]+", t))[[1]], regmatches(t, gregexpr("output\\$[A-Za-z0-9_.]+ <-", t))[[1]],
    regmatches(t, gregexpr("observeEvent\\(", t))[[1]], regmatches(t, gregexpr("observe\\(", t))[[1]])
}
inventario <- function() { x <- sort(unlist(lapply(c("app.R", list.files("R", "[.]R$", full.names = TRUE)), estrai))); tab <- table(x); paste(names(tab), as.integer(tab)) }
BASE <- "verifiche/struttura_app.txt"
ora <- inventario()
if (nzchar(Sys.getenv("TRAMA_STRUTTURA_SALVA"))) { writeLines(ora, BASE); cat("scritto", BASE, "\n") }
prima <- readLines(BASE, encoding = "UTF-8")
v(sprintf("inventario dell'interfaccia identico a %s (%d voci)", BASE, length(prima)), identical(ora, prima))
if (!identical(ora, prima)) { cat("  differenze:\n"); cat(paste0("   ", head(c(paste("manca ", setdiff(prima, ora)), paste("nuova ", setdiff(ora, prima))), 12)), sep = "\n") }
# Gira in un sottoprocesso con database e cartella dati NUOVI e temporanei, mai quelli veri.
sotto <- function(cmd) {
  out <- suppressWarnings(system2("Rscript", c("-e", shQuote(cmd)), stdout = TRUE, stderr = FALSE,
                                  env = c(paste0("TRAMA_DB=", tempfile(fileext = ".sqlite")), paste0("TRAMA_HOME=", tempfile("trama-lite-home-")))))
  trimws(tail(out, 1))
}
v("il server parte e i dati si leggono (nessun gestore lo rompe)", identical(sotto('shiny::testServer(".", cat("pronto", nrow(dati()), "\\n"))'), "pronto 0"))
# Le coorti si salvano dall'elenco Fascicoli: ricerca scritta, «Salva», un nome.
coorte <- 'shiny::testServer(".", { session$setInputs(cerca_fascicoli = "eresia"); session$setInputs(salva_coorte_elenco = 1); session$setInputs(coorte_nome_nuovo = "Prova", coorte_conferma = 1); c2 <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("TRAMA_DB")); cat("coorte", nrow(elenco_coorti(c2)), coorti_versione(), "\\n"); DBI::dbDisconnect(c2) })'
v("una coorte si salva dall'elenco Fascicoli", identical(sotto(coorte), "coorte 1 1"))
# Trama Lite non è legata alle parti di Trama (21/10/2026): nessun file, nessuna funzione, nessuna pagina di dashboard, mappe, gazetteer, RDF, schede persona.
v("nessun file di R/ delle parti di Trama (dashboard, mappe, figure, gazetteer, persone, RDF, stati, profilo)",
  !any(file.exists(file.path("R", c("dashboard.R", "figure.R", "mappa.R", "rdf.R", "stati.R", "profilo.R", "persone.R", "anagrafe.R", "gazetteer_editor.R", "confronto.R")))))
v("nessuna cartella o file di Trama accanto: dati/ non ha coordinate né gazetteer",
  !any(file.exists(file.path("dati", c("gazetteer.csv", "costa.csv", "fiumi.csv", "laghi.csv", "stati.csv", "tribunali.csv", "aree_stati.csv")))))
sorgenti <- unlist(lapply(c("app.R", list.files("R", "[.]R$", full.names = TRUE)), readLines, encoding = "UTF-8", warn = FALSE))
v("il codice non nomina più il gazetteer, la dashboard o trama-gioco nelle istruzioni (solo nei commenti storici)",
  !any(grepl("(source|file\\.path|readLines|read_csv|\\.\\./)\\(?.*(gazetteer|dashboard|trama-gioco)", sub("#.*$", "", sorgenti), ignore.case = TRUE)))
# Pacchetti (ML-01): Lite non chiede ggplot2; i pacchetti di servizio ci sono.
pk <- 'source("R/richiesti.R"); cat("ggplot2" %in% names(PACCHETTI_RICHIESTI), "shiny" %in% names(PACCHETTI_RICHIESTI), all(c("zip", "httpuv") %in% PACCHETTI_DI_SERVIZIO), "\\n")'
v("non chiede ggplot2 ma chiede shiny", identical(sotto(pk), "FALSE TRUE TRUE"))
# La pagina «Luoghi citati»: niente persone, niente Dashboard, niente Cruscotto.
pag <- 'h <- as.character(ui); cat(grepl("data-value=\\"Luoghi citati\\"", h), grepl("data-value=\\"Persone\\"", h), grepl("data-value=\\"Cruscotto\\"", h), grepl("data-value=\\"Fascicoli\\"", h), grepl("anagrafe_vista", h), "\\n")'
v("la UI ha «Luoghi citati» e «Fascicoli», non «Persone» né «Cruscotto»", identical(sotto(paste0('source("app.R"); ', pag)), "TRUE FALSE FALSE TRUE FALSE"))
sl <- 'source("app.R"); c2 <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("TRAMA_DB")); invisible(carica_esempio(c2)); d <- elenco_fascicoli(c2); n <- elenco_voci(c2)$nome[1]; h <- as.character(scheda_luogo_ui(profilo_luogo(c2, n, d))); DBI::dbDisconnect(c2); cat(grepl("Persone citate nei suoi", h, fixed = TRUE), grepl("[0-9]+[.][0-9]{4}, ", h), nzchar(h), "\\n")'
v("la scheda del luogo non ha persone né coordinate", identical(sotto(sl), "FALSE FALSE TRUE"))
# Qualità: solo i controlli. Niente «Luoghi senza coordinate» né omonimie; i luoghi si confrontano con la lista.
qual <- 'source("app.R"); c2 <- DBI::dbConnect(RSQLite::SQLite(), Sys.getenv("TRAMA_DB")); q <- qualita(c2, NULL); DBI::dbDisconnect(c2); h <- as.character(ui); cat(any(q$controllo == "Luoghi senza coordinate"), any(startsWith(q$controllo, "Omonimi possibili")), nrow(q) > 10, grepl("id=\\"riquadro_omonimie\\"", h, fixed = TRUE), any(q$controllo == "Luoghi fuori vocabolario"), "\\n")'
v("Qualità ha i controlli e «Luoghi fuori vocabolario», non «Luoghi senza coordinate» né omonimie", identical(sotto(qual), "FALSE FALSE TRUE FALSE TRUE"))
qs <- 'shiny::testServer(".", { a <- output$qualita_a_posto; t <- output$tab_qualita; g <- output$tab_grafie_l; cat("qual", !is.null(a$html), !is.null(t), !is.null(g), "\\n") })'
v("le tabelle di Qualità si disegnano", identical(sotto(qs), "qual TRUE TRUE TRUE"))
# Nessun output del server chiama una funzione che manca (l'elenco è nell'inventario).
nomi_out <- sub("^output[$]([A-Za-z0-9_.]+) .*$", "\\1", grep("^output[$]", prima, value = TRUE))
sorg <- sprintf('shiny::testServer(".", { session$flushReact(); mal <- character(0); for (n in %s) { m <- tryCatch({ output[[n]]; "" }, error = function(e) conditionMessage(e)); if (grepl("could not find function|non trovo la funzione|not found|impossibile trovare", m)) mal <- c(mal, paste(n, m)) }; if (length(mal)) cat(mal, sep = "\\n"); cat("mancanti", length(mal), "\\n") })', paste(deparse(nomi_out), collapse = ""))
v("nessun output chiama una funzione che manca", identical(sotto(sorg), "mancanti 0"))
cat(if (length(falliti)) sprintf("ESITO: NON TORNA (%s)\n", paste(falliti, collapse = "; ")) else "ESITO: ok\n"); if (length(falliti)) quit(status = 1)
