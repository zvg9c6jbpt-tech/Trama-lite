# ──────────────────────────────────────────────
# verifiche/menu_fondo.R: il fondo si sceglie in una finestra al centro, non in una tendina (24/10/2026, Stefano: la tendina si sovrapponeva
# alla barra della scheda). L'input `fondo_attivo` resta (nascosto) e il resto dell'app lo legge come prima.
#     Rscript verifiche/menu_fondo.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
copione <- tempfile(fileext = ".R")
writeLines(c(
  'db <- tempfile(fileext = ".sqlite"); Sys.setenv(TRAMA_DB = db, TRAMA_HOME = tempfile("h")); suppressMessages(library(shiny))',
  'imp <- function(k) { c2 <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(c2)); x <- DBI::dbGetQuery(c2, "SELECT valore FROM impostazioni WHERE chiave = ?", list(k))$valore; if (length(x)) x else "<nessuna>" }',
  'idf <- function(nome) { c2 <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(c2)); DBI::dbGetQuery(c2, "SELECT id FROM fondi WHERE denominazione = ?", list(nome))$id }',
  'testo <- function(o) gsub("<[^>]+>", "", paste(as.character(if (is.list(o)) o$html else o), collapse = ""))',
  'shiny::testServer(".", {',
  '  session$setInputs(home_esempio = 1); session$flushReact(); cat("RIS etichetta1:", trimws(testo(output$fondo_etichetta)), "\\n")',
  '  session$setInputs(home_nuovo_archivio = 1); session$setInputs(arch_archivio = "Archivio Z", arch_fondo = "Fondo Z", arch_serie = "", arch_fonte = ""); session$setInputs(arch_crea = 1); session$flushReact()',
  '  cat("RIS etichetta2:", trimws(testo(output$fondo_etichetta)), "\\n")',
  '  session$setInputs(fondo_apri = 1); session$flushReact(); cat("RIS finestra: aperta\\n")',
  '  session$setInputs(fondo_scelto = idf("Tribunale di esempio")); session$flushReact()',
  '  cat("RIS salvato:", identical(imp("fondo_attivo"), idf("Tribunale di esempio")), "\\n"); cat("RIS etichetta3:", trimws(testo(output$fondo_etichetta)), "\\n")',
  '  session$setInputs(fondo_scelto = ""); session$flushReact(); cat("RIS tutti:", trimws(testo(output$fondo_etichetta)), "\\n")',
  '})'), copione)
ris <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(copione), stdout = TRUE, stderr = FALSE)); ris <- trimws(sub("^RIS ", "", grep("^RIS ", ris, value = TRUE)))
v("la barra dice il fondo per esteso (con archivio e numero di schede)", length(ris) >= 1 && grepl("^etichetta1: Fondo Tribunale di esempio · Archivio di Esempio \\(24\\)$", ris[1]))
v("dopo «Nuovo fondo» la barra dice il fondo nuovo", length(ris) >= 2 && grepl("^etichetta2: Fondo Z · Archivio Z", ris[2]))
v("la finestra si apre senza errori", length(ris) >= 3 && identical(ris[3], "finestra: aperta"))
v("scegliere un fondo nella finestra lo rende attivo e lo ricorda nel database", length(ris) >= 5 && identical(ris[4], "salvato: TRUE") && grepl("Tribunale di esempio", ris[5]))
v("«(tutti i fondi)» si sceglie dalla finestra", length(ris) >= 6 && identical(ris[6], "tutti: (tutti i fondi)"))
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
