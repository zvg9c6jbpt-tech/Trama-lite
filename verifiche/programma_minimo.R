# ─────────────────────────────────────────────────────────────
# verifiche/programma_minimo.R: il programma di Trama Lite sta da solo, anche in sola lettura (8/10)
#
# Costruisce una copia del PROGRAMMA da distribuzione/programma.txt (niente dati
# d'archivio, niente script di lavoro), la rende di sola lettura e ci avvia l'app come
# runApp, con i dati altrove (TRAMA_HOME): se il programma tenta di scrivere nella
# propria cartella, o gli manca un file che l'elenco non porta, il collaudo cade. Poi
# carica il fondo di esempio, legge le carte e esporta nel modello.
#
#     Rscript verifiche/programma_minimo.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
righe <- trimws(readLines("distribuzione/programma.txt", encoding = "UTF-8"))
righe <- righe[nzchar(righe) & !startsWith(righe, "#")]
tmp <- tempfile("programma-"); dir.create(tmp)
for (p in righe) {
  da <- if (grepl("*", p, fixed = TRUE)) Sys.glob(p) else p
  for (f in da) {
    dest <- file.path(tmp, f)
    if (endsWith(f, "/") || dir.exists(f)) {
      dir.create(dest, recursive = TRUE, showWarnings = FALSE)
      file.copy(list.files(f, full.names = TRUE, all.files = FALSE), dest, recursive = TRUE, copy.mode = FALSE)
    } else {
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      stopifnot(file.copy(f, dest, copy.mode = FALSE))
    }
  }
}
unix <- .Platform$OS.type == "unix"
if (unix) system2("chmod", c("-R", "a-w", shQuote(tmp)))
pulisci <- function() { if (unix) system2("chmod", c("-R", "u+w", shQuote(tmp))); unlink(tmp, recursive = TRUE) }

home <- tempfile("home-")
codice <- paste0('suppressMessages({ library(shiny); library(DBI) })
grDevices::pdf(NULL)
fatti <- character(0); ko <- character(0)
v <- function(cond, msg) if (!isTRUE(cond)) ko <<- c(ko, msg)
shiny::testServer(".", {
  session$flushReact()
  v(is.numeric(nrow(dati())), "l app non parte")
  v(file.access(percorso_lista_luoghi(), 2) == 0, "lista dei luoghi personale non scrivibile")
  con <- apri_db(percorso_db())      # la connessione dell app non si vede da qui: se ne apre una nuova sullo stesso database
  invisible(carica_esempio(con))
  v(dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n == nrow(openxlsx::read.xlsx(FOGLIO_ESEMPIO)), "il fondo di esempio non si carica")
  v(file.exists("www/trama-en.json"), "manca www/trama-en.json")
  # 16/10: ogni file di dati/ letto dal codice deve esistere nella copia (la funzione che lo legge ricade su una tabella vuota, senza errori)
  letti <- unique(unlist(lapply(c(list.files("R", "[.]R$", full.names = TRUE), "app.R"), function(f) { l <- readLines(f, warn = FALSE, encoding = "UTF-8"); regmatches(l, gregexpr("[\\x22]dati/[A-Za-z0-9_./-]+[.](csv|json|xlsx)[\\x22]", l)) })))
  letti <- gsub("[\\x22]", "", letti)
  v(all(file.exists(letti)), paste("file di dati/ letti dal codice e assenti dalla copia:", paste(letti[!file.exists(letti)], collapse = ", ")))
  f <- dbGetQuery(con, "SELECT DISTINCT fondo FROM fascicoli")$fondo
  out <- tempfile(fileext = ".xlsx"); esporta_arianna(con, out, fondo = f[1])
  v(file.exists(out) && file.size(out) > 5000, "l export nel modello non riesce")
  dbDisconnect(con)
  v(!any(file.exists(file.path(getwd(), c("trama.sqlite", "backup", "Rplots.pdf", "luoghi")))), "ha scritto nella cartella del programma")
})
if (length(ko)) { cat("NON TORNA:", paste(ko, collapse = " | "), "\n"); quit(status = 1) }
cat("figlio ok\n")')
cod <- tempfile(fileext = ".R"); writeLines(codice, cod)
vecchio <- setwd(tmp)          # il figlio parte DALLA copia del programma, come runApp
out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(cod), stdout = TRUE, stderr = TRUE,
                                env = c(paste0("TRAMA_HOME=", home), "TRAMA_DB=")))
setwd(vecchio)
stato <- attr(out, "status")
if (!is.null(stato) || !any(out == "figlio ok")) {
  cat(paste(utils::tail(out, 25), collapse = "\n"), "\n")
  pulisci(); cat("ESITO: NON TORNA\n"); quit(status = 1)
}
n <- length(list.files(tmp, recursive = TRUE))
cat(sprintf("ESITO: ok - il programma (%d file, %s) parte e lavora da solo, in sola lettura, con i dati altrove\n", n,
            format(structure(sum(file.size(list.files(tmp, recursive = TRUE, full.names = TRUE))), class = "object_size"), units = "auto")))
pulisci()
