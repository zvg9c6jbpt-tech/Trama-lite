# ─────────────────────────────────────────────────────────────
# verifiche/guida.R: la guida HTML sta in piedi e non è rimasta indietro (9/10)
#
# Stefano, 9/10: «ogni modifica/aggiornamento che facciamo, aggiorniamo anche la guida html». La guida è
# `pubblicazione/guida/guida-trama-lite.html` (un file solo, senza rete). Questo collaudo verifica ciò che si può
# verificare da una macchina: il file è autosufficiente, i collegamenti interni portano da qualche parte, le 28
# colonne della guida sono quelle del modello (stesse intestazioni), le immagini hanno il testo alternativo, la data
# c'è e non è nel futuro, e le funzioni che la guida deve raccontare ci sono ancora (elenco `TERMINI` qui sotto: chi
# aggiunge una funzione visibile la aggiunge alla guida e, qui, al suo termine). In più AVVISA (senza far cadere
# niente) se il programma è cambiato dopo la data della guida o ha modifiche non committate mentre la guida no.
#
#     Rscript verifiche/guida.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
suppressMessages({ library(shiny); library(DBI); for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f) })
GUIDA <- "pubblicazione/guida/guida-trama-lite.html"
.ko <- character(0)
v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok        " else "NON TORNA ", nome, "\n"); if (!isTRUE(ok)) .ko <<- c(.ko, nome) }
g <- paste(readLines(GUIDA, encoding = "UTF-8", warn = FALSE), collapse = "\n")
testo <- gsub("<[^>]+>", " ", gsub("(src|href)=\"data:[^\"]*\"", "\\1=\"\"", g)); testo <- gsub("&[a-z#0-9]+;", " ", testo)

v("la guida esiste ed è un file solo: nessun indirizzo esterno (src, href, url())", file.exists(GUIDA) && !grepl("(src|href)=\"https?://|url\\(\\s*['\"]?https?://|<script[^>]+src=", g))
ancore <- unique(sub("^href=\"#", "", sub("\"$", "", regmatches(g, gregexpr("href=\"#[^\"]+\"", g))[[1]])))
ids <- sub("^id=\"", "", sub("\"$", "", regmatches(g, gregexpr("id=\"[^\"]+\"", g))[[1]]))
v(sprintf("i %d collegamenti interni portano a un titolo che esiste", length(ancore)), length(ancore) > 0 && all(ancore %in% ids))
img <- regmatches(g, gregexpr("<img [^>]*>", g))[[1]]; img <- img[grepl("src=\"", img)]      # il segnaposto vuoto dell'ingrandimento (alt="" e niente src) lo riempie lo script
v(sprintf("le %d immagini hanno il testo alternativo", length(img)), all(grepl("alt=\"[^\"]{8,}\"", img)))      # la guida di Lite non ha ancora schermate: zero immagini va bene
esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }
intest <- PROFILO_MODELLO$arianna
manca <- intest[!vapply(intest, function(h) grepl(esc(trimws(h)), g, fixed = TRUE), TRUE)]
v(sprintf("le %d colonne del modello ci sono tutte nella guida con la loro intestazione%s", length(intest), if (length(manca)) paste0(" (mancano: ", paste(manca, collapse = "; "), ")") else ""), length(intest) == 28L && !length(manca))
mesi <- c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")
m <- regmatches(g, regexpr("guida del [0-9]{1,2} [a-z]+ [0-9]{4}", g))
data_guida <- if (length(m)) { p <- strsplit(sub("^guida del ", "", m), " ")[[1]]; tryCatch(as.Date(sprintf("%s-%02d-%02d", p[3], match(p[2], mesi), as.integer(p[1]))), error = function(e) NA) } else NA
v("la data della guida c'è, si legge e non è nel futuro", length(m) == 1L && !is.na(data_guida) && data_guida <= Sys.Date())
v("la data dell'intestazione è la stessa del piede", length(m) == 1L && grepl(sub("^guida del ", "", m), testo, fixed = TRUE) && sum(gregexpr(sub("^guida del ", "", m), testo, fixed = TRUE)[[1]] > 0) >= 2)
TERMINI <- c("Da dove vengono i dati", "Controlla prima di importare", "Aggiungi alla lista", "Elimina", "Lascia com'è", "Togli tutte", "lista dei luoghi", "Luoghi citati",
             "Scarica pulito, senza importare", "Errori e discordanze", "Sostituisci questo fondo", "Accoda ai fascicoli esistenti", "Importa nuovo fondo",
             "28 colonne", "Per colonna", "Copia completa", "Apri la cartella", "CC BY 4.0", "English", "senza rete", "Documenti", "backup-luoghi",
             "non condividono il database", "Da guardare", "coorte", "A che punto sono le buste",
             "Continua", "formule «Sua»", "Scegliere per gruppi", "Registro modifiche")
assenti <- TERMINI[!vapply(TERMINI, function(t) grepl(t, testo, fixed = TRUE), TRUE)]
v(sprintf("la guida racconta ancora le %d funzioni che deve raccontare%s", length(TERMINI), if (length(assenti)) paste0(" (mancano: ", paste(assenti, collapse = "; "), ")") else ""), !length(assenti))

# Avvisi: non fanno cadere il collaudo, ma ricordano la regola («guida e modifica nello stesso commit»).
git <- function(...) suppressWarnings(tryCatch(system2("git", c(...), stdout = TRUE, stderr = FALSE), error = function(e) character(0)))
if (dir.exists(".git") && length(git("rev-parse", "HEAD"))) {
  ultimo <- git("log", "-1", "--format=%cs", "--", "app.R", "R", "www")
  if (length(ultimo) && !is.na(data_guida) && as.Date(ultimo) > data_guida)
    cat(sprintf("AVVISO    il programma è cambiato il %s, dopo la data della guida (%s): rileggere la guida e, se serve, aggiornarla (e la data)\n", ultimo, format(data_guida, "%d/%m/%Y")))
  modificati <- git("status", "--porcelain", "--", "app.R", "R", "www"); guida_toccata <- length(git("status", "--porcelain", "--", GUIDA)) > 0
  if (length(modificati) && !guida_toccata)
    cat(sprintf("AVVISO    %d file del programma sono modificati e non committati, e la guida no: se il cambiamento si vede, va nella guida nello stesso commit\n", length(modificati)))
}
cat("\n"); if (length(.ko)) { cat("ESITO: NON TORNA\n"); if (!interactive()) quit(status = 1) else stop("verifiche fallite") }
cat("ESITO: ok\n")
