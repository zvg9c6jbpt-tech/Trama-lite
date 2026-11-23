# ──────────────────────────────────────────────
# verifiche/corpus_normalizzazione.R: il corpus d'oro della normalizzazione (20/10/2026)
# `verifiche/corpus/normalizzazione.csv`: un caso per riga, con il verdetto di Stefano (NORMALIZZAZIONE_LITE.md, §7).
# Ogni caso fa il percorso vero, su un foglio di UNA riga nel modello a 28 colonne:
#   leggi_arianna → proposte_import → si applicano le correzioni spuntate di partenza (nessuna decisione presa)
#   → pulisci_foglio(revisionato = TRUE) → la cella del foglio pulito.
# Stati: A = la cella deve diventare `atteso`; N = non cambia e non si propone niente; P = non cambia da sola
# ma c'è una proposta o una decisione (se `cerca` non è vuoto, il suo testo deve comparire in proposte e avvisi).
# `attivo`: «si» = il codice di oggi deve rispettarlo (se no, NON TORNA); «no» = regola nuova, ancora da implementare
# (si stampa «da fare»; se già passa, si dice di accenderlo); «manuale» = si prova dall'interfaccia, non da una cella.
# Nel CSV `\uXXXX` e `\n` sono i caratteri veri (spazio non separabile, accento combinante, a capo).
#     Rscript verifiche/corpus_normalizzazione.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })

decodifica <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("\\n", "\n", x, fixed = TRUE)
  m <- gregexpr("\\\\u[0-9A-Fa-f]{4}", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(z) vapply(z, function(u) intToUtf8(strtoi(substring(u, 3), 16L)), ""))
  x
}
corpus <- utils::read.csv2("verifiche/corpus/normalizzazione.csv", colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0))
for (k in c("input", "ctx_input", "atteso")) corpus[[k]] <- decodifica(corpus[[k]])
COL_MODELLO <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
ARCHIVIO <- "Archivio Storico Diocesano di Napoli"; FONDO <- "Fondo di Prova"

# Un foglio di una riga nel modello, con la sola cella provata (e, se serve, un'altra di contesto), riletto come un foglio vero.
foglio_del_caso <- function(colonna, input, ctx_colonna = "", ctx_input = "") {
  riga <- stats::setNames(as.list(rep(NA_character_, length(COL_MODELLO))), COL_MODELLO)
  riga[["Segnatura"]] <- "b. 1, fasc. 1"
  stopifnot(colonna %in% COL_MODELLO); riga[[colonna]] <- input
  if (nzchar(ctx_colonna)) { stopifnot(ctx_colonna %in% COL_MODELLO); riga[[ctx_colonna]] <- ctx_input }
  f <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(as.data.frame(riga, check.names = FALSE, stringsAsFactors = FALSE), f)
  leggi_arianna(f)
}

esegui_caso <- function(k) {
  a <- foglio_del_caso(k$colonna, k$input, k$ctx_colonna, k$ctx_input)
  # `prep` = «campo|valore|forma»: una decisione già registrata per il fondo (D10), con un database di prova che la tiene
  da <- NULL; id_fondo <- NULL
  if (nzchar(k$prep) && !startsWith(k$prep, "elimina|")) {
    pz <- strsplit(k$prep, "|", fixed = TRUE)[[1]]
    da <- apri_db(tempfile(fileext = ".sqlite")); on.exit(try(DBI::dbDisconnect(da), silent = TRUE), add = TRUE)
    id_fondo <- crea_archivio(da, ARCHIVIO, FONDO); registra_valore(da, id_fondo, pz[1], pz[2], pz[3])
  }
  # `prep` = «elimina|Luogo»: la decisione sui luoghi ignoti è presa con «Elimina» spuntato (D9), come dall'interfaccia
  elimina <- if (startsWith(k$prep, "elimina|")) strsplit(k$prep, "|", fixed = TRUE)[[1]][2] else NULL
  if (!is.null(elimina)) { da <- NULL; id_fondo <- NULL }
  pr <- proposte_import(a, da, id_fondo)
  scelte <- list()
  if (!is.null(elimina)) for (d in pr$decisioni) if (identical(d$tipo, "luoghi_ignoti")) scelte[[as.character(d$id)]] <- paste0("\002", elimina)
  a2 <- applica_proposte(a, pr, ok = pr$celle$id[pr$celle$consigliata], scelte = scelte)
  p <- tryCatch(pulisci_foglio(a2, archivio = ARCHIVIO, fondo = FONDO, revisionato = TRUE), error = function(e) NULL)
  uscita <- NA_character_; respinta <- FALSE
  if (!is.null(p) && p$importati >= 1L) {
    pul <- as.data.frame(readxl::read_excel(p$percorso, sheet = 1, col_types = "text"), check.names = FALSE)
    uscita <- pul[[k$colonna]][1]
  } else respinta <- TRUE
  if (is.na(uscita)) uscita <- ""
  prop <- c(vapply(pr$decisioni, function(d) as.character(d$testo), ""), pr$celle$perche)      # proposte e decisioni
  avvisi <- tryCatch(as.character(correzioni_import(a)$regola), error = function(e) character(0))   # anteprima dell'import
  n_prop <- length(pr$decisioni) + sum(!pr$celle$consigliata)
  cerca_ok <- if (nzchar(k$cerca)) any(grepl(k$cerca, c(prop, avvisi))) else n_prop > 0L
  atteso <- if (identical(k$atteso, "=")) k$input else k$atteso
  ok <- switch(k$stato,
    A = identical(uscita, atteso),
    N = identical(uscita, k$input) && length(prop) == 0L,
    P = identical(uscita, atteso) && cerca_ok,
    FALSE)
  # «<respinta>» = la riga non entra (segnatura non ammessa): il testo resta nel foglio, qui basta che non esca
  if (respinta) ok <- identical(k$atteso, "<respinta>")
  else if (identical(k$atteso, "<respinta>")) ok <- FALSE
  list(ok = ok, uscita = if (respinta) "<respinta>" else uscita)
}

cat(sprintf("%-4s %-1s %-8s %-22s %s\n", "id", "S", "esito", "colonna", "caso"))
fallo_si <- character(); da_accendere <- character(); da_fare <- 0L
for (r in seq_len(nrow(corpus))) {
  k <- as.list(corpus[r, ])
  if (identical(k$attivo, "manuale")) { cat(sprintf("%-4s %-1s %-8s %-22s %s\n", k$id, k$stato, "manuale", k$colonna, substr(gsub("\n", "↵", k$input), 1, 50))); next }
  e <- esegui_caso(k)
  esito <- if (e$ok) (if (k$attivo == "si") "ok" else "GIÀ OK") else (if (k$attivo == "si") "NO" else "da fare")
  cat(sprintf("%-4s %-1s %-8s %-22s %s%s\n", k$id, k$stato, esito, k$colonna, substr(gsub("\n", "↵", k$input), 1, 50),
              if (!e$ok && k$attivo == "si") sprintf("   [uscita: «%s»]", substr(gsub("\n", "↵", e$uscita), 1, 90)) else ""))
  if (!e$ok && k$attivo == "si") fallo_si <- c(fallo_si, k$id)
  if (e$ok && k$attivo != "si") da_accendere <- c(da_accendere, k$id)
  if (!e$ok && k$attivo != "si") da_fare <- da_fare + 1L
}
# Le azioni sui luoghi ignoti combinate (D9, 21/10/2026): più luoghi in una cella, Aggiungi e Elimina insieme, lo stesso luogo in tutte e due (non si tocca).
local({
  riga <- stats::setNames(as.list(rep(NA_character_, length(COL_MODELLO))), COL_MODELLO); riga[["Segnatura"]] <- "b. 1, fasc. 1"
  riga[["Luoghi citati o indicizzati"]] <- "Napoli|Xyzzy|Qwerty|Roma"
  f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(as.data.frame(riga, check.names = FALSE), f)
  a <- leggi_arianna(f); pr <- proposte_import(a); col <- unname(a$colonna["Luoghi citati o indicizzati"])
  d <- Filter(function(x) identical(x$tipo, "luoghi_ignoti"), pr$decisioni)[[1]]
  prova <- function(s) applica_proposte(a, pr, scelte = stats::setNames(list(s), as.character(d$id)))
  ko <- c(
    if (!identical(prova("\002Xyzzy")$dati[[col]], "Napoli|Qwerty|Roma")) "elimina uno",
    if (!identical(prova("\002Xyzzy\001\002Qwerty")$dati[[col]], "Napoli|Roma")) "elimina due",
    if (!identical(prova("Xyzzy\001\002Qwerty")$dati[[col]], "Napoli|Xyzzy|Roma") || !identical(attr(prova("Xyzzy\001\002Qwerty"), "luoghi_da_aggiungere"), "Xyzzy")) "aggiungi uno, elimina l'altro",
    if (!identical(prova("Xyzzy\001\002Xyzzy")$dati[[col]], "Napoli|Xyzzy|Qwerty|Roma") || length(attr(prova("Xyzzy\001\002Xyzzy"), "luoghi_da_aggiungere"))) "lo stesso luogo in tutte e due",
    if (!identical(prova("\002Roma")$dati[[col]], "Napoli|Xyzzy|Qwerty|Roma")) "un luogo che non è ignoto non si toglie")
  # La traccia: la tabella dei luoghi tolti (riga, colonna, prima, dopo); la cella con un separatore ancora da correggere non si tocca e si segnala.
  t <- attr(prova("\002Xyzzy"), "luoghi_tolti")
  if (!(is.data.frame(t) && nrow(t) == 1L && identical(t$Riga, 2L) && identical(t$Prima, "Napoli|Xyzzy|Qwerty|Roma") && identical(t$Dopo, "Napoli|Qwerty|Roma"))) ko <- c(ko, "traccia dei luoghi tolti")
  riga2 <- stats::setNames(as.list(rep(NA_character_, length(COL_MODELLO))), COL_MODELLO); riga2[["Segnatura"]] <- "b. 1, fasc. 2"; riga2[["Luoghi citati o indicizzati"]] <- "Napoli, Xyzzy"
  f2 <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(rbind(as.data.frame(riga, check.names = FALSE), as.data.frame(riga2, check.names = FALSE)), f2)
  a3 <- leggi_arianna(f2); pr3 <- proposte_import(a3); d3 <- Filter(function(x) identical(x$tipo, "luoghi_ignoti"), pr3$decisioni)[[1]]
  r3 <- applica_proposte(a3, pr3, ok = integer(0), scelte = stats::setNames(list("\002Xyzzy"), as.character(d3$id)))
  rim <- attr(r3, "luoghi_rimasti")
  if (!(is.data.frame(rim) && nrow(rim) == 1L && identical(rim$Riga, 3L) && identical(r3$dati[[col]][2], "Napoli, Xyzzy"))) ko <- c(ko, "cella con separatore da correggere: non toccata e segnalata")
  cat(if (length(ko)) sprintf("NO   azioni sui luoghi ignoti: %s\n", paste(ko, collapse = "; ")) else "ok   azioni sui luoghi ignoti (Aggiungi/Elimina, D9)\n")
  if (length(ko)) fallo_si <<- c(fallo_si, "luoghi_ignoti")
})
cat(sprintf("\n%d casi: %d attivi, %d da fare, %d manuali\n", nrow(corpus), sum(corpus$attivo == "si"), sum(corpus$attivo == "no"), sum(corpus$attivo == "manuale")))
if (length(da_accendere)) cat("Già verdi ma segnati «no» (da accendere nel CSV):", paste(da_accendere, collapse = ", "), "\n")
if (length(fallo_si)) cat("ESITO: NON TORNA (", paste(fallo_si, collapse = ", "), ")\n", sep = "") else cat("ESITO: ok -", sum(corpus$attivo == "si"), "casi attivi, ", da_fare, "regole nuove ancora da fare\n")
if (length(fallo_si)) quit(status = 1)
