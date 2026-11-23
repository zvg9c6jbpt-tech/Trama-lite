# ──────────────────────────────────────────────
# verifiche/mutazioni.R: prova per mutazione sui tab veri (18/11/2026, audit di affidabilità, D36)
# In ogni tab, già pulito, si iniettano errori noti (spazi, maiuscole, separatori, refusi, anni, ecc.), una cella per riga, e si passa dal percorso dell'app
# (leggi → proposte → correzioni spuntate → pulisci). Ogni errore deve finire RIPRISTINATO, SEGNALATO o in un limite dichiarato (LIMITI).
# Falliscono: un errore del programma, la pulizia bloccata, una riga persa, o un errore che passa in silenzio (BUCO / cambiata altrimenti) fuori dai limiti.
# Seme fisso: l'esito è ripetibile. I tab veri stanno fuori dal progetto (`Tab Completi`): senza la cartella il collaudo stampa `SALTATO: …`.
#     Rscript verifiche/mutazioni.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
set.seed(20261118)
BASE <- Sys.getenv("TRAMA_TAB_REALI", file.path(Sys.getenv("HOME"), "Desktop", "Digital Inquisition", "Tab Completi"))
if (!dir.exists(BASE)) { cat("SALTATO: la cartella dei tab veri non c'è (", BASE, "): la prova per mutazione NON è stata fatta\n"); quit(status = 0) }
TAB <- list(
  list(f = "Torino/Tab ASTo.xlsx"),
  list(f = "Bologna/Tab Bologna - AGABo, aa.164-168.xlsx"),
  list(f = "Dublino - Trinity College/TAB Trinity College.xlsx", archivio = "Trinity College", fondo = "Inquisizione"),
  list(f = "Napoli/ASDNa buste 1-32.xlsx"))
nz <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
MULTI <- c("Persone citate o indicizzate", "Luogo di produzione (data topica)", "Luoghi citati o indicizzati", "Soggetto produttore", "Soggetti citati o indicizzati", "Soggettario", "Eventi")
CHIUSI <- c("Soggettario", "Lingua", "Tipologia documentaria", "Supporto (materiale)", "Luoghi citati o indicizzati", "Luogo di produzione (data topica)")
prima_voce <- function(v, sep = "|") strsplit(v, sep, fixed = TRUE)[[1]][1]
sost_prima <- function(v, nuova, sep = "|") { p <- strsplit(v, sep, fixed = TRUE)[[1]]; p[1] <- nuova; paste(p, collapse = sep) }
intermezzo <- function(s) { n <- nchar(s); if (n < 5) return(NA_integer_); sample(2:(n - 2), 1) }

# persona_senza_virgola non muta le voci con la qualifica fra parentesi: non si distinguono da «Francesco Antonio (sacerdote)» (D29) né dalla formula dei papi (D22), limite dichiarato in D36.
MUT <- list(
  spazio_doppio = function(col, v) if (grepl(" ", v)) sub(" ", "  ", v) else NULL,
  spazio_in_coda = function(col, v) paste0(v, " "),
  spazio_in_testa = function(col, v) paste0(" ", v),
  nbsp = function(col, v) if (grepl(" ", v)) sub(" ", " ", v) else NULL,
  invisibile = function(col, v) if (nchar(v) > 3) paste0(substr(v, 1, 2), "​", substr(v, 3, nchar(v))) else NULL,
  pipe_con_spazi = function(col, v) if (col %in% MULTI && grepl("|", v, fixed = TRUE)) sub("|", " | ", v, fixed = TRUE) else NULL,
  pipe_come_puntovirgola = function(col, v) if (col %in% MULTI && grepl("|", v, fixed = TRUE)) sub("|", ";", v, fixed = TRUE) else NULL,
  pipe_come_duepunti = function(col, v) if (col %in% MULTI && grepl("|", v, fixed = TRUE)) sub("|", ":", v, fixed = TRUE) else NULL,
  voce_doppia = function(col, v) if (col %in% MULTI) paste0(v, "|", prima_voce(v)) else NULL,
  voce_vuota = function(col, v) if (col %in% MULTI && grepl("|", v, fixed = TRUE)) sub("|", "||", v, fixed = TRUE) else NULL,
  maiuscole_in_mezzo = function(col, v) if (col == "Persone citate o indicizzate") { w <- prima_voce(v); if (nchar(w) > 4) sost_prima(v, paste0(substr(w, 1, 1), toupper(substr(w, 2, 3)), substr(w, 4, nchar(w)))) else NULL } else NULL,
  tutto_maiuscolo = function(col, v) if (col %in% c("Persone citate o indicizzate", "Soggettario", "Luoghi citati o indicizzati")) sost_prima(v, toupper(prima_voce(v))) else NULL,
  tutto_minuscolo = function(col, v) if (col %in% c("Persone citate o indicizzate", "Soggettario", "Luoghi citati o indicizzati")) sost_prima(v, tolower(prima_voce(v))) else NULL,
  refuso_cancella = function(col, v) if (col %in% c(CHIUSI, "Persone citate o indicizzate")) { w <- prima_voce(v); k <- intermezzo(w); if (is.na(k) || grepl("[( ]", w)) NULL else sost_prima(v, paste0(substr(w, 1, k - 1), substr(w, k + 1, nchar(w)))) } else NULL,
  refuso_scambia = function(col, v) if (col %in% c(CHIUSI, "Persone citate o indicizzate")) { w <- prima_voce(v); k <- intermezzo(w); if (is.na(k) || grepl("[( ]", w) || substr(w, k, k) == substr(w, k + 1, k + 1)) NULL else sost_prima(v, paste0(substr(w, 1, k - 1), substr(w, k + 1, k + 1), substr(w, k, k), substr(w, k + 2, nchar(w)))) } else NULL,
  refuso_sostituisci = function(col, v) if (col %in% c(CHIUSI, "Persone citate o indicizzate")) { w <- prima_voce(v); k <- intermezzo(w); if (is.na(k) || grepl("[( ]", w)) NULL else sost_prima(v, paste0(substr(w, 1, k - 1), "x", substr(w, k + 1, nchar(w)))) } else NULL,
  apostrofo_tipografico = function(col, v) if (grepl("'", v, fixed = TRUE) && col %in% c("Persone citate o indicizzate", "Soggetto produttore", "Luoghi citati o indicizzati")) gsub("'", "’", v, fixed = TRUE) else NULL,
  anno_con_decimale = function(col, v) if (col %in% c("Estremo remoto", "Estremo recente") && grepl("^\\d{4}$", v)) paste0(v, ".0") else NULL,
  anno_mese_senza_zero = function(col, v) if (col %in% c("Estremo remoto", "Estremo recente") && grepl("^\\d{4}-\\d{2}-\\d{2}$", v)) sub("-0", "-", v) else NULL,
  anno_fuori_intervallo = function(col, v) if (col %in% c("Estremo remoto", "Estremo recente") && grepl("^\\d{4}$", v)) "2055" else NULL,
  anno_con_lettera = function(col, v) if (col %in% c("Estremo remoto", "Estremo recente") && grepl("^\\d{4}$", v)) sub("^(\\d{3})\\d", "\\1O", v) else NULL,
  regesto_senza_punto = function(col, v) if (col %in% c("Regesto o descrizione sintetica", "Note libere") && grepl("\\.$", v)) sub("\\.$", "", v) else NULL,
  santo_abbreviato = function(col, v) if (col %in% c("Persone citate o indicizzate", "Luoghi citati o indicizzati") && grepl("San ", v)) sub("San ", "S. ", v) else NULL,
  tipologia_inventata = function(col, v) if (col == "Tipologia documentaria") "Verbali" else NULL,
  lingua_maiuscola = function(col, v) if (col == "Lingua") paste0(toupper(substr(v, 1, 1)), substr(v, 2, nchar(v))) else NULL,
  lingua_nuova = function(col, v) if (col == "Lingua") "tedesco" else NULL,
  reato_inventato = function(col, v) if (col == "Soggettario") sost_prima(v, "Stregoneria") else NULL,
  luogo_inventato = function(col, v) if (col == "Luoghi citati o indicizzati") sost_prima(v, "Pippolandia") else NULL,
  persona_con_punto_e_virgola_nel_nome = function(col, v) if (col == "Persone citate o indicizzate" && grepl(", ", prima_voce(v), fixed = TRUE)) sost_prima(v, sub(", ", "; ", prima_voce(v), fixed = TRUE)) else NULL,
  persona_senza_virgola = function(col, v) if (col == "Persone citate o indicizzate" && grepl(", ", prima_voce(v), fixed = TRUE) && !grepl("(", prima_voce(v), fixed = TRUE)) sost_prima(v, sub(", ", " ", prima_voce(v), fixed = TRUE)) else NULL
)


esito <- list(); ROUND <- 2
leggi_x <- function(pp) { if (grepl("[.]zip$", pp)) { dz <- tempfile(); dir.create(dz); utils::unzip(pp, exdir = dz); xs <- list.files(dz, "[.]xlsx$", full.names = TRUE, recursive = TRUE); pp <- xs[!grepl("respint|registro", basename(xs), ignore.case = TRUE)][1] }
  list(path = pp, df = as.data.frame(readxl::read_excel(pp, sheet = 1, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)) }
percorso_completo <- function(file, extra, nome) {      # esattamente quello che fa l'app: leggi → proposte → correzioni spuntate → pulisci
  a <- leggi_arianna(file); pr <- proposte_import(a, NULL, NULL, luoghi_ignoti = FALSE)
  a1 <- applica_proposte(a, pr, ok = pr$celle$id[pr$celle$consigliata], scelte = list())
  pf <- tryCatch(do.call(pulisci_foglio, c(list(a1, nome = nome, revisionato = TRUE), extra)), error = function(e) conditionMessage(e))
  list(pr = pr, pf = pf) }
for (t in TAB) {
  percorso <- file.path(BASE, t$f); nome <- sub("[.]xlsx$", "", basename(t$f))
  extra <- t[c("archivio", "fondo")[c("archivio", "fondo") %in% names(t)]]
  r0 <- percorso_completo(percorso, extra, basename(t$f)); if (is.character(r0$pf)) next
  base <- leggi_x(r0$pf$percorso); raw <- base$df; n <- nrow(raw)
  clean_file <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(raw, clean_file)
  base_pr <- proposte_import(leggi_arianna(clean_file), NULL, NULL, luoghi_ignoti = FALSE)
  base_prL <- proposte_import(leggi_arianna(clean_file), NULL, NULL)
  cat(sprintf("\n== %s: %d righe\n", nome, n)); flush.console()
  for (r in seq_len(ROUND)) {
    d <- raw; scelta <- vector("list", n)
    for (i in sample(n)) {
      cand <- list()
      for (col in names(raw)) { v <- nz(raw[[col]])[i]; if (!nzchar(v)) next
        for (m in names(MUT)) { w <- tryCatch(MUT[[m]](col, v), error = function(e) NULL); if (!is.null(w) && !identical(w, v)) cand[[length(cand) + 1]] <- list(col = col, m = m, w = w, v = v) } }
      if (length(cand)) { x <- cand[[sample(length(cand), 1)]]; scelta[[i]] <- x; d[[x$col]][i] <- x$w }
    }
    f_mut <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, f_mut)
    rr <- tryCatch(percorso_completo(f_mut, extra, basename(t$f)), error = function(e) NULL)
    if (is.null(rr)) { cat("  giro", r, ": errore del programma\n"); esito[[length(esito) + 1]] <- data.frame(tab = nome, mutazione = "-", colonna = "-", riga = NA, esito = "ERRORE del programma", orig = "", mutata = "", finale = "", stringsAsFactors = FALSE); next }
    if (is.character(rr$pf)) { cat("  giro", r, ": pulisci fallito:", substr(rr$pf, 1, 140), "\n"); esito[[length(esito) + 1]] <- data.frame(tab = nome, mutazione = "-", colonna = "-", riga = NA, esito = "PULIZIA BLOCCATA", orig = "", mutata = "", finale = substr(rr$pf, 1, 100), stringsAsFactors = FALSE); next }
    fin <- leggi_x(rr$pf$percorso)$df
    if (nrow(fin) != n) { cat("  giro", r, ": righe", nrow(fin), "invece di", n, "(respinte:", nrow(rr$pf$respinti), ")\n"); next }
    pr <- rr$pr
    prL <- tryCatch(proposte_import(leggi_arianna(f_mut), NULL, NULL), error = function(e) NULL)
    if (is.null(prL)) prL <- pr
    for (i in seq_len(n)) { x <- scelta[[i]]; if (is.null(x)) next
      riga <- i + 1L; finale <- nz(fin[[x$col]])[i]
      segn <- prL$celle[prL$celle$riga == riga, , drop = FALSE]; base_riga <- base_prL$celle[base_prL$celle$riga == riga, , drop = FALSE]
      voci <- unique(trimws(unlist(strsplit(x$w, "[|;]"))))
      dec <- any(vapply(prL$decisioni, function(z) isTRUE(z$riga == riga) || isTRUE(!is.null(z$bersagli) && any(trimws(z$bersagli$da) %in% c(voci, trimws(x$w)), na.rm = TRUE)) || (!is.null(z$testo) && any(vapply(voci[nchar(voci) > 3], function(vv) isTRUE(grepl(vv, z$testo, fixed = TRUE)), NA))), NA))
      segnale <- isTRUE(nrow(segn) > nrow(base_riga)) || isTRUE(dec)
      stato <- if (identical(finale, x$v)) "RIPRISTINATA" else if (segnale) "segnalata" else if (identical(finale, x$w)) "BUCO" else "cambiata altrimenti"
      esito[[length(esito) + 1]] <- data.frame(tab = nome, mutazione = x$m, colonna = x$col, riga = riga, esito = stato, orig = substr(x$v, 1, 100), mutata = substr(x$w, 1, 100), finale = substr(finale, 1, 100), stringsAsFactors = FALSE)
    }
    cat(sprintf("  giro %d fatto (%d mutazioni)\n", r, sum(!vapply(scelta, is.null, NA)))); flush.console()
  }
}
E <- do.call(rbind, esito)
cat("\n"); print(table(E$mutazione[E$esito %in% c("BUCO", "cambiata altrimenti")], E$esito[E$esito %in% c("BUCO", "cambiata altrimenti")]))
print(table(E$esito))
# Limiti dichiarati (NORMALIZZAZIONE_LITE.md, D36): mutazioni che oggi non si distinguono da un dato legittimo.
LIMITI <- character(0)
falliti <- character()
v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
v("nessun errore del programma né pulizia bloccata", !any(E$esito %in% c("ERRORE del programma", "PULIZIA BLOCCATA")))
brutti <- E[E$esito %in% c("BUCO", "cambiata altrimenti") & !E$mutazione %in% LIMITI, ]
v(sprintf("nessun errore passa in silenzio (%d su %d)", nrow(brutti), nrow(E)), nrow(brutti) == 0L)
if (nrow(brutti)) print(utils::head(brutti[, c("tab", "mutazione", "colonna", "mutata", "finale")], 15), row.names = FALSE)
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
