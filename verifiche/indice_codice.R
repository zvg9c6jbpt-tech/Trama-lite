# ─────────────────────────────────────────────────────────────
# verifiche/indice_codice.R: l'indice locale del codice di Trama (16/10)
#
# Legge R/*.R e app.R con le funzioni di analisi di R (`getParseData`): niente pacchetti in più, niente rete,
# niente scritto nel progetto. Costruisce quattro tabelle e le interroga:
#   simboli      id, nome, file, riga, tipo (funzione | valore), contenitore (la funzione dentro cui è definito, se annidato)
#   chiamate     da (la funzione che chiama), a, file, riga
#   riferimenti  nome, da, file, riga (un simbolo usato senza chiamarlo: per esempio passato a lapply)
#   dipendenze   file → file (A usa un simbolo definito in B), con il numero di legami; e l'elenco dei pacchetti (pkg::f, library)
#
# Uso (dalla cartella del progetto):
#   Rscript verifiche/indice_codice.R riepilogo
#   Rscript verifiche/indice_codice.R dove NOME            dove è definito
#   Rscript verifiche/indice_codice.R chi-chiama NOME      chi lo chiama (o lo usa)
#   Rscript verifiche/indice_codice.R cosa-chiama NOME     che cosa chiama una funzione
#   Rscript verifiche/indice_codice.R file FILE.R          da quali file dipende e chi dipende da lui
#   Rscript verifiche/indice_codice.R non-usate            definiti e mai usati (candidati al codice morto: da guardare a mano)
#   Rscript verifiche/indice_codice.R esporta [CARTELLA]   scrive i quattro CSV (default: cartella temporanea del sistema)
# Da R: `source("verifiche/indice_codice.R"); ind <- indice_codice(); interroga(ind, "chi-chiama", "posizione_scritta")`
# Limite dichiarato: legge solo il testo del codice. Le chiamate costruite a runtime (do.call con un nome in una stringa,
# i nomi degli input/output di Shiny) non si vedono; `non-usate` è un elenco di sospetti, non una prova.
# ─────────────────────────────────────────────────────────────

.file_codice <- function(radice = ".") c(sort(list.files(file.path(radice, "R"), pattern = "[.]R$", full.names = TRUE)), file.path(radice, "app.R"))

# un file → tabelle grezze
.leggi_file <- function(f, radice = ".") {
  nome <- sub(paste0("^", normalizePath(radice), "/?"), "", normalizePath(f))
  pd <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"), includeText = NA)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  pd <- pd[order(pd$line1, pd$col1), ]
  rownames(pd) <- as.character(pd$id)
  gen <- integer(max(pd$id)); gen[pd$id] <- pd$parent                       # genitore per id (la radice ha genitore 0)
  su <- function(i) if (i == 0L) 0L else gen[i]                             # il genitore, con la radice che resta 0
  figli <- split(pd$id, pd$parent)
  # le definizioni «nome <- function(...)»: il nodo di assegnazione ha tre figli (espressione-nome, LEFT_ASSIGN o EQ_ASSIGN, espressione-valore)
  assegn <- pd$id[pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")]
  def <- lapply(assegn, function(a) {
    n <- gen[a]; k <- figli[[as.character(n)]]
    if (length(k) < 3L) return(NULL)
    k <- k[order(pd[as.character(k), "col1"] + 1e4 * pd[as.character(k), "line1"])]
    lhs <- k[1]; rhs <- k[length(k)]
    if (pd[as.character(lhs), "token"] != "expr") return(NULL)
    t1 <- figli[[as.character(lhs)]]; if (length(t1) != 1L || !pd[as.character(t1), "token"] %in% c("SYMBOL", "STR_CONST")) return(NULL)
    nm <- gsub("^[\"'`]|[\"'`]$", "", pd[as.character(t1), "text"])
    r1 <- figli[[as.character(rhs)]]
    es_fun <- length(r1) && pd[as.character(r1[1]), "token"] == "FUNCTION"
    data.frame(nodo = n, nome = nm, riga = pd[as.character(n), "line1"], riga_fine = pd[as.character(n), "line2"], tipo = if (es_fun) "funzione" else "valore",
               stringsAsFactors = FALSE)
  })
  def <- do.call(rbind, def)
  if (is.null(def)) def <- data.frame(nodo = integer(0), nome = character(0), riga = integer(0), riga_fine = integer(0), tipo = character(0))
  def_fun <- def[def$tipo == "funzione", , drop = FALSE]
  # la funzione che racchiude un nodo: risale i genitori finché trova un nodo di definizione di funzione
  nome_nodo <- character(length(gen)); nome_nodo[def_fun$nodo] <- def_fun$nome   # per id: il nome se il nodo è una definizione di funzione
  contenitore <- function(id) {
    x <- gen[id]
    while (x != 0L) { if (nzchar(nome_nodo[x])) return(nome_nodo[x]); x <- gen[x] }
    "(livello del file)"
  }
  # simboli: contenitore = la funzione dentro cui è definito (esclusa se stessa)
  def$contenitore <- vapply(seq_len(nrow(def)), function(i) { x <- su(su(def$nodo[i]))
    r <- "(livello del file)"; while (x != 0L) { if (nzchar(nome_nodo[x])) { r <- nome_nodo[x]; break }; x <- gen[x] }; r }, "")
  # lo stesso nome definito dentro una funzione più volte (variabili locali) non è un simbolo del progetto: tengo i livello-file e le funzioni
  simboli <- def[def$contenitore == "(livello del file)" | def$tipo == "funzione", c("nome", "riga", "tipo", "contenitore"), drop = FALSE]
  simboli$file <- nome
  cal <- pd[pd$token %in% c("SYMBOL_FUNCTION_CALL", "SPECIAL"), ]       # anche gli operatori «%…%» (es. `%||%`) sono chiamate
  chiamate <- if (nrow(cal)) data.frame(da = vapply(cal$id, contenitore, ""), a = cal$text, file = nome, riga = cal$line1, stringsAsFactors = FALSE) else NULL
  riferimenti_g <- pd[pd$token == "SYMBOL", ]
  riferimenti <- if (nrow(riferimenti_g)) data.frame(nome = riferimenti_g$text, da = vapply(riferimenti_g$id, contenitore, ""), file = nome, riga = riferimenti_g$line1, stringsAsFactors = FALSE) else NULL
  pacchetti <- pd[pd$token == "SYMBOL_PACKAGE", c("text", "line1")]
  pk <- if (nrow(pacchetti)) data.frame(pacchetto = pacchetti$text, file = nome, riga = pacchetti$line1, stringsAsFactors = FALSE) else NULL
  list(simboli = simboli, chiamate = chiamate, riferimenti = riferimenti, pacchetti = pk)
}

# Con la cache: si rifà solo se è cambiato (o aggiunto) un file. La cache sta nella cartella temporanea del sistema, mai nel progetto.
indice_codice <- function(radice = ".", cache = TRUE) {
  firma <- paste(unlist(lapply(.file_codice(radice), function(f) paste(f, file.mtime(f), file.size(f)))), collapse = "|")
  rds <- file.path(Sys.getenv("TMPDIR", tempdir()), "trama-indice", "indice.rds")
  if (cache && file.exists(rds)) { c0 <- tryCatch(readRDS(rds), error = function(e) NULL); if (!is.null(c0) && identical(c0$firma, firma)) return(c0$indice) }
  ind <- .costruisci_indice(radice)
  if (cache) { dir.create(dirname(rds), showWarnings = FALSE, recursive = TRUE); try(saveRDS(list(firma = firma, indice = ind), rds), silent = TRUE) }
  ind
}

.costruisci_indice <- function(radice = ".") {
  parti <- lapply(.file_codice(radice), function(f) tryCatch(.leggi_file(f, radice), error = function(e) { message("non si legge: ", f, " (", conditionMessage(e), ")"); NULL }))
  unisci <- function(k) { x <- lapply(parti, `[[`, k); x <- x[!vapply(x, is.null, TRUE)]; if (length(x)) do.call(rbind, x) else NULL }
  simboli <- unisci("simboli"); simboli$id <- seq_len(nrow(simboli)); rownames(simboli) <- NULL
  simboli <- simboli[, c("id", "nome", "file", "riga", "tipo", "contenitore")]
  chiamate <- unisci("chiamate"); riferimenti <- unisci("riferimenti"); pacchetti <- unisci("pacchetti")
  nomi <- unique(simboli$nome)
  # chiamate e riferimenti restano solo se il nome è un simbolo del progetto
  chiamate_p <- chiamate[chiamate$a %in% nomi, ]
  # un riferimento è un SYMBOL che non è la definizione stessa e non è argomento con nome: approssimato (vedi limite in testa)
  riferimenti_p <- riferimenti[riferimenti$nome %in% simboli$nome[simboli$tipo == "funzione"], ]
  # via le righe che sono la definizione (stessa riga, stesso file, stesso nome)
  chiave_def <- paste(simboli$nome, simboli$file, simboli$riga)
  riferimenti_p <- riferimenti_p[!paste(riferimenti_p$nome, riferimenti_p$file, riferimenti_p$riga) %in% chiave_def, ]
  # dipendenze fra file: a → b se in a si usa (chiama o riferisce) un nome definito SOLO in b (un nome definito in più file è ambiguo: si salta)
  glob <- simboli[simboli$contenitore == "(livello del file)", ]     # i legami fra file contano solo i simboli di primo livello (non gli aiutanti locali di una funzione)
  una_def <- tapply(glob$file, glob$nome, function(x) if (length(unique(x)) == 1L) unique(x) else NA_character_)
  uso <- rbind(data.frame(file = chiamate_p$file, nome = chiamate_p$a), data.frame(file = riferimenti_p$file, nome = riferimenti_p$nome))
  uso$def <- unname(una_def[uso$nome]); uso <- uso[!is.na(uso$def) & uso$def != uso$file, ]
  dip <- if (nrow(uso)) { t <- as.data.frame(table(da = uso$file, a = uso$def), stringsAsFactors = FALSE); t <- t[t$Freq > 0, ]; names(t)[3] <- "legami"; t[order(-t$legami), ] } else data.frame(da = character(0), a = character(0), legami = integer(0))
  pac <- if (!is.null(pacchetti)) { t <- as.data.frame(table(pacchetto = pacchetti$pacchetto, file = pacchetti$file), stringsAsFactors = FALSE); t[t$Freq > 0, ] } else NULL
  list(simboli = simboli, chiamate = chiamate_p, riferimenti = riferimenti_p, dipendenze = dip, pacchetti = pac,
       definiti_in_piu_file = names(una_def)[is.na(una_def)])
}

interroga <- function(ind, cmd, arg = NULL) {
  s <- ind$simboli
  stampa <- function(x, ...) if (is.null(x) || !nrow(x)) cat("(nessun risultato)\n") else print(x, row.names = FALSE, right = FALSE, ...)
  switch(cmd,
    riepilogo = {
      cat(sprintf("%d file · %d simboli (%d funzioni) · %d chiamate a funzioni del progetto · %d riferimenti · %d legami fra file\n", length(unique(s$file)), nrow(s), sum(s$tipo == "funzione"), nrow(ind$chiamate), nrow(ind$riferimenti), nrow(ind$dipendenze)))
      cat("\nFile con più funzioni:\n"); t <- sort(table(s$file[s$tipo == "funzione"]), decreasing = TRUE); print(utils::head(t, 8))
      cat("\nFunzioni più chiamate:\n"); t <- sort(table(ind$chiamate$a), decreasing = TRUE); print(utils::head(t, 10))
      cat("\nFile da cui dipendono più file:\n"); t <- sort(table(ind$dipendenze$a), decreasing = TRUE); print(utils::head(t, 8))
      if (length(ind$definiti_in_piu_file)) cat("\nNomi definiti in più file (ambigui, esclusi dai legami):", paste(utils::head(ind$definiti_in_piu_file, 15), collapse = ", "), "\n")
    },
    dove = stampa(s[s$nome == arg, c("id", "nome", "file", "riga", "tipo", "contenitore")]),
    `chi-chiama` = { x <- rbind(cbind(tipo = rep("chiama", nrow(ind$chiamate[ind$chiamate$a == arg, ])), ind$chiamate[ind$chiamate$a == arg, c("da", "file", "riga")]),
                              cbind(tipo = rep("riferisce", nrow(ind$riferimenti[ind$riferimenti$nome == arg, ])), ind$riferimenti[ind$riferimenti$nome == arg, c("da", "file", "riga")]))
                    stampa(x[order(x$file, x$riga), ]) },
    `cosa-chiama` = { x <- ind$chiamate[ind$chiamate$da == arg, ]; if (nrow(x)) { t <- as.data.frame(table(a = x$a), stringsAsFactors = FALSE); t <- t[order(-t$Freq), ]; names(t)[2] <- "volte"; d <- s[match(t$a, s$nome), c("file", "riga")]; stampa(cbind(t, d)) } else cat("(nessun risultato)\n") },
    file = { a <- sub("^R/", "", arg); f <- function(x) sub("^R/", "", x)
             d <- ind$dipendenze; cat("Dipende da:\n"); stampa(d[f(d$da) == a, c("a", "legami")]); cat("\nDipendono da lui:\n"); stampa(d[f(d$a) == a, c("da", "legami")]) },
    `non-usate` = { f <- s[s$tipo == "funzione" & s$contenitore == "(livello del file)", ]
                    usati <- unique(c(ind$chiamate$a, ind$riferimenti$nome))
                    # i nomi dentro l'app (server, handler) non hanno chiamate dichiarate: si elencano a parte
                    x <- f[!f$nome %in% usati, c("nome", "file", "riga")]
                    # i collaudi e gli strumenti (verifiche/, traduzione/, dati/, verifica-dati/) non fanno parte dell'indice: si guarda se li nominano
                    altri <- unlist(lapply(c(list.files("verifiche", "[.]R$", full.names = TRUE), list.files("traduzione", "[.]R$", full.names = TRUE),
                                             list.files("dati", "[.]R$", full.names = TRUE), list.files("verifica-dati", "[.]R$", full.names = TRUE)), function(g) if (!grepl("indice_codice", g)) readLines(g, warn = FALSE, encoding = "UTF-8")))
                    x$nei_collaudi_o_strumenti <- vapply(x$nome, function(n) any(grepl(n, altri, fixed = TRUE)), TRUE)
                    cat(nrow(x), "funzioni di primo livello mai chiamate né riferite in R/ e app.R (sospetti, da guardare a mano); l'ultima colonna dice se i collaudi o gli strumenti le nominano:\n"); stampa(x[order(x$file, x$riga), ]) },
    esporta = { cartella <- if (is.null(arg)) file.path(Sys.getenv("TMPDIR", tempdir()), "trama-indice") else arg; dir.create(cartella, showWarnings = FALSE, recursive = TRUE)
                for (n in c("simboli", "chiamate", "riferimenti", "dipendenze", "pacchetti")) if (!is.null(ind[[n]])) utils::write.csv(ind[[n]], file.path(cartella, paste0(n, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
                cat("scritti in", cartella, ":", paste(list.files(cartella), collapse = ", "), "\n") },
    stop("comando sconosciuto: ", cmd, " (riepilogo, dove, chi-chiama, cosa-chiama, file, non-usate, esporta)"))
  invisible(ind)
}

if (sys.nframe() == 0L && !interactive()) {
  a <- commandArgs(TRUE)
  if (!length(a)) a <- "riepilogo"
  invisible(interroga(indice_codice("."), a[1], if (length(a) > 1L) a[2] else NULL))
}
