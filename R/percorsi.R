# `%||%` è in base R solo dal 4.4; con R più vecchi lo si definisce qui (usato in app.R, crud.R,
# gestione.R, selezione.R). Il progetto è provato con R 4.6.1 (renv.lock).
if (!exists("%||%", envir = baseenv())) `%||%` <- function(x, y) if (is.null(x)) y else x

# ─────────────────────────────────────────────────────────────
# percorsi.R: dove stanno i dati dell'utente
# Il programma (R/, www/, dati/*.csv di sola lettura) e i dati dell'utente stanno in posti diversi: aggiornare il programma non li tocca.
# Trama Lite ha la SUA cartella, distinta da quella di Trama.
#   <cartella_dati>/
#     trama.sqlite                 il database
#     backup/                      giornalieri + «prima del ripristino»
#     luoghi/luoghi.csv            la lista dei luoghi personale, l'unica che si modifica
#     luoghi/backup-luoghi/        una copia a ogni modifica
#     luoghi/base.md5              impronta della base da cui è nata
# Apre una cartella nel Finder, in Esplora file o nel gestore di file; errore chiaro se la cartella non c'è.
apri_cartella <- function(dir, lancia = system2) {
  if (!dir.exists(dir)) stop("La cartella non esiste: ", dir, call. = FALSE)
  dir <- normalizePath(dir)
  sistema <- Sys.info()[["sysname"]]
  if (identical(sistema, "Windows")) lancia("explorer", shQuote(dir), wait = FALSE)
  else if (identical(sistema, "Darwin")) lancia("open", shQuote(dir), wait = FALSE)
  else lancia("xdg-open", shQuote(dir), wait = FALSE)
  invisible(dir)
}

# Un avviso che dice cosa è successo, senza percorsi né nomi di file: «Mostra nella cartella» apre la cartella giusta
# (`input$mostra_cartella`: «backup» o «export», gestito in aiuto_server.R). `testo` può essere una stringa o un tag.
# `esito_ok()` segna un avviso di operazione riuscita: il tema (tema/12-avvisi.css) gli disegna la spunta. Un avviso neutro non la porta.
esito_ok <- function(...) shiny::div(class = "esito-ok", ...)

notifica_con_cartella <- function(testo, cartella = c("backup", "export"), type = "message", duration = 8) {
  cartella <- match.arg(cartella)
  shiny::showNotification(
    shiny::div(class = if (identical(type, "message")) "esito-ok", testo, shiny::div(class = "toast-azione",
      shiny::tags$a(href = "#", class = "toast-mostra", role = "button",
        onclick = sprintf("Shiny.setInputValue('mostra_cartella', {tipo: '%s', n: Date.now()}, {priority: 'event'}); return false;", cartella),
        "Mostra nella cartella"))),
    type = type, duration = duration)
}

# Precedenza: TRAMA_DB (database di prova, senza backup automatico), poi TRAMA_HOME (altra cartella, per i collaudi), poi ~/Documents/Trama Lite.
# Il CSV distribuito col programma (dati/luoghi.csv) è la BASE, in sola lettura: la copia personale nasce da lì.
# ─────────────────────────────────────────────────────────────

NOME_DB         <- "trama.sqlite"
LISTA_LUOGHI_BASE <- "dati/luoghi.csv"

cartella_dati_predefinita <- function() {
  base <- if (.Platform$OS.type == "windows") "~" else "~/Documents"
  normalizePath(file.path(path.expand(base), "Trama Lite"), winslash = "/", mustWork = FALSE)
}
cartella_dati <- function() {
  h <- Sys.getenv("TRAMA_HOME", "")
  if (nzchar(h)) return(normalizePath(h, winslash = "/", mustWork = FALSE))
  cartella_dati_predefinita()
}

cartella_backup     <- function() file.path(cartella_dati(), "backup")
cartella_export     <- function() file.path(cartella_dati(), "export")

# Ogni export scaricato lascia una copia in <cartella dati>/export, così il file c'è anche se il browser non fa partire lo scarico (mai un backup: quelli stanno in backup/). Restituisce il percorso, o NULL.
# Il nome della copia dice fondo e giorno, per non sovrascrivere; un nome che ha già la data resta com'è. Lo stesso fondo due volte nello stesso giorno lascia l'ultimo.
nome_copia_export <- function(nome, pezzo_fondo = NULL, giorno = Sys.Date()) {
  nome <- basename(nome); g <- format(giorno)
  if (grepl(g, nome, fixed = TRUE)) return(nome)
  ext <- tools::file_ext(nome); base <- sub("\\.[^.]*$", "", nome)
  if (!nzchar(ext)) base <- nome
  paste0(paste(c(base, if (!is.null(pezzo_fondo) && nzchar(pezzo_fondo)) pezzo_fondo, g), collapse = "-"), if (nzchar(ext)) paste0(".", ext) else "")
}

salva_copia_export <- function(file, nome, cartella = cartella_export(), pezzo_fondo = NULL) {
  tryCatch({
    dir.create(cartella, showWarnings = FALSE, recursive = TRUE)
    dest <- file.path(cartella, nome_copia_export(nome, pezzo_fondo))
    if (file.copy(file, dest, overwrite = TRUE, copy.mode = FALSE)) dest else NULL
  }, error = function(e) NULL)
}
cartella_luoghi     <- function() file.path(cartella_dati(), "luoghi")

# Il database da aprire: TRAMA_DB (prove) o quello della cartella dati.
percorso_db <- function() {
  e <- Sys.getenv("TRAMA_DB", "")
  if (nzchar(e)) return(e)
  file.path(cartella_dati(), NOME_DB)
}

# La lista dei luoghi da leggere: la copia personale se c'è, altrimenti la base (la copia la crea prepara_cartella_dati()).
percorso_lista_luoghi <- function() {
  p <- file.path(cartella_luoghi(), "luoghi.csv")
  if (file.exists(p)) p else LISTA_LUOGHI_BASE
}

# Crea le cartelle, semina la copia personale della lista e, se la base è cambiata, ci AGGIUNGE le voci nuove senza toccare quelle dell'utente.
prepara_cartella_dati <- function(base = LISTA_LUOGHI_BASE) {
  dir.create(cartella_backup(), recursive = TRUE, showWarnings = FALSE)
  g <- cartella_luoghi()
  dir.create(g, recursive = TRUE, showWarnings = FALSE)
  pers <- file.path(g, "luoghi.csv")
  sha  <- file.path(g, "base.md5")
  if (!file.exists(base)) return(invisible(list(lista = pers, aggiunte = 0L)))
  impronta <- unname(tools::md5sum(base))
  aggiunte <- 0L
  if (!file.exists(pers)) {
    # copy.mode = FALSE e chmod: in una cartella di sola lettura (app in /Applications o Program Files) la copia ereditava il permesso e non si poteva scrivere.
    if (!file.copy(base, pers, copy.mode = FALSE)) stop("Impossibile creare la lista dei luoghi personale in ", g)
    Sys.chmod(pers, "0644")
    writeLines(impronta, sha)
  } else if (!file.exists(sha) || !identical(trimws(readLines(sha, warn = FALSE)[1]), impronta)) {
    mio <- leggi_lista_luoghi(pers)
    base_d <- leggi_lista_luoghi(base)
    nuove <- base_d[!base_d$chiave %in% mio$chiave, , drop = FALSE]
    # Le voci che la copia personale ha già prendono dalla base identificativo e coordinate, se loro non li hanno; un nome sbagliato di prima
    # (dati/luoghi_correzioni.csv: chiave, nome_prima, nome) si corregge solo se è ancora quello di prima, e una voce con nome nuovo vuoto si toglie;
    # un identificativo sbagliato (colonne wikidata_prima, wikidata, coordinate) si sostituisce solo se la voce ha ancora quello di prima.
    mio_prima <- mio
    cor_ <- file.path(dirname(base), "luoghi_correzioni.csv")
    if (file.exists(cor_)) {
      cor <- utils::read.csv2(cor_, colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0))
      for (j in seq_len(nrow(cor))) {
        if (nzchar(cor$nome_prima[j])) {
          i <- which(mio$chiave == cor$chiave[j] & mio$nome == cor$nome_prima[j])
          if (length(i)) { if (nzchar(cor$nome[j])) mio$nome[i] <- cor$nome[j] else mio <- mio[-i, , drop = FALSE] }
        }
        # Un identificativo sbagliato (con le sue coordinate) si cambia solo se la voce ha ancora quello di prima.
        if ("wikidata_prima" %in% names(cor) && nzchar(cor$wikidata_prima[j])) {
          i <- which(mio$chiave == cor$chiave[j] & mio$wikidata == cor$wikidata_prima[j])
          if (length(i)) { mio$wikidata[i] <- cor$wikidata[j]; mio$coordinate[i] <- cor$coordinate[j] }
        }
      }
    }
    # Solo se anche il nome è lo stesso: una voce che l'utente ha scritto con la stessa chiave ma per un altro luogo («Torricella» del suo archivio) non prende i dati di un luogo del Sud.
    ib <- match(mio$chiave, base_d$chiave); bb <- base_d[ib, , drop = FALSE]
    stesso <- !is.na(ib) & chiave_grafia(mio$nome) == chiave_grafia(bb$nome)
    for (c_ in c("wikidata", "coordinate")) { v <- stesso & !nzchar(mio[[c_]]) & nzchar(bb[[c_]]); mio[[c_]][v] <- bb[[c_]][v] }
    cambiata <- !isTRUE(all.equal(mio, mio_prima, check.attributes = FALSE))
    if (nrow(nuove) || cambiata) {
      copie <- file.path(g, "backup-luoghi")
      dir.create(copie, showWarnings = FALSE)
      file.copy(pers, file.path(copie, paste0("luoghi-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-prima-aggiornamento-base.csv")))
      tmp <- tempfile("luoghi-", tmpdir = g, fileext = ".csv")
      .scrivi_lista_luoghi(rbind(mio, nuove), tmp)
      if (!file.rename(tmp, pers)) { unlink(tmp); stop("Impossibile aggiornare la lista dei luoghi personale.") }
      aggiunte <- nrow(nuove)
    }
    writeLines(impronta, sha)
  }
  invisible(list(lista = pers, aggiunte = aggiunte))
}
