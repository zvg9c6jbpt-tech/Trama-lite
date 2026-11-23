# ─────────────────────────────────────────────────────────────
# R/arianna_export.R: dal database al foglio: fascicolo → riga, xlsx, CSV, JSON delle schede
# Dipende da: arianna.R (profili e alias), profili.R (PROFILO_MODELLO), crud.R.
# ─────────────────────────────────────────────────────────────

# Convenzioni solo per export; il database e il backup restano fedeli.
# Particelle che nel foglio vanno in fondo al cognome (le più lunghe prima, «della» prima di «del»); con l'apostrofo il cognome è uno solo.
# «de la Ascia»: la particella è di due parole.
.PARTICELLE_RE <- "(de la|de las|de los|de lo|della|dello|delle|degli|dalla|dalle|del|dei|dal|dai|de|di|da)"
# La particella sta dopo il nome: «de Santis, Antonio» → «Santis, Antonio de»; una qualifica finale resta in coda. L'import legge anche la forma vecchia.
# I titoli che la riconoscono sono del profilo (`sovrani`).
# Formula per papi, re, imperatori e consorti: «Nome (forma italiana; titolo)», senza virgola; non si indicizza. Si riconosce dal titolo fra parentesi.
.formula_sovrano <- function(n) {
  n <- trimws(as.character(n)[1]); if (is.na(n) || .virgola_alta(n) > 0L) return(FALSE)
  m <- regmatches(n, regexec("^[^()]+\\(([^()]*)\\)$", n))[[1]]
  if (length(m) != 2L) return(FALSE)
  titolo <- trimws(utils::tail(strsplit(m[2], ";", fixed = TRUE)[[1]], 1L))
  length(titolo) == 1L && grepl(paste0("^(", paste(.pd_chr(PROFILO_DOMINIO$sovrani$titoli), collapse = "|"), ")([[:space:]]|$)"), titolo, ignore.case = TRUE)
}
# Un nome senza virgola con qualifica o particella in mezzo non dice il cognome: non si indovina. Certo è solo un nome di 2–4 parole di lettere.
.cognome_incerto <- function(n) {
  n <- trimws(n)
  !grepl("^[[:alpha:]'’-]+([[:space:]][[:alpha:]'’-]+){1,3}$", n, perl = TRUE) ||
    grepl("(^|[[:space:]])(alias|detto|detta|di|da|dal|dalla|dai)([[:space:]]|$)", n, perl = TRUE)
}
# Nomi di una scheda: il nome senza virgola non si gira qui, lo decide l'anteprima (concordanza.R, 0f).
.nomi_persone_export <- function(v) vapply(as.character(v), .nome_persona_export, character(1))
.nome_persona_export <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return("")
  n <- trimws(x)
  if (.formula_sovrano(n)) return(n)
  # Il nome senza virgola non si gira mai in uscita (vale anche per «fra Paolo», «suor Maria»): una correzione rifiutata resta tale.
  if (grepl(paste0("^", .PARTICELLE_RE, "[[:space:]]+"), n, ignore.case = TRUE)) {
    i <- .virgola_alta(n)
    if (i > 0L) {
      cogn <- trimws(substr(n, 1, i - 1)); resto <- trimws(substring(n, i + 1))
      # La particella è quella seguita da uno spazio: «de Lama» è «de» + «Lama», non «de la» + «ma».
      particella <- tolower(sub(paste0("^", .PARTICELLE_RE, "[[:space:]]+.*$"), "\\1", cogn, ignore.case = TRUE))
      cogn <- sub(paste0("^", .PARTICELLE_RE, "[[:space:]]+"), "", cogn, ignore.case = TRUE)
      m <- regmatches(resto, regexec("^(.*?)[[:space:]]*(\\([^()]*\\))$", resto))[[1]]
      resto <- if (length(m) == 3L && nzchar(m[2])) paste0(m[2], " ", particella, " ", m[3]) else paste0(resto, " ", particella)
      n <- paste0(cogn, ", ", resto)
    }
  }
  n
}
# Il punto si aggiunge alla presentazione, mai a un regesto vuoto.
.regesto_export <- function(x) {
  if (.vuoto(x)) return("")
  x <- trimws(as.character(x)[1])
  # Un indirizzo in fondo non riceve il punto.
  if (endsWith(x, ".") || grepl("https?://[^[:space:]]+$", x)) x else paste0(x, ".")
}
# Inverso delle due convenzioni, per l'import: «Santis de, Antonio» → «de Santis, Antonio»; «italiano; latino» → due lingue maiuscole.
.nome_persona_import <- function(x) {
  n <- trimws(as.character(x))
  i <- .virgola_alta(n)
  if (i > 1L) {
    cogn <- trimws(substr(n, 1, i - 1)); resto <- trimws(substring(n, i + 1))
    # forma di prima, la particella dopo il cognome: «Santis de, Antonio»
    m <- regmatches(cogn, regexec(paste0("^(.+?)[[:space:]]+", .PARTICELLE_RE, "$"), cogn, ignore.case = TRUE))[[1]]
    if (length(m)) {
      n <- paste0(tolower(m[3]), " ", m[2], ", ", resto)
    } else {
      # Forma della guida: «Santis, Antonio de» (con qualifica: «…, Charles-Louis de (barone…)»).
      m <- regmatches(resto, regexec(paste0("^(.+?)[[:space:]]+", .PARTICELLE_RE, "([[:space:]]*\\([^()]*\\))?$"), resto, ignore.case = TRUE))[[1]]
      if (length(m)) n <- paste0(tolower(m[3]), " ", cogn, ", ", m[2], if (nzchar(m[4])) paste0(" ", trimws(m[4])) else "")
    }
  }
  # Particella del cognome sempre minuscola («di Taibi, Matteo»); con l'apostrofo («De'Bertolati») non si tocca.
  m <- regmatches(n, regexec("^(Di|De|Da|Del|Della|Dello|Delle|Degli|Dei|Dal|Dalla|Dalle|Dai)[[:space:]]+([^[:space:]].*)$", n))[[1]]
  if (length(m)) paste0(tolower(m[2]), " ", m[3]) else n
}
.lingue_import <- function(x) {
  # Il modello separa le lingue con «; » e le scrive minuscole; altri fogli usano «|» (Bologna) o
  # la virgola. Qui tutte si leggono, e nel database restano «Italiano», «Latino».
  x <- trimws(unlist(strsplit(as.character(x), "[;|,]"))); x <- x[!is.na(x) & nzchar(x)]
  note <- VOCABOLARI$lingua
  x <- vapply(x, function(l) { i <- match(tolower(l), tolower(note))
    if (!is.na(i)) note[i] else paste0(toupper(substr(l, 1, 1)), substr(l, 2, nchar(l))) }, character(1), USE.NAMES = FALSE)
  unique(x)
}
.lingue_export <- function(x) {
  x <- tolower(trimws(as.character(x)))
  paste(x[!is.na(x) & nzchar(x)], collapse = "; ")
}
esporta_json_schede <- function(con, percorso, fondo = NULL) {
  jsonlite::write_json(tabella_arianna(con, fondo = fondo), percorso,
                       dataframe = "rows", pretty = TRUE, na = "null")
  invisible(percorso)
}

# --- export: da fascicolo a riga del foglio -----------------------

# Ogni sala di studio ha la sua segnatura («b.1, f.1», «b. 213, fasc. 13»): le colonne sono una sola, il formato segue l'archivio.
FORMATI_SEGNATURA <- list(
  
  ASDNa = function(f) paste0(f$archivio, ", ", f$fondo,
                             ", ", sigla_busta(f$sigla_busta), f$busta, if (.vuoto(f$busta_suffisso)) "" else f$busta_suffisso, ", ", if (.vuoto(f$sigla_fascicolo)) "f." else sigla_fascicolo(f$sigla_fascicolo), f$fascicolo,
                             if (.vuoto(f$fascicolo_suffisso)) "" else f$fascicolo_suffisso),
  
  ASMo  = function(f) componi_segnatura(f$archivio, f$fondo, f$serie,
                                        f$busta, f$fascicolo, f$fascicolo_suffisso, f$sigla_busta, f$sigla_fascicolo, f$busta_suffisso)
)

.formato_segnatura <- function(f) {
  k <- unname(PROFILO_PER_ARCHIVIO[f$archivio])
  # Un archivio che non conosciamo scrive la segnatura completa, serie compresa.
  if (length(k) && !is.na(k) && !is.null(FORMATI_SEGNATURA[[k]])) FORMATI_SEGNATURA[[k]] else FORMATI_SEGNATURA$ASMo
}


TIPOLOGIA_ARIANNA <- "Atti"
# Le tipologie del foglio secondo la guida al Tab (14/10); più d'una si separano con «; ».
TIPOLOGIE_FOGLIO <- c("Atti", "Lettere", "Decreti")

# Colonna «Tipologia documentaria»: «Atti», salvo altro nel campo apposito (EXTRA_TIPOLOGIA_FOGLIO); mai il tipo del titolo.
tipologia_foglio <- function(f) {
  v <- trimws(as.character(unlist(f$extra[[EXTRA_TIPOLOGIA_FOGLIO]])))
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) paste(v, collapse = "; ") else TIPOLOGIA_ARIANNA
}

# Riga del foglio per un fascicolo, sempre nel modello; `profilo` e `profilo_nome` si ignorano.
riga_da_fascicolo <- function(f, profilo = PROFILO_MODELLO, profilo_nome = MODELLO) {
  
  profilo <- PROFILO_MODELLO
  unisci <- function(v) if (.vuoto(v)) "" else paste(as.character(v), collapse = "|")
  solo   <- function(v) if (.vuoto(v)) "" else as.character(v)[1]
  formato <- .formato_segnatura(f)
  
  out <- setNames(vector("list", nrow(profilo)), profilo$arianna)
  # Le colonne del profilo come vettori, non `profilo[i, ]` a ogni cella: su 5.254 schede × 28 colonne
  # la riga di data.frame costava più di metà dell'export (13/10: 7 s → vedi verifiche/prestazioni.R).
  dove <- profilo$dove; campo <- profilo$campo; nome_col <- profilo$arianna
  
  for (i in seq_len(nrow(profilo))) {
    r <- list(dove = dove[i], campo = campo[i], arianna = nome_col[i])
    
    out[[i]] <-
      if      (r$dove == "derivato")  formato(f)
    else if (r$dove == "extra") {
      # Una colonna extra del modello può essere nel database col nome di un
      # vecchio formato (importata prima del 2/10): si cerca anche lì.
      valore <- NULL
      for (n in .extra_sinonimi(r$arianna)) if (.vuoto(valore)) valore <- f$extra[[n]]
      unisci(valore)
    }
    else if (identical(r$dove, "persone")) unisci(.nomi_persone_export(f$persone))
    else if (identical(r$dove, "lingue")) .lingue_export(f$lingue)
    else if (identical(r$campo, "regesto")) .regesto_export(f$regesto)
    else if (identical(r$campo, "note")) .regesto_export(note_per_export(f))
    # «Tipologia documentaria» dice «Atti» salvo modifica a mano; il campo del database regge il tipo del titolo, che nel foglio non esce.
    else if (identical(r$campo, "tipologia_documentaria")) tipologia_foglio(f)
    # Data assente per dichiarazione: nel foglio si scrive «n.d.» (e si rilegge come tale).
    else if (identical(r$campo, "estremo_remoto") && .vuoto(f$estremo_remoto) &&
             isTRUE(suppressWarnings(as.integer(f$data_nd)) == 1L)) "n.d."
    # Giorno e mese escono per intero se la data intera ha lo stesso anno del campo; altrimenti l'anno.
    else if (r$campo %in% names(EXTRA_DATA_PRECISA) && !.vuoto(f[[r$campo]]) && length(unlist(f$extra[[EXTRA_DATA_PRECISA[[r$campo]]]])) &&
             !is.null(dp_ <- .data_precisa(unlist(f$extra[[EXTRA_DATA_PRECISA[[r$campo]]]])[1])) && identical(dp_$anno, suppressWarnings(as.integer(f[[r$campo]]))))
      dp_$canonica
    else if (r$campo %in% names(EXTRA_DATA_NON_RISOLTA) && .vuoto(f[[r$campo]]) && length(unlist(f$extra[[EXTRA_DATA_NON_RISOLTA[[r$campo]]]])))
      as.character(unlist(f$extra[[EXTRA_DATA_NON_RISOLTA[[r$campo]]]])[1])      # la data non valida esce com'era scritta
    else if (r$dove == "fascicoli") solo(f[[r$campo]])
    else                            unisci(f[[r$dove]])
  }
  
  out
}


# --- export: il foglio intero -------------------------------------

# Costruisce la tabella del modello (non scrive): intestazioni del modello nel suo ordine; `profilo_nome` e `intestazioni` si ignorano.
# Id in ordine di segnatura: busta, suffisso busta, fascicolo, suffisso fascicolo (come SQLite, i NULL prima).
.ids_in_ordine_di_segnatura <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  q <- "SELECT id, busta, busta_suffisso, fascicolo, fascicolo_suffisso FROM fascicoli"
  d <- if (is.null(fondo)) dbGetQuery(con, q) else dbGetQuery(con, paste(q, "WHERE fondo_id = ?"), list(fondo))
  if (!nrow(d)) return(character(0))
  sb <- ifelse(is.na(d$busta_suffisso), "", d$busta_suffisso); sf <- ifelse(is.na(d$fascicolo_suffisso), "", d$fascicolo_suffisso)
  d$id[order(d$busta, .rango_suffisso(sb), sb, d$fascicolo, .rango_suffisso(sf), sf, na.last = FALSE)]
}

tabella_arianna <- function(con, profilo_nome = MODELLO, intestazioni = NULL, fondo = NULL) {

  p <- PROFILO_MODELLO

  # Un export è di UN fondo solo (2/10): in un database con più fondi, senza
  # dire quale, non esce niente invece di mescolarli.
  if (is.null(fondo) && dbGetQuery(con, "SELECT COUNT(DISTINCT fondo_id) n FROM fascicoli")$n > 1)
    stop("Il database ha più fondi: l'export è sempre di un fondo solo, indica quale.", call. = FALSE)

  ids <- .ids_in_ordine_di_segnatura(con, fondo)

  if (length(ids) == 0) stop("Nessun fascicolo da esportare.")

  righe <- lapply(leggi_fascicoli(con, ids), riga_da_fascicolo)       # una lettura per tabella, non dieci per scheda (crud.R)

  d <- as.data.frame(do.call(rbind, lapply(righe, unlist)),
                     stringsAsFactors = FALSE)
  d <- d[, match(p$arianna, names(d)), drop = FALSE]
  names(d) <- p$arianna
  attr(d, "ids") <- ids       # riga per riga, per chi deve risalire alla scheda (R/anomalie.R)
  d
}


esporta_arianna <- function(con, percorso, profilo_nome = MODELLO,
                            intestazioni = NULL, fondo = NULL) {
  d <- tabella_arianna(con, profilo_nome, intestazioni, fondo)
  scrivi_xlsx_modello(d, percorso, nome_foglio = .nome_foglio(con, fondo),
                      proprieta = proprieta_xlsx(con, if (.vuoto(fondo)) NULL else nome_fondo(con, fondo), if (.vuoto(fondo)) NULL else fonte_del_fondo(con, fondo)))
  invisible(list(righe = nrow(d), colonne = ncol(d), percorso = percorso))
}

# Il nome del foglio: città dell'archivio e fondo, al massimo 31 caratteri e
# senza i segni che Excel non accetta nel nome di un foglio.
.nome_foglio <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  r <- dbGetQuery(con, if (is.null(fondo)) "SELECT archivio, fondo FROM fascicoli LIMIT 1"
                       else "SELECT archivio, fondo FROM fascicoli WHERE fondo_id = ? LIMIT 1",
                  params = if (is.null(fondo)) NULL else list(fondo))
  if (!nrow(r)) return("Fascicoli")
  citta <- trimws(sub("^.*\\b(di|del|della)\\s+", "", r$archivio[1]))
  nome  <- if (is.null(fondo)) citta else paste(citta, "-", r$fondo[1])
  nome  <- gsub("[\\[\\]:*?/\\\\]", "", nome, perl = TRUE)
  if (!nzchar(trimws(nome))) "Fascicoli" else substr(nome, 1, 31)
}

# Larghezza di colonna in unità Excel: la cella più lunga (prima riga), intestazione compresa, fra 8 e 70; una colonna vuota si restringe al minimo.
LARGHEZZA_COLONNA_VUOTA <- 2.5
.larghezza_colonna <- function(intestazione, valori, minimo = 8, massimo = 70) {
  v <- as.character(valori); v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(LARGHEZZA_COLONNA_VUOTA - 0.71)
  lung <- max(nchar(sub("\n.*$", "", v), type = "width"))
  max(minimo, min(massimo, max(lung, nchar(intestazione, type = "width")) * 1.1 + 2)) - 0.71   # openxlsx somma 0,71
}

# Righe che una cella occupa in una colonna larga `larghezza`: gli a capo scritti più quelli del testo (stima prudente, 95%).
.righe_a_capo <- function(valori, larghezza) {
  v <- as.character(valori); out <- rep(1L, length(v)); i <- which(!is.na(v) & nzchar(v))
  if (!length(i)) return(out)
  capienza <- max(1, floor(larghezza * 0.95))
  out[i] <- vapply(strsplit(v[i], "\n", fixed = TRUE),
                   function(p) if (!length(p)) 1L else as.integer(sum(pmax(1L, ceiling(nchar(p, type = "width") / capienza)))), 1L)
  out
}

# Altezza di ogni riga: quella del modello, o quanto serve alla cella che va più a capo (max 409 punti).
.altezze_righe <- function(d, larghezze, minima, per_riga = 15) {
  if (!nrow(d)) return(numeric(0))
  righe <- rep(1L, nrow(d))
  for (j in seq_along(d)) righe <- pmax(righe, .righe_a_capo(d[[j]], larghezze[j] + 0.71))
  pmin(409, ifelse(righe == 1L, minima, righe * per_riga))
}

# Aspetto del modello (dati/layout_export.json): intestazione, larghezze, altezza di riga. Indipendente dai dati.
scrivi_xlsx_modello <- function(d, percorso, nome_foglio = "Fascicoli", proprieta = NULL) {
  modello <- jsonlite::fromJSON("dati/layout_export.json", simplifyVector = FALSE)
  wb <- if (is.null(proprieta)) openxlsx::createWorkbook()
        else openxlsx::createWorkbook(creator = proprieta$creator, title = proprieta$title,
                                      subject = proprieta$subject, category = proprieta$category)
  openxlsx::addWorksheet(wb, nome_foglio, gridLines = TRUE)
  openxlsx::modifyBaseFont(wb, fontSize = 11, fontName = "Calibri", fontColour = "#000000")
  valori <- d
  for (j in seq_along(valori)) valori[[j]][which(valori[[j]] == "")] <- NA
  openxlsx::writeData(wb, 1, valori, keepNA = FALSE)
  h <- modello$intestazioni
  larghezze <- numeric(length(d))
  for (j in seq_along(d)) {
    nome <- names(d)[j]
    k <- match(.chiave(nome), .chiave(names(modello$colonne)))
    if (is.na(k)) stop("Colonna senza stile di export: ", nome)
    # Larghezza adattata al contenuto (riga più lunga, intestazione compresa, fra un minimo e un massimo); il resto dell'aspetto è del modello.
    larghezze[j] <- .larghezza_colonna(nome, d[[j]])
    openxlsx::setColWidths(wb, 1, j, widths = larghezze[j])
    intestazione <- openxlsx::createStyle(fontName = "Calibri", fontSize = h$corpo,
      fontColour = "#000000", textDecoration = if (isTRUE(h$grassetto)) "bold" else NULL,
      fgFill = paste0("#", h$colore), border = "TopBottomLeftRight", borderStyle = h$bordo,
      borderColour = "#000000", halign = "left", valign = "top",
      # Nel modello anche le intestazioni degli estremi sono in formato testo.
      numFmt = if (nome %in% c("Estremo remoto", "Estremo recente")) "@" else "GENERAL")
    corpo <- openxlsx::createStyle(fontName = "Calibri", fontSize = 11,
      fontColour = "#000000", halign = "left", valign = "top",
      wrapText = TRUE,       # il testo lungo va a capo nella cella, e l'altezza della riga lo segue (21/10/2026)
      numFmt = if (nome %in% c("Estremo remoto", "Estremo recente")) "@" else "GENERAL")
    openxlsx::addStyle(wb, 1, intestazione, rows = 1, cols = j)
    openxlsx::addStyle(wb, 1, corpo, rows = seq_len(nrow(d)) + 1L, cols = j, gridExpand = TRUE)
  }
  n <- nrow(d) + 1L
  openxlsx::setRowHeights(wb, 1, rows = seq_len(n),
    heights = c(modello$altezza_intestazione, .altezze_righe(d, larghezze, modello$altezza_predefinita)))
  openxlsx::freezePane(wb, 1, firstRow = TRUE)
  openxlsx::pageSetup(wb, 1, orientation = "portrait", paperSize = 9,
    left = 0.7, right = 0.7, top = 0.75, bottom = 0.75, header = 0, footer = 0)
  openxlsx::saveWorkbook(wb, percorso, overwrite = TRUE)
  .togli_relazioni_pendenti(percorso)
  invisible(percorso)
}


# CSV per Excel italiano: separatore ';' e BOM, altrimenti Excel
# apre tutto in una colonna sola e mangia gli accenti.
esporta_csv <- function(con, percorso, profilo_nome = MODELLO,
                        intestazioni = NULL, fondo = NULL) {

  d <- tabella_arianna(con, profilo_nome, intestazioni, fondo)

  conn <- file(percorso, open = "wb")
  on.exit(close(conn), add = TRUE)
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), conn)          # BOM UTF-8
  writeLines(
    c(paste(sprintf('"%s"', gsub('"', '""', names(d))), collapse = ";"),
      apply(d, 1, function(r)
        paste(sprintf('"%s"', gsub('"', '""', ifelse(is.na(r), "", r))), collapse = ";"))),
    conn, useBytes = TRUE)

  invisible(list(righe = nrow(d), colonne = ncol(d), percorso = percorso))
}

# Nome dei soli export delle schede: sigla archivio e ultima busta del fondo.
nome_file_schede <- function(con, fondo, estensione) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  stopifnot(estensione %in% c("xlsx", "csv", "json"))
  d <- DBI::dbGetQuery(con,
    "SELECT DISTINCT archivio FROM fascicoli WHERE fondo_id = ?", list(fondo))
  if (nrow(d) != 1L) stop("Scegli un fondo appartenente a un solo archivio.")
  archivio <- d$archivio[1]
  sigla <- unname(PROFILO_PER_ARCHIVIO[archivio])
  if (!length(sigla) || is.na(sigla)) {
    # Per gli altri archivi: conserva una sigla già breve, altrimenti iniziali.
    parole <- strsplit(trimws(archivio), "[[:space:]]+")[[1]]
    parole <- parole[!tolower(parole) %in% c("di", "del", "della", "dello", "dei", "delle", "degli")]
    sigla <- if (length(parole) == 1L) parole else paste(substr(parole, 1, 1), collapse = "")
  }
  sigla <- gsub("[^a-z0-9]", "", tolower(sigla))
  if (!nzchar(sigla)) stop("Impossibile ricavare la sigla dell'archivio.")
  ultima <- DBI::dbGetQuery(con,
    "SELECT MAX(busta) AS ultima FROM fascicoli WHERE fondo_id = ?", list(fondo))$ultima[1]
  paste0(sigla, if (estensione == "xlsx" && !is.na(ultima)) " " else "",
         if (is.na(ultima)) "" else as.character(ultima), ".", estensione)
}



# openxlsx può lasciare, in ogni foglio, relazioni verso disegni che nel file non ci sono (anche senza immagini): i lettori OpenXML rigorosi
# (openpyxl, pandas) rifiutano il file. Si tolgono da tutti i fogli e si riscrive il file.
.togli_relazioni_pendenti <- function(percorso) {
  cartella <- tempfile(); dir.create(cartella)
  on.exit(unlink(cartella, recursive = TRUE), add = TRUE)
  utils::unzip(percorso, exdir = cartella)
  rels <- list.files(file.path(cartella, "xl/worksheets/_rels"), pattern = "[.]rels$", full.names = TRUE)
  cambiato <- FALSE
  for (rel in rels) {
    testo <- paste(readLines(rel, warn = FALSE, encoding = "UTF-8"), collapse = "")
    nuovo <- gsub('<Relationship [^>]*Type="[^"]*/(drawing|vmlDrawing)"[^>]*/>', "", testo)
    if (!identical(nuovo, testo)) { writeLines(nuovo, rel, useBytes = TRUE); cambiato <- TRUE }
  }
  if (cambiato) zip::zipr(normalizePath(percorso), list.files(cartella, all.files = TRUE, no.. = TRUE), root = cartella, include_directories = FALSE)
  invisible(percorso)
}


# Un export non si consegna se non si rilegge uguale (19/11/2026): scritto in un file temporaneo, riletto, e confrontato con il database
# (stesse schede, stesse 28 colonne del modello). Errore con il motivo in italiano; mai un file «riuscito» a metà.
verifica_export <- function(con, percorso, fondo) {
  attese <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(fondo))$n
  r <- tryCatch(leggi_tabella(percorso), error = function(e) e)
  if (inherits(r, "error")) stop("il file scritto non si rilegge: ", conditionMessage(r), call. = FALSE)
  modello <- PROFILO_MODELLO$arianna
  if (!identical(names(r), modello)) stop("le colonne del file non sono quelle del modello (", ncol(r), " invece di ", length(modello), ")", call. = FALSE)
  if (nrow(r) != attese) stop("il file ha ", nrow(r), " righe, il database ", attese, " schede", call. = FALSE)
  invisible(TRUE)
}
