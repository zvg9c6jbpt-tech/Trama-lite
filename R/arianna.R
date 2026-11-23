# ─────────────────────────────────────────────────────────────
# arianna.R: lettura e riconoscimento del foglio (schema Arianna) e correzioni dell'import
# Dipende da: profili.R, compose.R (analizza_segnatura, componi_segnatura…). Il resto è in arianna_import.R, arianna_export.R e arianna_pulisci.R.
# Il riconoscimento delle colonne è tollerante agli spazi; la riscrittura è sempre nel MODELLO (PROFILO_MODELLO).
# ─────────────────────────────────────────────────────────────

# Il profilo del materiale (profili/<nome>/profilo.json): se questo file è letto da solo, lo si carica prima.
if (!exists("PROFILO_DOMINIO", inherits = FALSE)) source("R/profilo_dominio.R")

library(readxl)


# PROFILI (i profili noti, per nome) sta in profili.R: Shiny carica
# R/ in ordine alfabetico, e qui PROFILO_ASDNA non esisterebbe ancora.


# Chiave di confronto (senza maiuscole, accenti, punteggiatura): serve a riconoscere, mai a scrivere.
.chiave <- function(x) {
  x <- .senza_accenti(tolower(trimws(as.character(x))))   # .senza_accenti: qualita.R
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}


# Sinonimi visti nei fogli reali: chiave = nome del profilo, valori = grafie accettate, per tutti i profili.
ALIAS_COLONNE <- list(
  # ── le colonne del modello (2/10) e i nomi con cui sono arrivate prima ──
  "Regesto o descrizione sintetica"                      = c("Regesto o descrizione sintetica del documento", "Regesto", "Regesto o descrizione"),
  "Persone citate o indicizzate"                         = c("Persone citate", "Persone citate o indicizzati", "Persone indicizzate"),
  "Luoghi citati o indicizzati"                          = c("Luoghi citati", "Luoghi citati o indicizzate", "Luoghi indicizzati"),
  "Luogo di produzione (data topica)"                    = c("Luogo di produzione (data topica ove presente)", "Data topica", "Luogo di produzione"),
  "Soggetti citati o indicizzati"                        = c("Altro soggetto (citato o indicizzato)", "Altro soggetto", "Organizzazioni citate"),
  "consistenza.quantità"                                 = c("Consistenza (quantità)", "Consistenza quantità", "Consistenza"),
  "consistenza.unitàMisura"                              = c("Consistenza (unità di misura)", "Consistenza unità di misura", "Unità di misura"),
  "Manifest IIIF"                                        = c("Manifest", "IIIF"),
  # Bologna (5/10): «INDICAZIONE IMMAGINI jpeg tiff» è la colonna delle immagini, e prima non si leggeva.
  "Identificativo immagine o cartella"                   = c("Identificativo immagine", "Immagine", "Cartella immagini", "Indicazione immagini jpeg tiff", "Indicazione immagini"),
  # ── i vecchi profili (ASDNa, ASMo), che si leggono ancora ──
  "Regesto o descrizione sintetica "                     = c("Regesto o descrizione sintetica del documento", "Regesto", "Regesto o descrizione"),
  "Regesto o descrizione sintetica del documento"        = c("Regesto o descrizione sintetica", "Regesto"),
  "Luogo di produzione (data topica ove presente)"       = c("Luogo di produzione (data topica)", "Data topica", "Luogo di produzione"),
  "Data topica"                                          = c("Luogo di produzione (data topica ove presente)", "Luogo di produzione (data topica)", "Luogo di produzione"),
  "definizione.tipologia"                                = c("Supporto (tipologia)", "Definizione", "Definizione/tipologia"),
  "Supporto (tipologia)"                                 = c("definizione.tipologia", "Definizione"),
  "Supporto (materiale)"                                 = c("Supporto", "Materiale"),
  "Altra segnatura"                                      = c("Vecchia segnatura", "Segnatura precedente"),
  "Vecchia segnatura"                                    = c("Altra segnatura", "Segnatura precedente"),
  "Soggetto conservatore"                                = c("Soggetto conservatore (ente)"),
  "Soggetto conservatore (ente)"                         = c("Soggetto conservatore"),
  "Soggetto produttore"                                  = c("Autore (organizzazione)", "Ente produttore"),
  "Autore (organizzazione)"                              = c("Soggetto produttore", "Ente produttore"),
  "dimensioni/altezza"                                   = c("Altezza"),
  "Altezza"                                              = c("dimensioni/altezza"),
  "dimensioni/larghezza"                                 = c("Larghezza"),
  "Larghezza"                                            = c("dimensioni/larghezza"),
  "Note libere"                                          = c("Note")
)


# ── riconoscimento e lettura ──────────────────────────────────────

# Dove sta ogni colonna del profilo nel foglio: per nome, poi per sinonimo (`esatto` li distingue: un sinonimo va detto).
posizioni_colonne <- function(p, reali) {
  kr    <- .chiave(reali)
  nomi  <- p$arianna
  pos   <- rep(NA_integer_, length(nomi))
  esatto <- rep(FALSE, length(nomi))
  for (i in seq_along(nomi)) {
    if (is.na(nomi[i])) next
    j <- match(.chiave(nomi[i]), kr)
    if (!is.na(j)) { pos[i] <- j; esatto[i] <- TRUE; next }
    for (al in ALIAS_COLONNE[[nomi[i]]]) {
      j <- match(.chiave(al), kr)
      if (!is.na(j)) { pos[i] <- j; break }
    }
  }
  # Una colonna del foglio non può servire due volte: tiene la prima.
  doppie <- which(duplicated(pos) & !is.na(pos))
  pos[doppie] <- NA_integer_
  list(pos = pos, esatto = esatto)
}


# Il profilo che spiega meglio il foglio: contano i nomi, i sinonimi solo a parità.
riconosci_profilo <- function(intestazioni) {

  punteggio <- function(p) {
    x <- posizioni_colonne(p, intestazioni)
    attesi <- !is.na(p$arianna)
    c(sum(x$esatto[attesi]) / sum(attesi),
      sum(!is.na(x$pos[attesi])) / sum(attesi))
  }

  m <- vapply(PROFILI, punteggio, numeric(2))
  if (max(m[2, ]) < 0.8) return(NA_character_)
  ammessi <- which(m[2, ] >= 0.8)
  names(PROFILI)[ammessi[which.max(m[1, ammessi] + m[2, ammessi] / 1000)]]
}


# Quanto ogni profilo spiega il foglio e che cosa gli manca: si dice a chi importa, non si rifiuta in silenzio.
diagnosi_profilo <- function(intestazioni) {
  do.call(rbind, lapply(names(PROFILI), function(nome) {
    p      <- PROFILI[[nome]]
    x      <- posizioni_colonne(p, intestazioni)
    attesi <- !is.na(p$arianna)
    trovate <- !is.na(x$pos) & attesi
    data.frame(profilo  = nome,
               attese   = sum(attesi),
               trovate  = sum(trovate),
               quota    = sum(trovate) / sum(attesi),
               mancanti = paste(p$arianna[attesi & !trovate], collapse = " | "),
               stringsAsFactors = FALSE)
  }))
}


# Messaggio di rifiuto per chi ha il foglio: classifica dei profili, colonne mancanti, intestazioni ignote, causa probabile.
# Righe separate da \n (l'anteprima le manda a capo).
.elenca <- function(x, quanti = 8) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return("")
  t <- paste0("«", utils::head(x, quanti), "»", collapse = " · ")
  if (length(x) > quanti) t <- paste0(t, " · e altre ", length(x) - quanti)
  t
}

.perche_profilo_ignoto <- function(intestazioni) {
  d <- diagnosi_profilo(intestazioni)
  d <- d[order(-d$quota), , drop = FALSE]
  vicino <- d[1, ]

  # Le intestazioni che nessun profilo sa collocare, nemmeno per sinonimo.
  note <- unlist(lapply(names(PROFILI), function(nome) {
    x <- posizioni_colonne(PROFILI[[nome]], intestazioni)
    intestazioni[stats::na.omit(x$pos)]
  }))
  ignote <- setdiff(intestazioni[nzchar(trimws(intestazioni))], note)

  righe <- c(
    "Profilo non riconosciuto: nessuno dei profili noti spiega abbastanza le intestazioni di questo foglio (ne serve almeno l'80%).",
    "",
    "Quanto ci va vicino ciascuno:",
    sprintf("   %-6s %d colonne su %d (%d%%)", d$profilo, d$trovate, d$attese,
            round(d$quota * 100)))

  if (vicino$quota >= 0.4 && nzchar(vicino$mancanti))
    righe <- c(righe, "",
      sprintf("Al profilo %s mancano: %s", vicino$profilo,
              .elenca(strsplit(vicino$mancanti, " \\| ")[[1]])))

  if (length(ignote))
    righe <- c(righe, "",
      if (length(ignote) == 1) sprintf("Nel foglio c'è 1 intestazione che nessun profilo conosce: %s", .elenca(ignote))
      else sprintf("Nel foglio ci sono %d intestazioni che nessun profilo conosce: %s", length(ignote), .elenca(ignote)))

  if (vicino$quota < 0.4) {
    # Intestazioni quasi tutte presenti ma tutte sconosciute: non è una riga di titolo di troppo, è un foglio con colonne sue.
    proprie <- length(ignote) >= 3 && mean(nzchar(trimws(intestazioni))) >= 0.5
    prima_riga <- "Le intestazioni devono stare nella PRIMA riga del foglio: se sopra c'è un titolo, una riga vuota o due righe di intestazione, togli quelle di troppo e riprova."
    righe <- c(righe, "",
      if (proprie)
        c("Se le intestazioni elencate qui sopra sono quelle giuste, il foglio ha colonne sue, che non sono quelle del modello né di un vecchio formato: Trama Lite non sa a quali colonne del modello corrispondano. Va prima portato nel modello a 28 colonne, copiando i dati sotto le intestazioni giuste (un foglio esportato da Trama Lite, «Foglio .xlsx», ha già le intestazioni del modello), e poi importato.",
          "", paste("Se invece quelle non sono le intestazioni:", prima_riga))
      else
        paste("Succede quasi sempre per un motivo solo:", prima_riga))
  }

  paste(righe, collapse = "\n")
}


# Un foglio, due contenitori. In entrambi i casi tutto resta TESTO e
# le intestazioni restano intatte, spazio finale compreso.
leggi_tabella <- function(percorso, foglio = NULL) {

  ext <- tolower(tools::file_ext(percorso))

  # Un .xlsx che non si apre è di solito rinominato a mano o scaricato a metà: l'errore di R non aiuta, qui si spiega.
  if (ext %in% c("xlsx", "xls")) {
    d <- try(if (is.null(foglio)) read_excel(percorso, col_types = "text", .name_repair = "minimal")
             else read_excel(percorso, sheet = foglio, col_types = "text", .name_repair = "minimal"),
             silent = TRUE)
    if (inherits(d, "try-error"))
      stop("Questo file non si apre come foglio Excel. Di solito succede quando ",
           "l'estensione \u00e8 stata cambiata a mano, o quando il file si \u00e8 scaricato a ",
           "met\u00e0: riaprilo in Excel e salvalo di nuovo come .xlsx, oppure esportalo ",
           "in CSV (separatore «;», UTF-8) e importa quello.\n\n",
           "Errore tecnico: ", conditionMessage(attr(d, "condition")), call. = FALSE)
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    # Google Sheets scrive gli interi con «.0» («1555.0»): nelle colonne numeriche per natura diventa «1555»; «1600.9» resta (errore).
    for (c in which(trimws(names(d)) %in% c("Estremo remoto", "Estremo recente", "consistenza.quantit\u00e0"))) {
      i <- !is.na(d[[c]]) & grepl("^[0-9]+[.]0+$", trimws(d[[c]]))
      d[[c]][i] <- sub("[.]0+$", "", trimws(d[[c]][i]))
    }
    return(d)
  }

  if (ext %in% c("csv", "txt")) {
    # read.csv2 toglie il «\r» dentro i campi fra virgolette: se il file ne ha, si proteggono prima e si rimettono dopo.
    SEGNO_CR <- "\u0001CR\u0001"
    # Si decodifica sempre il file intero prima di leggerlo: UTF-8 com'è; se non lo è, Windows-1252 (il «CSV» di Excel su Windows).
    # Un byte non valido non deve fermare la lettura a metà senza dirlo (read.csv2 con fileEncoding si fermava e restituiva le righe fin lì).
    codifica <- "UTF-8"
    testo <- try({
      r <- readBin(percorso, "raw", file.size(percorso))
      if (length(r) >= 3L && identical(r[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) r <- r[-(1:3)]
      x <- rawToChar(r); Encoding(x) <- "UTF-8"
      if (!validUTF8(x)) {
        y <- iconv(rawToChar(r), from = "CP1252", to = "UTF-8")
        if (is.na(y)) stop("byte non convertibili da Windows-1252")
        # Un file con righe in UTF-8 e righe in Windows-1252 (due fogli incollati) darebbe «NiccolÃ²»: meglio rifiutarlo.
        if (grepl("Ã[ -¿]|â€", y)) stop("il file mescola righe UTF-8 e righe Windows-1252")
        x <- y; codifica <- "Windows-1252"
      }
      sub("^\ufeff", "", x)
    }, silent = TRUE)
    if (inherits(testo, "try-error"))
      stop("Questo CSV non si legge: non \u00e8 né UTF-8 né Windows-1252. ",
           "In Excel, «Salva con nome \u203a CSV UTF-8 (delimitato da separatore di elenco)».\n\n",
           "Errore tecnico: ", conditionMessage(attr(testo, "condition")), call. = FALSE)
    if (!is.null(testo)) {
      # Un «\r» è dentro un campo se prima di lui c'è un numero dispari di virgolette («""» conta due).
      cr <- as.integer(gregexpr("\r", testo, fixed = TRUE)[[1]])
      vg <- as.integer(gregexpr('"', testo, fixed = TRUE)[[1]])
      dentro <- cr[if (length(vg) && vg[1] > 0) findInterval(cr, vg) %% 2L == 1L else FALSE]
      if (length(dentro)) {
        car <- strsplit(testo, "")[[1]]
        car[dentro] <- SEGNO_CR
        testo <- paste(car, collapse = "")
      }
    }
    avvisi <- character(0)
    d <- try(withCallingHandlers({
               x <- utils::read.csv2(text = testo, colClasses = "character", check.names = FALSE,
                                     na.strings = character(0))
               x[] <- lapply(x, function(col) gsub(SEGNO_CR, "\r", col, fixed = TRUE))
               names(x) <- gsub(SEGNO_CR, "\r", names(x), fixed = TRUE)
               x
             }, warning = function(w) { avvisi <<- c(avvisi, conditionMessage(w)); invokeRestart("muffleWarning") }),
             silent = TRUE)
    # «EOF within quoted string» e simili: R ha letto solo una parte del file. Meglio rifiutarlo che importare righe a metà.
    if (!inherits(d, "try-error") && any(grepl("quoted string|EOF|embedded nul|line \\d+ appears to contain", avvisi)))
      stop("Questo CSV non si legge per intero (R si \u00e8 fermato a met\u00e0: ", paste(unique(avvisi), collapse = "; "),
           "). Di solito una virgoletta aperta e mai chiusa.", call. = FALSE)
    if (inherits(d, "try-error"))
      stop("Questo CSV non si legge. Servono il separatore «;» e la codifica UTF-8: ",
           "in Excel, «Salva con nome \u203a CSV UTF-8 (delimitato da separatore di elenco)». ",
           "\u00c8 lo stesso formato che esce da «Scarica foglio Arianna (.csv)».\n\n",
           "Errore tecnico: ", conditionMessage(attr(d, "condition")), call. = FALSE)
    d <- as.data.frame(d, stringsAsFactors = FALSE); attr(d, "codifica") <- codifica
    return(d)
  }

  stop("Formato non gestito: .", ext, " \u2014 si importano fogli .xlsx e .csv. ",
       "Se il file arriva da un altro programma, esportalo in uno dei due.", call. = FALSE)
}


# Profilo con cui si legge un foglio: al vecchio profilo si aggiungono le colonne extra del modello che non ha, così i dati non si perdono.
profilo_di_lettura <- function(nome) {
  p <- PROFILI[[nome]]
  if (identical(nome, MODELLO)) return(p)
  gia <- .chiave(vapply(p$arianna[!is.na(p$arianna) & p$dove == "extra"], .nome_extra, ""))
  mx  <- PROFILO_MODELLO[PROFILO_MODELLO$dove == "extra" & !.chiave(PROFILO_MODELLO$arianna) %in% gia, , drop = FALSE]
  rbind(p, mx)
}

# Strato L1, il carattere (D1, D2): ogni cella si legge con la stessa codifica.
#   - NFC: «à» è sempre il carattere unico;
#   - lo spazio non separabile diventa spazio; i caratteri invisibili (U+200B, U+200C, U+200D, U+2060, U+FEFF, U+00AD) cadono;
#   - l'apostrofo tipografico (U+2019, U+02BC) diventa «'». Le celle toccate si contano (`a$carattere`).
.nfc <- function(x) {
  if (requireNamespace("stringi", quietly = TRUE)) return(stringi::stri_trans_nfc(x))
  base <- c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U", "c", "C", "n", "N")
  for (m in list(c("\u0300", "àèìòùÀÈÌÒÙ"), c("\u0301", "áéíóúÁÉÍÓÚ"), c("\u0302", "âêîôûÂÊÎÔÛ"), c("\u0308", "äëïöüÄËÏÖÜ"),
                 c("\u0327", "çÇ"), c("\u0303", "ñÑ"))) {
    com <- m[1]; pre <- strsplit(m[2], "")[[1]]
    b <- if (m[1] == "\u0327") c("c", "C") else if (m[1] == "\u0303") c("n", "N") else base[1:length(pre)]
    for (k in seq_along(pre)) x <- gsub(paste0(b[k], com), pre[k], x, fixed = TRUE)
  }
  x
}
.carattere_pulito <- function(x) {
  v <- as.character(x); i <- which(!is.na(v))
  if (!length(i)) return(x)
  w <- .nfc(v[i])
  w <- gsub("\u00a0", " ", w, fixed = TRUE)
  for (z in c("\u200b", "\u200c", "\u200d", "\u2060", "\ufeff", "\u00ad")) w <- gsub(z, "", w, fixed = TRUE)
  w <- gsub("[\u2019\u02bc]", "'", w)
  v[i] <- w
  v
}

leggi_arianna <- function(percorso) {
  
  # col_types = "text"        : nessuna conversione automatica di Excel
  # .name_repair = "minimal"  : intestazioni intatte, spazio finale compreso
  grezzo <- leggi_tabella(percorso)
  
  nome <- riconosci_profilo(names(grezzo))
  # Se il primo foglio non è nel modello (Numbers: «Riepilogo di esportazione») si provano gli altri e si dice.
  foglio_usato <- NA_character_; primo_foglio <- NA_character_
  if (is.na(nome) && tolower(tools::file_ext(percorso)) %in% c("xlsx", "xls")) {
    fogli <- tryCatch(readxl::excel_sheets(percorso), error = function(e) character(0))
    primo_foglio <- if (length(fogli)) fogli[1] else NA_character_
    for (fg in fogli[-1]) {
      g <- tryCatch(leggi_tabella(percorso, foglio = fg), error = function(e) NULL)
      if (is.null(g) || !ncol(g)) next
      n <- riconosci_profilo(names(g))
      if (!is.na(n)) { grezzo <- g; nome <- n; foglio_usato <- fg; break }
    }
  }
  if (is.na(nome))
    stop(.perche_profilo_ignoto(names(grezzo)), call. = FALSE)
  
  p     <- profilo_di_lettura(nome)
  reali <- names(grezzo)
  
  x      <- posizioni_colonne(p, reali)
  indice <- x$pos
  usate  <- stats::na.omit(indice)

  # Colonne che il profilo non conosce; quelle senza intestazione o vuote sono scorie e si elencano a parte.
  altre  <- setdiff(seq_along(reali), usate)
  vuota  <- vapply(altre, function(j) all(.vuoto(grezzo[[j]])), logical(1))
  senza  <- .chiave(reali[altre]) == ""
  spurie <- reali[altre[vuota | senza]]

  # Colonne di un vecchio formato che il modello non ha: i dati restano nel database ma non escono; si dicono all'anteprima.
  nel_modello <- function(n) {
    kn <- .chiave(n)
    kn %in% .chiave(c(PROFILO_MODELLO$arianna, names(EXTRA_CANONICI), "Personaggi indicizzati",
                      unlist(ALIAS_COLONNE[PROFILO_MODELLO$arianna[PROFILO_MODELLO$arianna %in% names(ALIAS_COLONNE)]])))
  }
  fm <- data.frame(colonna = character(0), celle = integer(0), stringsAsFactors = FALSE)
  if (nome != MODELLO) for (i in which(!is.na(indice) & !is.na(p$arianna))) {
    if (nel_modello(p$arianna[i])) next
    cella <- grezzo[[indice[i]]]; piene <- sum(!is.na(cella) & nzchar(trimws(as.character(cella))))   # .vuoto() non è vettoriale
    if (piene > 0) fm <- rbind(fm, data.frame(colonna = reali[indice[i]], celle = piene, stringsAsFactors = FALSE))
  }

  # Strato L1: il carattere. Si conta dove ha cambiato qualcosa, per dirlo all'anteprima.
  dati <- as.data.frame(grezzo, stringsAsFactors = FALSE)
  carattere <- data.frame(colonna = character(0), celle = integer(0), stringsAsFactors = FALSE)
  for (j in seq_along(dati)) if (is.character(dati[[j]])) {
    nuovo <- .carattere_pulito(dati[[j]]); n <- sum(!is.na(nuovo) & nuovo != dati[[j]], na.rm = TRUE)
    if (n > 0) carattere <- rbind(carattere, data.frame(colonna = names(dati)[j], celle = n, stringsAsFactors = FALSE))
    dati[[j]] <- nuovo
  }

  list(
    profilo  = nome,
    carattere = carattere,
    foglio   = foglio_usato, primo_foglio = primo_foglio,   # NA = la tabella era nel primo foglio
    fuori_modello = fm,
    colonne  = reali,                                # intestazioni ESATTE del foglio
    colonna  = setNames(reali[indice], p$arianna),   # profilo -> intestazione reale
    mancanti = p$arianna[is.na(indice) & !is.na(p$arianna)],
    # riconosciute per sinonimo: da dire, non da applicare in silenzio
    sinonimi = data.frame(profilo = p$arianna[!x$esatto & !is.na(indice)],
                          foglio  = reali[indice[!x$esatto & !is.na(indice)]],
                          stringsAsFactors = FALSE),
    ignote   = setdiff(reali[altre], spurie),
    spurie   = spurie,
    dati     = dati
  )
}


# Che cosa cambierà l'import: conta le celle su cui scatta ogni regola, colonna per colonna; non tocca niente.
correzioni_import <- function(a, con = NULL, concordanza = TRUE) {
  p    <- profilo_di_lettura(a$profilo)
  cols <- function(campo) {
    nomi <- p$arianna[!is.na(p$campo) & p$campo == campo]
    stats::na.omit(unlist(a$colonna[nomi]))
  }
  pieno <- function(col) { v <- a$dati[[col]]; v[!is.na(v) & nzchar(v)] }
  righe <- list()
  agg <- function(regola, colonna, n) if (n > 0)
    righe[[length(righe) + 1]] <<- data.frame(regola = regola, colonna = colonna, celle = n,
                                              stringsAsFactors = FALSE)
  # La tabella non era nel primo foglio (Numbers): si dice.
  if (!is.null(a$foglio) && !is.na(a$foglio))
    agg(sprintf("tabella letta dal foglio «%s»: il primo, «%s», non è una tabella nel modello", a$foglio, a$primo_foglio),
        "(foglio)", nrow(a$dati))

  # Strato L1: apostrofo dritto, NFC, spazi non separabili e invisibili (D1, D2).
  if (!is.null(a$carattere)) for (k in seq_len(nrow(a$carattere)))
    agg("carattere: apostrofo dritto, NFC, spazio non separabile e invisibili (leggendo)", a$carattere$colonna[k], a$carattere$celle[k])
  # Il «;» al posto del «|» fra le voci si legge come separatore.
  for (campo in c("persona", "luogo", "reato", "soggetto")) for (col in cols(campo)) {
    n <- sum(vapply(pieno(col), function(v) {
      car <- strsplit(v, "")[[1]]; any(car == ";" & .livello_parentesi(car) <= 0)
    }, TRUE))
    agg("«;» al posto di «|» fra le voci: corretto in «|»", col, n)
  }
  # I due punti non sono un separatore: errore, si propone il «|» (o «; » per lingue e tipologie).
  for (campo in c("persona", "luogo", "reato", "soggetto", "lingua", "tipologia_documentaria")) for (col in cols(campo)) {
    n <- sum(vapply(pieno(col), function(v) !identical(.due_punti_a_separatore(v), v), TRUE))
    agg("«:» non è un separatore fra le voci: errore, si propone «|»", col, n)
  }
  # Ogni altro connettore (virgola, «/», «+», «&», «e», « - ») è un errore: i separatori sono «|» e «; ».
  noti_ <- tryCatch(chiave_grafia(leggi_lista_luoghi()$nome), error = function(e) character(0))
  conta_sep <- function(col, f) sum(vapply(pieno(col), function(v) !identical(f(v), v), TRUE))
  for (col in cols("reato"))    agg("separatore non ammesso fra le voci (solo «|» e «; »): errore", col, conta_sep(col, function(v) .separatori_con_classi(v, vocabolario("reato"), "|")))
  for (col in cols("lingua"))   agg("separatore non ammesso fra le voci (solo «|» e «; »): errore", col, conta_sep(col, function(v) .separatori_con_classi(v, vocabolario("lingua"), "; ", ";")))
  for (col in cols("tipologia_documentaria")) agg("separatore non ammesso fra le voci (solo «|» e «; »): errore", col, conta_sep(col, function(v) .separatori_con_classi(v, vocabolario("tipologia_documentaria"), "; ", ";")))
  # «|» in un campo a un valore solo o di testo: errore, da guardare a mano
  for (nm in c("Segnatura", "Altra segnatura", "Titolo", "Regesto o descrizione sintetica", "Luogo di conservazione", "Note libere"))
    for (col in stats::na.omit(unlist(a$colonna[nm]))) agg("«|» in un campo che non è un elenco: errore, da guardare a mano", col, sum(grepl("|", pieno(col), fixed = TRUE)))
  if (length(noti_)) for (col in cols("luogo")) agg("separatore non ammesso fra le voci (solo «|» e «; »): errore", col, conta_sep(col, function(v) .separatori_luoghi(v, noti_)))
  for (campo in c("persona", "soggetto")) for (col in cols(campo)) agg("separatore non ammesso fra le voci (solo «|» e «; »): errore", col, conta_sep(col, function(v) .separatori_persone(v)$v))
  for (campo in .CAMPI_A_VOCABOLARIO) for (col in cols(campo)) {
    v <- pieno(col); voc <- vocabolario(campo)
    i <- match(tolower(trimws(v)), tolower(voc))
    agg("vocabolario: forma canonica", col, sum(!is.na(i) & v != voc[i]))
  }
  # «S.» sciolto sempre (22/10/2026): luoghi, persone, soggetti, titolo e regesto
  for (campo in c("luogo", "persona", "soggetto", "titolo", "regesto")) for (col in cols(campo))
    agg("«S.» sciolto in San, Santo, Santa o Sant' (si corregge con «Scarica pulito, senza importare»)", col, sum(vapply(pieno(col), function(v) !identical(sciogli_santi(v, guardia = campo != "luogo", ignoti = if (campo == "persona") "persone" else if (campo == "soggetto") "lascia" else "forma"), v), TRUE)))
  # La datazione per secolo non è un anno e non si decide: resta com'è scritta, e si dice.
  for (col in stats::na.omit(c(cols("estremo_remoto"), cols("estremo_recente"))))
    agg("datazione per secolo (Guida al Tab): resta com'è scritta, senza anno nel campo numerico", col, sum(vapply(pieno(col), .datazione_secolo, TRUE)))
  # forme del vocabolario riscritte (NORMALIZZAZIONI, es. "Eresia*")
  for (col in cols("reato")) {
    v <- pieno(col)
    pezzi <- lapply(strsplit(v, "\\|"), trimws)
    agg("vocabolario dei reati: forma normalizzata", col,
        sum(vapply(pezzi, function(x) any(x %in% names(NORMALIZZAZIONI)), TRUE)))
  }
  # Valori controllati (soggettario, lingua): ciò che l'import riscrive con le regole dei fondi presenti e ciò che resta da decidere.
  # Il foglio non dice in che fondo va ogni riga: valgono le regole di tutti i fondi.
  if (!is.null(con)) {
    reg <- .con_equivalenze(dbGetQuery(con, "SELECT campo, valore, forma FROM vocabolario_fondo"))
    for (k in c("reato", "lingua")) for (col in cols(k)) {
      voci <- trimws(unlist(strsplit(pieno(col), if (k == "reato") "[|;]" else "[|;,]")))
      voci <- voci[nzchar(voci) & !(voci %in% names(NORMALIZZAZIONI))]
      if (!length(voci)) next
      r <- reg[reg$campo == k, , drop = FALSE]
      cl <- classifica_valori(unique(voci), vocabolario(k), r$valore[is.na(r$forma)],
                              stats::setNames(r$forma[!is.na(r$forma)], r$valore[!is.na(r$forma)]))
      etichetta <- .riga_campo(k)$etichetta
      for (i in seq_len(nrow(cl))) {
        n <- sum(voci == cl$valore[i]); s <- cl$stato[i]
        if (k == "lingua" && s %in% c("maiuscole", "variante")) next   # la forma «Italiano» è la regola delle lingue
        agg(switch(s,
          maiuscole = sprintf("%s: «%s» diventa «%s»", etichetta, cl$valore[i], cl$proposta[i]),
          variante  = sprintf("%s: «%s» diventa «%s» (%s)", etichetta, cl$valore[i], cl$proposta[i],
                              if (tolower(cl$valore[i]) %in% tolower(names(EQUIVALENZE_ESPLICITE[[k]]))) "equivalenza della Guida al Tab"
                              else if (tolower(cl$valore[i]) %in% tolower(names(EQUIVALENZE[[k]]))) "termine del tesauro: resta registrato sotto la classe" else "variante registrata"),
          vicino    = sprintf("%s: «%s» forse un refuso di «%s»; lo decidi nella finestra «Controlla prima di importare»", etichetta, cl$valore[i], cl$proposta[i]),
          nuovo     = sprintf("%s: «%s» fuori vocabolario; lo decidi nella finestra «Controlla prima di importare»", etichetta, cl$valore[i]),
          NULL) %||% "", col, if (s %in% c("maiuscole", "variante", "vicino", "nuovo")) n else 0L)
      }
    }
  }
  for (col in cols("note")) {
    v <- pieno(col)
    agg("note: blocco «(auto)» tolto",        col, sum(grepl(.BLOCCO_AUTO, v, perl = TRUE)))
    agg("note: «non consultabile» -> campo",  col, sum(grepl(.NON_CONSULT, v, perl = TRUE)))
  }
  for (col in stats::na.omit(unlist(a$colonna))) {
    v <- pieno(col)
    agg("refuso: spazi doppi",             col, sum(grepl("[^\n ][ \t]{2,}[^ ]", v)))
  }
  for (col in cols("persona")) {
    v <- pieno(col)
    agg("refuso: virgola senza spazio",    col, sum(grepl(",(?=[^ ,|])", v, perl = TRUE)))
  }
  for (campo in c("luogo", "persona")) for (col in cols(campo)) {
    v <- pieno(col)
    agg("varianti: «|» dentro le parentesi diventa «;»", col, sum(v != .pipe_in_parentesi(v)))
  }
  # Le righe senza segnatura non sono fascicoli e non entrano (note in fondo al foglio).
  agg("righe senza segnatura: non sono fascicoli e non entrano (note in fondo al foglio?)", a$colonna[["Segnatura"]],
      length(righe_senza_segnatura(a)))
  # correzioni di segnatura dichiarate che toccano QUESTO foglio
  cs <- a$colonna[["Segnatura"]]; ct <- a$colonna[["Titolo"]]
  if (!is.na(cs) && !is.na(ct) && nrow(a$dati) > 0) {
    n <- sum(mapply(function(x, y) !is.null(.correzione_segnatura(x, y)), a$dati[[cs]], a$dati[[ct]]))
    agg("segnatura corretta per dichiarazione (vedi CORREZIONI_SEGNATURA)", cs, n)
    # segnature ripetute: due fascicoli diversi con lo stesso numero
    dopo <- vapply(seq_len(nrow(a$dati)), function(i) {
      c <- .correzione_segnatura(a$dati[[cs]][i], a$dati[[ct]][i])
      if (is.null(c)) as.character(a$dati[[cs]][i]) else paste0(a$dati[[cs]][i], " -> ", c$fascicolo)
    }, "")
    # Le righe senza segnatura non contano: non sono fascicoli e hanno la loro regola.
    agg("segnature ripetute: la seconda riga non entra", cs, sum(duplicated(dopo[!is.na(dopo) & nzchar(trimws(dopo))])))
    # Le eccezioni alla convenzione «b. N, fasc. N» si dicono: «aa. 164» al posto di «b. 164» è
    # una scelta dell'inventario e l'import la conserva, ma chi importa deve saperlo.
    s <- lapply(a$dati[[cs]], .scomponi_segnatura)
    # «7 bis», «7ter», «4-ter.» si scrivono «-bis», «-ter»: una forma sola.
    agg("segnatura: il suffisso bis, ter… si scrive «-bis», «-ter»", cs, sum(vapply(a$dati[[cs]], function(x) {
      if (.vuoto(x)) return(FALSE)
      r <- .scomponi_segnatura(x)
      any(vapply(c(r$busta_suffisso, r$fascicolo_suffisso), function(s) grepl("^-(bis|ter|quater|quinquies|sexies|septies|octies|novies|decies)$", s) &&
                   !grepl(paste0("[0-9]", s, "(,|$)"), x, fixed = FALSE), TRUE))
    }, TRUE)))
    for (sg in unique(stats::na.omit(vapply(s, function(x) x$sigla_busta, ""))))
      agg(sprintf("segnatura: la busta si numera «%s» al posto di «b.»", sg), cs, sum(vapply(s, function(x) identical(x$sigla_busta, sg), TRUE)))
    for (sg in unique(stats::na.omit(vapply(s, function(x) x$sigla_fascicolo, ""))))
      agg(sprintf("segnatura: il fascicolo si numera «%s» al posto di «fasc.»", sg), cs, sum(vapply(s, function(x) identical(x$sigla_fascicolo, sg), TRUE)))
  }

  # Convenzioni di uscita: «Pulisci e scarica» le scrive comunque; qui si contano, perché nessuna modifica resti non dichiarata.
  for (campo in c("regesto", "note")) for (col in cols(campo))
    agg("punto finale aggiunto in uscita (convenzione del foglio)", col, sum(vapply(pieno(col), function(v) !identical(.regesto_export(trimws(v)), trimws(v)), TRUE)))
  for (col in cols("tipologia_documentaria")) agg("tipologia documentaria vuota: esce «Atti» (convenzione del modello)", col, sum(is.na(a$dati[[col]]) | !nzchar(trimws(a$dati[[col]]))))
  for (col in cols("luogo_conservazione")) if (!is.na(cs_ <- unname(a$colonna["Segnatura"])) && cs_ %in% names(a$dati)) {
    seg_ <- a$dati[[cs_]]; lc_ <- a$dati[[col]]
    n <- sum(vapply(seq_along(lc_), function(i) {
      ar <- .scomponi_segnatura(seg_[i])$archivio; if (.vuoto(ar) || .vuoto(lc_[i])) return(FALSE)
      ci <- localita_archivio(ar)$localita; !is.na(ci) && identical(tolower(trimws(lc_[i])), tolower(ci)) }, TRUE))
    agg("luogo di conservazione: la città dell'archivio diventa l'ente (12/10)", col, n)
  }
  # Nomi che non concordano e luoghi con un refuso: li corregge «Pulisci e scarica».
  if (isTRUE(concordanza) && nrow(a$dati) <= 3000L) {
    cz <- tryCatch(concordanza_righe(a), error = function(e) NULL)
    if (!is.null(cz) && nrow(cz$registro)) {
      k <- unique(cz$registro[, c("perche", "colonna")])
      for (r in seq_len(nrow(k))) agg(sprintf("%s (si corregge con «Scarica pulito, senza importare»)", k$perche[r]), k$colonna[r],
                                      sum(cz$registro$perche == k$perche[r] & cz$registro$colonna == k$colonna[r]))
    }
    if (!is.null(cz) && nrow(cz$da_decidere)) agg("nomi o luoghi che non si decidono da soli (vedi «Da decidere» nel foglio pulito)", "(più colonne)", nrow(cz$da_decidere))
  }

  if (!length(righe)) return(data.frame(regola = character(0), colonna = character(0), celle = integer(0)))
  do.call(rbind, righe)
}


# Correzioni di segnatura dichiarate: errori della fonte non deducibili da una regola; valgono solo per segnatura E titolo e l'anteprima le mostra.
# Sono del profilo (`correzioni_segnatura`).
CORREZIONI_SEGNATURA <- PROFILO_DOMINIO$correzioni_segnatura

.correzione_segnatura <- function(segnatura, titolo) {
  for (c in CORREZIONI_SEGNATURA)
    if (identical(trimws(as.character(segnatura)), c$segnatura) &&
        identical(trimws(as.character(titolo)),    c$titolo)) return(c)
  NULL
}
