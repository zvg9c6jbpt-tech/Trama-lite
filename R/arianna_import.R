# ─────────────────────────────────────────────────────────────
# R/arianna_import.R: dal foglio ai fascicoli: riga → fascicolo e import (accoda / sostituisci / nuovo archivio)
# Dipende da: arianna.R (lettura, riconoscimento, correzioni), compose.R, crud.R (fascicolo_vuoto, salva_fascicolo).
# ─────────────────────────────────────────────────────────────

# Il profilo del materiale (profili/<nome>/profilo.json): se questo file è letto da solo, lo si carica prima.
if (!exists("PROFILO_DOMINIO", inherits = FALSE)) source("R/profilo_dominio.R")

# ── da riga del foglio a fascicolo ────────────────────────────────

# Colonne extra dei vecchi formati con un altro nome nel modello: si conservano col nome del MODELLO; le altre restano col loro nome.
EXTRA_CANONICI <- c(
  "Altezza"                              = "dimensioni/altezza",
  "Larghezza"                            = "dimensioni/larghezza",
  "Consistenza (quantità)"               = "consistenza.quantità",
  "Consistenza (unità di misura)"        = "consistenza.unitàMisura",
  "Altro soggetto (citato o indicizzato)" = "Soggetti citati o indicizzati",
  "Organizzazioni citate"                = "Soggetti citati o indicizzati"
)
.nome_extra <- function(nome) if (nome %in% names(EXTRA_CANONICI)) unname(EXTRA_CANONICI[nome]) else nome

# Un foglio vecchio ha la mappa con i nomi del SUO profilo, i controlli cercano quelli del modello: si aggiungono i nomi mancanti, nulla si toglie.
.con_nomi_modello <- function(colonna, profilo) {
  if (is.null(profilo) || length(profilo) != 1L || is.na(profilo) || identical(profilo, MODELLO)) return(colonna)
  p <- PROFILI[[profilo]]; if (is.null(p)) return(colonna)
  agg <- character(0)
  for (i in seq_len(nrow(p))) {
    col <- unname(colonna[p$arianna[i]]); if (!length(col) || is.na(col)) next
    m <- if (p$dove[i] == "extra") .nome_extra(p$arianna[i]) else PROFILO_MODELLO$arianna[match(p$campo[i], PROFILO_MODELLO$campo)]
    if (length(m) && !is.na(m) && !(m %in% names(colonna)) && !(m %in% names(agg))) agg[[m]] <- col
  }
  c(colonna, agg)
}

# I nomi con cui un'extra del modello può essere già nel database (importata
# prima del 2/10 col nome del vecchio formato).
.extra_sinonimi <- function(nome) c(nome, names(EXTRA_CANONICI)[EXTRA_CANONICI == nome])

# Il luogo di conservazione è un ENTE, mai una città: la città dell'archivio riceve l'ente. Un campo vuoto resta vuoto.
.ente_conservatore <- function(f) {
  f$produttori <- .produttore_canonico(f$produttori)      # «Tribunale dell'Inquisizione di X» = «Inquisizione di X» (12/10)
  if (.vuoto(f$archivio) || .vuoto(f$luogo_conservazione)) return(f)
  citta <- localita_archivio(f$archivio)$localita
  if (!is.na(citta) && identical(tolower(trimws(f$luogo_conservazione)), tolower(citta))) f$luogo_conservazione <- f$archivio
  f
}

# Le colonne extra che sono elenchi di nomi e accettano il «;» come separatore (13/10).
.EXTRA_CON_PUNTO_E_VIRGOLA <- c("Soggetti citati o indicizzati")
fascicolo_da_riga <- function(riga, profilo, colonna) {
  
  f     <- fascicolo_vuoto(con_default = FALSE)
  extra <- list()
  indicizzati <- character(0)
  
  valore <- function(nome_profilo) {
    col <- colonna[[nome_profilo]]
    if (is.null(col) || is.na(col)) return(NA_character_)
    v <- riga[[col]]
    if (.vuoto(v)) NA_character_ else trimws(as.character(v))
  }
  
  # Il separatore è «|», non dentro le parentesi (le varianti di un nome stanno lì).
  # Il «;» separa solo nelle colonne di voci (persone, luoghi, reati, soggetti citati, personaggi); altrove resta com'è.
  spezza <- function(v, anche_punto_e_virgola = FALSE) {
    if (.vuoto(v)) return(character(0))
    car <- strsplit(as.character(v), "")[[1]]
    liv <- .livello_parentesi(car)
    sep <- car %in% (if (anche_punto_e_virgola) c("|", ";") else "|") & liv <= 0        # fuori dalle parentesi: separa
    grp <- cumsum(sep)
    car[sep] <- ""                      # via solo i separatori veri
    x <- trimws(vapply(split(car, grp), paste, "", collapse = ""))
    unname(x[nzchar(x)])
  }
  
  for (i in seq_len(nrow(profilo))) {
    r <- profilo[i, ]
    if (r$dove == "derivato") next
    v <- valore(r$arianna)
    if (.vuoto(v)) next
    
    if (r$dove == "extra") {
      # «Personaggi indicizzati» sono persone: nel modello stanno con le persone citate.
      if (identical(r$arianna, "Personaggi indicizzati")) { indicizzati <- c(indicizzati, spezza(v, TRUE)); next }
      k <- .nome_extra(r$arianna)
      extra[[k]] <- unique(c(extra[[k]], spezza(v, k %in% .EXTRA_CON_PUNTO_E_VIRGOLA)))
      next
    }
    
    if (r$dove == "fascicoli") {
      # «n.d.» nell'estremo remoto è la data assente per dichiarazione (data_nd),
      # non un numero che non si legge: nel modello si scrive così (2/10).
      if (identical(r$campo, "estremo_remoto") && tolower(gsub("[ .]", "", v)) %in% c("nd", "nondisponibile")) {
        f$data_nd <- 1L
        next
      }
      # «Tipologia documentaria»: nel foglio di uscita è sempre «Atti», salvo modifica a mano nella Scheda; il valore di un foglio che arriva non la cambia.
      if (identical(r$campo, "tipologia_documentaria")) {
        # Le tipologie della guida al Tab (Atti, Lettere, Decreti, anche più d'una con «; »): se non sono la sola «Atti» si conservano nel campo apposito.
        voci <- trimws(strsplit(v, "[;|]")[[1]]); voci <- voci[nzchar(voci)]
        if (length(voci) && all(tolower(voci) %in% tolower(TIPOLOGIE_FOGLIO))) {
          canoniche <- unique(TIPOLOGIE_FOGLIO[match(tolower(voci), tolower(TIPOLOGIE_FOGLIO))])
          if (!identical(canoniche, "Atti")) { extra[[EXTRA_TIPOLOGIA_FOGLIO]] <- paste(canoniche, collapse = "; "); next }
        }
        if (!(tolower(v) %in% tolower(VOCABOLARI$tipologia_documentaria))) next
      }
      # Un numero entra solo se è un intero («circa 1600» e «1600.9» restano vuote); un estremo deve stare nell'intervallo ammesso.
      # Una data intera («1625-01-09», «marzo 1644») entra come anno nel campo numerico e per intero nel suo extra.
      if (r$campo %in% names(EXTRA_DATA_PRECISA) && !is.null(dp <- .data_precisa(v)) && dp$anno >= ANNI_AMMESSI[1] && dp$anno <= ANNI_AMMESSI[2]) {
        f[[r$campo]] <- dp$anno; extra[[EXTRA_DATA_PRECISA[[r$campo]]]] <- dp$canonica
        next
      }
      f[[r$campo]] <- if (r$widget == "numero") .intero_stretto(v, if (r$campo %in% c("estremo_remoto", "estremo_recente")) ANNI_AMMESSI else NULL) else v
      # Una data non valida non si perde: il testo resta in un campo a parte, esce com'era e l'import la segnala (date_non_inserite).
      if (r$campo %in% names(EXTRA_DATA_NON_RISOLTA) && is.na(f[[r$campo]])) extra[[EXTRA_DATA_NON_RISOLTA[[r$campo]]]] <- v
      next
    }
    
    f[[r$dove]] <- spezza(v, r$dove %in% c("persone", "luoghi", "reati", "produttori"))    # persone, luoghi, reati, produttori, lingue
    # Le due convenzioni del foglio si disfano leggendo, o un foglio riesportato cambierebbe il database (lingue in una voce, particelle dei cognomi).
    if (identical(r$dove, "lingue"))  f$lingue  <- .lingue_import(f$lingue)
  }
  
  if (length(indicizzati)) f$persone <- unique(c(as.character(f$persone), indicizzati))
  # Le due convenzioni dei nomi si disfano su TUTTE le persone, anche sui «Personaggi indicizzati»
  # (prima passavano di lato: «Taibi di, Matteo» restava com'era).
  if (length(f$persone)) f$persone <- unique(vapply(as.character(f$persone), .nome_persona_import, character(1), USE.NAMES = FALSE))

  # segnatura: una stringa, cinque componenti
  s <- analizza_segnatura(valore("Segnatura"))
  for (k in names(s)) if (!is.na(s[[k]])) f[[k]] <- s[[k]]

  # errore della fonte, corretto per dichiarazione esplicita
  cs <- .correzione_segnatura(valore("Segnatura"), valore("Titolo"))
  if (!is.null(cs)) {
    if (!is.null(cs$fascicolo)) f$fascicolo <- cs$fascicolo
    if (!is.null(cs$busta))     f$busta     <- cs$busta
  }
  
  # regesto: strutturato o prosa
  g <- analizza_regesto(f$regesto)
  if (isTRUE(g$strutturato)) {
    f$luogo_reato     <- g$luogo_reato
    f$tortura         <- g$tortura
    f$sentenza        <- g$sentenza
    # I reati stanno in Soggettario e regesto: si tengono TUTTI, nell'ordine della colonna e in coda quelli solo del regesto; Qualità elenca le discordanze.
    dalla_colonna <- normalizza(unique(trimws(as.character(f$reati))))
    dal_regesto   <- normalizza(unique(trimws(as.character(g$reati))))
    if (length(dalla_colonna) && length(dal_regesto) && !setequal(dalla_colonna, dal_regesto))
      f$soggettario_discorde <- 1L
    if (length(dalla_colonna) || length(dal_regesto))
      f$reati <- unique(c(dalla_colonna, dal_regesto))
    # Un regesto con residui resta com'è: ricomposto, Salva ne cancellerebbe una parte.
    f$regesto_manuale <- if (isTRUE(g$residui)) 1L else 0L
  } else {
    # Prosa, o vuoto: com'è nel foglio; un regesto vuoto ricomposto scriverebbe al Salva ciò che la fonte non dice.
    f$regesto_manuale <- 1L
  }
  
  # Titolo dal foglio, anche vuoto: non si ricompone (sarebbe un titolo che il catalogo non ha).
  f$titolo_manuale <- 1L
  
  f$reati <- normalizza(f$reati)
  f$extra <- extra
  # L'import conosce gli extra del suo profilo: solo quelli riscrive.
  f$campi_extra_noti <- unique(c(vapply(profilo$arianna[profilo$dove == "extra"], .nome_extra, ""), unname(EXTRA_DATA_NON_RISOLTA), unname(EXTRA_DATA_PRECISA),
                                 PROFILO_MODELLO$arianna[PROFILO_MODELLO$dove == "extra"], EXTRA_TIPOLOGIA_FOGLIO))
  .ente_conservatore(normalizza_fascicolo(f))
}


# ── import ────────────────────────────────────────────────────────

# Dataset distinto nel selettore dei fondi: tutto o niente, anche il registro.
# Archivi e fondi che il foglio dice nelle segnature, con le righe di ciascuno (serve all'anteprima).
archivi_nel_foglio <- function(a) {
  cs <- a$colonna[["Segnatura"]]
  vuoto <- data.frame(archivio = character(0), fondo = character(0), righe = integer(0), stringsAsFactors = FALSE)
  if (is.null(cs) || is.na(cs) || !nrow(a$dati)) return(vuoto)
  v <- as.character(a$dati[[cs]]); v <- v[!is.na(v) & nzchar(trimws(v))]
  if (!length(v)) return(vuoto)
  u <- unique(v)
  p <- lapply(u, analizza_segnatura)
  chiave <- paste(vapply(p, function(x) if (is.null(x$archivio) || is.na(x$archivio)) "" else x$archivio, ""),
                  vapply(p, function(x) if (is.null(x$fondo) || is.na(x$fondo)) "" else x$fondo, ""), sep = "\r")[match(v, u)]
  t <- table(chiave); k <- strsplit(names(t), "\r", fixed = TRUE)
  d <- data.frame(archivio = vapply(k, `[`, "", 1), fondo = vapply(k, function(x) if (length(x) > 1) x[2] else "", ""),
                  righe = as.integer(t), stringsAsFactors = FALSE)
  d[order(-d$righe), , drop = FALSE]
}

# Le righe senza segnatura (note in fondo, totali) non sono fascicoli: non entrano mai e si dice. Indici nelle righe di `a$dati`.
righe_senza_segnatura <- function(a) {
  cs <- a$colonna[["Segnatura"]]
  if (is.null(cs) || is.na(cs) || !nrow(a$dati)) return(integer(0))
  v <- as.character(a$dati[[cs]])
  which(is.na(v) | !nzchar(trimws(v)))
}
.MOTIVO_SENZA_SEGNATURA <- "Senza segnatura: non è un fascicolo (note in fondo al foglio?), non entra."

importa_nuovo_archivio <- function(con, a, archivio, fondo, serie = NULL, fonte = NULL) {
  p <- profilo_di_lettura(a$profilo)
  if (is.null(p) || !nrow(a$dati)) stop("Carica un foglio valido con almeno una riga.", call. = FALSE)
  senza <- righe_senza_segnatura(a)
  if (length(senza) == nrow(a$dati)) stop("Nessuna riga ha la segnatura: non c'è niente da importare.", call. = FALSE)
  rif <- righe_segnatura_non_ammessa(a); rif <- rif[!(rif$i %in% senza), , drop = FALSE]      # segnatura non ammessa: non entra (20/10)
  fuori <- c(senza, rif$i)
  # Nessuna riga può entrare: un fondo vuoto in Home non serve a nessuno (18/11/2026). Si dice il perché della prima riga rifiutata.
  if (length(fuori) == nrow(a$dati))
    stop(sprintf("Nessuna riga può entrare, quindi il fondo non è stato creato. Per esempio la riga %d: %s", min(rif$i, na.rm = TRUE) + 1L,
                 if (nrow(rif)) rif$motivo[which.min(rif$i)] else .MOTIVO_SENZA_SEGNATURA), call. = FALSE)
  .atomico(con, {
    archivio <- .carattere_pulito(trimws(archivio)); nome_nuovo <- .carattere_pulito(trimws(fondo))
    nuovo_fondo <- crea_archivio(con, archivio, nome_nuovo, serie, fonte = fonte)       # l'id del fondo, con la grafia che avranno le schede
    dentro <- setdiff(seq_len(nrow(a$dati)), fuori)                       # le righe sorgente che entrano: il registro delle modifiche le lega al fondo
    for (i in dentro) {
      tryCatch({
        f <- normalizza_valori_fascicolo(con, fascicolo_da_riga(a$dati[i, ], p, a$colonna), fondo_id = nuovo_fondo)
        # Conserva l'identità della fonte prima di assegnare il nuovo dataset.
        f$extra$import_identita_originale <- jsonlite::toJSON(
          f[c("archivio", "fondo", "serie", "luogo_conservazione")],
          auto_unbox = TRUE, na = "null")
        f$archivio <- archivio
        f$fondo <- nome_nuovo
        f$luogo_conservazione <- archivio
        if (!.vuoto(serie)) f$serie <- trimws(serie)
        salva_fascicolo(con, f)
      # Un fondo nuovo entra tutto o niente; l'errore si dice in italiano.
      }, error = function(e) stop(sprintf("Riga %d: %s Nuovo fondo non importato; dati precedenti invariati.",
                                          i + 1L, sub("([^.])$", "\\1.", .spiega_rifiuto(conditionMessage(e)))), call. = FALSE))
    }
    list(letti = nrow(a$dati), importati = nrow(a$dati) - length(fuori), fondo = nuovo_fondo,
         riga_fondo = data.frame(riga = dentro + 1L, fondo = rep(nuovo_fondo, length(dentro)), stringsAsFactors = FALSE),
         respinti = data.frame(riga = c(senza, rif$i) + 1L, segnatura = c(rep("", length(senza)), rif$segnatura),
                               motivo = c(rep(.MOTIVO_SENZA_SEGNATURA, length(senza)), rif$motivo), stringsAsFactors = FALSE))
  })
}

# Motivo di una riga respinta in italiano, non l'errore del database.
.spiega_rifiuto <- function(m) {
  if (grepl("NOT NULL constraint failed: fascicoli\\.fondo", m))
    return("La segnatura non dice il fondo (ha un nome solo prima di «b.»): scrivi «archivio, fondo, b. N, fasc. N», oppure indica archivio e fondo nella finestra dell'import.")
  if (grepl("UNIQUE constraint failed: (fascicoli\\.|index .idx_segnatura)", m))
    return("Segnatura già occupata: lo stesso fondo, busta, fascicolo e suffisso compaiono due volte (o sono già nel database).")
  m
}

# Righe non entrate: quelle con segnatura già occupata sono «già presenti», le altre «respinte»; il motivo sta nella tabella.
conta_non_entrate <- function(respinti) {
  m <- if (is.null(respinti) || !nrow(respinti)) character() else as.character(respinti$motivo)
  presenti <- sum(startsWith(m, "Segnatura già occupata"))
  list(presenti = presenti, respinte = length(m) - presenti)
}
# «24 letti · 0 importati · 24 già presenti» (con «… · 4 respinti» se c'è altro).
riassunto_import <- function(letti, importati, respinti, verbo = c("importati", "pulite")) {
  n <- conta_non_entrate(respinti)
  paste0(sprintf("%d letti \u00b7 %d importati", letti, importati),
         if (n$presenti > 0) sprintf(" \u00b7 %d già presenti", n$presenti),
         if (n$respinte > 0 || n$presenti == 0) sprintf(" \u00b7 %d respinti", n$respinte))
}

# Anni ammessi: 1200–1899; fuori intervallo si decide, la Scheda non lo salva, l'import non lo inserisce.
# L'intervallo è del profilo (`anni_ammessi`).
ANNI_AMMESSI <- PROFILO_DOMINIO$anni_ammessi
# Dove resta il testo di una data non valida: non è un anno nel database, esce com'era nel foglio.
EXTRA_DATA_NON_RISOLTA <- c(estremo_remoto = "Estremo remoto non risolto", estremo_recente = "Estremo recente non risolto")

# Data intera della Guida al Tab («aaaa-mm-gg», «mese aaaa», «aaaa-mm»): restituisce anno e forma canonica, o NULL.
MESI_DATE <- c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")
.data_precisa <- function(v) {
  v <- trimws(as.character(v)[1]); if (is.na(v) || !nzchar(v)) return(NULL)
  m <- regmatches(v, regexec("^([0-9]{4})-([0-9]{1,2})-([0-9]{1,2})$", v))[[1]]
  if (length(m)) {
    a <- as.integer(m[2]); me <- as.integer(m[3]); g <- as.integer(m[4])
    if (me < 1L || me > 12L || g < 1L) return(NULL)
    ultimo <- c(31L, if ((a %% 4L == 0L && a %% 100L != 0L) || a %% 400L == 0L) 29L else 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[me]
    if (g > ultimo) return(NULL)
    return(list(anno = a, mese = me, giorno = g, canonica = sprintf("%04d-%02d-%02d", a, me, g)))
  }
  m <- regmatches(v, regexec("^([0-9]{4})-([0-9]{1,2})$", v))[[1]]
  if (length(m)) { me <- as.integer(m[3]); if (me < 1L || me > 12L) return(NULL); return(list(anno = as.integer(m[2]), mese = me, giorno = NA_integer_, canonica = paste(MESI_DATE[me], m[2]))) }
  m <- regmatches(v, regexec("^([[:alpha:]]+)\\s+([0-9]{4})$", v))[[1]]
  if (length(m)) { me <- match(tolower(m[2]), MESI_DATE); if (is.na(me)) return(NULL); return(list(anno = as.integer(m[3]), mese = me, giorno = NA_integer_, canonica = paste(MESI_DATE[me], m[3]))) }
  NULL
}
# Datazione per secolo («sec. XV», «prima metà del sec. XV», «inizio sec. XVI», dal XIII al XIX): non è un anno, esce com'era, non è un errore.
.datazione_secolo <- function(v) {
  v <- trimws(as.character(v)[1]); if (is.na(v) || !nzchar(v)) return(FALSE)
  grepl("^((prima|seconda) metà del |(primo|secondo|terzo|ultimo) quarto del |inizio |fine )?sec\\. (XIII|XIV|XV|XVI|XVII|XVIII|XIX)$",
        gsub("\\s+", " ", v), ignore.case = TRUE)
}
# Fra due date intere la prima viene dopo la seconda? Si confrontano solo le parti che hanno entrambe.
.data_dopo <- function(a, b) {
  for (k in c("anno", "mese", "giorno")) {
    x <- if (is.null(a[[k]])) NA else a[[k]]; y <- if (is.null(b[[k]])) NA else b[[k]]
    if (is.na(x) || is.na(y)) return(FALSE)
    if (x != y) return(x > y)
  }
  FALSE
}
# Come sta scritta una data: intera se è una data della guida, altrimenti l'anno.
.data_o_anno <- function(v) {
  d <- .data_precisa(v); if (!is.null(d)) return(d)
  v <- trimws(as.character(v)[1]); if (!is.na(v) && grepl("^[0-9]{4}$", v)) list(anno = as.integer(v), mese = NA_integer_, giorno = NA_integer_, canonica = v) else NULL
}

# Un intero, e solo un intero («1566», «1566.0»); con `intervallo`, anche entro quegli estremi.
.intero_stretto <- function(v, intervallo = NULL) {
  v <- trimws(as.character(v)[1]); if (is.na(v) || !nzchar(v) || !grepl("^[0-9]+(\\.0+)?$", v)) return(NA_integer_)
  n <- suppressWarnings(as.integer(sub("\\.0+$", "", v)))
  if (is.na(n) || (!is.null(intervallo) && (n < intervallo[1] || n > intervallo[2]))) NA_integer_ else n
}
# Le date scritte nel foglio e non entrate (cella piena, valore non risolto).
.date_non_inserite <- function(riga, colonna, f) {
  out <- data.frame(campo = character(0), valore = character(0), stringsAsFactors = FALSE)
  for (x in list(c("Estremo remoto", "estremo_remoto"), c("Estremo recente", "estremo_recente"))) {
    col <- unname(colonna[x[1]]); if (is.na(col) || !(col %in% names(riga))) next
    v <- trimws(as.character(riga[[col]])[1])
    if (!is.na(v) && nzchar(v) && !(tolower(v) %in% c("n.d.", "nd")) && !.datazione_secolo(v) && is.na(f[[x[2]]]))
      out <- rbind(out, data.frame(campo = x[1], valore = v, stringsAsFactors = FALSE))
  }
  out
}

# `fondo` vale solo per «sostituisci»: si cancellano le schede di quel fondo e le righe di un altro sono respinte. Senza, si sostituisce tutto.
# `senza_segnatura_ok`: la finestra di conferma ha mostrato le righe senza segnatura; quelle non annullano la sostituzione.
# I dati solo-database (tortura, sentenza, attribuzione, flag, coppie persona-reato) si leggono prima, per segnatura, e si rimettono dove il foglio tace.
.CAMPI_SOLO_DATABASE <- c("tortura", "tortura_esaminata", "sentenza", "sentenza_esaminata", "attribuzione", "attribuzione_luoghi",
                          "identita_imputato_dubbia", "imputati_esaminati", "reato_ignoto", "luogo_ignoto", "non_procedimento")
.chiave_segnatura <- function(fondo_id, busta, busta_suffisso, fascicolo, fascicolo_suffisso) {
  v <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  paste(v(fondo_id), v(busta), v(busta_suffisso), v(fascicolo), v(fascicolo_suffisso), sep = "\r")
}
.dati_solo_database <- function(con, fondo = NULL) {
  dove <- if (is.null(fondo)) "" else " WHERE fondo_id = ?"
  par  <- if (is.null(fondo)) NULL else list(fondo)
  d <- dbGetQuery(con, paste0("SELECT id, fondo_id, busta, busta_suffisso, fascicolo, fascicolo_suffisso, titolo, titolo_manuale, regesto, regesto_manuale, ",
                              paste(.CAMPI_SOLO_DATABASE, collapse = ", "), " FROM fascicoli", dove), par)
  if (!nrow(d)) return(list(schede = list(), imputati = list(), persona_reati = list(), reato_luoghi = list(), reato_termini = list()))
  coppie <- function(tabella, cols) {
    r <- dbGetQuery(con, sprintf("SELECT fascicolo_id, %s FROM %s", paste(cols, collapse = ", "), tabella))
    r <- r[r$fascicolo_id %in% d$id, , drop = FALSE]
    lapply(split(r[cols], r$fascicolo_id), function(x) { rownames(x) <- NULL; x })
  }
  k <- .chiave_segnatura(d$fondo_id, d$busta, d$busta_suffisso, d$fascicolo, d$fascicolo_suffisso)
  schede <- split(d, factor(k, levels = unique(k)))
  list(schede = lapply(schede, function(x) as.list(x[1, , drop = FALSE])),
       imputati = coppie("imputati", c("esteso", "indicizzato")),
       persona_reati = coppie("persona_reati", c("persona", "reato")), reato_luoghi = coppie("reato_luoghi", c("reato", "luogo")),
       reato_termini = coppie("reato_termini", c("reato", "termine")))
}
.riporta_dati_solo_database <- function(f, vecchi, fondo_id) {
  k <- .chiave_segnatura(fondo_id, f$busta, f$busta_suffisso, f$fascicolo, f$fascicolo_suffisso)
  v <- vecchi$schede[[k]]
  if (is.null(v)) return(f)
  for (campo in .CAMPI_SOLO_DATABASE) {
    nuovo <- f[[campo]]; vecchio <- v[[campo]]
    vuoto <- .vuoto(nuovo) || identical(as.character(nuovo), "n.d.") || identical(as.character(nuovo), "0")
    if (vuoto && !.vuoto(vecchio)) f[[campo]] <- vecchio
  }
  # Titolo e regesto identici al foglio non cambiano natura: composta resta composta, scritta a mano resta tale.
  if (!.vuoto(v$titolo) && identical(trimws(as.character(f$titolo)), v$titolo)) f$titolo_manuale <- v$titolo_manuale
  if (!.vuoto(v$regesto) && !.vuoto(f$regesto) && trimws(as.character(f$regesto)) %in% c(v$regesto, paste0(v$regesto, "."))) {
    f$regesto <- v$regesto; f$regesto_manuale <- v$regesto_manuale
  }
  # Gli imputati stanno solo nel database: le coppie persona-reato hanno senso solo con loro.
  if (!NROW(f$imputati) && NROW(vecchi$imputati[[v$id]])) f$imputati <- vecchi$imputati[[v$id]]
  if (is.null(f$persona_reati) || !NROW(f$persona_reati)) f$persona_reati <- vecchi$persona_reati[[v$id]]
  if (is.null(f$reato_luoghi) || !NROW(f$reato_luoghi)) f$reato_luoghi <- vecchi$reato_luoghi[[v$id]]
  # Il foglio dice la classe, il termine sta solo nel database: si riporta se il foglio non lo porta (salvando, cade se la classe non c'è più).
  if (is.null(f$reato_termini) || !NROW(f$reato_termini)) f$reato_termini <- vecchi$reato_termini[[v$id]]
  f
}

# Schede di prima che «Sostituisci» non ritroverebbe (dati solo-database e nessuna riga con la loro segnatura): la finestra le elenca.
schede_senza_corrispondenza <- function(con, a, fondo) {
  vuota <- data.frame(segnatura = character(0), perde = character(0), stringsAsFactors = FALSE)
  fondo <- .fid(con, fondo); p <- profilo_di_lettura(a$profilo)
  if (.vuoto(fondo) || is.null(p) || !nrow(a$dati)) return(vuota)
  vecchi <- .dati_solo_database(con, fondo)
  if (!length(vecchi$schede)) return(vuota)
  fuori <- c(righe_senza_segnatura(a), righe_segnatura_non_ammessa(a)$i)
  chiavi <- character(0)
  for (i in setdiff(seq_len(nrow(a$dati)), fuori)) {
    f <- tryCatch(fascicolo_da_riga(a$dati[i, ], p, a$colonna), error = function(e) NULL)
    if (!is.null(f)) chiavi <- c(chiavi, .chiave_segnatura(fondo, f$busta, f$busta_suffisso, f$fascicolo, f$fascicolo_suffisso))
  }
  etichette <- c(tortura = "tortura", sentenza = "sentenza", attribuzione = "attribuzione dei capi",
                 attribuzione_luoghi = "attribuzione dei luoghi", reato_ignoto = "«reato ignoto»", luogo_ignoto = "«luogo ignoto»",
                 non_procedimento = "«non è un procedimento»", identita_imputato_dubbia = "identità dubbia", imputati_esaminati = "imputati esaminati",
                 tortura_esaminata = "tortura esaminata", sentenza_esaminata = "sentenza esaminata")
  out <- vuota
  for (k in setdiff(names(vecchi$schede), chiavi)) {
    v <- vecchi$schede[[k]]
    ha <- vapply(names(etichette), function(campo) { x <- v[[campo]]
      !.vuoto(x) && !as.character(x) %in% c("n.d.", "0") }, TRUE)
    cosa <- unname(etichette[ha])
    if (NROW(vecchi$imputati[[v$id]])) cosa <- c(cosa, "imputati")
    if (NROW(vecchi$persona_reati[[v$id]]) || NROW(vecchi$reato_luoghi[[v$id]])) cosa <- c(cosa, "coppie dei capi")
    if (NROW(vecchi$reato_termini[[v$id]])) cosa <- c(cosa, "termini dei reati")
    if (!length(cosa)) next
    out[nrow(out) + 1L, ] <- list(sprintf("b. %s%s, fasc. %s%s", v$busta, v$busta_suffisso, v$fascicolo, v$fascicolo_suffisso), paste(cosa, collapse = ", "))
  }
  out
}

importa_arianna <- function(con, a, modo = c("accoda", "sostituisci"), fondo = NULL, senza_segnatura_ok = FALSE) {
  
  modo <- match.arg(modo)
  p    <- profilo_di_lettura(a$profilo)
  d    <- a$dati
  
  if (is.null(p)) stop("Profilo di import non riconosciuto.")
  # Un foglio con le sole intestazioni (il modello vuoto) non ha niente da
  # importare: e in modo «sostituisci» cancellerebbe i fascicoli per nulla.
  if (nrow(d) == 0) stop("Il foglio ha le intestazioni ma nessuna riga: non c'è niente da importare.", call. = FALSE)
  
  ok       <- 0L
  riga_fondo <- list()
  respinti <- list()
  date_non_inserite <- list()
  
  # Una sola transazione: senza, SQLite sincronizza il disco a ogni INSERT.
  dbBegin(con)
  concluso <- FALSE
  on.exit(if (!concluso) try(dbRollback(con), silent = TRUE), add = TRUE)
  fondo <- if (modo == "sostituisci") .fid(con, fondo) else NULL
  vecchi <- if (modo == "sostituisci") .dati_solo_database(con, fondo)
  if (modo == "sostituisci") {
    if (is.null(fondo)) dbExecute(con, "DELETE FROM fascicoli")
    else dbExecute(con, "DELETE FROM fascicoli WHERE fondo_id = ?", list(fondo))
  }
  
  senza <- righe_senza_segnatura(a)
  rif <- righe_segnatura_non_ammessa(a)
  for (i in seq_len(nrow(d))) {
    
    if (i %in% rif$i && !(i %in% senza)) {
      respinti[[length(respinti) + 1]] <- data.frame(riga = i + 1L, segnatura = rif$segnatura[match(i, rif$i)], motivo = rif$motivo[match(i, rif$i)],
                                                     stringsAsFactors = FALSE)
      next
    }
    if (i %in% senza) {
      respinti[[length(respinti) + 1]] <- data.frame(riga = i + 1L, segnatura = "", motivo = .MOTIVO_SENZA_SEGNATURA,
                                                     stringsAsFactors = FALSE)
      next
    }
    f0    <- fascicolo_da_riga(d[i, ], p, a$colonna)
    f     <- tryCatch(normalizza_valori_fascicolo(con, f0), error = function(e) e)
    if (inherits(f, "error")) {
      # Solo il fondo omonimo in più archivi, con la segnatura che non dice quale, respinge la riga (e il motivo si legge); ogni altro errore ferma l'import e annulla tutto.
      if (!grepl("in più archivi", conditionMessage(f), fixed = TRUE)) stop(f)
      respinti[[length(respinti) + 1]] <- data.frame(riga = i + 1L, segnatura = as.character(d[[a$colonna[["Segnatura"]]]][i]), motivo = conditionMessage(f), stringsAsFactors = FALSE)
      next
    }
    if (!is.null(vecchi)) f <- .riporta_dati_solo_database(f, vecchi, fondo_id_da(con, f$archivio, f$fondo))
    sc    <- .date_non_inserite(d[i, ], a$colonna, f)
    if (nrow(sc)) date_non_inserite[[length(date_non_inserite) + 1L]] <- cbind(riga = i + 1L, sc)
    esito <- if (!is.null(fondo) && !identical(fondo_id_da(con, f$archivio, f$fondo), fondo))
      structure("non è del fondo che stai sostituendo", class = "try-error", condition = simpleError("non è del fondo che stai sostituendo"))
    else try(salva_fascicolo(con, f), silent = TRUE)
    
    if (inherits(esito, "try-error")) {
      respinti[[length(respinti) + 1]] <- data.frame(
        riga      = i + 1L,
        segnatura = componi_segnatura(f$archivio, f$fondo, f$serie,
                                      f$busta, f$fascicolo, f$fascicolo_suffisso,
                                      sigla = f$sigla_busta, sigla_f = f$sigla_fascicolo, suffisso_busta = f$busta_suffisso),
        motivo    = .spiega_rifiuto(conditionMessage(attr(esito, "condition"))),
        stringsAsFactors = FALSE)
    } else {
      ok <- ok + 1L
      riga_fondo[[ok]] <- data.frame(riga = i + 1L, fondo = as.character(fondo_id_da(con, f$archivio, f$fondo))[1], stringsAsFactors = FALSE)
    }
  }
  riga_fondo <- if (length(riga_fondo)) do.call(rbind, riga_fondo) else data.frame(riga = integer(0), fondo = character(0), stringsAsFactors = FALSE)
  
  # Sostituzione atomica: anche un solo rifiuto annulla cancellazioni e inserimenti.
  # Accoda mantiene invece le righe valide e restituisce quelle respinte.
  bloccanti <- sum(vapply(respinti, function(r) !(senza_segnatura_ok && identical(r$motivo, .MOTIVO_SENZA_SEGNATURA)), TRUE))
  if (modo == "sostituisci" && (bloccanti > 0L || ok == 0L)) {
    dbRollback(con); concluso <- TRUE
    return(list(letti = nrow(d), importati = 0L, sostituzione_annullata = TRUE, date_non_inserite = NULL,
                respinti = if (length(respinti)) do.call(rbind, respinti)
                           else data.frame(riga = integer(0), segnatura = character(0), motivo = character(0), stringsAsFactors = FALSE)))
  }
  
  dbCommit(con)
  concluso <- TRUE
  
  list(letti     = nrow(d),
       importati = ok,
       riga_fondo = riga_fondo,
       date_non_inserite = if (length(date_non_inserite)) do.call(rbind, date_non_inserite) else NULL,
       respinti  = if (length(respinti)) do.call(rbind, respinti)
       else data.frame(riga = integer(0), segnatura = character(0),
                       motivo = character(0), stringsAsFactors = FALSE))
}
