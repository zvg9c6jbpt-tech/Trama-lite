# ─────────────────────────────────────────────────────────────
# backup.R: backup e ripristino in JSON
# Dipende da crud.R (leggi_fascicolo, .riscrivi, .righe_*, .ora)
# L'unico formato che non passa dal profilo Arianna: porta i dati strutturati, extra, flag manuali e timestamp,
# ma non la lista dei luoghi personale, la storia delle modifiche né le impostazioni (per quelli c'è la copia completa .zip).
# Il piano di busta non è una scheda: sta accanto ai fascicoli e non li tocca al ripristino.
# ─────────────────────────────────────────────────────────────

library(jsonlite)

VERSIONE_BACKUP <- 2L

# Un fascicolo (forma di leggi_fascicolo) per il JSON: i campi multivalore restano array anche con 0 o 1 elementi (auto_unbox li ridurrebbe a stringa).
.fascicolo_per_json <- function(f) {
  f$reati      <- as.list(f$reati)
  f$persone    <- as.list(f$persone)
  f$luoghi     <- as.list(f$luoghi)
  f$lingue     <- as.list(f$lingue)
  f$produttori <- as.list(f$produttori)
  f$extra      <- lapply(f$extra, as.list)
  f
}

# L'inverso: dal JSON (letto con simplifyVector = FALSE, quindi tutto
# liste) alla forma che salva_fascicolo/.riscrivi si aspettano.
.fascicolo_da_json <- function(x) {
  scalare <- function(v) if (is.null(v)) NA else v[[1]]

  imp <- x$imputati
  x$imputati <- if (length(imp) == 0) {
    data.frame(esteso = character(), indicizzato = character(), stringsAsFactors = FALSE)
  } else {
    data.frame(
      esteso      = vapply(imp, function(r) as.character(scalare(r$esteso)),      character(1)),
      indicizzato = vapply(imp, function(r) as.character(scalare(r$indicizzato)), character(1)),
      stringsAsFactors = FALSE
    )
  }

  x$reati      <- as.character(unlist(x$reati))
  x$persone    <- as.character(unlist(x$persone))
  x$luoghi     <- as.character(unlist(x$luoghi))
  x$lingue     <- as.character(unlist(x$lingue))
  x$produttori <- as.character(unlist(x$produttori))
  x$extra      <- lapply(x$extra, function(v) as.character(unlist(v)))

  campi_scalari <- c("id", "archivio", "fondo", "serie", "altra_segnatura", "busta", "fascicolo",
                     "titolo", "titolo_manuale", "tipo_titolo", "connettore", "data_nd", "non_consultabile", "soggettario_discorde", "fascicolo_suffisso", "busta_suffisso",
                     "sigla_busta", "sigla_fascicolo",
                     "estremo_remoto", "estremo_recente", "definizione",
                     "supporto", "tipologia_documentaria",
                     "luogo_conservazione", "luogo_produzione", "note", "luogo_reato", "tortura",
                     "tortura_esaminata", "sentenza", "sentenza_esaminata", "attribuzione",
                     "attribuzione_luoghi", "imputati_esaminati", "reato_ignoto", "luogo_ignoto", "non_procedimento", "identita_imputato_dubbia", "regesto",
                     "regesto_manuale", "creato", "modificato")
  for (k in campi_scalari) x[[k]] <- scalare(x[[k]])

  x
}


# --- export ---------------------------------------------------------

# Una scheda persona con varianti e id assorbiti, per il JSON: array sempre array.
.scheda_per_json <- function(con, s) {
  s$varianti  <- as.list(dbGetQuery(con,
    "SELECT variante FROM persona_varianti WHERE scheda_id = ? ORDER BY variante COLLATE NOCASE",
    list(s$id))$variante)
  s$assorbiti <- as.list(dbGetQuery(con,
    "SELECT id_assorbito FROM persona_assorbiti WHERE scheda_id = ?", list(s$id))$id_assorbito)
  s
}

.scheda_da_json <- function(x) {
  scalare <- function(v) if (is.null(v)) NA else v[[1]]
  list(
    id             = scalare(x$id),
    forma_canonica = scalare(x$forma_canonica),
    note           = scalare(x$note),
    creato         = scalare(x$creato),
    modificato     = scalare(x$modificato),
    varianti       = as.character(unlist(x$varianti)),
    assorbiti      = as.character(unlist(x$assorbiti))
  )
}

# Tutti i fascicoli, i piani di busta e le schede persona, con la stessa lettura della maschera (leggi_fascicolo): ciò che la maschera mostra finisce nel backup.
backup_json <- function(con) .atomico(con, .backup_json(con))

.backup_json <- function(con) {
  ids <- dbGetQuery(con, "SELECT id FROM fascicoli ORDER BY fondo, busta, busta_suffisso, fascicolo, fascicolo_suffisso")$id
  fascicoli <- lapply(unname(leggi_fascicoli(con, ids)), .fascicolo_per_json)   # una lettura per tabella, non dieci per scheda (leggi_fascicoli)
  piani <- dbGetQuery(con, "SELECT fondo_id, busta, busta_suffisso, primo, ultimo, creato FROM piani_busta ORDER BY fondo_id, busta, busta_suffisso")

  schede_grezze <- dbGetQuery(con, "SELECT * FROM schede_persona ORDER BY forma_canonica COLLATE NOCASE")
  schede <- lapply(seq_len(nrow(schede_grezze)),
                   function(i) .scheda_per_json(con, as.list(schede_grezze[i, ])))

  # Le coppie dichiarate nella maschera (editor dal 18/09).
  persona_reati <- dbGetQuery(con, "SELECT fascicolo_id, persona, reato FROM persona_reati")
  # Chi è chi: le identità dichiarate sugli omonimi (TODO 20).
  persona_identita <- dbGetQuery(con,
    "SELECT fascicolo_id, persona, scheda_id, fondamento, creato FROM persona_identita")
  reato_luoghi  <- dbGetQuery(con, "SELECT fascicolo_id, reato, luogo FROM reato_luoghi")
  reato_termini <- dbGetQuery(con, "SELECT fascicolo_id, reato, termine FROM reato_termini")

  list(
    formato        = "trama-backup",
    versione       = VERSIONE_BACKUP,
    esportato_il   = .ora(),
    # Di chi sono i dati e con quale licenza (5/10). Chi legge il backup lo ignora.
    attribuzione   = c(attribuzione_dati(con), list(riga = riga_attribuzione(attribuzione_dati(con)), licenza = LICENZA_DATI$nome, licenza_uri = LICENZA_DATI$uri)),
    fascicoli      = fascicoli,
    piani_busta    = piani,
    schede_persona = schede,
    persona_reati  = persona_reati,
    reato_luoghi   = reato_luoghi,
    reato_termini  = reato_termini,       # assente nei backup di prima del tesauro (17/11/2026)
    persona_identita = persona_identita,
    # Verifiche d'omonimia (TODO 20): decisioni, non derivabili dai dati.
    omonimie_verificate = dbGetQuery(con, "SELECT persona, esito, fascicoli, creato FROM omonimie_verificate"),
    coorti = dbGetQuery(con, "SELECT nome, query, creato FROM coorti ORDER BY nome"),
    ordine_fascicoli = dbGetQuery(con, "SELECT fascicolo_id, posizione FROM ordine_fascicoli"),
    # Vocabolario per fondo e varianti registrate (7/10); assenti nei backup di prima.
    vocabolario_fondo = dbGetQuery(con, "SELECT fondo_id, campo, valore, forma, creato FROM vocabolario_fondo ORDER BY fondo_id, campo, valore"),
    # I fondi con il loro id interno (R/fondi.R); fascicoli, piani e vocabolario li richiamano per id. I backup vecchi hanno `archivi` (per nome): si leggono ancora.
    fondi = dbGetQuery(con, "SELECT id, archivio, denominazione, serie, profilo, creato, dichiarato, fonte FROM fondi ORDER BY denominazione, archivio")
  )
}

esporta_backup_json <- function(con, percorso) {
  scrivi <- toJSON(backup_json(con), auto_unbox = TRUE, pretty = TRUE, na = "null")
  writeLines(scrivi, percorso, useBytes = TRUE)
  invisible(percorso)
}


# --- ripristino -------------------------------------------------------

# Tre modi in cui un file non è un backup, detti per quello che sono (non l'errore del lettore JSON).
leggi_backup_json <- function(percorso) {
  testo <- paste(readLines(percorso, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  dati <- try(fromJSON(testo, simplifyVector = FALSE), silent = TRUE)
  if (inherits(dati, "try-error"))
    stop("Questo file non si legge fino in fondo: \u00e8 interrotto o danneggiato ",
         "(capita con un download a met\u00e0, o con un file aperto e salvato da un ",
         "altro programma). Il database non \u00e8 stato toccato. Prova con un altro ",
         "backup .json. Le copie quotidiane nella cartella backup/ dei dati di Trama sono il database ",
         "intero: per usarne una, chiudi l'app e mettila al posto di trama.sqlite (stessa cartella).",
         "\n\nErrore tecnico: ", conditionMessage(attr(dati, "condition")), call. = FALSE)
  if (!is.list(dati) || is.null(dati$fascicoli) || is.null(dati$versione))
    stop("Questo file \u00e8 un JSON, ma non un backup di Trama: gli mancano ",
         "i fascicoli o la versione. Il backup si scarica da \u00abScarica backup ",
         "(.json)\u00bb qui accanto, e il suo nome comincia con trama-backup- ",
         "(o datatrama-backup- / regestum-backup-, se \u00e8 di prima del 16/10). Il database non ",
         "\u00e8 stato toccato.", call. = FALSE)
  if (!is.null(dati$formato) && !dati$formato %in% c("trama-backup", "datatrama-backup"))   # il secondo è il nome di prima del 31/10/2026
    stop("Formato del backup non riconosciuto. Il database non è stato toccato.", call. = FALSE)
  if (length(dati$versione) != 1 || !is.numeric(dati$versione) ||
      !(dati$versione[[1]] %in% seq_len(VERSIONE_BACKUP)))
    stop(sprintf("Backup di una versione che questa app non conosce (%s; qui si legge la %s). Il database non \u00e8 stato toccato.",
                 as.character(dati$versione[[1]]), VERSIONE_BACKUP), call. = FALSE)
  electron <- !is.null(dati$esportatoIl) && is.null(dati$esportato_il)
  if (!electron && dati$versione == 2) {
    richieste <- c("fascicoli", "piani_busta", "schede_persona", "persona_reati", "reato_luoghi",
                   "persona_identita", "omonimie_verificate", "coorti", "ordine_fascicoli")
    mancanti <- richieste[vapply(richieste, function(k) is.null(dati[[k]]), logical(1))]
    if (length(mancanti)) stop("Backup incompleto: mancano le sezioni ", paste(mancanti, collapse=", "),
                              ". Il database non è stato toccato.", call. = FALSE)
  }
  .valida_struttura_backup(dati)
  if (!is.null(dati$esportatoIl) && is.null(dati$esportato_il))
    dati <- .converti_backup_electron(dati)
  dati$fascicoli <- lapply(dati$fascicoli, .fascicolo_da_json)

  p <- dati$piani_busta
  dati$piani_busta <- if (length(p) == 0) {
    data.frame(fondo_id = character(), fondo = character(), busta = integer(), busta_suffisso = character(), primo = integer(),
               ultimo = integer(), creato = character(), stringsAsFactors = FALSE)
  } else {
    data.frame(
      fondo_id = vapply(p, function(r) if (.vuoto(r$fondo_id[[1]])) NA_character_ else as.character(r$fondo_id[[1]]), character(1)),
      fondo  = vapply(p, function(r) if (.vuoto(r$fondo[[1]])) NA_character_ else as.character(r$fondo[[1]]), character(1)),   # backup di prima: per nome
      busta  = vapply(p, function(r) as.integer(r$busta[[1]]),             integer(1)),
      busta_suffisso = vapply(p, function(r) if (.vuoto(r$busta_suffisso[[1]])) "" else as.character(r$busta_suffisso[[1]]), character(1)),
      primo  = vapply(p, function(r) as.integer(r$primo[[1]]),             integer(1)),
      ultimo = vapply(p, function(r) as.integer(r$ultimo[[1]]),            integer(1)),
      creato = vapply(p, function(r) as.character(r$creato[[1]]),          character(1)),
      stringsAsFactors = FALSE
    )
  }

  pr <- dati$persona_reati
  dati$persona_reati <- if (length(pr) == 0) {
    data.frame(fascicolo_id = character(), persona = character(), reato = character(),
               stringsAsFactors = FALSE)
  } else {
    data.frame(
      fascicolo_id = vapply(pr, function(r) as.character(r$fascicolo_id[[1]]), character(1)),
      persona      = vapply(pr, function(r) as.character(r$persona[[1]]),      character(1)),
      reato        = vapply(pr, function(r) as.character(r$reato[[1]]),        character(1)),
      stringsAsFactors = FALSE
    )
  }

  rt <- dati$reato_termini
  dati$reato_termini <- if (length(rt) == 0) {
    data.frame(fascicolo_id = character(), reato = character(), termine = character(), stringsAsFactors = FALSE)
  } else {
    data.frame(
      fascicolo_id = vapply(rt, function(r) as.character(r$fascicolo_id[[1]]), character(1)),
      reato        = vapply(rt, function(r) as.character(r$reato[[1]]),        character(1)),
      termine      = vapply(rt, function(r) as.character(r$termine[[1]]),      character(1)),
      stringsAsFactors = FALSE
    )
  }

  rl <- dati$reato_luoghi
  dati$reato_luoghi <- if (length(rl) == 0) {
    data.frame(fascicolo_id = character(), reato = character(), luogo = character(),
               stringsAsFactors = FALSE)
  } else {
    data.frame(
      fascicolo_id = vapply(rl, function(r) as.character(r$fascicolo_id[[1]]), character(1)),
      reato        = vapply(rl, function(r) as.character(r$reato[[1]]),        character(1)),
      luogo        = vapply(rl, function(r) as.character(r$luogo[[1]]),        character(1)),
      stringsAsFactors = FALSE
    )
  }

  pi <- dati$persona_identita
  dati$persona_identita <- if (length(pi) == 0) {
    data.frame(fascicolo_id = character(), persona = character(), scheda_id = character(),
               fondamento = character(), creato = character(), stringsAsFactors = FALSE)
  } else {
    campo <- function(k) vapply(pi, function(r) as.character(r[[k]][[1]]), character(1))
    data.frame(fascicolo_id = campo("fascicolo_id"), persona = campo("persona"),
               scheda_id = campo("scheda_id"), fondamento = campo("fondamento"),
               creato = campo("creato"), stringsAsFactors = FALSE)
  }

  # Assente nei backup scritti prima del 18/09: nessuna verifica.
  ov <- dati$omonimie_verificate
  dati$omonimie_verificate <- if (length(ov) == 0) {
    data.frame(persona = character(), esito = character(), fascicoli = character(), creato = character(),
               stringsAsFactors = FALSE)
  } else {
    campo <- function(k) vapply(ov, function(r) as.character(r[[k]][[1]]), character(1))
    data.frame(persona = campo("persona"), esito = campo("esito"), fascicoli = campo("fascicoli"),
               creato = campo("creato"), stringsAsFactors = FALSE)
  }

  dati$schede_persona <- lapply(dati$schede_persona, .scheda_da_json)
  dati
}

# Scrive un fascicolo completo (id, creato, modificato) così com'è: non ricalcola i timestamp. Sempre INSERT: un id già presente non si sovrascrive in silenzio.
.inserisci_fascicolo_backup <- function(con, f) .atomico(con, .scrivi_fascicolo_backup(con, f))

.scrivi_fascicolo_backup <- function(con, f) {
  f <- .riporta_luogo_reato(f)
  campi <- c("id", "archivio", "fondo", "fondo_id", "serie", "altra_segnatura", "busta", "fascicolo", "titolo",
             "titolo_manuale", "tipo_titolo", "connettore", "estremo_remoto", "estremo_recente", "data_nd", "non_consultabile", "soggettario_discorde", "fascicolo_suffisso", "busta_suffisso", "sigla_busta", "sigla_fascicolo", "definizione",
             "supporto", "tipologia_documentaria", "luogo_conservazione",
             "luogo_produzione", "note", "luogo_reato", "tortura", "tortura_esaminata",
             "sentenza", "sentenza_esaminata", "attribuzione", "attribuzione_luoghi",
             "imputati_esaminati", "reato_ignoto", "luogo_ignoto", "non_procedimento", "identita_imputato_dubbia", "regesto", "regesto_manuale", "creato", "modificato")
  # I backup scritti prima del 18/09 non hanno data_nd: 0, non NULL.
  if (.vuoto(f$imputati_esaminati)) f$imputati_esaminati <- 0L
  # I backup scritti prima del 12/10 non hanno i tre flag sulla fonte: 0, non NULL.
  for (k in c("reato_ignoto", "luogo_ignoto", "non_procedimento")) if (.vuoto(f[[k]])) f[[k]] <- 0L
  if (.vuoto(f$data_nd)) f$data_nd <- 0L
  if (.vuoto(f$non_consultabile)) f$non_consultabile <- 0L
  if (.vuoto(f$fascicolo_suffisso)) f$fascicolo_suffisso <- ""
  if (.vuoto(f$busta_suffisso)) f$busta_suffisso <- ""            # i backup di prima dell'8/10 non l'hanno
  if (.vuoto(f$soggettario_discorde)) f$soggettario_discorde <- 0L
  f$fondo_id <- fondo_id_da(con, f$archivio, f$fondo, crea = TRUE)   # stesso (archivio, denominazione) = stesso id: i fondi del backup sono già stati scritti
  valori <- lapply(campi, function(k) f[[k]])

  dbExecute(con, paste0(
    "INSERT INTO fascicoli (", paste(campi, collapse = ", "), ") ",
    "VALUES (", paste(rep("?", length(campi)), collapse = ", "), ")"),
    params = valori)

  .riscrivi(con, "imputati",    f$id, .righe_imputati(f$imputati))
  .riscrivi(con, "reati",       f$id, .righe_ordinate(f$reati,      "reato"))
  .riscrivi(con, "persone",     f$id, .righe_ordinate(f$persone,    "persona"))
  .riscrivi(con, "luoghi",      f$id, .righe_ordinate(f$luoghi,     "luogo"))
  .riscrivi(con, "lingue",      f$id, .righe_ordinate(f$lingue,     "lingua"))
  .riscrivi(con, "produttori",  f$id, .righe_ordinate(f$produttori, "soggetto"))
  .riscrivi(con, "campi_extra", f$id, .righe_extra(f$extra))
  invisible(f$id)
}

# Come per le schede persona: id, creato e modificato così come sono, sempre INSERT.
.inserisci_scheda_backup <- function(con, s) .atomico(con, .scrivi_scheda_backup(con, s))

.scrivi_scheda_backup <- function(con, s) {
  dbExecute(con,
    "INSERT INTO schede_persona (id, forma_canonica, note, creato, modificato) VALUES (?, ?, ?, ?, ?)",
    list(s$id, s$forma_canonica, s$note, s$creato, s$modificato))
  for (v in s$varianti)
    dbExecute(con, "INSERT INTO persona_varianti (scheda_id, variante) VALUES (?, ?)", list(s$id, v))
  for (a in s$assorbiti)
    dbExecute(con, "INSERT INTO persona_assorbiti (scheda_id, id_assorbito) VALUES (?, ?)", list(s$id, a))
  invisible(s$id)
}

# `modo = "sostituisci"` cancella prima fascicoli e piani di busta (cascata sulle figlie); `"accoda"` inserisce solo il nuovo, un id già presente va fra i respinti.
# Sempre in una transazione: o entra tutto o niente. Verifica strutturale prima della conversione: mai inventare UUID né troncare array di campi scalari.
.valida_struttura_backup <- function(d) {
  array <- function(x, nome) {
    if (!is.list(x) || (length(x) && !is.null(names(x))))
      stop(nome, ": atteso un elenco di record.", call. = FALSE)
    for (r in x) if (!is.list(r) || is.null(names(r)))
      stop(nome, ": record non valido.", call. = FALSE)
  }
  for (k in c("fascicoli", "schede_persona", "schedePersona", "piani_busta",
              "persona_reati", "reato_luoghi", "reato_termini", "persona_identita", "omonimie_verificate",
              "coorti", "ordine_fascicoli", "vocabolario_fondo"))
    if (!is.null(d[[k]])) array(d[[k]], k)
  for (k in c("piani_busta", "persona_reati", "reato_luoghi", "reato_termini", "persona_identita",
              "omonimie_verificate", "coorti", "ordine_fascicoli", "vocabolario_fondo"))
    for (r in d[[k]]) for (campo in names(r))
      if (!is.null(r[[campo]]) && (is.list(r[[campo]]) || length(r[[campo]]) != 1))
        stop(k, ": il campo ", campo, " deve avere un solo valore.", call. = FALSE)
  for (k in c("fascicoli", "schede_persona", "schedePersona")) {
    for (i in seq_along(d[[k]])) {
      r <- d[[k]][[i]]
      if (!is.character(r$id) || length(r$id) != 1 || !nzchar(trimws(r$id)))
        stop(sprintf("%s, record %d: identificativo mancante o non valido.", k, i), call. = FALSE)
      if (k == "fascicoli") {
        for (campo in c("busta", "fascicolo", "estremo_remoto", "estremo_recente", "estremoRemoto", "estremoRecente")) {
          v <- r[[campo]]
          if (!is.null(v) && (!is.numeric(v) || length(v) != 1 || !is.finite(v) || v != floor(v)))
            stop("Fascicolo ", r$id, ": ", campo, " deve essere un numero intero o null.", call. = FALSE)
        }
        for (campo in c("reati", "persone", "luoghi", "lingue", "produttori")) {
          # In Electron 'persone' è invece la mappa dei legami, conservata negli extra.
          if (campo == "persone" && !is.null(d$esportatoIl)) next
          v <- r[[campo]]
          if (!is.null(v) && (is.null(names(v)) == FALSE ||
              !all(vapply(as.list(v), function(z) is.character(z) && length(z) == 1, logical(1)))))
            stop("Fascicolo ", r$id, ": ", campo, " deve essere un elenco di testi.", call. = FALSE)
        }
      }
      multipli <- c("imputati", "reati", "persone", "luoghi", "lingue", "produttori", "extra",
                    "varianti", "assorbiti", "idAssorbiti", "soggettoProduttore", "lingua", "tipoReato",
                    "personeCitate", "luoghiCitati", "campiOriginali", "esaminati", "certezze",
                    "ruoliPersone", "legamiReati", "persona_reati", "reato_luoghi", "reato_termini")
      for (campo in setdiff(names(r), multipli))
        if (!is.null(r[[campo]]) && (is.list(r[[campo]]) || length(r[[campo]]) != 1))
          stop(sprintf("%s, record %d: il campo %s deve avere un solo valore.", k, i, campo), call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Le copie «prima di un'azione» (trama-prima-<motivo>-…) si accumulavano: si tengono le ultime `tieni`. Manuali, prima di una cancellazione,
# giornaliere e periodiche hanno la loro rotazione. Non fallisce mai: un file che non si cancella resta.
ruota_copie_prima <- function(cartella = cartella_backup(), tieni = 20L, tieni_gravi = 10L, giorni_gravi = 90) {
  if (!dir.exists(cartella)) return(invisible(0L))
  f <- list.files(cartella, pattern = "^(trama|regestum)-prima-.*\\.sqlite$", full.names = TRUE)
  # Quelle prima di una cancellazione (fondo eliminato, svuotamento, sostituzione) sono l'unico modo di tornare indietro: hanno un conto più largo, sotto.
  f <- f[!grepl("^(trama|regestum)-prima-(manuale|eliminazione|svuotamento|sostituzione)", basename(f))]
  vecchie <- character(0)
  if (length(f) > tieni) {
    f <- f[order(file.mtime(f), decreasing = TRUE)]
    vecchie <- f[-seq_len(tieni)]
  }
  # Anche quelle prima di un'azione grave non crescono senza fine: per motivo si tengono le ultime `tieni_gravi`, e si tolgono solo le più vecchie di `giorni_gravi` giorni.
  gravi <- list.files(cartella, pattern = "^(trama|regestum)-prima-(manuale|eliminazione|svuotamento|sostituzione).*\\.sqlite$", full.names = TRUE)
  if (length(gravi)) {
    motivo <- sub("^(trama|regestum)-prima-(manuale|eliminazione|svuotamento|sostituzione).*$", "\\2", basename(gravi))
    for (m in unique(motivo)) {
      g <- gravi[motivo == m]; g <- g[order(file.mtime(g), decreasing = TRUE)]
      if (length(g) > tieni_gravi) {
        extra <- g[-seq_len(tieni_gravi)]
        vecchie <- c(vecchie, extra[difftime(Sys.time(), file.mtime(extra), units = "days") > giorni_gravi])
      }
    }
  }
  if (!length(vecchie)) return(invisible(0L))
  ok <- vapply(vecchie, function(x) isTRUE(suppressWarnings(file.remove(x))), TRUE)
  if (any(ok)) registra("INFO", "copie di sicurezza ruotate", sprintf("%d tolte, ultime %d tenute", sum(ok), tieni))
  invisible(sum(ok))
}

# Copia completa con API SQLite, unica per ogni ripristino: la copia quotidiana
# può essere precedente agli ultimi salvataggi. Se fallisce, non si procede.
backup_prima_ripristino <- function(con, cartella, motivo = "ripristino") {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  percorso <- file.path(cartella, paste0("trama-prima-", motivo, "-",
    format(Sys.time(), "%Y%m%d-%H%M%S"), "-", uuid::UUIDgenerate(), ".sqlite"))
  copia <- dbConnect(RSQLite::SQLite(), percorso)
  ok <- FALSE
  on.exit({ dbDisconnect(copia); if (!ok) unlink(percorso) }, add = TRUE)
  RSQLite::sqliteCopyDatabase(con, copia)
  if (!identical(dbGetQuery(copia, "PRAGMA integrity_check")[[1]], "ok"))
    stop("La copia di sicurezza non è valida: ripristino interrotto.")
  ok <- TRUE
  registra("INFO", "copia di sicurezza", sprintf("%s: %s", motivo, basename(percorso)))
  try(ruota_copie_prima(cartella), silent = TRUE)
  percorso
}

# Solo i conflitti già individuati in Accoda sono saltabili. Ogni altro errore
# annulla TUTTE le scritture, compresi schede persona e collegamenti.
.ripristina_dati_json <- function(con, dati, modo) .atomico(con, {
  if (modo == "sostituisci") {
    for (t in c("fascicoli", "piani_busta", "coorti", "persona_identita",
                "schede_persona", "omonimie_verificate", "fondi", "vocabolario_fondo"))
      dbExecute(con, paste("DELETE FROM", t))
  }
  dettagli <- data.frame(tipo=character(), riferimento=character(), motivo=character())
  annota <- function(tipo, riferimento, motivo) {
    dettagli[nrow(dettagli) + 1L, ] <<- list(tipo, riferimento, motivo)
  }
  # I fondi per primi: il backup nuovo ha `fondi` con gli id, quello vecchio `archivi`. Se il fondo (archivio, denominazione) c'è già vale l'id di qui; `mappa` porta l'id del backup a quello di qui.
  mappa <- character()
  for (r in c(dati$fondi, dati$archivi)) {
    den <- r$denominazione %||% r$fondo; arc <- r$archivio
    if (.vuoto(den) || .vuoto(arc)) next
    esistente <- fondo_id_da(con, arc, den)
    id_nuovo <- if (!is.null(r$id) && !.vuoto(r$id) && !nrow(dbGetQuery(con, "SELECT 1 FROM fondi WHERE id = ?", list(r$id)))) r$id else NULL
    id <- if (!is.na(esistente)) esistente
          else fondo_id_da(con, arc, den, crea = TRUE, serie = r$serie, profilo = r$profilo, id = id_nuovo,
                           dichiarato = is.null(r$id) || isTRUE(as.integer(r$dichiarato %||% 1L) == 1L))
    # Un fondo creato da qui riprende la data di creazione del backup; uno già presente conserva la sua.
    if (is.na(esistente) && !.vuoto(r$creato)) dbExecute(con, "UPDATE fondi SET creato = ? WHERE id = ?", list(as.character(r$creato), id))
    # La fonte dei dati: un fondo nuovo la riprende dal backup; uno già presente la conserva se ce l'ha.
    if (!.vuoto(r$fonte) && !nzchar(fonte_del_fondo(con, id))) imposta_fonte_fondo(con, id, r$fonte)
    if (!is.null(r$id) && !.vuoto(r$id)) mappa[[r$id]] <- id
  }
  respinti <- character()
  if (modo == "sostituisci") dbExecute(con, "DROP INDEX IF EXISTS idx_segnatura_senza_numeri")     # si rifà a fine ripristino
  for (f in dati$fascicoli) {
    chiave <- fondo_id_da(con, f$archivio, f$fondo)
    conflitto <- dbGetQuery(con, "SELECT id FROM fascicoli WHERE id = ? OR
      (fondo_id IS ? AND busta IS ? AND busta_suffisso = ? AND fascicolo IS ? AND fascicolo_suffisso = ?)",      # IS: senza numero è una segnatura anche lei
      list(f$id, chiave, f$busta %||% NA_integer_, if (.vuoto(f$busta_suffisso)) "" else f$busta_suffisso, f$fascicolo %||% NA_integer_,
           if (.vuoto(f$fascicolo_suffisso)) "" else f$fascicolo_suffisso))$id
    # Un backup può avere doppioni senza numero (prima dell'indice): in «Sostituisci» si riportano, Qualità li elenca; in «Accoda» il secondo è respinto con il motivo.
    senza_numero <- is.null(f$busta) || is.na(f$busta) || is.null(f$fascicolo) || is.na(f$fascicolo)
    if (length(conflitto) && modo == "sostituisci" && (f$id %in% conflitto || !senza_numero))
      stop("Fascicolo ", f$id, ": identificativo o segnatura ripetuti nel backup.", call. = FALSE)
    if (length(conflitto) && modo == "sostituisci") conflitto <- character(0)
    if (length(conflitto) && modo == "accoda") {
      respinti <- c(respinti, f$id)
      annota("Fascicolo", f$id, if (f$id %in% conflitto) "Identificativo già presente" else "Segnatura già occupata")
    } else tryCatch(.inserisci_fascicolo_backup(con, f), error=function(e)
      stop("Fascicolo ", f$id, ": ", conditionMessage(e), call. = FALSE))
  }
  aggiungi <- function(tabella, r, chiavi) {
    where <- paste(paste(chiavi, "IS ?"), collapse=" AND ")
    presente <- nrow(dbGetQuery(con, paste0("SELECT 1 FROM ", tabella, " WHERE ", where), unname(r[chiavi]))) > 0
    if (modo == "accoda" && presente) {
      annota(tabella, paste(unlist(r[chiavi]), collapse=" / "), "Voce già presente: mantenuta la versione attuale")
    } else dbAppendTable(con, tabella, as.data.frame(Filter(Negate(is.null), r), stringsAsFactors=FALSE))
  }
  for (s in dati$schede_persona) {
    if (modo == "accoda" && nrow(dbGetQuery(con, "SELECT 1 FROM schede_persona WHERE id = ?", list(s$id))))
      annota("Persona", s$id, "Identificativo già presente: mantenuta la scheda attuale")
    else tryCatch(.inserisci_scheda_backup(con, s), error=function(e)
      stop("Scheda persona ", s$id, ": ", conditionMessage(e), call. = FALSE))
  }
  # Il fondo di una riga di piano o vocabolario: l'id del backup (mappato), o per nome nei backup vecchi.
  fondo_della_riga <- function(r) {
    id <- if (!.vuoto(r$fondo_id) && !is.na(mappa[r$fondo_id])) unname(mappa[r$fondo_id]) else NA_character_
    if (is.na(id) && !.vuoto(r$fondo)) id <- .fondo_per_nome(con, r$fondo)
    if (is.na(id)) stop("Il fondo di una riga del backup non si trova (", r$fondo_id %||% r$fondo %||% "?", ").", call. = FALSE)
    id
  }
  tabelle <- list(piani_busta=c("fondo_id", "busta", "busta_suffisso"), persona_reati=c("fascicolo_id", "persona", "reato"),
    reato_luoghi=c("fascicolo_id", "reato", "luogo"), reato_termini=c("fascicolo_id", "reato", "termine"), persona_identita=c("fascicolo_id", "persona"),
    omonimie_verificate="persona", coorti="nome", ordine_fascicoli="fascicolo_id",
    vocabolario_fondo=c("fondo_id", "campo", "valore"))
  for (t in names(tabelle)) {
    x <- dati[[t]]
    righe <- if (is.data.frame(x)) lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop=FALSE])) else x
    for (r in righe) {
      if (!is.null(r$fascicolo_id) && r$fascicolo_id %in% respinti) next
      if (t %in% c("piani_busta", "vocabolario_fondo")) { r$fondo_id <- fondo_della_riga(r); r$fondo <- NULL }
      aggiungi(t, r, tabelle[[t]])
    }
  }
  if (nrow(dbGetQuery(con, "PRAGMA foreign_key_check"))) stop("Collegamenti non validi nel database.")
  if (modo == "sostituisci") {
    if (!.indice_senza_numeri(con))
      annota("Segnature", "senza numero", "Ci sono segnature doppie senza numero: restano come nel backup, Qualità le elenca")
  }
  list(letti=length(dati$fascicoli), importati=length(dati$fascicoli)-length(respinti),
       respinti=respinti, dettagli=dettagli)
})

ripristina_backup_json <- function(con, percorso, modo = c("accoda", "sostituisci"), cartella_sicurezza = NULL) {
  modo <- match.arg(modo)
  dati <- leggi_backup_json(percorso)
  # Il file intero deve poter vivere da solo: anche i record che sarebbero
  # duplicati in destinazione vengono verificati, senza modificare dati reali.
  prova <- apri_db(":memory:")
  on.exit(dbDisconnect(prova), add = TRUE)
  tryCatch(.ripristina_dati_json(prova, dati, "sostituisci"), error=function(e)
    stop("Backup non ripristinabile. Il database non è stato toccato.\n", conditionMessage(e), call. = FALSE))
  copia <- if (!is.null(cartella_sicurezza)) backup_prima_ripristino(con, cartella_sicurezza) else NULL
  esito <- tryCatch(.ripristina_dati_json(con, dati, modo), error=function(e)
    stop("Ripristino annullato: nessuna modifica applicata.\n", conditionMessage(e), call. = FALSE))
  esito$backup_sicurezza <- copia
  esito
}


# --- backup automatico del database ---------------------------
# All'avvio, una copia del .sqlite al giorno in `cartella` (trama-AAAA-MM-GG.sqlite), tenute le ultime `tieni`.
# Con l'API di backup di SQLite (sqliteCopyDatabase), non file.copy: a connessione aperta il file potrebbe uscire incoerente.
# Non sostituisce il backup JSON (leggibile e portabile): è il paracadute contro un database rovinato.
backup_automatico <- function(con, cartella = cartella_backup(), tieni = 14, oggi = Sys.Date()) {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(cartella, sprintf("trama-%s.sqlite", format(oggi, "%Y-%m-%d")))
  fatto <- FALSE
  if (!file.exists(dest)) {
    # Un fallimento non si perde più in silenzio (2/10): torna come `errore`
    # e la striscia del backup lo dice.
    esito <- tryCatch({ .copia_sqlite(con, dest); NULL }, error = function(e) .spiega_errore_copia(conditionMessage(e)))
    if (!is.null(esito)) {
      registra("ERRORE", "copia giornaliera non riuscita", esito)
      return(invisible(list(percorso = NULL, nuovo = FALSE, errore = esito)))
    }
    fatto <- TRUE
    registra("INFO", "copia giornaliera", basename(dest))
  }
  tutti <- .ordina_copie(list.files(cartella, pattern = "^(trama|regestum)-\\d{4}-\\d{2}-\\d{2}\\.sqlite$", full.names = TRUE))
  if (length(tutti) > tieni) unlink(tutti[-seq_len(tieni)])
  invisible(list(percorso = dest, nuovo = fatto, errore = NULL))
}

# Il motivo di una copia non riuscita, in parole: i messaggi di SQLite sono in
# inglese e dicono «unable to open database file» quando la cartella non si scrive.
.spiega_errore_copia <- function(m) {
  if (grepl("full|no space|disk", m, ignore.case = TRUE)) "il disco è pieno"
  else if (grepl("unable to open|permission|denied|read-only|readonly|Could not connect", m, ignore.case = TRUE))
    "non riesco a scrivere nella cartella dei backup (permessi, o la cartella non esiste più)"
  else m
}

# Copia coerente di un database aperto e controllo di integrità: se qualcosa non va il file parziale si toglie e l'errore sale.
.copia_sqlite <- function(con, dest) {
  copia <- DBI::dbConnect(RSQLite::SQLite(), dest)
  ok <- FALSE
  on.exit({ try(DBI::dbDisconnect(copia), silent = TRUE); if (!ok) unlink(dest) }, add = TRUE)
  RSQLite::sqliteCopyDatabase(con, copia)
  if (!identical(DBI::dbGetQuery(copia, "PRAGMA integrity_check")[[1]], "ok"))
    stop("la copia non ha superato il controllo di integrità")
  ok <- TRUE
  invisible(dest)
}

# Copie periodiche durante il lavoro: se dall'ultima copia il database è cambiato e sono passati `minimo_minuti`, se ne fa un'altra (trama-AAAA-MM-GG-HHMM.sqlite),
# tenendo le ultime `tieni`. Il cambiamento si legge dall'orario del file del database (e del suo -wal).
backup_periodico <- function(con, cartella = cartella_backup(), minimo_minuti = 30, tieni = 20, ora = Sys.time()) {
  db <- DBI::dbGetInfo(con)$dbname
  if (!nzchar(db) || identical(db, ":memory:") || !file.exists(db))
    return(invisible(list(percorso = NULL, nuovo = FALSE, errore = NULL)))
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  modifica <- max(file.mtime(Filter(file.exists, c(db, paste0(db, "-wal")))))
  copie <- elenco_copie(cartella)
  ultima <- if (nrow(copie)) max(copie$quando) else as.POSIXct(0, origin = "1970-01-01")
  if (nrow(copie) && !(modifica > ultima && as.numeric(difftime(ora, ultima, units = "mins")) >= minimo_minuti))
    return(invisible(list(percorso = NULL, nuovo = FALSE, errore = NULL)))
  dest <- file.path(cartella, sprintf("trama-%s.sqlite", format(ora, "%Y-%m-%d-%H%M")))
  esito <- tryCatch({ .copia_sqlite(con, dest); NULL }, error = function(e) .spiega_errore_copia(conditionMessage(e)))
  if (!is.null(esito)) return(invisible(list(percorso = NULL, nuovo = FALSE, errore = esito)))
  periodiche <- .ordina_copie(list.files(cartella, pattern = "^(trama|regestum)-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.sqlite$", full.names = TRUE))
  if (length(periodiche) > tieni) unlink(periodiche[-seq_len(tieni)])
  invisible(list(percorso = dest, nuovo = TRUE, errore = NULL))
}

# Il prefisso delle copie era «regestum-», ora è «trama-»: le vecchie si riconoscono e si ruotano come le nuove. Ordine per data, la più recente prima.
.ordina_copie <- function(f) f[order(sub("^(trama|regestum)-", "", basename(f)), decreasing = TRUE)]

# Le copie presenti, con il loro tipo e quando sono state fatte.
elenco_copie <- function(cartella = cartella_backup()) {
  f <- list.files(cartella, pattern = "^(trama|regestum)-.*\\.sqlite$", full.names = TRUE)
  b <- basename(f)
  tipo <- ifelse(grepl("^(trama|regestum)-\\d{4}-\\d{2}-\\d{2}\\.sqlite$", b), "automatica del giorno",
          ifelse(grepl("^(trama|regestum)-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.sqlite$", b), "periodica",
          ifelse(grepl("^(trama|regestum)-prima-manuale-", b), "manuale", "prima di un'azione")))
  data.frame(file = f, tipo = tipo, quando = if (length(f)) file.mtime(f) else as.POSIXct(character(0)),
             stringsAsFactors = FALSE)
}

# Che cosa dice la striscia del backup: `errore` (ultimo tentativo fallito) viene prima di tutto; una copia più vecchia di `ore_limite` è «vecchia» (rosso).
stato_backup <- function(copie, ora = Sys.time(), errore = NULL, ore_limite = 36) {
  n <- nrow(copie)
  dettaglio <- if (n == 0) "Nessuna copia nella cartella dei backup."
    else if (n == 1) sprintf("Una copia conservata: è %s, del %s.", copie$tipo[1], format(copie$quando[1], "%d/%m/%Y"))
    else sprintf("%d copie conservate; l'ultima è %s; la più vecchia è del %s.", n,
                 copie$tipo[which.max(copie$quando)], format(min(copie$quando), "%d/%m/%Y"))
  # `parti`: le frasi una per una, perché l'interfaccia le mette ciascuna nel suo
  # elemento (il traduttore dello schermo confronta il testo di un nodo intero).
  fai <- function(livello, parti) list(livello = livello, testo = paste(parti, collapse = " "), parti = parti, dettaglio = dettaglio)
  if (!is.null(errore) && nzchar(errore))
    return(fai("errore", c("L'ultima copia non è riuscita:", errore)))
  if (n == 0)
    return(fai("nessuna", "Nessuna copia di sicurezza ancora"))
  ultima <- max(copie$quando)
  if (as.numeric(difftime(ora, ultima, units = "hours")) > ore_limite)
    return(fai("vecchia", c("Ultima copia", .tempo_fa(ultima, ora), "È passato troppo tempo.")))
  fai("ok", c("Ultima copia", .tempo_fa(ultima, ora)))
}


# L'ultimo backup automatico presente, per dirlo nell'interfaccia.
ultimo_backup_automatico <- function(cartella = cartella_backup()) {
  f <- .ordina_copie(list.files(cartella, pattern = "^(trama|regestum)-\\d{4}-\\d{2}-\\d{2}\\.sqlite$", full.names = TRUE))
  if (length(f)) f[1] else NULL
}


# «3 minuti fa», «2 ore fa», «ieri alle 18:05», «il 28/09»: per la striscia
# del backup.
.tempo_fa <- function(quando, ora = Sys.time()) {
  min <- as.numeric(difftime(ora, quando, units = "mins"))
  giorni <- as.integer(as.Date(format(ora, "%Y-%m-%d")) - as.Date(format(quando, "%Y-%m-%d")))
  m <- round(min); h <- round(min / 60)
  if (min < 1) "adesso"
  else if (m == 1) "1 minuto fa"
  else if (min < 60) sprintf("%d minuti fa", m)
  else if (giorni == 0 && h == 1) "1 ora fa"
  else if (giorni == 0) sprintf("%d ore fa", h)
  else if (giorni == 1) sprintf("ieri alle %s", format(quando, "%H:%M"))
  else sprintf("il %s", format(quando, "%d/%m"))
}


# --- copia completa ---------------------------------------------
# Un .zip con tutto per ricominciare su un altro computer: il database (copia controllata), lo stesso contenuto in JSON, la lista dei luoghi personale (CSV, vedi percorsi.R) e un LEGGIMI.
.LEGGIMI_COMPLETO <- c(
  "Copia completa di Trama Lite",
  "",
  "Contenuto:",
  "  trama.sqlite                il database (schede, persone, luoghi, archivi, coorti)",
  "  trama-backup.json           lo stesso contenuto in JSON leggibile (formato di scambio)",
  "  luoghi/luoghi.csv           la lista dei luoghi personale",
  "",
  "Per ripristinare: chiudi Trama, poi usa ripristina_backup_completo() (R/backup.R)",
  "oppure copia a mano trama.sqlite in Documenti/Trama Lite e luoghi.csv in",
  "Documenti/Trama Lite/luoghi. Prima di sostituire, salva i file che ci sono.",
  "Il JSON si può anche importare da Import / Export, in un database vuoto o esistente."
)

backup_completo <- function(con, zipfile, cart_luoghi = cartella_luoghi()) {
  tmp <- tempfile("completo-"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  .copia_sqlite(con, file.path(tmp, NOME_DB))
  esporta_backup_json(con, file.path(tmp, "trama-backup.json"))
  gz <- file.path(cart_luoghi, "luoghi.csv")
  if (file.exists(gz)) {
    dir.create(file.path(tmp, "luoghi"))
    file.copy(gz, file.path(tmp, "luoghi", "luoghi.csv"))
  }
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM fascicoli")$n
  att <- attribuzione_dati(con); riga <- riga_attribuzione(att)
  writeLines(c(.LEGGIMI_COMPLETO, "",
               sprintf("Fatta il %s · %d fascicoli.", format(Sys.time(), "%d/%m/%Y alle %H:%M"), n),
               "", sprintf("Licenza dei dati: %s (%s).", LICENZA_DATI$nome, LICENZA_DATI$uri),
               if (nzchar(riga)) paste("Da citare:", riga) else "Attribuzione non scritta: si imposta in Import / Export."),
             file.path(tmp, "LEGGIMI.txt"), useBytes = TRUE)
  files <- list.files(tmp, recursive = TRUE, all.files = FALSE)
  if (file.exists(zipfile)) unlink(zipfile)
  zip::zipr(zipfile, files = files, root = tmp, mode = "mirror")
  invisible(list(percorso = zipfile, fascicoli = n, luoghi = file.exists(gz), file = files))
}

# ── Copia del database in SQL e in SQLite ────────────────
# Il database com'è, per altri programmi (DBeaver, DB Browser for SQLite, R, Python…): le tabelle interne di Trama, non il foglio a 28 colonne.
# Tutti i fondi, o solo `fondo` (con il suo archivio dichiarato, il piano di busta, le schede persona e le verifiche d'omonimia che lo toccano).
# Fuori resta la storia delle schede.
.copia_per_export <- function(con, fondo = NULL) {
  tmp <- tempfile("export-db-", fileext = ".sqlite")
  .copia_sqlite(con, tmp)
  x <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  ok <- FALSE
  on.exit({ try(DBI::dbDisconnect(x), silent = TRUE); if (!ok) unlink(tmp) }, add = TRUE)
  DBI::dbExecute(x, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(x, "DELETE FROM storia_fascicoli")
  DBI::dbExecute(x, "DELETE FROM impostazioni WHERE chiave = 'fondo_attivo'")   # lo stato della finestra, non un dato
  if (!is.null(fondo)) {
    if (.vuoto(fondo)) stop("Scegli il fondo.", call. = FALSE)
    fondo <- .fid(con, fondo)
    for (f in setdiff(DBI::dbGetQuery(x, "SELECT id FROM fondi")$id, fondo)) elimina_fondo(x, f)
    DBI::dbExecute(x, "DELETE FROM schede_persona WHERE forma_canonica NOT IN (SELECT persona FROM persone)
                         AND id NOT IN (SELECT scheda_id FROM persona_varianti WHERE variante IN (SELECT persona FROM persone))")
    DBI::dbExecute(x, "DELETE FROM omonimie_verificate WHERE persona NOT IN (SELECT persona FROM persone)")
    DBI::dbExecute(x, "DELETE FROM coorti")
  }
  # La licenza e la data viaggiano con il file: chi lo apre trova chi citare.
  att <- attribuzione_dati(con)
  for (kv in list(c("licenza_dati", paste0(LICENZA_DATI$nome, " (", LICENZA_DATI$uri, ")")),
                  c("copia_fatta_il", format(Sys.time(), "%Y-%m-%d %H:%M")),
                  c("copia_di", if (is.null(fondo)) "tutti i fondi" else nome_fondo_esteso(con, fondo)),
                  c("da_citare", riga_attribuzione(att))))
    DBI::dbExecute(x, "INSERT OR REPLACE INTO impostazioni (chiave, valore) VALUES (?, ?)", list(kv[1], kv[2]))
  DBI::dbExecute(x, "PRAGMA journal_mode = DELETE")
  DBI::dbExecute(x, "VACUUM")
  ok <- TRUE
  tmp
}

esporta_sqlite <- function(con, percorso, fondo = NULL) {
  tmp <- .copia_per_export(con, fondo); on.exit(unlink(tmp), add = TRUE)
  if (file.exists(percorso)) unlink(percorso)
  if (!file.copy(tmp, percorso)) stop("Non riesco a scrivere il file.", call. = FALSE)
  invisible(percorso)
}

# Un valore come lo vuole SQL: NULL, un numero, o un testo fra apici (l'apice si raddoppia).
.sql_colonna <- function(v) {
  if (is.character(v)) return(ifelse(is.na(v), "NULL", paste0("'", gsub("'", "''", v, fixed = TRUE), "'")))
  if (is.integer(v) || is.logical(v)) return(ifelse(is.na(v), "NULL", as.character(as.integer(v))))
  ifelse(is.na(v), "NULL", formatC(v, digits = 15, format = "g"))
}

# Struttura e dati come testo, nel dialetto di SQLite (`sqlite3 nuovo.sqlite < file.sql`); per PostgreSQL o MySQL va adattato. I campi sono solo TEXT, INTEGER e REAL.
esporta_sql <- function(con, percorso, fondo = NULL) {
  tmp <- .copia_per_export(con, fondo); on.exit(unlink(tmp), add = TRUE)
  x <- DBI::dbConnect(RSQLite::SQLite(), tmp); on.exit(DBI::dbDisconnect(x), add = TRUE)
  n <- DBI::dbGetQuery(x, "SELECT COUNT(*) AS n FROM fascicoli")$n
  riga <- riga_attribuzione(attribuzione_dati(con))
  f <- file(percorso, "wb"); on.exit(close(f), add = TRUE)
  scrivi <- function(testo) writeLines(enc2utf8(testo), f, useBytes = TRUE)
  scrivi(c("-- Trama: copia del database in SQL (dialetto SQLite)",
           sprintf("-- Fatta il %s · %d fascicoli · %s", format(Sys.time(), "%d/%m/%Y alle %H:%M"), n,
                   if (is.null(fondo)) "tutti i fondi" else paste0("solo il fondo «", nome_fondo_esteso(con, fondo), "»")),
           sprintf("-- Licenza dei dati: %s (%s)", LICENZA_DATI$nome, LICENZA_DATI$uri),
           if (nzchar(riga)) paste("-- Da citare:", riga) else "-- Attribuzione non scritta: si imposta in Import / Export.",
           "-- Non è il foglio del modello a 28 colonne: sono le tabelle interne di Trama.",
           "-- Si carica con:  sqlite3 nuovo.sqlite < questo-file.sql",
           "", "PRAGMA foreign_keys = OFF;", "BEGIN TRANSACTION;", ""))
  tabelle <- DBI::dbGetQuery(x, "SELECT name, sql FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY rowid")
  for (i in seq_len(nrow(tabelle))) {
    t <- tabelle$name[i]
    scrivi(c(paste0(tabelle$sql[i], ";"), ""))
    d <- DBI::dbGetQuery(x, sprintf('SELECT * FROM "%s"', t))
    if (!nrow(d)) next
    cols <- paste0('"', names(d), '"', collapse = ", ")
    valori <- do.call(paste, c(lapply(d, .sql_colonna), sep = ", "))
    for (da in seq(1L, nrow(d), by = 100L)) {
      r <- da:min(da + 99L, nrow(d))
      scrivi(c(sprintf('INSERT INTO "%s" (%s) VALUES', t, cols), paste0("(", valori[r], ")", c(rep(",", length(r) - 1L), ";"))))
    }
    scrivi("")
  }
  indici <- DBI::dbGetQuery(x, "SELECT sql FROM sqlite_master WHERE type = 'index' AND sql IS NOT NULL ORDER BY rowid")$sql
  if (length(indici)) scrivi(c(paste0(indici, ";"), ""))
  scrivi(c("COMMIT;", "PRAGMA foreign_keys = ON;"))
  invisible(percorso)
}

# Apre lo zip senza toccare nulla e dice se è ripristinabile: contenuto, database apribile, integrità, numero di fascicoli. Il database dentro è «trama.sqlite».
.membro_database <- function(elenco) { m <- intersect(NOME_DB, elenco); if (length(m)) m[1] else NA_character_ }

leggi_backup_completo <- function(zipfile) {
  if (!file.exists(zipfile)) stop("Il file non esiste: ", zipfile, call. = FALSE)
  elenco <- tryCatch(zip::zip_list(zipfile)$filename,
                     error = function(e) stop("Non è una copia completa valida (il file zip non si apre).", call. = FALSE))
  membro <- .membro_database(elenco)
  if (is.na(membro))
    stop("Non è una copia completa di Trama: manca trama.sqlite.", call. = FALSE)
  tmp <- tempfile("lettura-"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  zip::unzip(zipfile, files = membro, exdir = tmp)
  db <- suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, membro))); on.exit(DBI::dbDisconnect(db), add = TRUE)
  ok <- tryCatch(identical(DBI::dbGetQuery(db, "PRAGMA integrity_check")[[1]], "ok"), error = function(e) FALSE)
  if (!ok) stop("Il database nella copia è danneggiato: ripristino interrotto.", call. = FALSE)
  n <- tryCatch(DBI::dbGetQuery(db, "SELECT COUNT(*) AS n FROM fascicoli")$n,
                error = function(e) stop("Il database nella copia non è un database di Trama.", call. = FALSE))
  list(file = elenco, membro = membro, fascicoli = n, luoghi = "luoghi/luoghi.csv" %in% elenco, json = any(c("trama-backup.json", "datatrama-backup.json") %in% elenco))
}

# Dall'interfaccia il ripristino completo vale solo sul database della cartella dati (trama.sqlite): con una copia di prova (TRAMA_DB) non si fa. NULL = si può.
ripristino_completo_impedito <- function(percorso_db) {
  if (!identical(basename(percorso_db), NOME_DB))
    return("Stai lavorando su una copia di prova: il ripristino completo si fa solo sul database vero.")
  if (!requireNamespace("zip", quietly = TRUE)) return("Manca il pacchetto «zip».")
  NULL
}

# Rimette database e lista dei luoghi in `dove`; Trama deve essere chiusa. Prima si controlla lo zip e si salvano i file presenti in backup/ (trama-prima-ripristino-completo-…).
ripristina_backup_completo <- function(zipfile, dove = cartella_dati()) {
  info <- leggi_backup_completo(zipfile)
  cart_backup <- file.path(dove, "backup")
  dir.create(cart_backup, recursive = TRUE, showWarnings = FALSE)
  marca <- format(Sys.time(), "%Y%m%d-%H%M%S")
  db <- file.path(dove, NOME_DB)
  gz <- file.path(dove, "luoghi", "luoghi.csv")
  salvati <- character(0)
  if (file.exists(db)) {
    s <- file.path(cart_backup, paste0("trama-prima-ripristino-completo-", marca, ".sqlite"))
    if (!file.copy(db, s)) stop("Non riesco a salvare il database attuale: ripristino interrotto.", call. = FALSE)
    salvati <- c(salvati, s)
  }
  if (file.exists(gz)) {
    s <- file.path(cart_backup, paste0("luoghi-prima-ripristino-completo-", marca, ".csv"))
    if (!file.copy(gz, s)) stop("Non riesco a salvare la lista dei luoghi attuale: ripristino interrotto.", call. = FALSE)
    salvati <- c(salvati, s)
  }
  tmp <- tempfile("ripristino-"); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # Solo i due file che servono, per nome: estrarre tutto lascerebbe a un membro «../x» di scrivere fuori da `tmp` (zip slip).
  zip::unzip(zipfile, files = c(info$membro, if (info$luoghi) "luoghi/luoghi.csv"), exdir = tmp)
  dir.create(dirname(gz), recursive = TRUE, showWarnings = FALSE)
  # sqlite -wal e -shm di un database vecchio non devono sopravvivere al file nuovo
  unlink(paste0(db, c("-wal", "-shm")))
  if (!file.copy(file.path(tmp, info$membro), db, overwrite = TRUE)) stop("Non riesco a scrivere il database.", call. = FALSE)
  if (info$luoghi && !file.copy(file.path(tmp, "luoghi", "luoghi.csv"), gz, overwrite = TRUE))
    stop("Non riesco a scrivere la lista dei luoghi.", call. = FALSE)
  c(info, list(copie_di_prima = salvati))
}
