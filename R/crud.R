# ─────────────────────────────────────────────────────────────
# crud.R: scrittura di un fascicolo
# Dipende da compose.R (componi_*, .vuoto), attribuzione.R (persone_imputate, .coppie_valide) e da una connessione aperta.
# ─────────────────────────────────────────────────────────────

library(uuid)

.ora <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S")


# Busta e fascicolo al salvataggio dalla Scheda: per una scheda nuova servono entrambi (errore); una già nel database può non averli
# (arrivano così dal foglio o da un ripristino) e va corretta comunque, quindi per lei è solo un avviso. Torna list(errore, avvisi).
controllo_collocazione <- function(f, nuova) {
  senza_busta <- .vuoto(f$busta); senza_fascicolo <- .vuoto(f$fascicolo)
  if (nuova && (senza_busta || senza_fascicolo))
    return(list(errore = "Busta e fascicolo sono obbligatori per una scheda nuova.", avvisi = character(0)))
  list(errore = NULL, avvisi = as.character(c(
    if (senza_busta) "Questa scheda non ha busta, come nel foglio di origine: salvata così.",
    if (senza_fascicolo) "Questa scheda non ha numero di fascicolo, come nel foglio di origine: salvata così.")))
}

fascicolo_vuoto <- function(con_default = TRUE) {
  # Archivio, fondo, serie e lingua non hanno un valore di partenza: arrivano dal fondo a schermo (EREDITATI) o dalla fonte.
  # Solo i collaudi (TRAMA_DEFAULT_DI_PROVA) usano ancora una scheda di prova con i vecchi valori.
  prova <- nzchar(Sys.getenv("TRAMA_DEFAULT_DI_PROVA"))
  f <- list(
    id                     = UUIDgenerate(),
    archivio               = if (prova) PROFILO_DOMINIO$prova$archivio else NA_character_,
    fondo                  = if (prova) PROFILO_DOMINIO$prova$fondo else NA_character_,
    serie                  = if (prova) PROFILO_DOMINIO$prova$serie else NA_character_,
    altra_segnatura        = NA_character_,
    busta                  = NA_integer_,
    fascicolo              = NA_integer_,
    titolo                 = NA_character_,
    titolo_manuale         = 0L,
    tipo_titolo            = NA_character_,
    connettore             = NA_character_,
    estremo_remoto         = NA_integer_,
    estremo_recente        = NA_integer_,
    data_nd                = 0L,
    non_consultabile       = 0L,
    soggettario_discorde   = 0L,
    fascicolo_suffisso     = "",
    busta_suffisso         = "",
    sigla_busta            = NA_character_,   # vuota = «b.» (sigla_busta(), compose.R)
    sigla_fascicolo        = NA_character_,   # vuota = «fasc.»
    definizione            = "fascicolo",
    supporto               = "cartaceo",
    tipologia_documentaria = NA_character_,   # caso per caso, mai di partenza (13/10, Stefano); nel foglio esce comunque «Atti» (tipologia_foglio)
    luogo_conservazione    = NA_character_,
    luogo_produzione       = NA_character_,
    note                   = NA_character_,
    luogo_reato            = NA_character_,
    tortura                = "n.d.",
    tortura_esaminata      = 0L,
    sentenza               = "n.d.",
    sentenza_esaminata     = 0L,
    attribuzione             = "n.d.",
    attribuzione_luoghi      = "n.d.",
    identita_imputato_dubbia = 0L,
    imputati_esaminati = 0L,
    reato_ignoto = 0L, luogo_ignoto = 0L, non_procedimento = 0L,
    regesto                = NA_character_,
    regesto_manuale        = 0L,
    imputati   = data.frame(esteso = character(), indicizzato = character(),
                            stringsAsFactors = FALSE),
    reati      = character(0),
    persone    = character(0),
    luoghi     = character(0),
    lingue     = if (prova) "Italiano" else character(0),
    produttori = character(0)
  )

  # L'import NON deve ereditare i default di una scheda nuova:
  # attribuirebbe alla fonte cio' che la fonte non dice.
  if (!con_default) {
    for (k in c("archivio", "fondo", "serie", "definizione", "supporto",
                "tipologia_documentaria"))
      f[[k]] <- NA_character_
    f$lingue <- character(0)
  }

  f
}


# --- helper per le tabelle figlie ---------------------------------

.righe_semplici <- function(valori, colonna) {
  if (.vuoto(valori)) return(NULL)
  v <- unique(trimws(as.character(valori)))
  v <- v[!is.na(v) & v != ""]
  if (length(v) == 0) return(NULL)
  setNames(data.frame(v, stringsAsFactors = FALSE), colonna)
}

.righe_ordinate <- function(valori, colonna) {
  d <- .righe_semplici(valori, colonna)
  if (is.null(d)) return(NULL)
  cbind(ordine = seq_len(nrow(d)), d)
}

.righe_imputati <- function(imp) {
  if (is.null(imp) || nrow(imp) == 0) return(NULL)
  data.frame(ordine      = seq_len(nrow(imp)),
             esteso      = as.character(imp$esteso),
             indicizzato = as.character(imp$indicizzato),
             stringsAsFactors = FALSE)
}

.riscrivi <- function(con, tabella, id, df, nuova = FALSE) {
  if (!nuova) dbExecute(con, paste0("DELETE FROM ", tabella, " WHERE fascicolo_id = ?"), list(id))   # una scheda appena inserita non ha ancora figlie
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  .accoda_righe(con, tabella, cbind(fascicolo_id = id, df))
}

# Come dbAppendTable, con una sola istruzione preparata e tutte le righe come parametri: dbAppendTable apre un punto di salvataggio e rilegge lo
# schema a ogni chiamata, e con una decina di tabelle figlie per scheda era metà del tempo di un import (Napoli, 367 schede: da 7,3 a 3 s).
.accoda_righe <- function(con, tabella, df) {
  if (is.null(df) || !nrow(df)) return(invisible(0L))
  df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)
  dbExecute(con, sprintf("INSERT INTO %s (%s) VALUES (%s)", dbQuoteIdentifier(con, tabella),
                         paste(dbQuoteIdentifier(con, names(df)), collapse = ", "), paste(rep("?", ncol(df)), collapse = ", ")),
            params = unname(as.list(df)))
}


# --- salvataggio --------------------------------------------------

# `storia = TRUE` (solo la maschera): se il salvataggio cambia qualcosa, la scheda di prima entra in `storia_fascicoli`. Import, ripristino e azioni in blocco hanno le loro copie.
salva_fascicolo <- function(con, f, atteso = NULL, storia = FALSE) .atomico(con, {
  if (!is.null(atteso) && (!identical(f$id, atteso$id) ||
      !identical(leggi_fascicolo(con, f$id), atteso)))
    stop("La scheda è stata modificata o rimossa dopo l'apertura. Riaprila prima di salvare; nessun dato è stato sovrascritto.", call. = FALSE)
  prima <- if (isTRUE(storia)) leggi_fascicolo(con, f$id)
  id <- .salva_fascicolo(con, f)
  if (isTRUE(storia) && !is.null(prima)) registra_storia(con, prima, leggi_fascicolo(con, f$id))
  invisible(id)
})

# --- storia della scheda (5/10) -----------------------------------

# Come si chiama a schermo ciò che è cambiato. I nomi che mancano qui (i flag:
# titolo_manuale, data_nd…) finiscono sotto una voce sola.
ETICHETTE_STORIA <- c(
  titolo = "Titolo", regesto = "Regesto", note = "Note", busta = "Busta", fascicolo = "Fascicolo",
  fascicolo_suffisso = "Fascicolo", busta_suffisso = "Busta", estremo_remoto = "Estremo remoto", estremo_recente = "Estremo recente",
  persone = "Persone citate", imputati = "Imputati", reati = "Soggettario", luoghi = "Luoghi citati",
  lingue = "Lingua", produttori = "Soggetto produttore", tortura = "Tortura", sentenza = "Sentenza",
  tipologia_documentaria = "Tipologia documentaria", archivio = "Archivio", fondo = "Fondo", serie = "Serie",
  altra_segnatura = "Altra segnatura", definizione = "Definizione", supporto = "Supporto",
  luogo_conservazione = "Luogo di conservazione",
  luogo_produzione = "Luogo di produzione", extra = "Altri campi",
  persona_reati = "Attribuzione dei capi", reato_luoghi = "Attribuzione dei luoghi")
STORIA_MASSIMA <- 20L   # modifiche conservate per scheda

# I campi diversi fra due letture della stessa scheda, con il nome a schermo.
# Le date di creazione e modifica non contano: cambiano a ogni Salva.
campi_cambiati <- function(prima, dopo) {
  nomi <- setdiff(union(names(prima), names(dopo)), c("creato", "modificato"))
  # Assente e vuoto sono la stessa cosa per chi legge: la maschera scrive "" dove
  # l'import aveva lasciato NA, e non è una modifica.
  piano <- function(x) if (is.atomic(x) && length(x) <= 1 && (length(x) == 0 || is.na(x) || !nzchar(as.character(x)))) "" else x
  diversi <- nomi[!vapply(nomi, function(k) identical(piano(prima[[k]]), piano(dopo[[k]])), logical(1))]
  et <- unname(ETICHETTE_STORIA[diversi]); et[is.na(et)] <- "Opzioni della scheda"
  unique(et)
}

registra_storia <- function(con, prima, dopo) {
  campi <- campi_cambiati(prima, dopo)
  if (!length(campi)) return(invisible(FALSE))   # salvataggio neutro: nessuna voce
  dbExecute(con, "INSERT INTO storia_fascicoli (fascicolo_id, quando, campi, prima) VALUES (?, ?, ?, ?)",
            list(prima$id, .ora(), paste(campi, collapse = " \u00b7 "),
                 jsonlite::base64_enc(serialize(prima, NULL))))
  dbExecute(con, "DELETE FROM storia_fascicoli WHERE fascicolo_id = ? AND id NOT IN
                    (SELECT id FROM storia_fascicoli WHERE fascicolo_id = ? ORDER BY id DESC LIMIT ?)",
            list(prima$id, prima$id, STORIA_MASSIMA))
  invisible(TRUE)
}

# Le modifiche conservate di una scheda, dalla più recente: quando e che cosa.
storia_fascicolo <- function(con, id) {
  if (.vuoto(id)) return(data.frame(id = integer(0), quando = character(0), campi = character(0)))
  dbGetQuery(con, "SELECT id, quando, campi FROM storia_fascicoli WHERE fascicolo_id = ? ORDER BY id DESC", list(id))
}

# Vero se `x` è fatto solo di dati (vettori, liste, NULL): niente funzioni, ambienti, espressioni.
.solo_dati <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.function(x) || is.environment(x) || is.language(x) || is.symbol(x) || typeof(x) %in% c("externalptr", "promise", "closure", "builtin", "special", "S4")) return(FALSE)
  if (is.list(x)) return(all(vapply(x, .solo_dati, logical(1))))
  is.atomic(x)
}

# Rimette la scheda com'era prima dell'ultima modifica registrata e toglie la voce dalla storia; titolo e regesto composti si ricompongono.
# Restituisce i campi tornati indietro, o NULL se non c'è nulla da annullare.
annulla_ultima_modifica <- function(con, id) {
  if (.vuoto(id)) return(NULL)
  u <- dbGetQuery(con, "SELECT id, campi, prima FROM storia_fascicoli WHERE fascicolo_id = ? ORDER BY id DESC LIMIT 1", list(id))
  if (!nrow(u)) return(NULL)
  prima <- tryCatch(unserialize(jsonlite::base64_dec(u$prima[1])), error = function(e) NULL)
  if (!.solo_dati(prima) || !is.list(prima))      # la storia può arrivare da una copia esterna: si rimette solo ciò che è dato
    stop("Questa voce della storia non è leggibile: la scheda non viene toccata.", call. = FALSE)
  .atomico(con, {
    .salva_fascicolo(con, prima)
    dbExecute(con, "DELETE FROM storia_fascicoli WHERE id = ?", list(u$id[1]))
  })
  strsplit(u$campi[1], " \u00b7 ", fixed = TRUE)[[1]]
}

# reato_ignoto / luogo_ignoto / non_procedimento: sempre 0 o 1, con spegnimento automatico (vedi .salva_fascicolo).
.normalizza_flag_fonte <- function(f) {
  uno <- function(x) as.integer(isTRUE(suppressWarnings(as.integer(x)) == 1L))
  f$reato_ignoto <- uno(f$reato_ignoto); f$luogo_ignoto <- uno(f$luogo_ignoto); f$non_procedimento <- uno(f$non_procedimento)
  r <- trimws(as.character(unlist(f$reati))); r <- r[!is.na(r) & nzchar(r)]
  if (length(r)) f$reato_ignoto <- 0L
  if (length(.luoghi_pieni(f))) f$luogo_ignoto <- 0L
  if (f$non_procedimento == 1L) { f$reato_ignoto <- 0L; f$luogo_ignoto <- 0L }
  f
}

# «Luogo del reato» non esiste più: il valore di un database, backup o foglio di prima entra fra i luoghi citati e la colonna resta vuota.
.riporta_luogo_reato <- function(f) {
  l <- f$luogo_reato
  if (length(l) == 1 && !is.na(l) && nzchar(trimws(l))) {
    nuovi <- setdiff(separa_luoghi(l), f$luoghi)
    if (length(nuovi)) f$luoghi <- unique(c(nuovi, f$luoghi))
  }
  f$luogo_reato <- NA_character_
  f
}

.salva_fascicolo <- function(con, f) {
  f <- .riporta_luogo_reato(f)
  # I tre flag sulla fonte sono 0/1 e si spengono da soli se un dato li contraddice (un reato spegne «reato ignoto», un luogo «luogo ignoto»);
  # «non è un procedimento» esclude gli altri due. Prima della composizione: il regesto ne dipende.
  f <- .normalizza_flag_fonte(f)
  f$produttori <- .produttore_canonico(f$produttori)     # «Tribunale dell'Inquisizione di X» = «Inquisizione di X»

  # Un regesto composto con la convenzione di prima («per eresia; magia») si tiene se equivalente a quello di adesso («per eresia e magia»).
  if (!isTRUE(f$regesto_manuale == 1L)) {
    nuovo_regesto <- componi_regesto_scheda(f)
    if (!.regesto_uguale(nuovo_regesto, f$regesto)) f$regesto <- nuovo_regesto
  }
  
  if (!isTRUE(f$titolo_manuale == 1L))
    f$titolo <- componi_titolo_scheda(f)

  # Un sì/no prova già che si è guardata la fonte: la spunta serve solo a dichiarare l'altro caso (guardato, la fonte tace: 'n.d.').
  if (!.vuoto(f$tortura)  && !identical(f$tortura,  "n.d.")) f$tortura_esaminata  <- 1L
  if (!.vuoto(f$sentenza) && !identical(f$sentenza, "n.d.")) f$sentenza_esaminata <- 1L

  # Spunta assente (chiamanti vecchi, import): la data non è dichiarata n.d.
  f$imputati_esaminati <- as.integer(isTRUE(as.integer(f$imputati_esaminati) == 1L))
  f$data_nd <- as.integer(isTRUE(suppressWarnings(as.integer(f$data_nd)) == 1))
  f$non_consultabile <- as.integer(isTRUE(suppressWarnings(as.integer(f$non_consultabile)) == 1))
  f$soggettario_discorde <- as.integer(isTRUE(suppressWarnings(as.integer(f$soggettario_discorde)) == 1))
  # NOT NULL: chiamanti vecchi e import senza suffisso danno stringa vuota.
  # Non si ripulisce: "fasc. 18 -bis" ha quello spazio nella fonte.
  f$fascicolo_suffisso <- if (.vuoto(f$fascicolo_suffisso)) "" else as.character(f$fascicolo_suffisso)[1]
  f$busta_suffisso <- if (.vuoto(f$busta_suffisso)) "" else as.character(f$busta_suffisso)[1]     # «232-bis»: come sta nella fonte
  # «b.» è il predefinito e non si scrive: solo una sigla diversa («aa.») sta nel database.
  f$sigla_busta <- if (.vuoto(f$sigla_busta) || identical(sigla_busta(f$sigla_busta), "b.")) NA_character_ else sigla_busta(f$sigla_busta)
  f$sigla_fascicolo <- if (.vuoto(f$sigla_fascicolo) || identical(sigla_fascicolo(f$sigla_fascicolo), "fasc.")) NA_character_ else sigla_fascicolo(f$sigla_fascicolo)

  f$fondo_id <- fondo_id_da(con, f$archivio, f$fondo, crea = TRUE)      # il fondo è (archivio, denominazione): due archivi con lo stesso nome di fondo restano due fondi (19/10)
  ora  <- .ora()
  prec <- dbGetQuery(con, "SELECT creato FROM fascicoli WHERE id = ?", list(f$id))
  f$creato     <- if (nrow(prec)) prec$creato[1] else ora
  f$modificato <- ora
  
  campi <- c("id","archivio","fondo","fondo_id","serie","altra_segnatura","busta","fascicolo","titolo",
             "titolo_manuale","tipo_titolo","connettore","fascicolo_suffisso","busta_suffisso","sigla_busta","sigla_fascicolo","estremo_remoto","estremo_recente","data_nd","non_consultabile","soggettario_discorde","definizione",
             "supporto","tipologia_documentaria","luogo_conservazione",
             "luogo_produzione","note","luogo_reato","tortura","tortura_esaminata",
             "sentenza","sentenza_esaminata","attribuzione","attribuzione_luoghi",
             "imputati_esaminati", "reato_ignoto", "luogo_ignoto", "non_procedimento", "identita_imputato_dubbia","regesto","regesto_manuale","creato","modificato")

  valori <- lapply(campi, function(k) {
    v <- f[[k]]
    if (is.null(v) || length(v) == 0) NA else v[[1]]
  })

  # UPDATE o INSERT espliciti: con INSERT OR REPLACE una segnatura duplicata cancellerebbe in silenzio il record in conflitto;
  # così il vincolo unico esplode e chi chiama può intercettarlo.
  nuova <- !nrow(prec)
  if (nrow(prec)) {
    dbExecute(con, paste0(
      "UPDATE fascicoli SET ",
      paste(paste0(campi[-1], " = ?"), collapse = ", "),
      " WHERE id = ?"),
      params = c(valori[-1], list(f$id)))
  } else {
    dbExecute(con, paste0(
      "INSERT INTO fascicoli (", paste(campi, collapse = ", "), ") ",
      "VALUES (", paste(rep("?", length(campi)), collapse = ", "), ")"),
      params = valori)
    # «Soggetto conservatore» è eliminato: nei database vecchi la colonna resta, con un default che non deve entrare nelle schede nuove.
    tryCatch(dbExecute(con, "UPDATE fascicoli SET soggetto_conservatore = NULL WHERE id = ?", list(f$id)), error = function(e) NULL)
  }

  # tabelle figlie: cancellate e riscritte a ogni salvataggio
  .riscrivi(con, "imputati",   f$id, .righe_imputati(f$imputati), nuova)
  .riscrivi(con, "reati",      f$id, .righe_ordinate(f$reati,      "reato"), nuova)
  .riscrivi(con, "persone",    f$id, .righe_ordinate(f$persone,    "persona"), nuova)
  .riscrivi(con, "luoghi",     f$id, .righe_ordinate(f$luoghi,     "luogo"), nuova)
  .riscrivi(con, "lingue",     f$id, .righe_ordinate(f$lingue,     "lingua"), nuova)
  .riscrivi(con, "produttori", f$id, .righe_ordinate(f$produttori, "soggetto"), nuova)
  # I campi extra si riscrivono per intero solo se chi salva li conosce tutti: con `campi_extra_noti` si toccano solo quelli (la maschera non vede gli extra ASMo).
  if (is.null(f$campi_extra_noti)) {
    .riscrivi(con, "campi_extra", f$id, .righe_extra(f$extra), nuova)
  } else {
    noti <- unique(c(as.character(f$campi_extra_noti), names(f$extra)))
    if (length(noti))
      dbExecute(con, paste0("DELETE FROM campi_extra WHERE fascicolo_id = ? AND campo IN (",
                            paste(rep("?", length(noti)), collapse = ", "), ")"),
                params = c(list(f$id), as.list(noti)))
    righe <- .righe_extra(f$extra)
    if (!is.null(righe) && nrow(righe)) .accoda_righe(con, "campi_extra", cbind(fascicolo_id = f$id, righe))
  }
  # Le coppie si riscrivono solo se il chiamante le porta (la maschera):
  # import e altri chiamanti non le conoscono, e non devono cancellarle.
  if (!is.null(f$persona_reati))
    .riscrivi(con, "persona_reati", f$id,
              .coppie_valide(f$persona_reati, persone_imputate(f), unique(trimws(f$reati))))
  if (!is.null(f$reato_luoghi))
    .riscrivi(con, "reato_luoghi", f$id,
              .coppie_valide(f$reato_luoghi, unique(trimws(f$reati)), unique(trimws(f$luoghi))))
  # I termini del tesauro: solo sotto una classe che la scheda ha ancora, e solo coppie (classe, termine) che il profilo conosce.
  if (!is.null(f$reato_termini))
    .riscrivi(con, "reato_termini", f$id, .termini_validi(f$reato_termini, unique(trimws(f$reati))))
  else {      # chi non li porta non li cambia, ma un termine non resta sotto una classe che la scheda non ha più
    tenuti <- unique(trimws(f$reati)); tenuti <- tenuti[nzchar(tenuti)]
    dbExecute(con, sprintf("DELETE FROM reato_termini WHERE fascicolo_id = ? AND reato NOT IN (%s)", paste(rep("?", max(1L, length(tenuti))), collapse = ",")),
              c(list(f$id), if (length(tenuti)) as.list(tenuti) else list("")))
  }
  invisible(f$id)
}


# --- fascicolo nuovo: da dove prende collocazione e numeri --------

# Il fascicolo toccato per ultimo: da lì una scheda nuova prende la collocazione se a schermo non ce n'è una. NULL a database vuoto.
# `fondo`: l'ultimo modificato di QUEL fondo.
ultimo_modificato <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  id <- if (is.null(fondo))
    dbGetQuery(con, "SELECT id FROM fascicoli ORDER BY modificato DESC LIMIT 1")$id
  else
    dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo_id = ? ORDER BY modificato DESC LIMIT 1",
               list(fondo))$id
  if (length(id)) leggi_fascicolo(con, id[1]) else NULL
}


# Ordine dei suffissi numerali: «» < bis < ter < quater… < gli altri (poi lessicale).
.rango_suffisso <- function(x) {
  k <- sub("^[-– ]+", "", tolower(trimws(ifelse(is.na(x), "", x))))
  r <- match(k, c("", "bis", "ter", "quater", "quinquies", "sexies", "septies", "octies", "novies", "decies"))
  r[is.na(r)] <- 11L
  r
}

# La busta come chiave di filtro e di scaffale: «232» o «232-bis»; NA se non ha numero.
chiave_busta <- function(busta, suffisso = "") {
  suffisso <- if (length(suffisso) == length(busta)) ifelse(is.na(suffisso), "", suffisso) else ""
  ifelse(is.na(busta), NA_character_, paste0(busta, suffisso))
}

# Il suffisso della busta come sta nel database: mai NA, vuoto = «».
.suffisso_busta_db <- function(x) if (.vuoto(x)) "" else as.character(x)[1]

# «232» o «232-bis»: la busta come si legge, per i messaggi e le etichette.
.busta_etichetta <- function(busta, suffisso = "") paste0(busta, vapply(suffisso, .suffisso_busta_db, ""))

# Il primo numero libero dopo il più alto usato: il fascicolo nella `busta` data, altrimenti la busta nel fondo.
# `anche` conta un numero non ancora salvato (la scheda nuova a schermo).
prossimo_numero <- function(con, fondo, busta = NULL, anche = NA, suffisso_b = "") {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  m <- if (is.null(busta))
    dbGetQuery(con, "SELECT MAX(busta) AS m FROM fascicoli WHERE fondo_id = ?",
               list(fondo))$m
  else
    dbGetQuery(con, "SELECT MAX(fascicolo) AS m FROM fascicoli WHERE fondo_id = ? AND busta = ? AND busta_suffisso = ?",
               list(fondo, as.integer(busta), .suffisso_busta_db(suffisso_b)))$m
  x <- suppressWarnings(as.integer(c(m, anche)))
  x <- x[!is.na(x)]
  if (length(x)) max(x) + 1L else 1L
}


# --- piano di busta -----------------------------------------------
# Fascicoli attesi in una busta, come intervallo. Non sono schede: nessun export o statistica li vede; il piano dice che cosa manca.

PIANO_MASSIMO <- 1000L    # oltre, è quasi certamente un errore di battitura

# 9, 10, 11, 13, 20, 21 -> "9–11, 13, 20–21"
.intervalli <- function(x) {
  if (!length(x)) return("")
  x <- sort(unique(x))
  inizio <- x[c(TRUE, diff(x) != 1)]
  fine   <- x[c(diff(x) != 1, TRUE)]
  paste(ifelse(inizio == fine, inizio, paste0(inizio, "–", fine)), collapse = ", ")
}


# Che cosa succederebbe registrando il piano. Non scrive niente.
anteprima_piano <- function(con, fondo, busta, primo, ultimo, suffisso_b = "") {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  suffisso_b <- .suffisso_busta_db(suffisso_b)
  intero <- function(x) if (.vuoto(x)) NA_integer_ else suppressWarnings(as.integer(x))
  busta <- intero(busta); primo <- intero(primo); ultimo <- intero(ultimo)
  errore <- function(m) list(ok = FALSE, messaggio = m)

  if (.vuoto(fondo))
    return(errore("Serve il fondo: apri o crea prima un fascicolo di quel fondo."))
  if (anyNA(c(busta, primo, ultimo)))
    return(errore("Busta, primo e ultimo fascicolo sono obbligatori."))
  if (busta < 1 || primo < 1)
    return(errore("Busta e fascicoli si contano da 1."))
  if (primo > ultimo)
    return(errore("Il primo fascicolo non può superare l'ultimo."))
  if (ultimo - primo + 1L > PIANO_MASSIMO)
    return(errore(sprintf("Più di %d fascicoli in una busta: controlla i numeri.", PIANO_MASSIMO)))

  esistenti <- dbGetQuery(con, "SELECT fascicolo FROM fascicoli WHERE fondo_id = ? AND busta = ? AND busta_suffisso = ?",
                          list(fondo, busta, suffisso_b))$fascicolo
  prima  <- dbGetQuery(con, "SELECT primo, ultimo FROM piani_busta WHERE fondo_id = ? AND busta = ? AND busta_suffisso = ?",
                       list(fondo, busta, suffisso_b))
  attesi <- primo:ultimo

  list(ok = TRUE, fondo = fondo, busta = busta, suffisso_b = suffisso_b, primo = primo, ultimo = ultimo,
       attesi   = length(attesi),
       schedati = sort(intersect(attesi, esistenti)),
       mancanti = sort(setdiff(attesi, esistenti)),
       fuori    = sort(setdiff(esistenti, attesi)),    # schede della busta fuori dall'intervallo
       sostituisce = if (nrow(prima)) sprintf("%d–%d", prima$primo, prima$ultimo) else NA_character_)
}


# Registra il piano della busta, o lo aggiorna: UPDATE o INSERT espliciti.
salva_piano <- function(con, fondo, busta, primo, ultimo, suffisso_b = "") {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  a <- anteprima_piano(con, fondo, busta, primo, ultimo, suffisso_b)
  if (!a$ok) stop(a$messaggio)
  if (is.na(a$sostituisce))
    dbExecute(con, "INSERT INTO piani_busta (fondo_id, busta, busta_suffisso, primo, ultimo, creato) VALUES (?, ?, ?, ?, ?, ?)",
              list(a$fondo, a$busta, a$suffisso_b, a$primo, a$ultimo, .ora()))
  else
    dbExecute(con, "UPDATE piani_busta SET primo = ?, ultimo = ? WHERE fondo_id = ? AND busta = ? AND busta_suffisso = ?",
              list(a$primo, a$ultimo, a$fondo, a$busta, a$suffisso_b))
  invisible(a)
}


# Busta per busta, i numeri del piano ancora senza scheda.
mancanti_piani <- function(con) {
  p <- dbGetQuery(con, "SELECT p.fondo_id, f.denominazione AS fondo, p.busta, p.busta_suffisso, p.primo, p.ultimo FROM piani_busta p JOIN fondi f ON f.id = p.fondo_id ORDER BY f.denominazione, f.archivio, p.busta, p.busta_suffisso")
  righe <- lapply(seq_len(nrow(p)), function(i) {
    a <- anteprima_piano(con, p$fondo_id[i], p$busta[i], p$primo[i], p$ultimo[i], p$busta_suffisso[i])
    data.frame(fondo_id = p$fondo_id[i], fondo = p$fondo[i], busta = p$busta[i], suffisso_b = p$busta_suffisso[i], attesi = a$attesi,
               da_schedare = length(a$mancanti), numeri = .intervalli(a$mancanti),
               stringsAsFactors = FALSE)
  })
  if (length(righe)) do.call(rbind, righe)
  else data.frame(fondo_id = character(0), fondo = character(0), busta = integer(0), suffisso_b = character(0), attesi = integer(0),
                  da_schedare = integer(0), numeri = character(0), stringsAsFactors = FALSE)
}


# Dove porta «Successivo» secondo il piano: il primo mancante dopo il numero a schermo, o il primo in assoluto. NA se non c'è piano o è tutto schedato.
prossimo_nel_piano <- function(con, fondo, busta, corrente = NA, suffisso_b = "") {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo) || .vuoto(busta)) return(NA_integer_)
  p <- dbGetQuery(con, "SELECT primo, ultimo FROM piani_busta WHERE fondo_id = ? AND busta = ? AND busta_suffisso = ?",
                  list(fondo, as.integer(busta), .suffisso_busta_db(suffisso_b)))
  if (!nrow(p)) return(NA_integer_)
  corrente <- if (.vuoto(corrente)) NA_integer_ else suppressWarnings(as.integer(corrente))
  m <- setdiff(anteprima_piano(con, fondo, busta, p$primo, p$ultimo, suffisso_b)$mancanti, corrente)
  if (!length(m)) return(NA_integer_)
  dopo <- if (is.na(corrente)) m else m[m > corrente]
  as.integer(if (length(dopo)) min(dopo) else min(m))
}


# --- lettura ------------------------------------------------------

leggi_fascicolo <- function(con, id) {
  riga <- dbGetQuery(con, "SELECT * FROM fascicoli WHERE id = ?", list(id))
  if (nrow(riga) == 0) return(NULL)
  riga$soggetto_conservatore <- NULL      # colonna eliminata (12/10), ancora fisica nei database di prima: la scheda non la conosce
  
  f <- as.list(riga[1, ])
  
  f$imputati <- dbGetQuery(con,
                           "SELECT esteso, indicizzato FROM imputati
      WHERE fascicolo_id = ? ORDER BY ordine", list(id))
  
  # Per posizione nella fonte; le righe vecchie (ordine NULL) in coda,
  # in ordine alfabetico come prima.
  f$reati <- dbGetQuery(con,
                        "SELECT reato FROM reati WHERE fascicolo_id = ?
                          ORDER BY ordine IS NULL, ordine, reato COLLATE NOCASE", list(id))$reato

  f$persone <- dbGetQuery(con,
                          "SELECT persona FROM persone WHERE fascicolo_id = ?
                            ORDER BY ordine IS NULL, ordine, persona COLLATE NOCASE", list(id))$persona
  
  f$luoghi <- dbGetQuery(con,
                         "SELECT luogo FROM luoghi WHERE fascicolo_id = ? ORDER BY ordine", list(id))$luogo
  
  f$lingue <- dbGetQuery(con,
                         "SELECT lingua FROM lingue WHERE fascicolo_id = ? ORDER BY ordine, lingua COLLATE NOCASE", list(id))$lingua
  
  f$produttori <- dbGetQuery(con,
                             "SELECT soggetto FROM produttori WHERE fascicolo_id = ? ORDER BY ordine, soggetto COLLATE NOCASE", list(id))$soggetto

  # Le coppie dichiarate (R/attribuzione.R): chi risponde di quale capo,
  # quale capo in quale luogo.
  f$persona_reati <- dbGetQuery(con,
    "SELECT persona, reato FROM persona_reati WHERE fascicolo_id = ? ORDER BY persona, reato", list(id))
  f$reato_luoghi <- dbGetQuery(con,
    "SELECT reato, luogo FROM reato_luoghi WHERE fascicolo_id = ? ORDER BY reato, luogo", list(id))
  f$reato_termini <- dbGetQuery(con,
    "SELECT reato, termine FROM reato_termini WHERE fascicolo_id = ? ORDER BY reato, termine", list(id))
  
  ex <- dbGetQuery(con,
    "SELECT campo, valore FROM campi_extra
      WHERE fascicolo_id = ? ORDER BY campo, ordine", list(id))
  f$extra <- if (nrow(ex) == 0) list() else split(ex$valore, ex$campo)
  f
}


# Come leggi_fascicolo, per molte schede insieme: una lettura per tabella invece di dieci per scheda (serve a export e «Pronto per l'export»).
# Dà, nell'ordine di `ids`, esattamente ciò che dà leggi_fascicolo (lo prova verifiche/funzioni.R); un id inesistente fa errore.
leggi_fascicoli <- function(con, ids) {
  ids <- as.character(ids)
  if (!length(ids)) return(list())
  if (anyDuplicated(ids))                             # sotto si indicizza per posizione: le liste per scheda seguono `ids`
    stop("leggi_fascicoli: id ripetuto (", ids[anyDuplicated(ids)], ")", call. = FALSE)
  riga <- dbGetQuery(con, "SELECT * FROM fascicoli"); riga$soggetto_conservatore <- NULL
  i <- match(ids, riga$id)
  if (anyNA(i)) stop("leggi_fascicoli: id inesistente (", ids[which(is.na(i))[1]], ")", call. = FALSE)
  per_id <- function(sql, colonne) {                  # una tabella figlia, spezzata per scheda; per le schede senza righe, il vuoto di leggi_fascicolo
    x <- dbGetQuery(con, sql)
    vuoto <- dbGetQuery(con, paste0(sub("^SELECT fascicolo_id, ", "SELECT ", sub(" ORDER BY .*$", "", sql)), " WHERE 0"))[, colonne, drop = FALSE]
    g <- split(x[, colonne, drop = FALSE], factor(x$fascicolo_id, levels = unique(ids)))
    lapply(g, function(d) if (nrow(d)) { rownames(d) <- NULL; d } else vuoto)
  }
  imputati <- per_id("SELECT fascicolo_id, esteso, indicizzato FROM imputati ORDER BY fascicolo_id, ordine", c("esteso", "indicizzato"))
  # Una colonna sola per scheda: si spezza il vettore, non un data.frame per scheda.
  colonna <- function(sql, nome) {
    x <- dbGetQuery(con, sql)
    vuoto <- dbGetQuery(con, paste0(sub("^SELECT fascicolo_id, ", "SELECT ", sub(" ORDER BY .*$", "", sql)), " WHERE 0"))[[nome]]
    lapply(split(x[[nome]], factor(x$fascicolo_id, levels = unique(ids))), function(v) if (length(v)) v else vuoto)
  }
  reati <- colonna("SELECT fascicolo_id, reato FROM reati ORDER BY fascicolo_id, ordine IS NULL, ordine, reato COLLATE NOCASE", "reato")
  persone <- colonna("SELECT fascicolo_id, persona FROM persone ORDER BY fascicolo_id, ordine IS NULL, ordine, persona COLLATE NOCASE", "persona")
  luoghi <- colonna("SELECT fascicolo_id, luogo FROM luoghi ORDER BY fascicolo_id, ordine", "luogo")
  lingue <- colonna("SELECT fascicolo_id, lingua FROM lingue ORDER BY fascicolo_id, ordine, lingua COLLATE NOCASE", "lingua")
  produttori <- colonna("SELECT fascicolo_id, soggetto FROM produttori ORDER BY fascicolo_id, ordine, soggetto COLLATE NOCASE", "soggetto")
  persona_reati <- per_id("SELECT fascicolo_id, persona, reato FROM persona_reati ORDER BY fascicolo_id, persona, reato", c("persona", "reato"))
  reato_luoghi <- per_id("SELECT fascicolo_id, reato, luogo FROM reato_luoghi ORDER BY fascicolo_id, reato, luogo", c("reato", "luogo"))
  reato_termini <- per_id("SELECT fascicolo_id, reato, termine FROM reato_termini ORDER BY fascicolo_id, reato, termine", c("reato", "termine"))
  ex <- dbGetQuery(con, "SELECT fascicolo_id, campo, valore FROM campi_extra ORDER BY fascicolo_id, campo, ordine")
  ex <- split(ex[, c("campo", "valore")], factor(ex$fascicolo_id, levels = unique(ids)))
  # Le colonne della riga principale scheda per scheda con .mapply (un giro in C), non 70 `[` per scheda.
  nomi_riga <- names(riga)
  righe_principali <- .mapply(function(...) list(...), unname(lapply(riga, function(v) v[i])), NULL)
  lapply(seq_along(ids), function(k) {
    f <- righe_principali[[k]]; names(f) <- nomi_riga
    # Per posizione, non per nome (una lista col nome si cerca a scansione, quadratico): la k-esima lista è la scheda ids[k].
    f$imputati <- imputati[[k]]; f$reati <- reati[[k]]; f$persone <- persone[[k]]; f$luoghi <- luoghi[[k]]
    f$lingue <- lingue[[k]]; f$produttori <- produttori[[k]]
    f$persona_reati <- persona_reati[[k]]; f$reato_luoghi <- reato_luoghi[[k]]; f$reato_termini <- reato_termini[[k]]
    e <- ex[[k]]
    f$extra <- if (nrow(e) == 0) list() else split(e$valore, e$campo)
    f
  })
}


# Impostazioni di lavoro: una chiave, un valore. Vuoto = cancellata.
leggi_impostazione <- function(con, chiave, altrimenti = NULL) {
  v <- dbGetQuery(con, "SELECT valore FROM impostazioni WHERE chiave = ?", list(chiave))$valore
  if (length(v)) v[1] else altrimenti
}

scrivi_impostazione <- function(con, chiave, valore) {
  if (is.null(valore) || length(valore) == 0 || is.na(valore[1]) || !nzchar(trimws(valore[1])))
    dbExecute(con, "DELETE FROM impostazioni WHERE chiave = ?", list(chiave))
  else
    dbExecute(con, "INSERT INTO impostazioni (chiave, valore) VALUES (?, ?)
                    ON CONFLICT(chiave) DO UPDATE SET valore = excluded.valore",
              list(chiave, trimws(valore[1])))
  invisible(TRUE)
}


elenco_fascicoli <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  sql <- "
    SELECT
      f.id,
      f.busta,
      f.fascicolo,
      f.fascicolo_suffisso AS suffisso,
      f.busta_suffisso AS suffisso_b,
      f.sigla_busta AS sigla,
      f.sigla_fascicolo AS sigla_f,
      f.titolo,
      f.estremo_remoto AS anno,
      f.estremo_recente AS anno_a,
      (SELECT valore FROM campi_extra WHERE fascicolo_id = f.id AND campo = 'Estremo remoto non risolto' LIMIT 1) AS anno_non_risolto,
      f.reato_ignoto, f.luogo_ignoto, f.non_procedimento, f.data_nd,
      f.tortura,
      f.sentenza,
      (SELECT GROUP_CONCAT(reato, ' | ')
         FROM (SELECT reato FROM reati
                WHERE fascicolo_id = f.id
                ORDER BY ordine IS NULL, ordine, reato))      AS reati,
      (SELECT GROUP_CONCAT(termine, ' | ')
         FROM (SELECT termine FROM reato_termini
                WHERE fascicolo_id = f.id ORDER BY reato, termine))  AS termini,
      (SELECT GROUP_CONCAT(esteso, ' e ')
         FROM (SELECT esteso FROM imputati
                WHERE fascicolo_id = f.id ORDER BY ordine))   AS imputati,
      (SELECT GROUP_CONCAT(luogo, ' | ')
         FROM (SELECT luogo FROM luoghi
                WHERE fascicolo_id = f.id ORDER BY ordine))   AS luoghi,
      (SELECT GROUP_CONCAT(persona, ' | ')
         FROM (SELECT persona FROM persone
                WHERE fascicolo_id = f.id
                ORDER BY ordine IS NULL, ordine, persona))    AS persone,
      f.tortura_esaminata,
      f.sentenza_esaminata
    FROM fascicoli f
  "
  if (!is.null(fondo)) {
    sql <- paste(sql, "WHERE f.fondo_id = ?")
    dati <- dbGetQuery(con, paste(sql, "ORDER BY f.busta, f.busta_suffisso, f.fascicolo"), list(fondo))
  } else {
    dati <- dbGetQuery(con, paste(sql, "ORDER BY f.busta, f.busta_suffisso, f.fascicolo"))
  }
  dati
}
# ─── 1 ─── in fondo a R/crud.R, come nuovo helper

.righe_extra <- function(extra) {
  if (is.null(extra) || length(extra) == 0) return(NULL)
  pezzi <- lapply(names(extra), function(nome) {
    v <- as.character(extra[[nome]])
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (length(v) == 0) return(NULL)
    data.frame(campo = nome, ordine = seq_along(v), valore = v,
               stringsAsFactors = FALSE)
  })
  pezzi <- Filter(Negate(is.null), pezzi)
  if (length(pezzi) == 0) NULL else do.call(rbind, pezzi)
}


# --- suggerimenti per l'autocompletamento --------------------------
# Le voci già presenti nel fondo: ritrovare la forma esatta evita di moltiplicare le grafie.
# I nomi suggeriti nel campo Persone sono nella forma del campo («maria bianchi» → «Bianchi, Maria»); il database non cambia.
# Dove il cognome non è certo («Anna di Mosca») il nome resta com'è.
.suggerimento_persona <- function(v) {
  if (!length(v)) return(v)
  unique(vapply(as.character(v), function(n) {
    if (!grepl(",", n, fixed = TRUE) && (.cognome_incerto(n) || .formula_sovrano(n) || senza_cognome(n))) return(n)
    normalizza_persona(n)
  }, "", USE.NAMES = FALSE))
}
suggerimenti_da_db <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)

  d <- function(sql, params = NULL) {
    v <- dbGetQuery(con, sql, params = params)[[1]]
    v <- v[!is.na(v) & nzchar(trimws(v))]
    as.character(v)
  }
  per_fondo <- !.vuoto(fondo)

  # Ogni archivio va da solo: i suggerimenti sono quelli del fondo a schermo; senza fondo («tutti i fondi») restano tutti.
  del_fondo <- function(tabella, colonna, ordine)
    if (per_fondo) d(sprintf("SELECT DISTINCT t.%s FROM %s t JOIN fascicoli f ON f.id = t.fascicolo_id WHERE f.fondo_id = ? ORDER BY t.%s COLLATE NOCASE", colonna, tabella, ordine), list(fondo))
    else d(sprintf("SELECT DISTINCT %s FROM %s ORDER BY %s COLLATE NOCASE", colonna, tabella, ordine))
  persone  <- del_fondo("persone",  "persona", "persona")
  luoghi   <- del_fondo("luoghi",   "luogo",   "luogo")
  lingue   <- del_fondo("lingue",   "lingua",  "lingua")
  imputati <- del_fondo("imputati", "esteso",  "esteso")

  # Luogo di conservazione e soggetto produttore sono ENTI: i suggerimenti vengono dal fondo a schermo (l'archivio del fondo e i valori già scritti lì).
  # In un fondo nuovo il soggetto produttore non ha suggerimenti: la prima scheda lo fissa per le successive.
  archivi_tutti <- d("SELECT DISTINCT archivio FROM fascicoli UNION SELECT DISTINCT archivio FROM fondi")
  if (per_fondo) {
    enti <- d("SELECT archivio FROM fascicoli WHERE fondo_id = ? UNION SELECT archivio FROM fondi WHERE id = ?", list(fondo, fondo))
    usati <- d("SELECT DISTINCT luogo_conservazione FROM fascicoli WHERE fondo_id = ?", list(fondo))
    conservazione <- unique(c(enti, usati[usati %in% archivi_tutti]))
    soggetti <- d("SELECT DISTINCT p.soggetto FROM produttori p JOIN fascicoli f ON f.id = p.fascicolo_id WHERE f.fondo_id = ? ORDER BY p.soggetto COLLATE NOCASE", list(fondo))
  } else {
    conservazione <- archivi_tutti
    soggetti <- d("SELECT DISTINCT soggetto FROM produttori ORDER BY soggetto COLLATE NOCASE")
  }

  list(
    persona               = .suggerimento_persona(persone),
    luogo                 = luoghi,
    soggetto              = soggetti,
    lingua                = lingue,
    imputati              = unique(c(imputati, .estendi_nome(persone))),
    luogo_produzione      = luoghi,
    luogo_conservazione   = conservazione
  )
}

# Il luogo di conservazione è un ente, mai una città: vuoto o città dell'archivio diventa l'archivio; ogni altro valore resta.
# Senza `applica` non scrive e dice che cosa cambierebbe (fondo, da, a, schede).
allinea_luogo_conservazione <- function(con, applica = FALSE) {
  vuota <- data.frame(fondo = character(), da = character(), a = character(), schede = integer(), stringsAsFactors = FALSE)
  d <- dbGetQuery(con, "SELECT id, fondo, archivio, luogo_conservazione AS v FROM fascicoli WHERE archivio IS NOT NULL AND TRIM(archivio) <> ''")
  if (!nrow(d)) return(vuota)
  citta <- localita_archivio(d$archivio)$localita
  nullo <- is.na(d$v) | !nzchar(trimws(d$v))
  e_citta <- !is.na(citta) & !nullo & tolower(trimws(d$v)) == tolower(citta)
  da_cambiare <- d[nullo | e_citta, , drop = FALSE]
  if (!nrow(da_cambiare)) return(vuota)
  if (applica) .atomico(con, for (i in seq_len(nrow(da_cambiare)))
    dbExecute(con, "UPDATE fascicoli SET luogo_conservazione = ?, modificato = ? WHERE id = ?", list(da_cambiare$archivio[i], .ora(), da_cambiare$id[i])))
  r <- stats::aggregate(id ~ fondo + v + archivio, data = transform(da_cambiare, v = ifelse(is.na(v) | !nzchar(trimws(v)), "(vuoto)", v)), FUN = length)
  data.frame(fondo = r$fondo, da = r$v, a = r$archivio, schede = r$id, stringsAsFactors = FALSE)
}

# Porta i soggetti produttori alla forma «Inquisizione di <Città>» (se la scheda ha già entrambe, resta una). Senza `applica` non scrive.
allinea_produttori <- function(con, applica = FALSE) {
  d <- dbGetQuery(con, "SELECT fascicolo_id, soggetto, ordine FROM produttori")
  nuovo <- vapply(d$soggetto, function(x) { y <- .produttore_canonico(x); if (length(y)) y[1] else x }, "", USE.NAMES = FALSE)
  cambia <- which(nuovo != d$soggetto)
  if (applica && length(cambia)) .atomico(con, for (i in cambia) {
    dbExecute(con, "DELETE FROM produttori WHERE fascicolo_id = ? AND soggetto = ?", list(d$fascicolo_id[i], d$soggetto[i]))
    dbExecute(con, "INSERT OR IGNORE INTO produttori (fascicolo_id, soggetto, ordine) VALUES (?, ?, ?)", list(d$fascicolo_id[i], nuovo[i], d$ordine[i]))
  })
  if (!length(cambia)) return(data.frame(da = character(), a = character(), righe = integer(), stringsAsFactors = FALSE))
  r <- as.data.frame(table(da = d$soggetto[cambia], a = nuovo[cambia]), stringsAsFactors = FALSE)
  r <- r[r$Freq > 0, , drop = FALSE]; data.frame(da = r$da, a = r$a, righe = r$Freq, stringsAsFactors = FALSE)
}

# Soggetto produttore di partenza delle schede nuove: quello dell'ultima scheda salvata in QUEL fondo; un fondo senza schede parte vuoto.
produttore_del_fondo <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) return(NULL)
  v <- dbGetQuery(con, "SELECT p.soggetto FROM produttori p JOIN fascicoli f ON f.id = p.fascicolo_id
                          WHERE f.fondo_id = ? ORDER BY f.modificato DESC, p.rowid LIMIT 1", list(fondo))$soggetto
  if (length(v) && !is.na(v[1]) && nzchar(trimws(v[1]))) v[1] else NULL
}


# L'estremo remoto del fascicolo precedente nel fondo (stessa busta e numero minore, o l'ultimo datato di una busta precedente):
# serve a PROPORRE l'anno nella Scheda, non si scrive mai da solo.
anno_precedente <- function(con, fondo, busta, fascicolo, escludi = NA_character_, suffisso = "", suffisso_b = "") {
  f <- fascicolo_precedente(con, fondo, busta, fascicolo, escludi, suffisso, solo_datati = TRUE, suffisso_b = suffisso_b)
  if (is.null(f)) return(NULL)
  list(anno = f$estremo_remoto,
       da = sprintf("%s%d%s %s%d%s", sigla_busta(f$sigla_busta), f$busta, if (.vuoto(f$busta_suffisso)) "" else f$busta_suffisso,
                    sub("fasc\\.", "f.", sigla_fascicolo(f$sigla_fascicolo)), f$fascicolo, f$fascicolo_suffisso))
}


# Titoli e regesti scritti «a mano» uguali a quelli che si comporrebbero dai campi si segnano composti; cambiano solo i flag, mai il testo.
segna_composti <- function(con, fondo = NULL) .atomico(con, {
  ids <- dbGetQuery(con, .solo_fondo("SELECT id FROM fascicoli f", fondo))$id
  titoli <- regesti <- character(0)
  for (id in ids) {
    f <- leggi_fascicolo(con, id)
    # Coincide col composto, oppure è vuoto e dai campi non verrebbe niente (un titolo vuoto non è «scritto a mano»).
    tc <- componi_titolo_scheda(f)
    # Coincide anche un titolo nella forma di prima («Processo contro X»): resta com'è, ma non è «scritto a mano».
    prima <- .titoli_di_prima(f, if (!.vuoto(f$tipo_titolo)) f$tipo_titolo else tipo_da_tipologia(f$tipologia_documentaria),
                              isTRUE(as.integer(f$identita_imputato_dubbia) == 1L) || identical(trimws(as.character(f$tipologia_documentaria)), TIPOLOGIA_PER_DIVERSI))
    if (isTRUE(f$titolo_manuale == 1L) &&
        ((!.vuoto(f$titolo) && (identical(tc, f$titolo) || f$titolo %in% prima)) || (.vuoto(f$titolo) && is.na(tc))))
      titoli <- c(titoli, id)
    if (isTRUE(f$regesto_manuale == 1L) && !.vuoto(f$regesto) &&
        identical(componi_regesto_scheda(f), f$regesto)) regesti <- c(regesti, id)
  }
  for (id in titoli)  dbExecute(con, "UPDATE fascicoli SET titolo_manuale = 0 WHERE id = ?",  list(id))
  for (id in regesti) dbExecute(con, "UPDATE fascicoli SET regesto_manuale = 0 WHERE id = ?", list(id))
  list(titoli = length(titoli), regesti = length(regesti), fascicoli = length(ids))
})


# Il fascicolo subito prima nel fondo (stessa busta e numero minore, o l'ultimo di una busta precedente), per evidenziare le differenze nel riepilogo.
fascicolo_precedente <- function(con, fondo, busta, fascicolo, escludi = NA_character_, suffisso = "", solo_datati = FALSE, suffisso_b = "") {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  busta <- suppressWarnings(as.integer(busta)); fascicolo <- suppressWarnings(as.integer(fascicolo))
  if (.vuoto(fondo) || .vuoto(busta) || .vuoto(fascicolo)) return(NULL)
  suffisso <- if (.vuoto(suffisso)) "" else as.character(suffisso)[1]
  suffisso_b <- if (.vuoto(suffisso_b)) "" else as.character(suffisso_b)[1]
  # Le buste fino a questa, suffisso compreso: l'ordine lo decide l'ordinamento sotto («232» prima di «232-bis»).
  d <- dbGetQuery(con,
    "SELECT id, busta, busta_suffisso AS suffisso_b, fascicolo, fascicolo_suffisso AS suffisso FROM fascicoli
      WHERE fondo_id = ? AND id IS NOT ? AND busta <= ?
        AND NOT (busta = ? AND busta_suffisso = ? AND fascicolo = ? AND fascicolo_suffisso = ?)
        AND (? = 0 OR estremo_remoto IS NOT NULL)",
    list(as.character(fondo), escludi, busta, busta, suffisso_b, fascicolo, suffisso, as.integer(solo_datati)))
  # Si inserisce virtualmente la segnatura richiesta: il precedente è la riga prima. I suffissi comuni seguono l'ordine numerale, i liberi quello lessicale.
  d <- rbind(d, data.frame(id = NA_character_, busta = busta, suffisso_b = suffisso_b, fascicolo = fascicolo, suffisso = suffisso))
  rango_di <- .rango_suffisso
  chiave <- tolower(trimws(d$suffisso)); rango <- rango_di(d$suffisso)
  o <- order(d$busta, rango_di(d$suffisso_b), tolower(trimws(d$suffisso_b)), d$fascicolo, rango, chiave, d$suffisso, d$id, method = "radix")
  i <- match(nrow(d), o)
  if (i > 1L) leggi_fascicolo(con, d$id[o[i - 1L]]) else NULL
}


# ── archivi dichiarati (Home → Nuovo archivio) ──────────
# Fa esistere un fondo prima delle schede, vuoto, con archivio e serie. Ha un id interno (R/fondi.R); la coppia (archivio, nome) è unica.
# Restituisce l'ID del fondo.
crea_archivio <- function(con, archivio, fondo, serie = NA, profilo = "Modello", fonte = NA) {
  pulito <- function(x) if (.vuoto(x)) NA_character_ else .carattere_pulito(trimws(as.character(x)[1]))   # stessa grafia delle schede (D1, D2): «Sant’Uffizio» non fa due fondi
  archivio <- pulito(archivio); fondo <- pulito(fondo)
  serie <- pulito(serie); profilo <- pulito(profilo)
  if (is.na(archivio)) stop("Scrivi il nome dell'archivio (es. «Archivio di Stato di Modena»).", call. = FALSE)
  if (is.na(fondo)) stop("Scrivi il nome del fondo (es. «Inquisizione»).", call. = FALSE)
  if (!is.na(fondo_id_da(con, archivio, fondo)))
    stop("Il fondo «", fondo, "» di «", archivio, "» esiste già: sceglilo dal menu in alto.", call. = FALSE)
  fonte <- .fonte_pulita(fonte)                      # prima di creare: un testo troppo lungo non lascia un fondo a metà
  id <- fondo_id_da(con, archivio, fondo, crea = TRUE, serie = serie, profilo = profilo, dichiarato = TRUE)
  if (nzchar(fonte)) imposta_fonte_fondo(con, id, fonte)
  invisible(id)
}

# Da dove vengono i dati di un fondo: una riga facoltativa di testo libero; viaggia con l'export (proprietà del file) e col backup.
FONTE_MAX <- 500L
.fonte_pulita <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1])) return("")
  x <- trimws(gsub("[[:space:]]+", " ", as.character(x)[1]))
  if (nchar(x) > FONTE_MAX) stop(sprintf("La fonte dei dati è troppo lunga (%d caratteri, al massimo %d): scrivine una riga.", nchar(x), FONTE_MAX), call. = FALSE)
  x
}
fonte_del_fondo <- function(con, fondo) {
  if (.vuoto(fondo)) return("")
  f <- dbGetQuery(con, "SELECT fonte FROM fondi WHERE id = ?", list(.fid(con, fondo)))$fonte
  if (length(f) && !is.na(f[1])) f[1] else ""
}
imposta_fonte_fondo <- function(con, fondo, testo) {
  testo <- .fonte_pulita(testo)
  dbExecute(con, "UPDATE fondi SET fonte = ? WHERE id = ?", list(if (nzchar(testo)) testo else NA_character_, .fid(con, fondo)))
  invisible(testo)
}

# I fondi che l'app conosce: con schede, e dichiarati ancora vuoti (n = 0) in fondo. `id` è il valore di scelta, `fondo` la denominazione.
elenco_fondi <- function(con) {
  dbGetQuery(con, "SELECT f.id AS id, f.denominazione AS fondo, f.archivio AS archivio, COUNT(s.id) AS n
      FROM fondi f LEFT JOIN fascicoli s ON s.fondo_id = f.id
      GROUP BY f.id HAVING COUNT(s.id) > 0 OR f.dichiarato = 1
    ORDER BY n DESC, f.denominazione COLLATE NOCASE, f.archivio COLLATE NOCASE")
}

# L'archivio di un fondo, da dire accanto al nome («Sant'Ufficio» c'è a Napoli e a Bologna). NULL se il fondo non esiste.
archivio_del_fondo <- function(con, fondo) {
  if (.vuoto(fondo)) return(NULL)
  fondo <- .fid(con, fondo)
  a <- dbGetQuery(con, "SELECT archivio FROM fondi WHERE id = ?", list(fondo))$archivio
  if (length(a) && !is.na(a[1]) && nzchar(a[1])) a[1] else NULL
}

# La città di un archivio, quando il nome la dice («Archivio di Stato di Modena» → «Modena»); se non finisce con «di <Città>» resta vuota.
localita_archivio <- function(archivio) {
  a <- trimws(as.character(archivio))
  m <- regmatches(a, regexec("^(.*?)\\s+di\\s+([A-ZÀ-Ý][^ ]*(?: [A-ZÀ-Ý][^ ]*)*)$", a, perl = TRUE))
  data.frame(archivio = a,
             nome_breve = vapply(seq_along(a), function(i) if (length(m[[i]]) == 3L) m[[i]][2] else a[i], ""),
             localita   = vapply(seq_along(a), function(i) if (length(m[[i]]) == 3L) m[[i]][3] else NA_character_, ""),
             stringsAsFactors = FALSE)
}

# Testo del menu «Fondo»: «Fondo Sant'Ufficio · Archivio Storico Diocesano di Napoli (367)». L'ente è il luogo di conservazione più usato,
# o l'archivio se manca o è una città. Il valore del menu resta il nome del fondo.
ente_conservatore_del_fondo <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  v <- dbGetQuery(con, "SELECT luogo_conservazione AS v, COUNT(*) AS n FROM fascicoli
                         WHERE fondo_id = ? AND luogo_conservazione IS NOT NULL AND TRIM(luogo_conservazione) <> ''
                         GROUP BY 1 ORDER BY n DESC LIMIT 1", list(fondo))$v
  a <- archivio_del_fondo(con, fondo)
  if (!length(v)) return(a)
  if (!is.null(a) && identical(tolower(trimws(v)), tolower(localita_archivio(a)$localita))) return(a)
  v
}

etichetta_fondi <- function(con, fondi, n) {
  vapply(seq_along(fondi), function(i) {
    e <- ente_conservatore_del_fondo(con, fondi[i]); nome <- nome_fondo(con, fondi[i])
    nome <- con_parola_fondo(nome)
    if (is.null(e)) sprintf("%s (%d)", nome, n[i]) else sprintf("%s \u00b7 %s (%d)", nome, e, n[i])
  }, "")
}

# Ciò che la prima scheda di un fondo vuoto eredita: archivio, fondo e
# serie dichiarati. NULL se il fondo non è stato dichiarato.
contesto_archivio <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) return(NULL)
  a <- dbGetQuery(con, "SELECT archivio, denominazione AS fondo, serie FROM fondi WHERE id = ?", list(fondo))
  if (!nrow(a)) NULL else lapply(as.list(a[1, ]), function(x) if (is.na(x)) NULL else x)
}


# ── Correzione selettiva: un campo su più fascicoli ──
# Come la scheda: si legge il fascicolo, si cambia SOLO quel campo, titolo e regesto composti si ricompongono, si salva con salva_fascicolo.
# Niente campi d'identità (busta, fascicolo) né testi composti.
CAMPI_IN_BLOCCO <- function(p = profilo_completo("Generale")) {
  p <- p[!is.na(p$campo) & p$dove %in% c("fascicoli", "persone", "luoghi", "reati", "produttori", "lingue"), , drop = FALSE]
  p <- p[!p$campo %in% c("busta", "fascicolo", "fascicolo_suffisso", "titolo", "regesto",
                         "titolo_manuale", "regesto_manuale"), , drop = FALSE]
  data.frame(campo = p$campo, dove = p$dove, etichetta = p$etichetta,
             multi = p$dove != "fascicoli", stringsAsFactors = FALSE)
}

.valore_leggibile <- function(x) {
  x <- as.character(unlist(x)); x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) "—" else paste(x, collapse = "; ")
}

# modo: «sostituisci», «aggiungi» o «togli» (solo campi a più valori). prova = TRUE: solo anteprima. Una riga per fascicolo che cambia.
imposta_campo_in_blocco <- function(con, ids, campo, valore, modo = c("sostituisci", "aggiungi", "togli"),
                                    prova = FALSE) {
  modo <- match.arg(modo)
  def <- CAMPI_IN_BLOCCO(); def <- def[def$campo == campo, , drop = FALSE]
  if (!nrow(def)) stop("Campo non modificabile in blocco: ", campo, call. = FALSE)
  voci <- trimws(unlist(strsplit(paste(valore, collapse = ";"), ";", fixed = TRUE)))
  voci <- voci[!is.na(voci) & nzchar(voci)]
  if (!def$multi && modo != "sostituisci") stop("Aggiungi e togli valgono solo per i campi a più valori.", call. = FALSE)
  esito <- data.frame(id = character(0), segnatura = character(0), prima = character(0), dopo = character(0),
                      stringsAsFactors = FALSE)
  schede <- list()
  for (id in ids) {
    f <- leggi_fascicolo(con, id); if (is.null(f)) next
    if (def$multi) {
      v <- as.character(unlist(f[[def$dove]])); v <- v[!is.na(v) & nzchar(v)]
      nuovo <- switch(modo, sostituisci = voci, aggiungi = unique(c(v, voci)), togli = setdiff(v, voci))
      if (identical(nuovo, v)) next
      prima <- v; f[[def$dove]] <- nuovo
    } else {
      prima <- f[[campo]]
      nuovo <- if (!length(voci)) NA else voci[1]
      if (is.integer(prima)) nuovo <- suppressWarnings(as.integer(nuovo))
      if (identical(as.character(prima), as.character(nuovo)) || (is.na(prima) && is.na(nuovo))) next
      f[[campo]] <- nuovo
    }
    # Titolo e regesto composti seguono i campi, come nella maschera.
    if (!isTRUE(f$titolo_manuale == 1L)) { t <- componi_titolo_scheda(f); if (!is.na(t)) f$titolo <- t }
    if (!isTRUE(f$regesto_manuale == 1L)) { r <- componi_regesto_scheda(f); if (!is.na(r)) f$regesto <- r }
    esito[nrow(esito) + 1L, ] <- list(id, sprintf("b. %s, fasc. %s%s", f$busta, f$fascicolo, if (.vuoto(f$fascicolo_suffisso)) "" else f$fascicolo_suffisso),
                                      .valore_leggibile(prima), .valore_leggibile(f[[if (def$multi) def$dove else campo]]))
    schede[[id]] <- f
  }
  if (!prova && length(schede)) .atomico(con, for (f in schede) salva_fascicolo(con, f))
  esito
}


# ── Genera fascicoli vuoti ─────────────────────────────
# Segnaposto veri di una busta, con l'identità dell'ultimo fascicolo lavorato del fondo (EREDITATI), il soggetto produttore e la sua città come data topica.
# A differenza del piano di busta contano come fascicoli. Un numero già presente non si tocca.
genera_fascicoli_vuoti <- function(con, fondo, busta, da, a, produttore = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  busta <- suppressWarnings(as.integer(busta)); da <- suppressWarnings(as.integer(da)); a <- suppressWarnings(as.integer(a))
  if (.vuoto(fondo)) stop("Scegli prima il fondo in cui lavorare.", call. = FALSE)
  if (is.na(busta) || busta < 1) stop("Indica la busta.", call. = FALSE)
  if (is.na(da) || da < 1) stop("Indica il primo fascicolo.", call. = FALSE)
  if (is.na(a)) a <- da
  if (a < da) stop("L'ultimo fascicolo viene prima del primo.", call. = FALSE)
  if (a - da >= 500) stop("Più di 500 fascicoli in una volta: controlla i numeri.", call. = FALSE)
  u <- ultimo_modificato(con, fondo)
  base <- if (!is.null(u)) u else contesto_archivio(con, fondo)
  if (is.null(base)) stop("Il fondo «", nome_fondo(con, fondo), "» non ha ancora schede né un archivio dichiarato.", call. = FALSE)
  presenti <- dbGetQuery(con, "SELECT fascicolo FROM fascicoli WHERE fondo_id = ? AND busta = ? AND busta_suffisso = '' AND fascicolo_suffisso = ''",
                         list(fondo, busta))$fascicolo
  nuovi <- setdiff(da:a, presenti)
  campi <- c("archivio", "serie", "definizione", "supporto",
             "luogo_conservazione", "lingue")           # la tipologia documentaria no: si sceglie caso per caso (13/10)
  ids <- character(0)
  .atomico(con, for (n in nuovi) {
    f <- fascicolo_vuoto(); f$archivio <- NA_character_; f$serie <- NA_character_   # dal fondo, mai da Modena (12/10)
    for (k in campi) if (!is.null(base[[k]])) f[[k]] <- base[[k]]
    f$fondo <- nome_fondo(con, fondo); f$busta <- busta; f$fascicolo <- n
    if (!.vuoto(produttore)) { f$produttori <- produttore; f$luogo_produzione <- data_topica_con_produttori(NULL, produttore) }
    else if (!is.null(u) && length(u$produttori)) { f$produttori <- u$produttori; f$luogo_produzione <- u$luogo_produzione }
    ids <- c(ids, salva_fascicolo(con, f))
  })
  list(creati = length(nuovi), saltati = sort(intersect(da:a, presenti)), ids = ids)
}


# ── Scheda vuota: riprende il contesto ───────────────────
# Una scheda esistente senza contenuto riprende i campi di contesto vuoti dalla scheda di riferimento del fondo; sono modifiche da salvare, niente si scrive da solo.
scheda_senza_contenuto <- function(f) {
  .vuoto(c(f$titolo, f$regesto, f$estremo_remoto, f$estremo_recente, f$note)) &&
    !length(f$persone) && !length(f$reati) && !length(f$luoghi) && !NROW(f$imputati) && !length(f$extra) &&
    all(c(f$tortura, f$sentenza) %in% c("n.d.", NA, ""))
}

contesto_di_riferimento <- function(con, f) {
  if (.vuoto(f$fondo)) return(NULL)
  fid <- if (.e_id_fondo(f$fondo_id %||% "")) f$fondo_id else fondo_id_da(con, f$archivio, f$fondo)
  if (is.na(fid)) return(NULL)
  id <- dbGetQuery(con, "SELECT x.id FROM fascicoli x WHERE x.fondo_id = ? AND x.id <> ?
                           AND (COALESCE(TRIM(x.luogo_conservazione), '') <> ''
                                OR EXISTS (SELECT 1 FROM produttori p WHERE p.fascicolo_id = x.id)
                                OR EXISTS (SELECT 1 FROM lingue l WHERE l.fascicolo_id = x.id))
                         ORDER BY x.modificato DESC LIMIT 1", list(fid, f$id %||% ""))$id
  if (length(id)) leggi_fascicolo(con, id[1]) else NULL
}


# ── Note rapide da archivio ───────────────────────────
# Una riga per fascicolo, campi separati da «|»: numero | imputato | anno (o da-a) | reati (virgole) | tortura (sì/no) | sentenza (sì/no) | note.
# Aggiorna la scheda con quel numero o ne crea una; riempie solo ciò che è vuoto, un valore diverso è un conflitto nell'anteprima; le note si aggiungono in coda.
analizza_note_rapide <- function(testo) {
  righe <- trimws(unlist(strsplit(paste(testo, collapse = "\n"), "\n", fixed = TRUE)))
  righe <- righe[nzchar(righe)]
  si_no <- function(x) { x <- tolower(trimws(x)); if (!nzchar(x)) NA_character_
    else if (x %in% c("sì", "si", "s", "yes", "y", "x")) "sì" else if (x %in% c("no", "n")) "no"
    else if (x %in% c("n.d.", "nd", "n.d", "?")) "n.d." else paste0("?", x) }
  voc <- tryCatch(as.character(.scelte(.voc("reato"), NULL)), error = function(e) character(0))
  lapply(seq_along(righe), function(i) {
    x <- trimws(strsplit(righe[i], "|", fixed = TRUE)[[1]]); x <- c(x, rep("", 7))[1:7]
    n <- suppressWarnings(as.integer(x[1]))
    anni <- suppressWarnings(as.integer(regmatches(x[3], gregexpr("[0-9]{3,4}", x[3]))[[1]]))
    reati <- trimws(strsplit(x[4], ",", fixed = TRUE)[[1]]); reati <- reati[nzchar(reati)]
    reati <- vapply(reati, function(r) { k <- match(tolower(r), tolower(voc)); if (is.na(k)) r else voc[k] }, "", USE.NAMES = FALSE)
    errori <- c(if (is.na(n)) "numero di fascicolo mancante",
                if (grepl("^\\?", si_no(x[5]) %||% "")) "tortura: sì, no o n.d.",
                if (grepl("^\\?", si_no(x[6]) %||% "")) "sentenza: sì, no o n.d.")
    list(riga = i, testo = righe[i], fascicolo = n, imputato = if (nzchar(x[2])) x[2] else NA_character_,
         remoto = if (length(anni)) anni[1] else NA_integer_, recente = if (length(anni) > 1) anni[2] else NA_integer_,
         reati = reati, tortura = si_no(x[5]), sentenza = si_no(x[6]), note = if (nzchar(x[7])) x[7] else NA_character_,
         errori = errori)
  })
}

applica_note_rapide <- function(con, fondo, busta, testo, produttore = NULL, prova = FALSE) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  busta <- suppressWarnings(as.integer(busta))
  if (.vuoto(fondo)) stop("Scegli prima il fondo in cui lavorare.", call. = FALSE)
  if (is.na(busta)) stop("Indica la busta.", call. = FALSE)
  note <- analizza_note_rapide(testo)
  esito <- data.frame(riga = integer(0), fascicolo = character(0), azione = character(0),
                      cambia = character(0), conflitti = character(0), stringsAsFactors = FALSE)
  schede <- list()
  base <- ultimo_modificato(con, fondo); if (is.null(base)) base <- contesto_archivio(con, fondo)
  for (r in note) {
    segn <- sprintf("b. %d, fasc. %s", busta, if (is.na(r$fascicolo)) "?" else r$fascicolo)
    if (length(r$errori)) { esito[nrow(esito) + 1L, ] <- list(r$riga, segn, "errore", paste(r$errori, collapse = "; "), ""); next }
    id <- dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo_id = ? AND busta = ? AND busta_suffisso = '' AND fascicolo = ? AND fascicolo_suffisso = ''",
                     list(fondo, busta, r$fascicolo))$id
    nuovo <- !length(id)
    if (nuovo) {
      if (is.null(base)) stop("Il fondo «", nome_fondo(con, fondo), "» non ha ancora schede né un archivio dichiarato.", call. = FALSE)
      f <- fascicolo_vuoto(); f$archivio <- NA_character_; f$serie <- NA_character_   # dal fondo, mai da Modena (12/10)
      for (k in c("archivio", "serie", "definizione", "supporto",
                  "luogo_conservazione", "lingue")) if (!is.null(base[[k]])) f[[k]] <- base[[k]]
      f$fondo <- nome_fondo(con, fondo); f$busta <- busta; f$fascicolo <- r$fascicolo
      if (!.vuoto(produttore)) { f$produttori <- produttore; f$luogo_produzione <- data_topica_con_produttori(NULL, produttore) }
    } else f <- leggi_fascicolo(con, id[1])
    cambia <- character(0); conflitti <- character(0)
    metti <- function(campo, valore, etichetta, vuoto_se = NA) {
      if (.vuoto(valore)) return()
      attuale <- f[[campo]]
      if (!.vuoto(attuale) && identical(as.character(attuale), as.character(valore))) return()
      libero <- .vuoto(attuale) || identical(as.character(attuale), vuoto_se)
      if (libero) { f[[campo]] <<- valore; cambia <<- c(cambia, paste0(etichetta, " ", valore)) }
      else if (!identical(as.character(attuale), as.character(valore)))
        conflitti <<- c(conflitti, sprintf("%s: resta «%s» (appunto: «%s»)", etichetta, attuale, valore))
    }
    metti("estremo_remoto", r$remoto, "anno"); metti("estremo_recente", r$recente, "fino al")
    metti("tortura", r$tortura, "tortura", "n.d."); metti("sentenza", r$sentenza, "sentenza", "n.d.")
    if (length(r$reati)) {
      mancano <- setdiff(r$reati, f$reati)
      if (length(mancano)) { f$reati <- c(f$reati, mancano); cambia <- c(cambia, paste("reati", paste(mancano, collapse = ", "))) }
    }
    if (!is.na(r$imputato)) {
      ind <- indicizza_nome(r$imputato)       # la «persona principale» degli appunti: una persona nominata, senza distinzione di ruolo (19/10 sera)
      if (!ind %in% f$persone) {
        f$persone <- unique(c(ind, f$persone)); cambia <- c(cambia, paste("persona", r$imputato))
      }
    }
    if (!is.na(r$note)) {
      f$note <- if (.vuoto(f$note)) r$note else paste(f$note, r$note, sep = " · ")
      cambia <- c(cambia, "note")
    }
    if (!isTRUE(f$titolo_manuale == 1L)) { t <- componi_titolo_scheda(f); if (!is.na(t)) f$titolo <- t }
    if (!isTRUE(f$regesto_manuale == 1L)) { g <- componi_regesto_scheda(f); if (!is.na(g)) f$regesto <- g }
    esito[nrow(esito) + 1L, ] <- list(r$riga, segn, if (nuovo) "nuova scheda" else if (length(cambia)) "aggiorna" else "invariata",
                                      paste(cambia, collapse = "; "), paste(conflitti, collapse = "; "))
    if (nuovo || length(cambia)) schede[[length(schede) + 1L]] <- f
  }
  if (!prova && length(schede)) .atomico(con, for (f in schede) salva_fascicolo(con, f))
  esito
}


# ── Azioni sulle spuntate ──────────────────────────────
# Ricomponi titolo e regesto: i composti (e i vuoti) si rifanno dai campi; gli scritti a mano restano, salvo `anche_titoli` e `anche_regesti`
# (`anche_manuali = TRUE` li include entrambi). Un risultato vuoto non sostituisce mai un testo presente.
# Una riga per fascicolo che cambia; `protetti` dice quali testi a mano verrebbero sostituiti.
ricomponi_in_blocco <- function(con, ids, anche_manuali = FALSE, prova = FALSE,
                                anche_titoli = anche_manuali, anche_regesti = anche_manuali) {
  esito <- data.frame(id = character(0), segnatura = character(0), prima = character(0), dopo = character(0),
                      protetti = character(0), stringsAsFactors = FALSE)
  schede <- list()
  for (id in ids) {
    f <- leggi_fascicolo(con, id); if (is.null(f)) next
    prima <- c(f$titolo, f$regesto)
    sostituiti <- character(0)
    if (anche_titoli || !isTRUE(f$titolo_manuale == 1L) || .vuoto(f$titolo)) {
      t <- componi_titolo_scheda(f, forza = TRUE)
      if (!.vuoto(t)) {
        if (isTRUE(f$titolo_manuale == 1L) && !.vuoto(f$titolo) && !identical(t, f$titolo)) sostituiti <- c(sostituiti, "titolo")
        f$titolo <- t; f$titolo_manuale <- 0L } }
    if (anche_regesti || !isTRUE(f$regesto_manuale == 1L) || .vuoto(f$regesto)) {
      r <- componi_regesto_scheda(f, forza = TRUE)
      if (!.vuoto(r)) {
        if (isTRUE(f$regesto_manuale == 1L) && !.vuoto(f$regesto) && !identical(r, f$regesto)) sostituiti <- c(sostituiti, "regesto")
        f$regesto <- r; f$regesto_manuale <- 0L } }
    if (identical(prima, c(f$titolo, f$regesto))) next
    esito[nrow(esito) + 1L, ] <- list(id, sprintf("b. %s, fasc. %s", f$busta, f$fascicolo),
                                      paste(.valore_leggibile(prima[1]), "/", .valore_leggibile(prima[2])),
                                      paste(.valore_leggibile(f$titolo), "/", .valore_leggibile(f$regesto)),
                                      paste(sostituiti, collapse = " e "))
    schede[[id]] <- f
  }
  if (!prova && length(schede)) .atomico(con, for (f in schede) salva_fascicolo(con, f))
  esito
}

# Elimina fascicoli: le tabelle figlie seguono (ON DELETE CASCADE). La
# copia di sicurezza la fa chi chiama, prima.
elimina_fascicoli <- function(con, ids) {
  ids <- unique(as.character(ids)); if (!length(ids)) return(0L)
  .atomico(con, {
    dbExecute(con, "PRAGMA foreign_keys = ON")
    prima <- dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n
    for (id in ids) dbExecute(con, "DELETE FROM fascicoli WHERE id = ?", list(id))
    n <- prima - dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n
    if (nrow(dbGetQuery(con, "PRAGMA foreign_key_check"))) stop("Collegamenti non validi dopo l'eliminazione.")
    as.integer(n)
  })
}


# ── Elimina un fondo / svuota tutto ─────────────────────
# Qui solo le cancellazioni: la copia di sicurezza la fa chi chiama, l'interfaccia vuole la conferma scritta.

# Che cosa sparirebbe: serve alla finestra di conferma. Con `fondo`, solo quel fondo.
riepilogo_cancellazione <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  q <- function(sql, ...) as.integer(if (...length()) dbGetQuery(con, sql, list(...))$n else dbGetQuery(con, sql)$n)
  if (is.null(fondo)) list(
    fondi = q("SELECT COUNT(*) n FROM fondi"),
    fascicoli = q("SELECT COUNT(*) n FROM fascicoli"), persone = q("SELECT COUNT(*) n FROM schede_persona"),
    piani = q("SELECT COUNT(*) n FROM piani_busta"), ricerche = q("SELECT COUNT(*) n FROM coorti"))
  else list(
    fondi = 1L, fascicoli = q("SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", fondo), persone = 0L,
    piani = q("SELECT COUNT(*) n FROM piani_busta WHERE fondo_id = ?", fondo), ricerche = 0L)
}

# Un fondo intero: schede (con tabelle figlie), piano di busta, archivio dichiarato, decisioni del vocabolario. Restano schede persona e ricerche salvate.
elimina_fondo <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) stop("Scegli il fondo da eliminare.", call. = FALSE)
  .atomico(con, {
    ids <- dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo_id = ?", list(fondo))$id
    n <- elimina_fascicoli(con, ids)
    for (t in c("piani_busta", "vocabolario_fondo"))
      dbExecute(con, paste("DELETE FROM", t, "WHERE fondo_id = ?"), list(fondo))
    dbExecute(con, "DELETE FROM fondi WHERE id = ?", list(fondo))
    list(fascicoli = n)
  })
}

# Tutto il lavoro: fondi, schede, persone, piani, ricerche, verifiche, decisioni. La lista dei luoghi non si tocca (sta in file);
# le impostazioni restano se `tieni_impostazioni`; il fondo di lavoro si azzera sempre.
svuota_database <- function(con, tieni_impostazioni = TRUE) {
  r <- .atomico(con, {
    dbExecute(con, "PRAGMA foreign_keys = ON")
    n <- dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n
    for (t in c("fascicoli", "schede_persona", "piani_busta", "fondi", "omonimie_verificate", "coorti",
                "vocabolario_fondo", "ordine_fascicoli", "storia_fascicoli"))
      dbExecute(con, paste("DELETE FROM", t))
    if (isTRUE(tieni_impostazioni)) dbExecute(con, "DELETE FROM impostazioni WHERE chiave = 'fondo_attivo'")
    else dbExecute(con, "DELETE FROM impostazioni")
    if (nrow(dbGetQuery(con, "PRAGMA foreign_key_check"))) stop("Collegamenti non validi dopo lo svuotamento.")
    list(fascicoli = as.integer(n))
  })
  # Le pagine liberate tengono ancora i vecchi dati: VACUUM le riscrive (fuori dalla transazione).
  try(dbExecute(con, "VACUUM"), silent = TRUE)
  r
}


# ── Rimuovi duplicati ──────────────────────────────────
# Lo stesso fascicolo schedato due volte (di solito il fondo scritto in due modi, dopo un import): stessa busta, numero e suffissi
# e nomi di fondo quasi uguali. Si propone la copia più compilata (a parità, l'ultima modificata); niente si cancella senza conferma.
# Due nomi di fondo sono lo stesso fondo se hanno la stessa chiave o al più due lettere di differenza E gli stessi numeri («Serie 1» e «Serie 2» no). Matrice simmetrica.
.numeri_del_nome <- function(x) {
  t <- unlist(strsplit(as.character(x), "[^[:alnum:]]+"))
  romani <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
  paste(sort(t[grepl("^[0-9]+$", t) | t %in% romani]), collapse = ",")
}
.fondi_vicini <- function(nomi) {
  vicini <- utils::adist(chiave_grafia(nomi)) <= 2
  n <- vapply(nomi, .numeri_del_nome, "", USE.NAMES = FALSE)
  vicini & outer(n, n, "==")
}

duplicati_fascicoli <- function(con) {
  d <- elenco_fascicoli(con)
  vuoto <- data.frame(gruppo = integer(0), id = character(0), segnatura = character(0), titolo = character(0),
                      compilati = integer(0), tieni = logical(0), diversi = logical(0), stringsAsFactors = FALSE)
  if (!nrow(d)) return(vuoto)
  x <- dbGetQuery(con, "SELECT id, fondo, archivio, modificato FROM fascicoli")
  d <- merge(d, x, by = "id")
  pieno <- function(v) !is.na(v) & nzchar(trimws(as.character(v)))
  d$compilati <- as.integer(pieno(d$titolo)) + as.integer(!is.na(d$anno)) + as.integer(pieno(d$reati)) + as.integer(pieno(d$imputati)) + as.integer(pieno(d$luoghi)) + as.integer(pieno(d$persone)) +
    as.integer(pieno(d$tortura) & d$tortura != "n.d.") + as.integer(pieno(d$sentenza) & d$sentenza != "n.d.")
  # La chiave è quella dell'indice unico: busta + suffisso busta + fascicolo + suffisso fascicolo («b. 232» e «b. 232-bis» sono diversi).
  sfx_b <- ifelse(is.na(d$suffisso_b), "", d$suffisso_b)
  # Solo dentro lo stesso ARCHIVIO: fondi omonimi con la stessa segnatura in archivi diversi non sono doppioni.
  chiave <- paste(.nome_per_chiave(d$archivio), d$busta, sfx_b, d$fascicolo, ifelse(is.na(d$suffisso), "", d$suffisso), sep = "|")
  out <- vuoto; g <- 0L
  for (k in unique(chiave[duplicated(chiave)])) {
    r <- d[chiave == k, , drop = FALSE]
    vicini <- .fondi_vicini(r$fondo)
    visti <- rep(FALSE, nrow(r))
    for (i in seq_len(nrow(r))) {
      if (visti[i]) next
      membri <- i; coda <- i                       # componente connessa dei nomi vicini
      while (length(coda)) { j <- coda[1]; coda <- coda[-1]
        nuovi <- setdiff(which(vicini[j, ]), membri); membri <- c(membri, nuovi); coda <- c(coda, nuovi) }
      visti[membri] <- TRUE
      if (length(membri) < 2) next
      m <- r[membri, , drop = FALSE]
      m <- m[order(-m$compilati, -xtfrm(m$modificato)), , drop = FALSE]
      g <- g + 1L
      out <- rbind(out, data.frame(gruppo = g, id = m$id,
        segnatura = sprintf("%s, b. %s%s, fasc. %s%s", m$fondo, m$busta, ifelse(is.na(m$suffisso_b), "", m$suffisso_b), m$fascicolo, ifelse(is.na(m$suffisso), "", m$suffisso)),
        titolo = ifelse(is.na(m$titolo), "", m$titolo), compilati = m$compilati,
        tieni = seq_len(nrow(m)) == 1L,
        diversi = length(unique(trimws(m$titolo[!is.na(m$titolo) & nzchar(trimws(m$titolo))]))) > 1L,      # titoli diversi: forse non sono copie
        stringsAsFactors = FALSE))
    }
  }
  out
}

# Elimina le schede indicate, e solo se sono copie in più di ADESSO (l'analisi a schermo può essere vecchia) e se in ogni gruppo ne resta una.
# Nessun «elimina tutto»: chi chiama indica gruppo per gruppo (`scelta_duplicati`) e fa prima la copia di sicurezza.
rimuovi_duplicati <- function(con, elimina) {
  elimina <- unique(as.character(elimina))
  if (!length(elimina)) return(0L)
  d <- duplicati_fascicoli(con)
  fuori <- setdiff(elimina, d$id)
  if (length(fuori)) stop("Una scheda da eliminare non è più un duplicato: rifai l'analisi. Nulla è stato eliminato.", call. = FALSE)
  restano <- tapply(!(d$id %in% elimina), d$gruppo, sum)
  if (any(restano == 0L)) stop("Così sparirebbero tutte le copie di un gruppo: ne deve restare una. Nulla è stato eliminato.", call. = FALSE)
  elimina_fascicoli(con, elimina)
}

# Dalle scelte della finestra agli id da eliminare: `d` = duplicati_fascicoli(), `gruppi` spuntati, `tieni` = id della copia che resta per gruppo.
scelta_duplicati <- function(d, gruppi, tieni = list()) {
  da_eliminare <- character(0)
  for (g in as.character(gruppi)) {
    m <- d[as.character(d$gruppo) == g, , drop = FALSE]
    if (!nrow(m)) next
    resta <- tieni[[g]]
    if (is.null(resta) || !resta %in% m$id) resta <- m$id[m$tieni][1]
    da_eliminare <- c(da_eliminare, setdiff(m$id, resta))
  }
  da_eliminare
}




# --- licenza e attribuzione dei dati ----------------------
# Stanno qui perché le leggono tutti gli export. La licenza è una; schedatore e progetto cambiano da una copia all'altra (impostazioni del database, da Import / Export).
LICENZA_DATI <- list(nome = "CC BY 4.0", uri = "https://creativecommons.org/licenses/by/4.0/")

attribuzione_dati <- function(con) {
  leggi <- function(k) { v <- leggi_impostazione(con, k); if (is.null(v) || .vuoto(v)) "" else trimws(as.character(v)[1]) }
  list(schedatore = leggi("attribuzione_schedatore"), progetto = leggi("attribuzione_progetto"))
}

# La riga da citare: «Progetto; schedatura di Nome», o la parte che c'è.
riga_attribuzione <- function(a) {
  s <- if (is.null(a$schedatore)) "" else trimws(a$schedatore); p <- if (is.null(a$progetto)) "" else trimws(a$progetto)
  if (nzchar(p) && nzchar(s)) paste0(p, " \u2014 schedatura di ", s)
  else if (nzchar(s)) paste0("Schedatura di ", s) else p
}


# Proprietà del documento di un xlsx (Autore, Titolo, Oggetto, Categoria): dice di chi è il foglio e con quale licenza senza cambiare una cella. `titolo` di norma è il fondo.
proprieta_xlsx <- function(con, titolo = NULL, fonte = NULL) {
  a <- attribuzione_dati(con)
  list(creator = if (nzchar(a$schedatore)) a$schedatore else "Trama",
       title   = if (is.null(titolo) || is.na(titolo)) "" else as.character(titolo),
       subject = paste(c(riga_attribuzione(a), if (!.vuoto(fonte)) paste0("Fonte dei dati: ", .fonte_pulita(fonte))), collapse = ". "),
       category = sprintf("Licenza %s (%s)", LICENZA_DATI$nome, LICENZA_DATI$uri))
}
