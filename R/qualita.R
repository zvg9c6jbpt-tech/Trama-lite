# ─────────────────────────────────────────────────────────────
# qualita.R: controlli sulla consistenza del dato (SPEC §8)
# Dipende da profili.R (vocabolario), lista_luoghi.R (leggi_lista_luoghi), luoghi.R (forme_locali_luoghi), estrazione.R (estrai_da_titolo), crud.R (mancanti_piani).
# Segnala e basta: non corregge nulla. Ogni voce è una domanda da portare sulla fonte, non un errore da sanare d'ufficio.
# ─────────────────────────────────────────────────────────────

# Ripiegamento degli accenti a mano: iconv(to = "ASCII//TRANSLIT") darebbe risultati diversi su macOS e Windows.
.senza_accenti <- function(x) {
  chartr("àáâãäåèéêëìíîïòóôõöùúûüçñ",
         "aaaaaaeeeeiiiiooooouuuucn", x)
}

chiave_grafia <- function(x) {
  x <- .senza_accenti(tolower(trimws(as.character(x))))
  gsub("[^a-z0-9]", "", x)
}


# Forme diverse che collassano sulla stessa chiave: candidate a
# essere la stessa entità indicizzata due volte.
grafie_vicine <- function(con, tabella = c("persone", "luoghi"), fondo = NULL) {

  tabella <- match.arg(tabella)
  col     <- if (tabella == "persone") "persona" else "luogo"

  sql <- sprintf("SELECT DISTINCT x.%s AS v FROM %s x JOIN fascicoli f ON f.id = x.fascicolo_id",
                 col, tabella)
  v <- dbGetQuery(con, .solo_fondo(sql, fondo))$v
  vuota <- data.frame(forme = character(0), n = integer(0), stringsAsFactors = FALSE)
  if (length(v) < 2) return(vuota)
  
  g <- split(v, chiave_grafia(v))
  g <- g[vapply(g, length, integer(1)) > 1]
  if (!length(g)) return(vuota)

  # Le grafie già dichiarate la stessa persona (una fusione) non sono
  # più una domanda aperta: escono dall'elenco (TODO 20, 21/09).
  if (tabella == "persone") {
    id <- dbGetQuery(con, "SELECT persona, scheda_id FROM persona_identita")
    if (nrow(id)) {
      scheda_di <- split(id$scheda_id, id$persona)
      risolto <- function(forme) {
        s <- lapply(forme, function(f) unique(scheda_di[[f]]))
        all(lengths(s) == 1) && length(unique(unlist(s))) == 1
      }
      g <- g[!vapply(g, risolto, logical(1))]
      if (!length(g)) return(vuota)
    }
  }
  
  data.frame(
    forme = vapply(g, function(x) paste(sort(x), collapse = "   ·   "), character(1)),
    n     = vapply(g, length, integer(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}


# I controlli che sono insiemi di fascicoli: una query SQL che conta. Con SELECT id al posto di COUNT(*) dice QUALI (è ciò che il clic su una riga apre).
.CONTROLLI_QUALITA <- list(
  c("Anno assente",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE estremo_remoto IS NULL AND data_nd = 0",
    "senza data e senza «n.d.»: la data va cercata o dichiarata assente nella fonte"),
  c("Soggettario e regesto discordi",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE soggettario_discorde = 1",
    "nel foglio i due campi nominavano reati diversi: qui stanno uniti, da rivedere sulla fonte"),
  c("Anno dichiarato n.d.",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE data_nd = 1",
    "la fonte non dà la data: fuori dalle serie temporali, per scelta dichiarata"),
  c("Anno fuori 1480-1800",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE estremo_remoto NOT BETWEEN 1480 AND 1800",
    "da verificare sulla fonte: potrebbe essere giusto"),
  c("Segnature doppie senza numero",
    "SELECT COUNT(*) AS n FROM fascicoli f
       WHERE (f.busta IS NULL OR f.fascicolo IS NULL)
         AND EXISTS (SELECT 1 FROM fascicoli g WHERE g.id <> f.id AND g.fondo_id IS f.fondo_id
                       AND g.busta IS f.busta AND g.busta_suffisso = f.busta_suffisso
                       AND g.fascicolo IS f.fascicolo AND g.fascicolo_suffisso = f.fascicolo_suffisso)",
    "due schede con la stessa segnatura e il numero di busta o di fascicolo vuoto: o sono la stessa scheda scritta due volte, o manca un numero. Finché ce ne sono, il programma non può impedire che si ripetano"),
  c("Estremo recente anteriore al remoto",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE estremo_recente < estremo_remoto",
    "inversione di date: «Controlla e correggi…» chiede quale dei due è giusto"),
  c("Titolo assente",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE TRIM(COALESCE(titolo,'')) = ''",
    "si compone dalle persone citate, quando ci sono"),
  c("Regesto assente",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE TRIM(COALESCE(regesto,'')) = '' AND non_procedimento = 0",
    "una carta «non è un procedimento» (documenti sciolti, catalogo) non ha regesto composto"),
  c("Lingua assente",
    "SELECT COUNT(*) AS n FROM fascicoli f
       WHERE NOT EXISTS (SELECT 1 FROM lingue l WHERE l.fascicolo_id = f.id) AND f.non_procedimento = 0",
    "la scheda non dice in che lingua è scritta; il programma non la inventa: si scrive nella scheda, dopo aver guardato la fonte"),
  c("Senza tipo di reato",
    "SELECT COUNT(*) AS n FROM fascicoli f
       WHERE NOT EXISTS (SELECT 1 FROM reati r WHERE r.fascicolo_id = f.id)
         AND f.reato_ignoto = 0 AND f.non_procedimento = 0",
    "escluso da ogni aggregazione per reato; non conta chi è «reato ignoto» o «non è un procedimento»"),
  c("Termini senza la loro classe",
    "SELECT COUNT(*) AS n FROM fascicoli f
       WHERE EXISTS (SELECT 1 FROM reato_termini t WHERE t.fascicolo_id = f.id
                       AND NOT EXISTS (SELECT 1 FROM reati r WHERE r.fascicolo_id = f.id AND r.reato = t.reato))",
    "un termine del tesauro registrato sotto una classe che la scheda non ha più: si toglie salvando la scheda (il termine segue la sua classe)"),
  c("Senza regesto strutturato",
    "SELECT COUNT(*) AS n FROM fascicoli
      WHERE tortura = 'n.d.' AND sentenza = 'n.d.'
      AND non_procedimento = 0",
    "la schedatura che manca"),
  c("Tortura non esaminata",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE tortura_esaminata = 0",
    "coda di lavoro: nessuno ha ancora controllato la fonte, non è un «no»"),
  c("Sentenza non esaminata",
    "SELECT COUNT(*) AS n FROM fascicoli WHERE sentenza_esaminata = 0",
    "coda di lavoro: nessuno ha ancora controllato la fonte, non è un «no»")
)


# Qualità guarda un fondo solo: la stessa query ristretta ai fascicoli di quel fondo, riscrivendo il FROM (non c'è una seconda versione da tenere allineata).
.solo_fondo <- function(sql, fondo) {
  if (is.null(fondo) || !nzchar(fondo)) return(sql)
  # `fondo` è l'id del fondo; una denominazione, per script e collaudi, vale per i fondi che la portano.
  dentro <- if (.e_id_fondo(fondo)) sprintf("(SELECT * FROM fascicoli WHERE fondo_id = '%s')", fondo)
            else sprintf("(SELECT * FROM fascicoli WHERE fondo_id IN (SELECT id FROM fondi WHERE denominazione = '%s' COLLATE NOCASE))", gsub("'", "''", fondo))
  # FROM e JOIN, con o senza alias: le query dei controlli usano tutte e
  # due le forme ("FROM fascicoli f", "JOIN fascicoli f ON …").
  sql <- gsub("(FROM|JOIN) fascicoli f\\b", paste("\\1", dentro, "f"), sql)
  gsub("(FROM|JOIN) fascicoli(?! [a-z(])", paste("\\1", dentro), sql, perl = TRUE)
}

# Gli id dei fascicoli di un fondo (NULL = tutti): serve alle funzioni
# che lavorano per id e non per query.
ids_fondo <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (is.null(fondo) || !nzchar(fondo)) return(NULL)
  dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo_id = ?", list(fondo))$id
}


# I luoghi citati (i più citati per primi) la cui grafia non è nella lista. Le forme locali di `dati/luoghi_forma_locale.csv` («Avignon (Avignone)») sono vocabolario anche se la lista non le ha.
luoghi_fuori_vocabolario <- function(con, ids = NULL) {
  filtro <- ""
  if (!is.null(ids)) {
    if (length(ids) == 0) ids <- NA_character_    # nessun fascicolo, nessun luogo
    filtro <- sprintf("WHERE fascicolo_id IN (%s)", paste(rep("?", length(ids)), collapse = ", "))
  }
  d <- dbGetQuery(con, paste("SELECT luogo AS nome, COUNT(*) AS occorrenze FROM luoghi", filtro,
                             "GROUP BY luogo ORDER BY occorrenze DESC, luogo COLLATE NOCASE"),
                  params = if (is.null(ids)) NULL else as.list(ids))
  noti <- c(leggi_lista_luoghi()$chiave, chiave_grafia(forme_locali_luoghi()$locale))
  d$nome[is.na(match(chiave_grafia(d$nome), noti))]
}

qualita <- function(con, fondo = NULL) {
  fondo <- .fid(con, fondo)

  q <- function(sql) dbGetQuery(con, .solo_fondo(sql, fondo))$n
  controlli <- .CONTROLLI_QUALITA
  ids <- ids_fondo(con, fondo)

  base <- do.call(rbind, lapply(controlli, function(x)
    data.frame(controllo = x[1], casi = q(x[2]), nota = x[3],
               stringsAsFactors = FALSE)))
  
  # Il vocabolario è quello di base più i reati ammessi per il fondo (R/colonne.R).
  fuori <- dbGetQuery(con, .solo_fondo(
    "SELECT DISTINCT r.reato AS reato, f.fondo_id AS fondo FROM reati r JOIN fascicoli f ON f.id = r.fascicolo_id", fondo))
  fuori <- unique(fuori$reato[!mapply(function(r, fo) r %in% vocabolario_di(con, "reato", fo), fuori$reato, fuori$fondo)])
  
  vp <- grafie_vicine(con, "persone", fondo)
  vl <- grafie_vicine(con, "luoghi", fondo)

  # Gli esempi dentro una nota sono i primi, con quanti restano: senza limite la nota degli omonimi diventava un muro di testo.
  .esempi <- function(x, quanti = 20) {
    if (!length(x)) return("")
    t <- paste(head(x, quanti), collapse = " · ")
    if (length(x) > quanti) t <- paste0(t, " · e altri ", length(x) - quanti)
    t
  }

  # I luoghi citati che la lista non conosce: una coda di lavoro, non un difetto; i più citati per primi.
  sc <- luoghi_fuori_vocabolario(con, ids)
  coda <- .esempi(sc)

  # Fascicoli attesi dai piani di busta e non ancora schedati: un'altra
  # coda di lavoro. Il fondo compare solo se i piani ne toccano più d'uno.
  mp <- mancanti_piani(con)
  if (!is.null(fondo) && nzchar(fondo)) mp <- mp[mp$fondo_id == fondo, , drop = FALSE]
  mp <- mp[mp$da_schedare > 0, , drop = FALSE]
  voci <- sprintf("%sb. %s: %s", if (length(unique(mp$fondo)) > 1) paste0(mp$fondo, ", ") else "",
                  paste0(mp$busta, mp$suffisso_b), mp$numeri)
  piani <- paste(head(voci, 10), collapse = " · ")
  if (length(voci) > 10) piani <- paste0(piani, " · e altre ", length(voci) - 10, " buste")

  ris <- rbind(
    base,
    data.frame(controllo = "Reati fuori vocabolario", casi = length(fuori),
               nota = .esempi(fuori), stringsAsFactors = FALSE),
    data.frame(controllo = "Persone con grafie divergenti", casi = nrow(vp),
               nota = "stessa persona, due voci d'indice", stringsAsFactors = FALSE),
    data.frame(controllo = "Luoghi con grafie divergenti", casi = nrow(vl),
               nota = "", stringsAsFactors = FALSE),
    data.frame(controllo = "Luoghi fuori vocabolario", casi = length(sc),
               nota = if (length(sc)) paste("da aggiungere alla lista dei luoghi o da correggere:", coda) else "",
               stringsAsFactors = FALSE),
    data.frame(controllo = "Fascicoli pianificati da schedare", casi = sum(mp$da_schedare),
               nota = if (nrow(mp)) paste("coda di lavoro:", piani) else "",
               stringsAsFactors = FALSE)
  )
  ris
}

# Controlli che sono lo stato del lavoro sul fondo, non anomalie: finché nessuno ha guardato la fonte per tortura e sentenza valgono per tutti e coprirebbero i casi veri.
QUALITA_DI_FONDO <- c("Senza regesto strutturato", "Tortura non esaminata", "Sentenza non esaminata")

# L'ordine dei controlli: prima quelli con casi (dal più numeroso), poi il lavoro di fondo, in coda quelli a zero. Aggiunge `gruppo`; tabella e clic usano la stessa funzione.
qualita_ordinata <- function(q, a_posto = FALSE) {
  fondo <- q$controllo %in% QUALITA_DI_FONDO
  q$gruppo <- ifelse(fondo & q$casi > 0, "Lavoro di fondo, non anomalie",
              ifelse(q$casi > 0, "Da vedere", "A posto"))
  rango <- match(q$gruppo, c("Da vedere", "Lavoro di fondo, non anomalie", "A posto"))
  q <- q[order(rango, -q$casi, seq_len(nrow(q))), , drop = FALSE]
  # I controlli senza casi sono una riga chiusa sotto («A posto: N controlli»); `a_posto = TRUE` li rende.
  if (!a_posto) q <- q[q$gruppo != "A posto", , drop = FALSE] else q <- q[q$gruppo == "A posto", , drop = FALSE]
  rownames(q) <- NULL
  q
}

# Quali fascicoli stanno dietro una riga di Qualità. NULL se il controllo non è un insieme di fascicoli (grafie, luoghi, piani, reati fuori vocabolario).
qualita_fascicoli <- function(con, controllo, fondo = NULL) {
  for (x in .CONTROLLI_QUALITA) {
    if (!identical(x[1], controllo)) next
    sql <- sub("SELECT COUNT(*) AS n FROM fascicoli f", "SELECT f.id AS id FROM fascicoli f", x[2], fixed = TRUE)
    sql <- sub("SELECT COUNT(*) AS n FROM fascicoli",   "SELECT id FROM fascicoli",         sql,  fixed = TRUE)
    return(dbGetQuery(con, .solo_fondo(sql, fondo))$id)
  }
  NULL
}
