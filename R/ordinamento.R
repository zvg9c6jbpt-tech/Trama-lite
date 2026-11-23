# Ordine di consultazione e collocazione sono indipendenti.
# «232» e «232-bis» sono due buste (12/10): `suffisso_b` le distingue.
elenco_busta <- function(con, fondo, busta, suffisso_b = "") {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) stop("Scegli un fondo.")
  d <- dbGetQuery(con, "SELECT id, fondo_id, fondo, busta, busta_suffisso AS suffisso_b, fascicolo, fascicolo_suffisso, titolo,
                              modificato FROM fascicoli WHERE fondo_id = ? AND busta IS ?
                              AND (? IS NULL OR busta_suffisso = ?)
                        ORDER BY fascicolo, fascicolo_suffisso, id",
                  list(fondo, busta, if (is.na(busta)) NA_character_ else .suffisso_busta_db(suffisso_b),
                       if (is.na(busta)) NA_character_ else .suffisso_busta_db(suffisso_b)))
  ordina_consultazione(con, d)
}

ordina_consultazione <- function(con, d) {
  if (!nrow(d)) return(d)
  o <- dbGetQuery(con, "SELECT fascicolo_id, posizione FROM ordine_fascicoli")
  pos <- o$posizione[match(d$id, o$fascicolo_id)]
  # Le schede appena aggiunte, ancora senza posizione, vanno in fondo alla busta.
  sb <- if ("suffisso_b" %in% names(d)) d$suffisso_b else rep("", nrow(d))
  d[order(d$busta, .rango_suffisso(sb), tolower(trimws(ifelse(is.na(sb), "", sb))),
          ifelse(is.na(pos), Inf, pos), seq_len(nrow(d)), na.last = TRUE), , drop = FALSE]
}

salva_ordine_busta <- function(con, fondo, busta, ids, suffisso_b = "") .atomico(con, {
  attuali <- elenco_busta(con, fondo, busta, suffisso_b)$id
  if (anyDuplicated(ids) || !setequal(ids, attuali))
    stop("I fascicoli della busta sono cambiati. Ricarica l'elenco prima di salvare.")
  for (i in seq_along(ids))
    dbExecute(con, "INSERT INTO ordine_fascicoli (fascicolo_id, posizione) VALUES (?, ?)
                   ON CONFLICT(fascicolo_id) DO UPDATE SET posizione = excluded.posizione", list(ids[i], i))
  invisible(length(ids))
})

.numero_collocazione <- function(x, vuoto = FALSE) {
  if (.vuoto(x) && vuoto) return(NA_integer_)
  n <- suppressWarnings(as.numeric(x))
  if (length(n) != 1 || !is.finite(n) || n < 1 || n > .Machine$integer.max || n != floor(n))
    stop("Busta e fascicolo devono essere numeri interi positivi.")
  as.integer(n)
}

piano_collocazioni <- function(d, busta, primo, suffisso = NULL) {
  if (!nrow(d)) stop("Seleziona almeno un fascicolo.")
  busta <- .numero_collocazione(busta, vuoto = TRUE)
  primo <- .numero_collocazione(primo)
  numeri <- as.double(primo) + seq_len(nrow(d)) - 1
  if (any(numeri > .Machine$integer.max)) stop("Numerazione troppo alta.")
  d$nuova_busta <- busta
  d$nuovo_fascicolo <- as.integer(numeri)
  d$nuovo_suffisso <- if (is.null(suffisso)) d$fascicolo_suffisso else as.character(suffisso)
  d
}

applica_collocazioni <- function(con, piano) .atomico(con, {
  if (!nrow(piano) || anyDuplicated(piano$id)) stop("Piano di correzione non valido.")
  originali <- c("id", "fondo_id", "busta", "fascicolo", "fascicolo_suffisso", "modificato")
  for (i in seq_len(nrow(piano))) {
    r <- piano[i, ]
    attuale <- dbGetQuery(con, "SELECT id, fondo_id, busta, fascicolo, fascicolo_suffisso, modificato
                               FROM fascicoli WHERE id = ?", list(r$id))
    if (!isTRUE(all.equal(attuale, r[, originali, drop = FALSE], check.attributes = FALSE)))
      stop("Una scheda è cambiata dopo l'anteprima. Riapri la correzione.")
    .numero_collocazione(r$nuova_busta, vuoto = TRUE)
    .numero_collocazione(r$nuovo_fascicolo)
    if (is.na(r$nuovo_suffisso)) stop("Il suffisso non può essere mancante: lascialo vuoto.")
    conflitti <- dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo_id = ? AND busta IS ?
                      AND fascicolo = ? AND fascicolo_suffisso = ?",
                           list(r$fondo_id, r$nuova_busta, r$nuovo_fascicolo, r$nuovo_suffisso))$id
    if (any(!conflitti %in% piano$id)) stop("Una segnatura di destinazione è già occupata. Nulla è stato cambiato.")
  }
  destinazioni <- piano[, c("fondo_id", "nuova_busta", "nuovo_fascicolo", "nuovo_suffisso")]
  if (anyDuplicated(destinazioni)) stop("Due fascicoli avrebbero la stessa segnatura.")
  # Sgombra le segnature prima degli scambi, sempre dentro il SAVEPOINT.
  provvisorio <- paste0("__riordino_", uuid::UUIDgenerate())
  for (id in piano$id)
    dbExecute(con, "UPDATE fascicoli SET fascicolo_suffisso = ? WHERE id = ?", list(paste0(provvisorio, id), id))
  for (i in seq_len(nrow(piano))) {
    r <- piano[i, ]
    dbExecute(con, "UPDATE fascicoli SET busta = ?, fascicolo = ?, fascicolo_suffisso = ?, modificato = ? WHERE id = ?",
              list(r$nuova_busta, r$nuovo_fascicolo, r$nuovo_suffisso, .ora(), r$id))
    dbExecute(con, "DELETE FROM ordine_fascicoli WHERE fascicolo_id = ?", list(r$id))
  }
  invisible(nrow(piano))
})
