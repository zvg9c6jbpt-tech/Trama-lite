# ─────────────────────────────────────────────────────────────
# avanzamento.R: a che punto è ogni busta, schede incomplete, «Continua»
# Dipende da crud.R (elenco_fascicoli, mancanti_piani, scrivi_impostazione), ricerca.R (campi_core) e compose.R (posizione_scritta)
# Per lavorare per buste: a che punto è ogni busta, quali schede sono incomplete, quale scheda viene dopo l'ultima toccata.
# ─────────────────────────────────────────────────────────────

# A che punto è ogni busta del fondo: schede fatte, attese dal piano (NA senza piano), numeri che mancano, incomplete e la prima da riaprire. Solo lettura.
avanzamento_buste <- function(con, fondo) {
  fondo <- .fid(con, fondo)
  vuota <- data.frame(busta = integer(0), suffisso_b = character(0), schede = integer(0), attese = integer(0), mancano = integer(0),
                      numeri = character(0), incomplete = integer(0), prima_incompleta = integer(0), stringsAsFactors = FALSE)
  if (.vuoto(fondo)) return(vuota)
  d <- elenco_fascicoli(con, fondo = fondo)
  d$suffisso_b <- ifelse(is.na(d$suffisso_b), "", d$suffisso_b)
  piani <- mancanti_piani(con); piani <- piani[piani$fondo_id == fondo, , drop = FALSE]
  piani$suffisso_b <- ifelse(is.na(piani$suffisso_b), "", piani$suffisso_b)
  chiavi <- unique(rbind(d[, c("busta", "suffisso_b")], piani[, c("busta", "suffisso_b")]))
  if (!nrow(chiavi)) return(vuota)
  chiavi <- chiavi[order(chiavi$busta, chiavi$suffisso_b), , drop = FALSE]
  d$core <- completezza_core(d)
  righe <- lapply(seq_len(nrow(chiavi)), function(i) {
    b <- chiavi$busta[i]; sb <- chiavi$suffisso_b[i]
    x <- d[which(d$busta == b & d$suffisso_b == sb), , drop = FALSE]
    p <- piani[which(piani$busta == b & piani$suffisso_b == sb), , drop = FALSE]
    inc <- x[which(x$core < 4L), , drop = FALSE]
    data.frame(busta = b, suffisso_b = sb, schede = nrow(x), attese = if (nrow(p)) p$attesi[1] else NA_integer_,
               mancano = if (nrow(p)) p$da_schedare[1] else NA_integer_, numeri = if (nrow(p)) p$numeri[1] else "",
               incomplete = nrow(inc), prima_incompleta = if (nrow(inc)) inc$id[1] else NA_integer_, stringsAsFactors = FALSE)
  })
  do.call(rbind, righe)
}

# Dove porta «Continua» (Home): la scheda dopo l'ultima toccata. Nella stessa busta il primo numero mancante del piano (senza piano, l'ultimo più uno);
# finita la busta, la prossima aperta (con fascicoli da schedare). NULL se non resta niente da schedare nel piano e non c'è un'ultima scheda.
prossimo_da_schedare <- function(con, fondo) {
  fondo <- .fid(con, fondo); if (.vuoto(fondo)) return(NULL)
  u <- ultimo_modificato(con, fondo)
  sb <- function(x) if (.vuoto(x)) "" else as.character(x)
  dove <- function(busta, suffisso_b, n) list(busta = as.integer(busta), suffisso_b = suffisso_b, fascicolo = as.integer(n),
                                              sigla_busta = if (!is.null(u)) u$sigla_busta else "", sigla_fascicolo = if (!is.null(u)) u$sigla_fascicolo else "")
  tiene <- !is.null(u) && !.vuoto(u$busta)
  if (tiene) {
    n <- prossimo_nel_piano(con, fondo, u$busta, corrente = u$fascicolo, suffisso_b = sb(u$busta_suffisso))
    if (!is.na(n)) return(dove(u$busta, sb(u$busta_suffisso), n))
  }
  av <- avanzamento_buste(con, fondo); aperte <- av[which(!is.na(av$mancano) & av$mancano > 0L), , drop = FALSE]
  if (nrow(aperte)) {
    if (tiene) {
      dopo <- aperte[which(aperte$busta > u$busta | (aperte$busta == u$busta & .rango_suffisso(aperte$suffisso_b) > .rango_suffisso(sb(u$busta_suffisso)))), , drop = FALSE]
      if (nrow(dopo)) aperte <- dopo
    }
    b <- aperte[1, ]
    return(dove(b$busta, b$suffisso_b, prossimo_nel_piano(con, fondo, b$busta, suffisso_b = b$suffisso_b)))
  }
  # Con un piano tutto schedato non si propone un numero oltre il piano (`piano_concluso`): andare avanti è una scelta («Aggiungi fuori piano»).
  if (piano_concluso(con, fondo)) return(NULL)
  prossimo_fuori_piano(con, fondo)
}
# Vero se il fondo ha almeno un piano di busta e nessuna busta col piano ha ancora numeri che mancano.
piano_concluso <- function(con, fondo) {
  fondo <- .fid(con, fondo); if (.vuoto(fondo)) return(FALSE)
  av <- avanzamento_buste(con, fondo); piani <- !is.na(av$mancano)
  if (!any(piani) || any(av$mancano[piani] > 0L)) return(FALSE)
  # Se l'ultima scheda toccata sta in una busta che il piano non copre, si lavora fuori piano per scelta: non è «piano completato»
  u <- ultimo_modificato(con, fondo)
  if (!is.null(u) && !.vuoto(u$busta)) {
    sb <- if (.vuoto(u$busta_suffisso)) "" else as.character(u$busta_suffisso)
    k <- which(av$busta == as.integer(u$busta) & (if ("suffisso_b" %in% names(av)) av$suffisso_b else "") == sb)
    if (length(k) && is.na(av$mancano[k[1]])) return(FALSE)
  }
  TRUE
}
# Il numero che segue l'ultima scheda toccata, nella sua busta, senza guardare il piano (come «Fascicolo seguente»).
prossimo_fuori_piano <- function(con, fondo) {
  fondo <- .fid(con, fondo); if (.vuoto(fondo)) return(NULL)
  u <- ultimo_modificato(con, fondo); if (is.null(u) || .vuoto(u$busta)) return(NULL)
  sb <- if (.vuoto(u$busta_suffisso)) "" else as.character(u$busta_suffisso)
  list(busta = as.integer(u$busta), suffisso_b = sb, fascicolo = as.integer(prossimo_numero(con, fondo, u$busta, suffisso_b = sb)),
       sigla_busta = u$sigla_busta, sigla_fascicolo = u$sigla_fascicolo)
}

# La busta come si legge sulla Home: «b. 12» o «b. 232-bis».
etichetta_busta <- function(busta, suffisso_b = "") paste0("b. ", busta, ifelse(is.na(suffisso_b), "", suffisso_b))

# Le schede con meno di quattro campi fondamentali, e quali mancano.
# Le schede datate solo per secolo («sec. XV»): complete per i quattro campi, ma senza un anno per le statistiche.
schede_solo_secolo <- function(con, fondo) {
  fondo <- .fid(con, fondo); if (.vuoto(fondo)) return(0L)
  d <- elenco_fascicoli(con, fondo = fondo); if (!nrow(d) || !"anno_non_risolto" %in% names(d)) return(0L)
  as.integer(sum(is.na(d$anno) & vapply(d$anno_non_risolto, .datazione_secolo, TRUE, USE.NAMES = FALSE)))
}
schede_incomplete <- function(con, fondo) {
  fondo <- .fid(con, fondo)
  vuota <- data.frame(segnatura = character(0), cosa_manca = character(0), titolo = character(0), stringsAsFactors = FALSE)
  if (.vuoto(fondo)) return(vuota)
  d <- elenco_fascicoli(con, fondo = fondo)
  if (!nrow(d)) return(vuota)
  m <- campi_core(d); k <- which(rowSums(m) < 4L)
  if (!length(k)) return(vuota)
  etichette <- c(persone_o_titolo = "persone o titolo", anno = "anno", luogo = "luogo", reato = "reato")
  data.frame(segnatura = posizione_scritta(d$sigla[k], d$busta[k], d$sigla_f[k], d$fascicolo[k], d$suffisso[k], d$suffisso_b[k]),
             cosa_manca = vapply(k, function(i) paste(etichette[!unlist(m[i, ])], collapse = "; "), ""),
             titolo = ifelse(is.na(d$titolo[k]), "", d$titolo[k]), stringsAsFactors = FALSE)
}
