# ─────────────────────────────────────────────────────────────
# fusioni.R: le grafie divergenti: fusione dei luoghi e proposta
# Dipende da crud.R (.ora, .riscrivi, .righe_ordinate) e da qualita.R (.solo_fondo)
# Fondere due grafie di un LUOGO è una correzione di grafia: riscrive luoghi.luogo e i tre campi scalari dove un luogo fa da chiave.
# Le persone non si fondono: Lite non ha schede persona né identità dichiarate (le grafie vicine si vedono, non si uniscono).
# ─────────────────────────────────────────────────────────────

# Fonde un luogo in un altro: luoghi.luogo (i luoghi citati, con l'ordine) e i tre campi scalari dove un luogo fa da chiave
# (luogo_reato, luogo_produzione, luogo_conservazione). Nessuna scheda per i luoghi: non hanno biografia, solo grafie da correggere.
unisci_luoghi <- function(con, destinazione, assorbita) {
  destinazione <- trimws(destinazione); assorbita <- trimws(assorbita)
  if (!nzchar(destinazione) || !nzchar(assorbita) || destinazione == assorbita) return(0L)

  dbBegin(con)
  riscritti <- tryCatch({
    fascc <- dbGetQuery(con, "SELECT DISTINCT fascicolo_id FROM luoghi WHERE luogo = ?", list(assorbita))$fascicolo_id
    for (id in fascc) {
      attuali <- dbGetQuery(con, "SELECT luogo FROM luoghi WHERE fascicolo_id = ? ORDER BY ordine", list(id))$luogo
      nuovi <- ifelse(attuali == assorbita, destinazione, attuali)
      .riscrivi(con, "luoghi", id, .righe_ordinate(nuovi, "luogo"))
    }
    for (campo in c("luogo_produzione", "luogo_conservazione"))
      dbExecute(con, sprintf("UPDATE fascicoli SET %s = ? WHERE %s = ?", campo, campo),
                list(destinazione, assorbita))
    length(fascc)
  }, error = function(e) { dbRollback(con); stop(e) })
  dbCommit(con)
  riscritti
}


# Dai gruppi di grafie vicine (qualita.R) a una proposta di fusione: vince la forma più frequente, a parità l'ordine alfabetico (deterministico).
# `fondo`: la proposta guarda solo il fondo su cui si lavora, come la tabella «Grafie divergenti» sopra (.solo_fondo, R/qualita.R);
# l'unione poi vale per quelle grafie ovunque compaiano, perché è la stessa forma scritta.
proposta_fusione <- function(con, tabella = c("persone", "luoghi"), fondo = NULL) {
  tabella <- match.arg(tabella)
  col     <- if (tabella == "persone") "persona" else "luogo"
  vuota <- data.frame(gruppo = character(0), destinazione = character(0),
                      assorbite = character(0), stringsAsFactors = FALSE)

  # La frequenza va contata sui valori grezzi (una riga per occorrenza); i gruppi si formano sui valori distinti,
  # altrimenti la stessa grafia ripetuta in più fascicoli sembrerebbe un gruppo da fondere senza alcuna divergenza.
  grezzo    <- dbGetQuery(con, .solo_fondo(
                 sprintf("SELECT x.%s AS v FROM %s x JOIN fascicoli f ON f.id = x.fascicolo_id",
                         col, tabella), fondo))$v
  frequenza <- table(grezzo)
  distinti  <- unique(grezzo)
  if (length(distinti) < 2) return(vuota)

  g <- split(distinti, chiave_grafia(distinti))
  g <- g[vapply(g, length, integer(1)) > 1]
  if (!length(g)) return(vuota)

  # Le grafie già dichiarate la stessa persona non si propongono più:
  # la fusione è fatta, e rifarla non cambierebbe niente (21/09).
  if (tabella == "persone") {
    id <- dbGetQuery(con, "SELECT persona, scheda_id FROM persona_identita")
    if (nrow(id)) {
      scheda_di <- split(id$scheda_id, id$persona)
      g <- g[!vapply(g, function(forme) {
        s <- lapply(forme, function(f) unique(scheda_di[[f]]))
        all(lengths(s) == 1) && length(unique(unlist(s))) == 1
      }, logical(1))]
      if (!length(g)) return(vuota)
    }
  }

  righe <- lapply(g, function(forme) {
    conteggi <- frequenza[forme]
    vincitrici <- forme[conteggi == max(conteggi)]
    destinazione <- sort(vincitrici)[1]
    data.frame(gruppo = paste(sort(forme), collapse = "   ·   "),
               destinazione = destinazione,
               assorbite = paste(setdiff(forme, destinazione), collapse = " | "),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, righe)
}
