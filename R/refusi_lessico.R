# ─────────────────────────────────────────────────────────────
# R/refusi_lessico.R: check di base sui refusi nel latino (e in ogni testo libero)
# Titolo e Regesto sono testo libero: nessun dizionario li giudica, il riferimento è il corpus stesso (le parole di titoli e regesti nel foglio e nel database).
# Una parola scritta UNA volta, a una lettera da una parola frequente (≥ 3 volte), è un sospetto refuso («causaraum» → «causarum»): solo PROPOSTA, mai spuntata di partenza.
# Prudenza (meglio un refuso non visto che un latino giusto «corretto»): solo parole minuscole di almeno 6 lettere; la differenza non cade sull'ultima lettera
# («bello»/«bellum» sono forme valide); se due parole frequenti competono con lo stesso peso, si tace.
# Limite: con un corpus piccolo nessuna parola arriva a 3 occorrenze e il controllo tace.
# ─────────────────────────────────────────────────────────────

REFUSI_MIN_LETTERE <- 6L
REFUSI_FREQUENTE   <- 3L

# Le parole minuscole di un testo, nell'ordine; le maiuscole (nomi propri, inizio di frase) non entrano.
.parole_minuscole <- function(testo) {
  if (length(testo) == 0 || is.na(testo[1]) || !nzchar(testo[1])) return(character(0))
  p <- regmatches(testo[1], gregexpr("[[:alpha:]]+", testo[1], perl = TRUE))[[1]]
  p <- p[grepl("^[[:lower:]]", p, perl = TRUE) & nchar(p) >= REFUSI_MIN_LETTERE]
  p
}

# Quante volte compare ogni parola minuscola in un insieme di testi.
.lessico_corpus <- function(testi) {
  tutte <- unlist(lapply(as.character(testi), .parole_minuscole), use.names = FALSE)
  if (!length(tutte)) return(integer(0))
  tab <- table(tutte); stats::setNames(as.integer(tab), names(tab))
}

# La differenza è una sola lettera e NON è l'ultima: stessa lunghezza (sostituzione) o ±1 (lettera in più o in meno).
.una_lettera_non_finale <- function(w, v) {
  if (substr(w, nchar(w), nchar(w)) != substr(v, nchar(v), nchar(v))) return(FALSE)
  as.numeric(utils::adist(w, v)) == 1
}

# Le parole di `testo` sospette, con la forma proposta. `lessico` = .lessico_corpus() del corpus (testo compreso).
.sospetti_refusi <- function(testo, lessico) {
  vuoto <- data.frame(parola = character(0), proposta = character(0), stringsAsFactors = FALSE)
  if (!length(lessico)) return(vuoto)
  frequenti <- lessico[lessico >= REFUSI_FREQUENTE]
  if (!length(frequenti)) return(vuoto)
  out <- list()
  for (w in unique(.parole_minuscole(testo))) {
    if (!isTRUE(lessico[w] == 1L)) next
    cand <- names(frequenti)[abs(nchar(names(frequenti)) - nchar(w)) <= 1L]
    cand <- cand[vapply(cand, function(v) .una_lettera_non_finale(w, v), TRUE)]
    if (!length(cand)) next
    peso <- frequenti[cand]; migliori <- cand[peso == max(peso)]
    if (length(migliori) != 1L) next
    out[[length(out) + 1L]] <- data.frame(parola = w, proposta = migliori, stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out) else vuoto
}

# Riscrive il testo con le proposte, parola intera e stessa maiuscola iniziale.
.applica_refusi <- function(testo, sospetti) {
  for (k in seq_len(nrow(sospetti)))
    testo <- gsub(sprintf("(?<![[:alpha:]])%s(?![[:alpha:]])", sospetti$parola[k]), sospetti$proposta[k], testo, perl = TRUE)
  testo
}
