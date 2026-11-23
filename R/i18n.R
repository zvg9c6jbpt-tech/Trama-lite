# ─────────────────────────────────────────────────────────────
# i18n.R - vocabolario controllato in inglese (strato 1)
# Dipende da profili.R (VOCABOLARI, la stessa struttura a specchio)
# Solo il vocabolario controllato: non le etichette dei campi né il testo libero. Il database resta sempre italiano:
# la tabella traduce solo l'ETICHETTA mostrata, mai il valore salvato o esportato (l'export Arianna resta italiano, regola del programma).
# ─────────────────────────────────────────────────────────────

# Il profilo del materiale (profili/<nome>/profilo.json): se questo file è letto da solo, lo si carica prima.
if (!exists("PROFILO_DOMINIO", inherits = FALSE)) source("R/profilo_dominio.R")

# Le traduzioni dei vocabolari del materiale (soggettario, supporti, lingue, tipologie, produttori) sono del profilo (`traduzioni_en`);
# qui restano quelle dei valori che servono qualunque sia il materiale.
.EN_PROFILO <- lapply(PROFILO_DOMINIO$traduzioni_en$vocabolari, .pd_vett)
VOCABOLARI_EN <- list(
  reato = .EN_PROFILO$reato,
  tortura = c("n.d." = "not stated", "sì" = "yes", "no" = "no"),
  sentenza = c("n.d." = "not stated", "sì" = "yes", "no" = "no"),
  attribuzione        = c("n.d." = "not stated", "collettiva" = "collective", "per persona" = "by person"),
  attribuzione_luoghi = c("n.d." = "not stated", "collettiva" = "collective", "per reato" = "by offence"),
  definizione            = .EN_PROFILO$definizione,
  supporto               = .EN_PROFILO$supporto,
  tipologia_documentaria = .EN_PROFILO$tipologia_documentaria,
  lingua                 = .EN_PROFILO$lingua,
  soggetto               = .EN_PROFILO$soggetto
)

# Le scelte di un campo (già in italiano, da .scelte/.voc) come vettore NOMINATO per selectizeInput: nome = etichetta mostrata (inglese se lingua="en" e il valore è tradotto,
# altrimenti l'italiano), valore = sempre l'italiano. Ciò che arriva a raccogli_modulo() e al database non cambia mai.
# Confronto senza maiuscole: sui dati veri "Definizione" e "Supporto" hanno sia "Fascicolo" che "fascicolo" (incoerenza già nei dati, la ricerca deve solo ignorarla).
traduci_scelte <- function(campo, scelte, lingua = "it") {
  if (lingua != "en" || is.null(campo) || is.na(campo) || length(scelte) == 0) return(scelte)
  mappa <- VOCABOLARI_EN[[campo]]
  if (is.null(mappa)) return(scelte)
  chiavi <- tolower(names(mappa))
  etichette <- vapply(scelte, function(v) {
    i <- match(tolower(v), chiavi)
    if (is.na(i)) v else unname(mappa[i])
  }, character(1))
  stats::setNames(scelte, etichette)
}


# ── Strato 2: i testi dell'interfaccia ────────────────────
# Lo schermo lo traduce il browser (www/traduci.js) con www/trama-en.json, generato dal foglio di traduzione (traduzione/costruisci_dizionario.R).
# Lo stesso dizionario serve al server per ciò che il browser non può toccare: i grafici, che sono immagini.
.DIZ_EN <- new.env()
dizionario_en <- function(percorso = "www/trama-en.json") {
  if (is.null(.DIZ_EN$d)) {
    d <- if (file.exists(percorso)) jsonlite::fromJSON(percorso, simplifyVector = FALSE) else list(esatti = list(), modelli = list())
    e <- unlist(d$esatti)
    .DIZ_EN$d <- list(esatti = e, rifilati = stats::setNames(trimws(e), trimws(names(e))), modelli = d$modelli)
  }
  .DIZ_EN$d
}

# Un testo dello schermo in inglese: voce esatta, o modello con numeri
# («357 fascicoli datati · 1549–1578»). Ciò che non si riconosce resta com'è.
traduci_testo <- function(x, lingua = "it") {
  if (!identical(lingua, "en") || !length(x)) return(x)
  d <- dizionario_en()
  vapply(as.character(x), function(t) {
    if (is.na(t) || !nzchar(t)) return(t)
    if (!is.null(d$esatti[t]) && !is.na(d$esatti[t])) return(unname(d$esatti[t]))
    r <- trimws(t)
    if (!is.na(d$rifilati[r])) return(sub(r, d$rifilati[r], t, fixed = TRUE))
    for (m in d$modelli) {
      g <- regmatches(t, regexec(m$re, t, perl = TRUE))[[1]]
      if (!length(g)) next
      en <- if (!is.null(m$en1) && identical(g[2], "1")) m$en1 else m$en
      for (i in rev(seq_along(g)[-1])) en <- gsub(paste0("$", i - 1), g[i], en, fixed = TRUE)
      return(en)
    }
    t
  }, "", USE.NAMES = FALSE)
}

