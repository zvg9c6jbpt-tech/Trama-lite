# ─────────────────────────────────────────────────────────────
# estrazione.R: propone imputato/luogo/identità dubbia leggendo un titolo già scritto (import da catalogo esistente)
# Funzioni pure: non toccano il database. Verificata sui 279 titoli ASDNa reali (100% dei titoli non vuoti riconosciuti: 217 definito, 33 collettivo).
# Complementare a compose.R: quella compone il titolo per la schedatura dal vivo, questa lo legge per l'import.
# ─────────────────────────────────────────────────────────────

GENTILIZI <- list(greco = "Grecia", fiorentino = "Firenze", boemo = "Boemia",
                  castigliano = "Castiglia", gallo = "Francia")

.classifica_pezzo <- function(pezzo) {
  p  <- trimws(pezzo)
  pl <- tolower(p)
  if (grepl("^alias\\s+", pl))
    return(list(tipo = "alias", valore = trimws(sub("^alias\\s+", "", p, ignore.case = TRUE))))
  if (grepl("^(detto|detta)\\s+", pl))
    return(list(tipo = "soprannome", valore = trimws(sub("^(detto|detta)\\s+", "", p, ignore.case = TRUE))))
  if (pl %in% names(GENTILIZI))
    return(list(tipo = "luogo", valore = GENTILIZI[[pl]]))
  if (grepl("^di\\s+\\S", pl))
    return(list(tipo = "luogo", valore = trimws(sub("^di\\s+", "", p, ignore.case = TRUE))))
  list(tipo = "scarto", valore = p)
}

# Cosa si può dedurre da un titolo:
#   tipo = "vuoto"      titolo assente
#   tipo = "collettivo" «Atti per diversi» e varianti: nessun nome estratto, solo il segnale che l'identità va segnata dubbia
#   tipo = "definito"   circa/contro/su/di + nome: un imputato preciso
#   tipo = "nessuno"    non rientra nella grammatica, non si tocca nulla
estrai_da_titolo <- function(titolo) {
  vuoto <- list(tipo = "vuoto", nomi = character(0), alias = character(0),
               soprannomi = character(0), luoghi = character(0), scarti = character(0))
  if (is.null(titolo) || is.na(titolo) || !nzchar(trimws(titolo))) return(vuoto)

  if (grepl("per diversi|\\bdiversi\\b", titolo, ignore.case = TRUE)) {
    vuoto$tipo <- "collettivo"
    return(vuoto)
  }

  m <- regexpr("\\b(circa|contro|su|di)\\s+", titolo, ignore.case = TRUE, perl = TRUE)
  if (m[1] == -1) { vuoto$tipo <- "nessuno"; return(vuoto) }

  resto <- substring(titolo, m[1] + attr(m, "match.length"))

  alias_par <- character(0)
  mp <- regmatches(resto, gregexpr("\\(([^)]*)\\)", resto))[[1]]
  if (length(mp)) {
    contenuto <- gsub("^\\(|\\)$", "", mp)
    contenuto <- sub("^alias\\s+", "", contenuto, ignore.case = TRUE)
    alias_par <- trimws(contenuto)
  }
  resto <- trimws(gsub("\\([^)]*\\)", "", resto))

  pezzi <- trimws(strsplit(resto, ",")[[1]])
  pezzi <- pezzi[nzchar(pezzi)]
  nome  <- if (length(pezzi)) pezzi[1] else NA_character_
  altri <- if (length(pezzi) > 1) pezzi[-1] else character(0)

  luoghi <- character(0); alias <- alias_par; soprannomi <- character(0); scarti <- character(0)
  for (pz in altri) {
    c <- .classifica_pezzo(pz)
    if (c$tipo == "alias")      alias      <- c(alias,      c$valore)
    if (c$tipo == "soprannome") soprannomi <- c(soprannomi, c$valore)
    if (c$tipo == "luogo")      luoghi     <- c(luoghi,     c$valore)
    if (c$tipo == "scarto")     scarti     <- c(scarti,     c$valore)
  }

  list(tipo = "definito",
       nomi       = if (is.na(nome)) character(0) else nome,
       alias      = alias,
       soprannomi = soprannomi,
       luoghi     = luoghi,
       scarti     = scarti)
}
