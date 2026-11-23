# ─────────────────────────────────────────────────────────────
# luoghi.R: i luoghi citati: i più frequenti, la forma locale, «S.» sciolto, gli omonimi
# Dipende da qualita.R (chiave_grafia, .solo_fondo) e da lista_luoghi.R. Le coordinate della lista sono solo dati accanto al nome: Lite non ha carta né servizi esterni.
# ─────────────────────────────────────────────────────────────

# Il profilo del materiale (profili/<nome>/profilo.json): se questo file è letto da solo, lo si carica prima.
if (!exists("PROFILO_DOMINIO", inherits = FALSE)) source("R/profilo_dominio.R")

# I luoghi più citati del fondo: le pillole rapide sopra «Luoghi citati» nella Scheda. Dipende da qualita.R (.solo_fondo): si chiama a app avviata.
luoghi_frequenti <- function(con, fondo = NULL, quanti = 5L) {
  dbGetQuery(con, .solo_fondo(sprintf(
    "SELECT l.luogo AS v, COUNT(DISTINCT l.fascicolo_id) AS n
       FROM luoghi l JOIN fascicoli f ON f.id = l.fascicolo_id
      GROUP BY l.luogo ORDER BY n DESC, l.luogo LIMIT %d", as.integer(quanti)), fondo))$v
}


# ── Luoghi esteri nella lingua del paese ──────────────────
# `dati/luoghi_forma_locale.csv`: la forma italiana di un luogo straniero e la sua forma locale («Londra» → «London»); il vecchio toponimo resta fra parentesi («Zakynthos (Zante)»).
# Solo luoghi, non Stati né regioni.
FORME_LOCALI_LUOGHI <- "dati/luoghi_forma_locale.csv"
forme_locali_luoghi <- function() {
  if (!file.exists(FORME_LOCALI_LUOGHI)) return(data.frame(italiana = character(0), locale = character(0), paese = character(0), nota = character(0), chiave = character(0)))
  d <- utils::read.csv2(FORME_LOCALI_LUOGHI, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8", na.strings = character(0))
  d$chiave <- chiave_grafia(d$italiana); d
}
# La forma standard di ciascun luogo straniero: `Locale (Italiano)`, senza toponimo antico; senza parentesi se l'italiano è uguale al locale a meno dei diacritici.
# Restituisce `nuova` (NA se non c'è niente da cambiare) e `sicura`: TRUE se la correzione è automatica (la forma locale nuda, o una forma italiana già con parentesi),
# FALSE se è solo una proposta (la sola forma italiana, «Londra»: una scelta di chi scheda).
luogo_forma_locale <- function(x, tabella = forme_locali_luoghi()) {
  x <- as.character(x); k <- chiave_grafia(x)
  base_loc <- chiave_grafia(sub("\\s*\\(.*\\)\\s*$", "", tabella$locale))
  i <- match(k, tabella$chiave); j <- match(k, base_loc)
  nuova <- tabella$locale[ifelse(is.na(i), j, i)]
  sicura <- ifelse(!is.na(i), grepl("(", tabella$italiana[i], fixed = TRUE), !is.na(j))
  nuova[!is.na(nuova) & chiave_grafia(nuova) == k] <- NA_character_
  sicura[is.na(nuova)] <- NA
  data.frame(nuova = nuova, sicura = sicura, stringsAsFactors = FALSE)
}
forma_locale_luogo <- function(x, tabella = forme_locali_luoghi()) luogo_forma_locale(x, tabella)$nuova

# ── «S.» sciolto sempre in San / Santo / Santa / Sant' ───────
# In ogni colonna di testo che nomina luoghi, persone, soggetti e nel titolo e regesto; non nelle note né nelle segnature. La forma segue la grammatica:
# Sant' davanti a vocale, Santo davanti a s+consonante, Santa davanti a un nome femminile, San negli altri casi; «S.ta» Santa, «S.to» Santo, «S.ti» Santi.
# Eccezioni: «S. Uffizio»/«S. Officio» → «Sant'Uffizio»/«Sant'Officio», «S. Sede» → «Santa Sede», «S. Padre» → «Santo Padre».
# Non si toccano: «S.» = «Sacra» (S. Congregazione, S. Consulta, S. Rota…), «SS.», le sigle («S.M», «S.A.R.», «S.U») e «Sua» («S. Santità», «S. Eccellenza»).
# Le parole dell'ambiente ecclesiastico (curia, Sant'Uffizio, Santa Sede) sono del profilo: `abbreviazione_santo` in profili/<nome>/profilo.json.
.S_NON_SANTO <- .pd_chr(PROFILO_DOMINIO$abbreviazione_santo$non_santo)
.S_SANT_APOSTROFO <- .pd_chr(PROFILO_DOMINIO$abbreviazione_santo$sant_apostrofo)   # «S. Uffizio» → «Sant'Uffizio»
.S_SANTA <- .pd_chr(PROFILO_DOMINIO$abbreviazione_santo$santa)                      # «S. Sede» → «Santa Sede»
.S_SANTO <- .pd_chr(PROFILO_DOMINIO$abbreviazione_santo$santo)                        # «S. Padre» → «Santo Padre»
# «S.t Officio» (Torino) è «S. Officio»: le parole che seguono «S.t» sono del profilo.
# Senza parole nel profilo la regola non scatta mai (un'alternativa vuota la farebbe scattare su ogni «S.t»).
.S_T_PRIMA_DI_RE <- if (length(.pd_chr(PROFILO_DOMINIO$abbreviazione_santo$s_t_prima_di)))
  paste0("(?<![\\p{L}\\p{N}'’.])S\\.t[ \\t]+(?=(?:", paste(.pd_chr(PROFILO_DOMINIO$abbreviazione_santo$s_t_prima_di), collapse = "|"), "))") else "(?!)"
# Prima di «S.» una di queste parole dice che segue un luogo o un ente («Convento S. Maria»): si scioglie.
# Un'altra parola maiuscola («Mario S. Rossi») può essere un'iniziale: nelle colonne di persone e di testo si lascia e si propone.
.S_PRIMA_LUOGO <- c("convento", "chiesa", "monastero", "borgo", "castel", "castello", "monte", "porta", "ponte", "campo", "villa", "colle", "ospedale", "basilica",
                    "collegio", "oratorio", "compagnia", "confraternita", "parrocchia", "pieve", "abbazia", "cattedrale", "santuario", "seminario", "via", "piazza",
                    "contrada", "strada", "rione", "conservatorio", "badia", "priorato", "cappella", "isola", "porto", "torre")
.S_MASCHILI_IN_A <- c("luca", "andrea", "nicola", "mattia", "barnaba", "zaccaria", "geremia", "cosma", "giuda", "saba", "tobia", "elia", "isaia", "neemia")
.S_FEMMINILI_IN_E <- c("croce", "pace", "fede")
# Il dizionario dei santi (dati/santi.csv: nome;genere M/F) dice se «S.» è San/Santo o Santa. Un nome assente si indovina dalla desinenza, ma è un'ipotesi: nelle persone si propone soltanto.
.SANTI <- new.env(parent = emptyenv())
dizionario_santi <- function() {
  if (!is.null(.SANTI$d)) return(.SANTI$d)
  x <- tryCatch(utils::read.csv2("dati/santi.csv", colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0)), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) {                      # un guasto si dice e non si memorizza: ritrovato il file, il dizionario torna a funzionare
    tryCatch(registra("ERRORE", "dizionario_santi", "dati/santi.csv non si legge: i nomi dei santi si indovinano dalla desinenza"), error = function(e) NULL)
    return(stats::setNames(character(0), character(0)))
  }
  .SANTI$d <- stats::setNames(x$genere, tolower(x$nome))
  .SANTI$d
}
santo_noto <- function(parola) tolower(parola) %in% names(dizionario_santi())
.forma_santo <- function(abbr, parola) {
  w <- tolower(parola)
  g <- unname(dizionario_santi()[w])
  if (!is.na(g) && !w %in% .S_SANT_APOSTROFO && !tolower(abbr) %in% c("s.ti")) {
    if (grepl("^[aeiouàèéìòù]", w)) return(paste0("Sant'", parola))
    if (g == "F") return(paste("Santa", parola))
    if (grepl("^s[bcdfghlmnpqrstvz]", w)) return(paste("Santo", parola))
    return(paste("San", parola))
  }
  if (w %in% .S_SANT_APOSTROFO) return(paste0("Sant'", parola))
  if (w %in% .S_SANTA) return(paste("Santa", parola))
  if (w %in% .S_SANTO) return(paste("Santo", parola))
  if (w %in% .S_NON_SANTO) return(NA_character_)
  a <- tolower(abbr)
  if (a == "s.ti") return(paste("Santi", parola))
  if (grepl("^[aeiouàèéìòù]", w)) return(paste0("Sant'", parola))
  if (a %in% c("s.ta", "sta.")) return(paste("Santa", parola))
  if (a %in% c("s.to", "sto.")) return(paste("Santo", parola))
  if (grepl("^s[bcdfghlmnpqrstvz]", w)) return(paste("Santo", parola))
  if ((grepl("a$", w) && !w %in% .S_MASCHILI_IN_A) || w %in% .S_FEMMINILI_IN_E) return(paste("Santa", parola))
  paste("San", parola)
}
.s_dopo_nome <- function(prima) {
  m <- regmatches(prima, regexpr("\\p{Lu}[\\p{L}'’]*[ \\t]+$", prima, perl = TRUE))
  length(m) == 1L && !tolower(trimws(m)) %in% .S_PRIMA_LUOGO
}
# `guardia`: fuori dalle colonne di luoghi, «S.» dopo una parola maiuscola che non è un luogo si lascia (può essere un'iniziale); `dubbi`: salvo che si chiedano proprio quelli, per la proposta.
# `ignoti = "persone"`: nelle Persone nessun «S.» si scioglie da solo. `ignoti = "lascia"`: un «S.» davanti a un nome ignoto al dizionario («S. Rossi») resta com'è.
sciogli_santi <- function(x, guardia = TRUE, dubbi = FALSE, ignoti = c("forma", "lascia", "persone")) {
  ignoti <- match.arg(ignoti)
  x <- as.character(x)
  vapply(x, function(t) {
    if (is.na(t) || !grepl("S\\.|Sta\\.|Sto\\.", t)) return(t)
    t <- gsub(.S_T_PRIMA_DI_RE, "S. ", t, perl = TRUE)   # «S.t Officio» (Torino) è «S. Officio»
    re <- "(?<![\\p{L}\\p{N}'’.])(S\\.ta|S\\.to|S\\.ti|Sta\\.|Sto\\.|S\\.)[ \\t]*(\\p{Lu}[\\p{L}'’-]*)"
    m <- gregexpr(re, t, perl = TRUE)[[1]]
    if (m[1] == -1L) return(t)
    inizi <- as.integer(m); lun <- attr(m, "match.length")
    cs <- attr(m, "capture.start"); cl <- attr(m, "capture.length")
    for (k in rev(seq_along(inizi))) {
      abbr <- substr(t, cs[k, 1], cs[k, 1] + cl[k, 1] - 1L); parola <- substr(t, cs[k, 2], cs[k, 2] + cl[k, 2] - 1L)
      if (nchar(parola) == 1L || (nchar(parola) <= 3L && parola == toupper(parola))) next               # sigla: S.M, S.A.R., S.U
      if (guardia && !dubbi && .s_dopo_nome(substr(t, 1, inizi[k] - 1L))) next                          # forse un nome con l'iniziale: non si indovina
      if (ignoti == "persone") next                                                                 # nelle Persone «S.» può sempre essere un'iniziale: si propone, non si scioglie
      if (ignoti == "lascia" && grepl(",[ \\t]*$", substr(t, 1, inizi[k] - 1L))) next                # «Rossi, S. Giovanni»: dopo il cognome «S.» può essere l'iniziale
      if (ignoti == "lascia" && !santo_noto(parola) && !tolower(parola) %in% c(.S_SANT_APOSTROFO, .S_SANTA, .S_SANTO)) next
      forma <- .forma_santo(abbr, parola)
      if (!is.na(forma)) t <- paste0(substr(t, 1, inizi[k] - 1L), forma, substr(t, inizi[k] + lun[k], nchar(t)))
    }
    t
  }, "", USE.NAMES = FALSE)
}
