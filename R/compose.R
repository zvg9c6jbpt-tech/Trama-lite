# ─────────────────────────────────────────────────────────────
# compose.R: composizione automatica di regesto, titolo, segnatura
# Funzioni pure: non toccano il database.
# ─────────────────────────────────────────────────────────────

# Il profilo del materiale (profili/<nome>/profilo.json): se questo file è letto da solo, lo si carica prima.
if (!exists("PROFILO_DOMINIO", inherits = FALSE)) source("R/profilo_dominio.R")

# Vero se il valore è assente in uno qualsiasi dei modi in cui può esserlo.
.vuoto <- function(x) {
  # Via breve per il caso più comune (un testo solo): `trimws` usa due espressioni regolari, e l'export
  # chiama questa funzione centinaia di migliaia di volte (13/10). Stesso risultato della via lunga.
  if (is.null(x) || length(x) == 0L) return(TRUE)
  if (length(x) == 1L && is.atomic(x) && !is.factor(x)) {
    if (is.na(x)) return(TRUE)
    if (!is.character(x)) return(FALSE)       # un numero o un logico non vuoti non sono mai «vuoti»
    if (!nzchar(x)) return(TRUE)
    if (!(substr(x, 1L, 1L) %in% c(" ", "\t", "\r", "\n"))) return(FALSE)
  }
  is.null(x) || length(x) == 0 ||
    all(is.na(x)) || all(trimws(as.character(x)) == "")
}


componi_regesto <- function(luogo_reato = NA_character_,
                            reati       = character(0),
                            tortura     = "n.d.",
                            sentenza    = "n.d.",
                            ometti_nd   = TRUE) {
  
  riga <- function(etichetta, valore) {
    assente <- .vuoto(valore) ||
      all(tolower(trimws(as.character(valore))) %in% c("n.d.", "nd", "n.d"))
    if (assente && ometti_nd) return(NULL)
    paste0(etichetta, ": ",
           if (assente) "" else paste(valore, collapse = ", "))
  }
  
  righe <- c(
    riga("Nome del luogo di consumazione del reato", luogo_reato),
    riga("Tipo di reato",                            reati),
    riga("Applicazione della tortura",               tortura),
    riga("Sentenza o decreto formale di spedizione", sentenza)
  )
  
  if (length(righe) == 0) return(NA_character_)
  paste(righe, collapse = "\n")
}


# "Bernardina Stadera" -> "Stadera, Bernardina"; "Gilberto de Vasto" -> "de Vasto, Gilberto" (la particella resta davanti al cognome).
# Le forme con apostrofo («d'Africa») sono già un pezzo solo. Scalare: per una colonna si mappa con vapply.
PARTICELLE_COGNOME <- c("de", "de'", "di", "da", "del", "della", "dello", "delle",
                        "dei", "degli", "dal", "dalla", "dalle", "dai")

# La prima virgola fuori dalle parentesi (0 se non c'è).
.virgola_alta <- function(x) {
  car <- strsplit(as.character(x), "")[[1]]
  d <- cumsum(car == "(") - cumsum(car == ")")
  i <- which(car == "," & d <= 0)[1]
  if (is.na(i)) 0L else i
}

indicizza_nome <- function(esteso) {
  if (.vuoto(esteso)) return(NA_character_)
  # Le formule della guida al Tab non si riordinano: sovrani e papi («Louis XIV (Luigi XIV; re di Francia)») e religiosi col nome indicizzato in parentesi.
  senza_par <- trimws(gsub("\\s*\\([^()]*\\)", "", esteso))
  ultimo <- utils::tail(strsplit(senza_par, "\\s+")[[1]], 1)
  if (length(ultimo) && grepl("^[IVXLC]+$", ultimo) && grepl("\\s", senza_par)) return(trimws(esteso))
  if (grepl("\\([^()]*,[^()]*\\)", esteso)) return(trimws(esteso))
  pezzi <- strsplit(trimws(esteso), "\\s+")[[1]]
  # Una parentesi sta con la parola prima: «Guglielmo Cucco (Sobring)» ha per cognome «Cucco (Sobring)».
  uniti <- character(0); aperta <- FALSE
  for (x in pezzi) {
    if ((aperta || startsWith(x, "(")) && length(uniti)) uniti[length(uniti)] <- paste(uniti[length(uniti)], x)
    else uniti <- c(uniti, x)
    aperta <- (aperta || startsWith(x, "(")) && !grepl("\\)$", x)
  }
  pezzi <- uniti
  n <- length(pezzi)
  if (n < 2) return(trimws(esteso))
  inizio <- n
  while (inizio > 2 && tolower(pezzi[inizio - 1]) %in% PARTICELLE_COGNOME) inizio <- inizio - 1
  # Nessun nome di battesimo, solo particelle + cognome ("de Vasto"): resta com'è.
  if (all(tolower(pezzi[seq_len(inizio - 1)]) %in% PARTICELLE_COGNOME)) return(trimws(esteso))
  paste0(paste(pezzi[inizio:n], collapse = " "), ", ",
         paste(pezzi[seq_len(inizio - 1)], collapse = " "))
}


# Senza cognome non è una persona citata: solo il nome («Loisia») o nome e provenienza («Paolo da Roma»). Resta nella scheda, nel titolo e nell'export,
# ma non nell'elenco delle persone citate. Stretta di proposito: con due nomi davanti a «da» il secondo può essere un cognome.
.TITOLI_PERSONA <- "(?:don|donna|fra|frate|suor|suora|padre|madre|mastro|mastra|abate|clerico|chierico)"
senza_cognome <- function(x) {
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  nome <- paste0("(?:", .TITOLI_PERSONA, "\\s+)?[^\\s,]+")
  solo_nome  <- grepl(paste0("^", nome, "$"), x, perl = TRUE, ignore.case = TRUE)
  provenienza <- grepl(paste0("^", nome, "\\s+da\\s+[^,]+$"), x, perl = TRUE, ignore.case = TRUE)
  indice     <- grepl(paste0("^da\\s+[^,]+,\\s*", nome, "$"), x, perl = TRUE, ignore.case = TRUE)
  # La forma del foglio, con la particella dopo il nome: «Tunisi, Caterina da» (14/10)
  dopo       <- grepl(paste0("^[^,]+,\\s*", nome, "\\s+da$"), x, perl = TRUE, ignore.case = TRUE)
  nzchar(x) & (solo_nome | provenienza | indice | dopo)
}


# Un nome scritto a mano nelle persone («paolo gargiulo») diventa «Gargiulo, Paolo»:
# - maiuscola all'iniziale di ogni parola che non ne ha una; minuscole le particelle, i titoli (don, fra…) e «detto/detta/alias» con l'articolo;
# - senza virgola si gira con indicizza_nome; con la virgola resta com'è scritto;
# - non si gira chi non ha cognome né un soprannome («Domenico detto il mancino»).
# Vale solo per ciò che l'utente aggiunge: mai all'import, mai su un nome già nel database.
.MINUSCOLE_NOME <- c("don", "donna", "fra", "frate", "suor", "suora", "padre", "madre", "mastro", "mastra",
                     "abate", "clerico", "chierico", "detto", "detta", "alias", "il", "lo", "la", "e")
# «De'cefali» -> «De'Cefali»: dopo la particella con l'apostrofo la lettera seguente è maiuscola; la particella resta com'è.
.maiuscola_dopo_apostrofo <- function(w) {
  m <- regexpr("^\\(?(?:de|d|dell|dall|dal|nell|all|sull|l)['\u2019]\\p{Ll}", w, perl = TRUE, ignore.case = TRUE)
  if (m < 0) return(w)
  k <- attr(m, "match.length")
  paste0(substr(w, 1, k - 1), toupper(substr(w, k, k)), substr(w, k + 1, nchar(w)))
}
.maiuscola_parola <- function(w) {
  if (grepl("^\\(?(?:de|d|dell|dall|dal|nell|all|sull|l)['\u2019]", w, perl = TRUE, ignore.case = TRUE)) return(.maiuscola_dopo_apostrofo(w))
  if (grepl("\\p{Lu}", w, perl = TRUE) || tolower(w) %in% c(PARTICELLE_COGNOME, .MINUSCOLE_NOME)) return(w)
  # «d'africa» -> «d'Africa», «(sobring)» -> «(Sobring)»: si alza la prima lettera dopo l'apostrofo o la parentesi.
  m <- regexpr("^(\\(?(?:[dl]|dell|dall|nell|all|sull)['\u2019])?\\(?\\p{L}", w, perl = TRUE)
  if (m < 0) return(w)
  k <- attr(m, "match.length")
  paste0(substr(w, 1, k - 1), toupper(substr(w, k, k)), substr(w, k + 1, nchar(w)))
}
normalizza_persona <- function(x) {
  vapply(as.character(x), function(n) {
    if (is.na(n) || !nzchar(trimws(n))) return(n)
    n <- gsub("\\s+", " ", trimws(n)); n <- gsub(" ,", ",", n, fixed = TRUE); n <- gsub(",(?=\\S)", ", ", n, perl = TRUE)
    n <- paste(vapply(strsplit(n, " ", fixed = TRUE)[[1]], .maiuscola_parola, ""), collapse = " ")
    if (grepl(",", n, fixed = TRUE) || senza_cognome(n) ||
        grepl("(^|\\s)(detto|detta|alias)(\\s|$)", n, ignore.case = TRUE, perl = TRUE)) return(n)
    indicizza_nome(n)
  }, "", USE.NAMES = FALSE)
}


# La data topica è la città dell'Inquisizione che produce l'atto («Inquisizione di Capua» -> Capua): si propone nella maschera, mai all'import o al salvataggio.
# Il soggetto produttore è un ENTE nella forma «Inquisizione di <Città>»: la forma lunga si riscrive, i doppioni si tolgono, ogni altro ente resta.
# Le parole (ente, preposizione, prefisso lungo, eccezioni) sono del profilo: `produttore` in profili/<nome>/profilo.json.
.produttore_canonico <- function(x) {
  pr <- PROFILO_DOMINIO$produttore
  ente <- pr$ente; prep <- pr$preposizione
  x <- trimws(as.character(unlist(x))); x <- x[!is.na(x) & nzchar(x)]
  x <- sub(paste0("^", .re_frase(pr$prefisso_lungo), "\\s*", .re_frase(ente), "\\s+", .re_frase(prep), "\\s+"), paste0(ente, " ", prep, " "), x, ignore.case = TRUE)
  # «inquisizione di benevento» → «Inquisizione di Benevento»: maiuscola alla parola e alla città.
  x <- vapply(x, function(v) { m <- regmatches(v, regexec(paste0("^", .re_frase(ente), "\\s+", .re_frase(prep), "\\s+(.+)$"), v, ignore.case = TRUE))[[1]]
    if (length(m)) paste0(ente, " ", prep, " ", toupper(substr(m[2], 1, 1)), substring(m[2], 2)) else v }, "", USE.NAMES = FALSE)
  # Le eccezioni del profilo: nell'Inquisizione solo il tribunale di Roma si chiama «Sant'Uffizio» (guida al Tab, colonna U).
  for (citta in names(pr$eccezioni))
    x <- sub(paste0("^", .re_frase(ente), "\\s+", .re_frase(prep), "\\s+", .re_frase(citta), "$"), gsub("\\\\", "\\\\\\\\", pr$eccezioni[[citta]]), x, ignore.case = TRUE)
  unique(x)
}

citta_da_produttore <- function(soggetti) {
  s <- trimws(as.character(unlist(soggetti))); s <- s[!is.na(s) & nzchar(s)]
  pr <- PROFILO_DOMINIO$produttore
  nomi_ente <- gsub("([][.|()^${}+*?\\\\])", "\\\\\\1", c(pr$ente, .pd_chr(pr$sinonimi)), perl = TRUE)
  re <- paste0("^.*\\b(?:", paste(nomi_ente, collapse = "|"), ")\\s+(?:", pr$preposizione, "\\s+|", pr$preposizione_elisa, ")\\s*(.+?)\\s*$")
  ok <- grepl(re, s, perl = TRUE, ignore.case = TRUE)
  # La forma d'eccezione da sola (nell'Inquisizione «Sant'Uffizio») nomina la città dell'eccezione (Roma).
  dalle_eccezioni <- unlist(lapply(names(pr$eccezioni), function(citta)
    if (any(grepl(paste0("^", .re_frase(pr$eccezioni[[citta]]), "$"), s, ignore.case = TRUE))) citta))
  unique(c(sub(re, "\\1", s[ok], perl = TRUE, ignore.case = TRUE), dalle_eccezioni))
}
data_topica_con_produttori <- function(attuale, soggetti) {
  a <- if (.vuoto(attuale)) character(0) else trimws(strsplit(paste(attuale, collapse = "|"), "|", fixed = TRUE)[[1]])
  a <- a[nzchar(a)]
  citta <- citta_da_produttore(soggetti)
  tutte <- c(citta, a[!tolower(a) %in% tolower(citta)])
  if (!length(tutte)) NA_character_ else paste(tutte, collapse = "|")
}


# «A», «A e B», «A, B e C»: virgole e una sola «e» finale (Stefano, 2/10;
# prima «A e B e C»). Con due nomi non cambia nulla.
.elenca_nomi <- function(nomi) {
  n <- length(nomi)
  if (n <= 2) paste(nomi, collapse = " e ") else paste0(paste(nomi[-n], collapse = ", "), " e ", nomi[n])
}

# "Senia, Domenico della" -> "Domenico della Senia": l'inverso di indicizza_nome.
.estendi_nome <- function(x) {
  x <- trimws(as.character(x))
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    i <- .virgola_alta(s)
    if (i <= 1L) return(s)
    cogn <- trimws(substr(s, 1, i - 1)); resto <- trimws(substring(s, i + 1))
    # Una qualifica finale fra parentesi resta in coda al nome intero.
    m <- regmatches(resto, regexec("^(.*?)[[:space:]]*(\\([^()]*\\))$", resto))[[1]]
    if (length(m) == 3L && nzchar(m[2])) paste(m[2], cogn, m[3]) else paste(resto, cogn)
  }, "", USE.NAMES = FALSE)
}


# La grammatica del titolo è del profilo (`titolo` in profili/<nome>/profilo.json).
# Tipo -> connettore proposto, ricavato dai titoli ASDNa reali (4 eccezioni su 250: il connettore si sceglie a mano).
TIPI_TITOLO <- as.list(.pd_vett(PROFILO_DOMINIO$titolo$tipi_titolo))

# I tipi il cui connettore non si sceglie.
CONNETTORE_FISSO <- as.list(.pd_vett(PROFILO_DOMINIO$titolo$connettore_fisso))

# Con due nomi o più (anche uno di ignoto) la denuncia non è una sola: «Denunce contro X e Y», «Denunce per diversi». Solo le denunce.
PLURALE_TIPO <- .pd_vett(PROFILO_DOMINIO$titolo$plurale_tipo)
.al_plurale <- function(parola, n) {
  if (n >= 2 && !is.na(parola) && parola %in% names(PLURALE_TIPO)) unname(PLURALE_TIPO[parola]) else parola
}

# Il TITOLO: con 1–2 nomi, solo i nomi; il genere (Processo, Denuncia, Atti…) sta nella pillola della tipologia e da lì nasce il regesto.
# Con tre o più nomi o identità dubbia dice il genere («Atti per diversi»); senza nomi «Denunce per ignoti» (solo se si è guardata la fonte) o la tipologia da sola;
# senza nomi né tipologia resta vuoto. `tipo_titolo` e `connettore` restano nella firma (dati di prima) ma non compongono più niente.
componi_titolo <- function(tipo_titolo, connettore, imputati_estesi,
                           identita_imputato_dubbia = FALSE, soglia_diversi = 3, ignoti = FALSE) {
  nomi <- trimws(as.character(imputati_estesi))
  nomi <- nomi[!is.na(nomi) & nomi != ""]
  tipo <- if (.vuoto(tipo_titolo)) NA_character_ else trimws(as.character(tipo_titolo)[1])

  if (isTRUE(identita_imputato_dubbia) || length(nomi) >= soglia_diversi)
    return(paste0(.al_plurale(if (is.na(tipo)) "Atti" else tipo, length(nomi)), " per diversi"))

  if (length(nomi)) return(.elenca_nomi(nomi))

  if (isTRUE(ignoti)) return(paste0(.al_plurale(if (is.na(tipo)) "Atti" else tipo, 2L), " per ignoti"))
  tipo
}


# Il titolo nella forma di prima («<Tipo> circa|contro <nomi>»): serve solo a riconoscere i titoli già composti così e a non riscriverli.
# Solo «Ricomponi…» (`forza = TRUE`) li porta alla forma di adesso.
.componi_titolo_vecchio <- function(tipo_titolo, connettore, imputati_estesi, identita_imputato_dubbia = FALSE, soglia_diversi = 3) {
  if (.vuoto(tipo_titolo)) return(NA_character_)
  nomi <- trimws(as.character(imputati_estesi)); nomi <- nomi[!is.na(nomi) & nomi != ""]
  if (isTRUE(identita_imputato_dubbia) || length(nomi) >= soglia_diversi)
    return(paste0(.al_plurale(tipo_titolo, length(nomi)), " per diversi"))
  if (length(nomi) == 0) return(NA_character_)
  conn <- if (tipo_titolo %in% names(CONNETTORE_FISSO)) CONNETTORE_FISSO[[tipo_titolo]]
          else if (!.vuoto(connettore)) connettore
          else if (!is.null(TIPI_TITOLO[[tipo_titolo]])) TIPI_TITOLO[[tipo_titolo]]
          else "circa"
  paste0(.al_plurale(tipo_titolo, length(nomi)), " ", conn, " ", .elenca_nomi(nomi))
}

# ── titolo e regesto dalla tipologia ──
# La tipologia è la scelta che apre il titolo (Processi -> «Processo», Denunce -> «Denuncia»).
# Il regesto narrativo è tipologia + soggettario + imputati, come nel vecchio Regestum (componiRegestoNarrativo).
TIPOLOGIA_PER_DIVERSI <- PROFILO_DOMINIO$titolo$tipologia_per_diversi
PAROLA_TIPOLOGIA <- .pd_vett(PROFILO_DOMINIO$titolo$parola_tipologia)

# "Processi" -> "Processo"; "Atti per diversi" -> "Atti" (il "per diversi"
# lo aggiunge componi_titolo); una tipologia fuori elenco resta com'è.
tipo_da_tipologia <- function(tipologia) {
  t <- if (.vuoto(tipologia)) "" else trimws(as.character(tipologia)[1])
  if (t == TIPOLOGIA_PER_DIVERSI) return("Atti")
  if (t %in% names(PAROLA_TIPOLOGIA)) unname(PAROLA_TIPOLOGIA[t]) else if (nzchar(t)) t else NA_character_
}

# Reati in minuscolo: «A e B», «A; B e C»; connettore «contro» solo per i processi, «ignoti» senza imputati, «diversi soggetti» oltre quattro.
# Un regesto con la convenzione di prima («per eresia; magia») equivale a quello di adesso: la nuova forma vale per i regesti nuovi, i vecchi non si riscrivono.
.regesto_uguale <- function(a, b) {
  if (identical(a, b)) return(TRUE)
  if (is.null(a) || is.null(b) || length(a) != 1L || length(b) != 1L || is.na(a) || is.na(b)) return(FALSE)
  n <- function(x) { x <- gsub("bestemmia e miscredenza", "bestemmia_e_miscredenza", x, ignore.case = TRUE); gsub("; | e ", "|", x) }
  # Quattro nomi nel regesto di prima, «diversi soggetti» in quello di adesso: stesso regesto.
  ds <- function(x) grepl(" (circa|contro) diversi soggetti[.]?$", x)
  if (ds(a) || ds(b)) {
    pre <- function(x) n(sub(" (circa|contro) .*$", "", x))
    return(identical(pre(a), pre(b)) && grepl(" (circa|contro) ", a) && grepl(" (circa|contro) ", b))
  }
  identical(n(a), n(b))
}

.elenca_reati <- function(r) {
  n <- length(r)
  # Se una classe contiene già una «e» («bestemmia e miscredenza») il separatore è sempre «; ».
  if (n <= 1L) r else if (any(grepl(" e ", r, fixed = TRUE))) paste(r, collapse = "; ")
  else if (n == 2L) paste(r, collapse = " e ") else paste0(paste(r[-n], collapse = "; "), " e ", r[n])
}
componi_regesto_narrativo <- function(tipologia, reati, imputati_indicizzati, ignoti = TRUE) {
  r <- trimws(as.character(reati)); r <- tolower(r[!is.na(r) & nzchar(r)])
  # Più reati: due «A e B»; tre o più «A; B e C».
  per <- if (length(r)) paste0(" per ", .elenca_reati(r)) else ""
  t <- if (.vuoto(tipologia)) "" else trimws(as.character(tipologia)[1])
  if (t == TIPOLOGIA_PER_DIVERSI) return(paste0(TIPOLOGIA_PER_DIVERSI, per))
  # Senza tipologia il regesto non afferma «Processo»: dice «Atti» («Atti circa X, ignoto il reato contestato»).
  parola <- if (t %in% names(PAROLA_TIPOLOGIA)) unname(PAROLA_TIPOLOGIA[t]) else if (nzchar(t)) t else "Atti"
  imp <- trimws(as.character(imputati_indicizzati)); imp <- imp[!is.na(imp) & nzchar(imp)]
  parola <- .al_plurale(parola, length(imp))
  # Nel regesto al massimo tre nomi, oltre «diversi soggetti» (nel titolo due: da tre «per diversi»).
  # Senza nomi «ignoti» lo dice solo chi ha dichiarato di aver guardato la fonte (`ignoti = TRUE`); altrimenti il testo è impersonale.
  if (!length(imp) && !isTRUE(ignoti)) return(paste0(parola, per))
  chi <- if (!length(imp)) "ignoti" else if (length(imp) > 3) "diversi soggetti"
         else .elenca_nomi(.estendi_nome(imp))
  # Processo e Denuncia «contro», tutto il resto «circa»: come nei titoli.
  paste0(parola, per, " ", if (t %in% c("Processi", "Processo", "Denunce", "Denuncia")) "contro" else "circa", " ", chi)
}

# I NOMI da cui si compongono titolo e regesto: tutte le persone nominate, senza distinzione fra imputati e altre figure
# (titolo: al massimo due nomi; regesto: tre). Eccezione: chi ha dichiarato di aver guardato la fonte e di non trovarvi nessuno (`imputati_esaminati`: «ignoti»).
# La tabella `imputati` resta nel database (dati già scritti, backup) ma non compone più niente.
.persone_citate_scheda <- function(f) { p <- as.character(unlist(f$persone)); p[!is.na(p) & nzchar(trimws(p))] }
.imputati_scheda <- function(f) {
  if (isTRUE(as.integer(f$imputati_esaminati) == 1L)) return(character(0))
  .persone_citate_scheda(f)
}
# La regola di prima (imputati dichiarati, altrimenti persone): serve a riconoscere i testi già composti così e a non riscriverli.
.imputati_scheda_prima <- function(f) {
  if (!is.null(f$imputati) && NROW(f$imputati) > 0) return(as.character(f$imputati$indicizzato))
  .imputati_scheda(f)
}

# Regesto di una scheda nel modello in cui è già scritto (strutturato «Tipo di reato: …» o narrativo): una ricomposizione non cambia mai modello.
# `forza = TRUE`: sempre la regola di adesso («Ricomponi…»). Senza, un regesto non scritto a mano uguale a quello della regola di prima non si riscrive.
componi_regesto_scheda <- function(f, forza = FALSE) {
  nuovo <- .regesto_scheda(f, .imputati_scheda(f), ignoti = isTRUE(as.integer(f$imputati_esaminati) == 1L))
  if (isTRUE(forza) || isTRUE(f$regesto_manuale == 1L) || .vuoto(f$regesto)) return(nuovo)
  prima <- .regesto_scheda(f, .imputati_scheda_prima(f), ignoti = TRUE)
  if (!is.na(prima) && .regesto_uguale(prima, f$regesto) && !isTRUE(.regesto_uguale(prima, nuovo))) return(f$regesto)
  nuovo
}
# I luoghi citati della scheda, senza i vuoti (unico campo dei luoghi della vicenda).
.luoghi_pieni <- function(f) { l <- trimws(as.character(unlist(f$luoghi))); l[!is.na(l) & nzchar(l)] }
.regesto_scheda <- function(f, imputati, ignoti) {
  if (!.vuoto(f$regesto) && isTRUE(analizza_regesto(f$regesto)$strutturato))
    return(componi_regesto(analizza_regesto(f$regesto)$luogo_reato, f$reati, f$tortura, f$sentenza))   # il luogo resta com'è scritto nel regesto: non c'è più un campo a parte (26/10/2026)
  uno <- function(x) isTRUE(suppressWarnings(as.integer(x)) == 1L)
  # Una carta che non è un procedimento (documenti sciolti, catalogo, registro, lettere) non ha un regesto composto.
  if (uno(f$non_procedimento)) return(NA_character_)
  # Senza reato il regesto resta vuoto, anche con una persona citata: «Processo contro X» attribuirebbe un'accusa che la fonte non dice.
  # Eccezioni: chi ha dichiarato di aver guardato la fonte («imputati esaminati», «reato ignoto», «luogo ignoto») e «Atti per diversi».
  r <- trimws(as.character(f$reati)); r <- r[!is.na(r) & nzchar(r)]
  # «Reato ignoto» e «luogo ignoto» compongono la formula solo se c'è una persona di cui dirlo; «Imputati esaminati» fa di «ignoti» un fatto dichiarato.
  con_persona <- length(imputati) > 0
  dichiarato <- uno(f$imputati_esaminati) || (con_persona && (uno(f$reato_ignoto) || uno(f$luogo_ignoto))) ||
                identical(trimws(as.character(f$tipologia_documentaria)[1]), TIPOLOGIA_PER_DIVERSI)
  if (!length(r) && !dichiarato) return(NA_character_)
  testo <- componi_regesto_narrativo(f$tipologia_documentaria, f$reati, imputati, ignoti = ignoti)
  # La formula «Atti circa X, ignoto il reato contestato», solo se il dato manca davvero.
  ignoti <- c(if (con_persona && uno(f$reato_ignoto) && !length(r)) "ignoto il reato contestato",
              if (con_persona && uno(f$luogo_ignoto) && !length(.luoghi_pieni(f))) "ignoto il luogo")
  if (length(ignoti)) paste0(testo, ", ", paste(ignoti, collapse = ", ")) else testo
}

# Il titolo di una scheda: il tipo scelto a mano, se c'è, altrimenti quello
# della tipologia; "Atti per diversi" vale come identità dubbia.
componi_titolo_scheda <- function(f, forza = FALSE) {
  tipo <- if (!.vuoto(f$tipo_titolo)) f$tipo_titolo else tipo_da_tipologia(f$tipologia_documentaria)
  dubbia <- isTRUE(as.integer(f$identita_imputato_dubbia) == 1L) ||
            identical(trimws(as.character(f$tipologia_documentaria)), TIPOLOGIA_PER_DIVERSI)
  ignoti <- isTRUE(as.integer(f$imputati_esaminati) == 1L)
  nuovo <- componi_titolo(tipo, f$connettore, .estendi_nome(.imputati_scheda(f)), dubbia, ignoti = ignoti)
  # Un titolo non a mano uguale a uno composto con le regole di prima non si riscrive; solo `forza = TRUE` applica la regola di adesso.
  if (!isTRUE(forza) && !isTRUE(f$titolo_manuale == 1L) && !.vuoto(f$titolo) && !identical(f$titolo, nuovo) &&
      f$titolo %in% .titoli_di_prima(f, tipo, dubbia)) return(f$titolo)
  nuovo
}
# I titoli che le regole di prima avrebbero composto per questa scheda.
.titoli_di_prima <- function(f, tipo, dubbia) {
  t <- c(.componi_titolo_vecchio(tipo, f$connettore, .estendi_nome(.imputati_scheda(f)), dubbia),
         .componi_titolo_vecchio(tipo, f$connettore, .estendi_nome(.imputati_scheda_prima(f)), dubbia),
         componi_titolo(tipo, f$connettore, .estendi_nome(.imputati_scheda_prima(f)), dubbia,
                        ignoti = isTRUE(as.integer(f$imputati_esaminati) == 1L)))
  t[!is.na(t)]
}

# `suffisso` segue il numero nei fascicoli «16-bis», «1-ter», «1-2»: il numero resta un intero, il seguito com'era scritto.
# L'unità con cui un inventario numera i pezzi è di norma la busta («b.»), ma può avere un'altra sigla («aa.», «cart.», «reg.»): sta in `sigla_busta`, vuota = «b.».
sigla_busta <- function(x) {
  s <- if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) "b." else tolower(trimws(as.character(x[1])))
  if (!grepl("\\.$", s)) paste0(s, ".") else s
}

# La sigla del fascicolo: «fasc.» salvo diversa (un inventario può scrivere «n. 12»).
sigla_fascicolo <- function(x) {
  s <- if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))) "fasc." else tolower(trimws(as.character(x[1])))
  if (!grepl("\\.$", s)) paste0(s, ".") else s
}
sigle_fascicolo <- function(x) vapply(as.list(if (is.null(x)) NA_character_ else x), sigla_fascicolo, character(1), USE.NAMES = FALSE)

# La stessa, per un vettore di valori (colonne di una tabella).
sigle_busta <- function(x) vapply(as.list(if (is.null(x)) NA_character_ else x), sigla_busta, character(1), USE.NAMES = FALSE)

# La posizione di un fascicolo come la scrive il suo inventario (vettoriale): «b. 3, fasc. 1», o con altre sigle «aa. 164, fasc. 1».
posizione_scritta <- function(sigla_b, busta, sigla_f, fascicolo, suffisso = "", suffisso_b = "") {
  n <- max(length(busta), length(fascicolo))
  if (n == 0L) return(character(0))   # una tabella a zero righe
  sb <- rep_len(sigle_busta(sigla_b), n); sf <- rep_len(sigle_fascicolo(sigla_f), n)
  b <- rep_len(busta, n); f <- rep_len(fascicolo, n)
  # Un suffisso assente è NULL, NA o "": in tutti i casi non si scrive.
  vuoto_suf <- function(x) if (is.null(x) || !length(x)) "" else ifelse(is.na(x), "", x)
  su <- rep_len(vuoto_suf(suffisso), n)
  sub_b <- rep_len(vuoto_suf(suffisso_b), n)     # «232-bis», «5-ter»
  # Una parte che manca non si scrive («Ms., b. 1860» è una segnatura giusta senza fascicolo); se manca tutto, un punto interrogativo.
  pb <- ifelse(is.na(b), NA_character_, paste0(sb, " ", b, sub_b))
  pf <- ifelse(is.na(f), NA_character_, paste0(sf, " ", f, su))
  ifelse(!is.na(pb) & !is.na(pf), paste(pb, pf, sep = ", "), ifelse(!is.na(pb), pb, ifelse(!is.na(pf), pf, "?")))
}

componi_segnatura <- function(archivio, fondo, serie, busta, fascicolo, suffisso = NULL, sigla = NULL, sigla_f = NULL, suffisso_busta = NULL) {
  pezzi <- c(
    archivio, fondo, serie,
    if (!.vuoto(busta))     paste0(sigla_busta(sigla), " ", busta, if (.vuoto(suffisso_busta)) "" else suffisso_busta),
    if (!.vuoto(fascicolo)) paste0(sigla_fascicolo(sigla_f), " ", fascicolo,
                                   if (.vuoto(suffisso)) "" else suffisso)
  )
  pezzi <- pezzi[!vapply(pezzi, .vuoto, logical(1))]
  paste(pezzi, collapse = ", ")
}

# Quanto si è dentro le parentesi, carattere per carattere: contano solo quelle con la loro compagna, una «(» mai chiusa non fonde due luoghi.
.livello_parentesi <- function(car) {
  aperte <- integer(0); accoppiata <- logical(length(car))
  for (i in which(car %in% c("(", ")"))) {
    if (car[i] == "(") aperte <- c(aperte, i)
    else if (length(aperte)) { accoppiata[c(aperte[length(aperte)], i)] <- TRUE; aperte <- aperte[-length(aperte)] }
  }
  cumsum(car == "(" & accoppiata) - cumsum(car == ")" & accoppiata)
}

# Il luogo di consumazione può nominarne più d'uno, separati da «;»: il campo resta uno, fedele alla fonte; qui si dice QUALI luoghi sono.
# Le parentesi proteggono: dentro stanno le varianti di un nome.
separa_luoghi <- function(x) {
  if (.vuoto(x)) return(character(0))
  car <- strsplit(as.character(x)[1], "")[[1]]
  liv <- .livello_parentesi(car)
  sep <- car %in% c(";", "|") & liv <= 0
  grp <- cumsum(sep)
  car[sep] <- ""
  v <- trimws(vapply(split(car, grp), paste, "", collapse = ""))
  unname(v[nzchar(v)])
}


# --- analisi: l'inverso della composizione ------------------------

# "Archivio ..., Fondo ..., b.1, f.1"  ->  le sue componenti. I pezzi si riconoscono dall'etichetta, non dalla posizione:
#   - «b. 3», «busta 3»: la busta; «f. 2», «fasc. 2», «fasc. 2-bis»: il fascicolo;
#   - un'altra abbreviazione col punto e un numero («aa. 164», «n. 5»): se manca la busta è la busta, se manca il fascicolo è il fascicolo;
#     con due, la prima è la busta e la seconda il fascicolo; la sigla si conserva (sigla_busta, sigla_fascicolo);
#   - il resto, nell'ordine, è archivio, fondo, serie.
# Il suffisso di una busta («232-bis», «5 ter», «3-2»): bis… octies, o un numero dopo il trattino. Una lettera attaccata («b. 12A») resta fuori, e si dice.
.SUFFISSO_NUMERO <- "(\\s*-\\s*[0-9]+|\\s*-?\\s*(?:bis|ter|quater|quinquies|sexies|septies|octies|novies|decies)\\.?)?"
# «7 bis», «7bis», «7-Bis», «7-ter.» si scrivono «-bis», «-ter»: il suffisso numerale ha una forma sola; gli altri («-a», «-2», «A») restano.
.suffisso_canonico <- function(s) {
  if (.vuoto(s)) return("")
  m <- regmatches(s, regexec("^\\s*-?\\s*(bis|ter|quater|quinquies|sexies|septies|octies|novies|decies)\\.?\\s*$", s, ignore.case = TRUE, perl = TRUE))[[1]]
  if (length(m)) paste0("-", tolower(m[2])) else s
}
# Ciò che non si legge non sparisce: `.scomponi_segnatura` lo restituisce in `non_letto`, e
# l'anteprima dell'import lo mostra (segnature_da_guardare).
.scomponi_segnatura <- function(testo) {
  r <- list(archivio = NA_character_, fondo = NA_character_, serie = NA_character_,
            busta = NA_integer_, busta_suffisso = "", fascicolo = NA_integer_, fascicolo_suffisso = "",
            sigla_busta = NA_character_, sigla_fascicolo = NA_character_, non_letto = character(0), illeggibile = character(0))
  if (.vuoto(testo)) return(r)

  pezzi <- trimws(strsplit(as.character(testo), ",")[[1]])
  pezzi <- pezzi[nzchar(pezzi)]
  resto <- character(0); resto_pos <- integer(0); unita <- list(); pos_b <- NA_integer_; pos_f <- NA_integer_

  for (k in seq_along(pezzi)) {
    p <- pezzi[k]
    # «b. 232-bis», «b. 5 ter», «b. 3-2»: come nel fascicolo, il numero e poi il suffisso com'è scritto.
    if (grepl(paste0("^b(?:usta)?\\.?\\s*[0-9]+", .SUFFISSO_NUMERO, "$"), p, ignore.case = TRUE, perl = TRUE)) {
      m <- regmatches(p, regexec(paste0("^b(?:usta)?\\.?\\s*([0-9]+)", .SUFFISSO_NUMERO, "$"), p, ignore.case = TRUE, perl = TRUE))[[1]]
      r$busta <- as.integer(m[2]); r$busta_suffisso <- .suffisso_canonico(m[3]); pos_b <- k
    }
    # "fasc. 16", "fasc. 16-bis", "fasc. 18 -bis", "fasc. 1-2": il numero
    # e, dopo, tutto quello che c'è, conservato com'è scritto.
    else if (grepl("^f(asc)?\\.?\\s*[0-9]+", p, ignore.case = TRUE)) {
      m <- regmatches(p, regexec("^f(?:asc)?\\.?\\s*([0-9]+)(.*)$", p, ignore.case = TRUE))[[1]]
      r$fascicolo <- as.integer(m[2]); r$fascicolo_suffisso <- .suffisso_canonico(m[3]); pos_f <- k
    }
    # Il punto è obbligatorio: senza, «Armadio 3» finirebbe qui per errore.
    else if (grepl(paste0("^[[:alpha:]]{1,6}\\.\\s*[0-9]+", .SUFFISSO_NUMERO, "$"), p, ignore.case = TRUE, perl = TRUE)) {
      m <- regmatches(p, regexec(paste0("^([[:alpha:]]{1,6}\\.)\\s*([0-9]+)", .SUFFISSO_NUMERO, "$"), p, ignore.case = TRUE, perl = TRUE))[[1]]
      unita[[length(unita) + 1L]] <- list(sigla = sigla_busta(m[2]), n = as.integer(m[3]), suffisso = .suffisso_canonico(m[4]), pos = k)
    }
    else {
      resto <- c(resto, p); resto_pos <- c(resto_pos, k)
    }
  }

  # Le unità con un'altra sigla, nell'ordine in cui compaiono: la prima va sulla busta se manca,
  # poi sul fascicolo se manca; quelle in più restano fuori (non lette).
  fuori <- character(0); illeggibile <- character(0)
  for (u in unita) {
    if (is.na(r$busta))          { r$busta <- u$n;      r$sigla_busta <- u$sigla;     r$busta_suffisso <- u$suffisso; pos_b <- u$pos }
    else if (is.na(r$fascicolo)) { r$fascicolo <- u$n;  r$sigla_fascicolo <- u$sigla; r$fascicolo_suffisso <- u$suffisso; pos_f <- u$pos }
    else fuori <- c(fuori, paste0(u$sigla, " ", u$n, u$suffisso))      # busta e fascicolo già letti: l'unità in più si dice intera, non si respinge
  }

  # Un pezzo senza numeri fra busta e fascicolo («m.1, inv., f.2» e «m.1, non inv., f.2») qualifica la busta: sta nel suffisso della busta, che fa parte della chiave.
  if (!is.na(pos_b) && length(resto)) {
    tra <- resto_pos > pos_b & (is.na(pos_f) | resto_pos < pos_f) & nchar(resto) <= 30L & !grepl("[0-9]", resto)
    if (any(tra)) { r$busta_suffisso <- paste0(r$busta_suffisso, paste0(", ", resto[tra], collapse = "")); resto <- resto[!tra] }
  }
  # Ciò che sembra un numero scritto in un modo che non so leggere («b. 12A», «aa. IV») non
  # deve diventare archivio, fondo o serie: resta fuori, e si dice. Così i pezzi oltre i tre attesi.
  strano <- grepl("^[[:alpha:]]{1,6}(\\.\\s*|\\s+)[0-9IVXLC]+[[:alnum:]-]*$", resto)   # abbreviazione, separatore, numero
  fuori  <- c(fuori, resto[strano])
  # `illeggibile`: una sigla con un numero non leggibile («b. 12A») farebbe perdere il numero: la riga non entra finché non si corregge.
  # Una sigla senza numero («inv.») è accettata. Solo se il pezzo avrebbe preso il posto di una busta o un fascicolo ancora vuoti.
  if (is.na(r$busta) || is.na(r$fascicolo)) illeggibile <- c(illeggibile, resto[strano])
  resto  <- resto[!strano]
  if (length(resto) > 3) { fuori <- c(fuori, resto[-(1:3)]); resto <- resto[1:3] }

  r$archivio <- if (length(resto) >= 1) resto[1] else NA_character_
  r$fondo    <- if (length(resto) >= 2) resto[2] else NA_character_
  r$serie    <- if (length(resto) >= 3) resto[3] else NA_character_
  r$non_letto <- unique(fuori); r$illeggibile <- unique(illeggibile)
  r
}

analizza_segnatura <- function(testo) {
  r <- .scomponi_segnatura(testo)
  r[setdiff(names(r), c("non_letto", "illeggibile"))]
}

# Forma della segnatura: gli unici separatori sono «, »; il punto separa sigla e numero («b.12», «b. 12» si leggono); il doppio spazio è inammissibile.
# Restituisce la segnatura con la forma giusta e i motivi delle correzioni (vuoto = a posto). Non tocca il contenuto.
.segnatura_forma <- function(testo) {
  if (.vuoto(testo)) return(list(corretta = testo, motivi = character(0)))
  v <- as.character(testo)[1]; motivi <- character(0); n <- v
  m <- gsub("[ \t\u00a0]+", " ", n); if (!identical(m, n)) { if (grepl("[ \t\u00a0]{2,}", n)) motivi <- c(motivi, "doppio spazio non ammesso"); n <- m }
  m <- gsub("\\s*[;|]\\s*", ", ", n); if (!identical(m, n)) { motivi <- c(motivi, "«;» e «|» non separano la segnatura: solo «, »"); n <- m }
  m <- gsub("\\s+,", ",", n); if (!identical(m, n)) { motivi <- c(motivi, "spazio prima della virgola"); n <- m }
  m <- gsub(",(?=[^ ,0-9])", ", ", n, perl = TRUE); if (!identical(m, n)) { motivi <- c(motivi, "la virgola vuole lo spazio dopo («, »)"); n <- m }
  list(corretta = trimws(n), motivi = motivi)
}

# Segnature che non si leggono per intero: una riga per segnatura, con i pezzi che restano fuori o «senza numero di fascicolo» (per l'anteprima).
segnature_da_guardare <- function(segnature) {
  segnature <- as.character(segnature)
  motivo <- vapply(segnature, function(x) {
    if (.vuoto(x)) return("")
    r <- .scomponi_segnatura(x)
    m <- character(0)
    if (length(r$non_letto)) m <- c(m, paste0("non leggo: ", paste(sprintf("«%s»", r$non_letto), collapse = ", ")))
    # Busta senza fascicolo è giusta («Ms., b. 1860»): si segnala solo quando non si legge né l'una né l'altro.
    if (is.na(r$fascicolo) && is.na(r$busta)) m <- c(m, "senza numero di busta né di fascicolo")
    paste(m, collapse = " \u00b7 ")
  }, character(1), USE.NAMES = FALSE)
  i <- which(nzchar(motivo))
  data.frame(riga = i, segnatura = segnature[i], motivo = motivo[i], stringsAsFactors = FALSE)
}


# Perché una segnatura NON è ammessa (vuoto se lo è): numero illeggibile («b. 12A») o forma sbagliata. La riga non entra finché non si corregge; il testo originale non si perde.
segnatura_non_ammessa <- function(x) {
  if (.vuoto(x)) return("")
  m <- character(0)
  r <- .scomponi_segnatura(x)
  if (length(r$illeggibile)) m <- c(m, paste0("numero che non so leggere: ", paste(sprintf("«%s»", r$illeggibile), collapse = ", "), " (la busta o il fascicolo sono un numero, con un suffisso come «-bis»)"))
  f <- .segnatura_forma(x)
  if (length(f$motivi)) m <- c(m, paste0(paste(f$motivi, collapse = "; "), " (forma giusta: «", f$corretta, "»)"))
  paste(m, collapse = " \u00b7 ")
}
righe_segnatura_non_ammessa <- function(a) {
  cs <- a$colonna[["Segnatura"]]
  vuoto <- data.frame(i = integer(0), segnatura = character(0), motivo = character(0), stringsAsFactors = FALSE)
  if (is.null(cs) || is.na(cs) || !nrow(a$dati)) return(vuoto)
  v <- as.character(a$dati[[cs]])
  mot <- vapply(v, segnatura_non_ammessa, "", USE.NAMES = FALSE)
  i <- which(nzchar(mot))
  if (!length(i)) vuoto else data.frame(i = i, segnatura = v[i], motivo = mot[i], stringsAsFactors = FALSE)
}

# Il blocco strutturato dentro il Regesto, se c'è.
analizza_regesto <- function(testo) {
  
  vuoto <- list(strutturato = FALSE, luogo_reato = NA_character_,
                reati = character(0), tortura = "n.d.", sentenza = "n.d.", residui = FALSE)
  if (.vuoto(testo)) return(vuoto)
  
  righe <- trimws(strsplit(as.character(testo), "\r?\n")[[1]])
  # L'export mette il punto finale al regesto; in un regesto strutturato cadrebbe sull'ultima riga («Magia.») e riletto sarebbe un refuso.
  k <- length(righe)
  if (k && grepl("^[^:]{3,60}:\\s*\\S", righe[k])) righe[k] <- sub("\\.$", "", righe[k])
  
  prendi <- function(etichetta) {
    i <- grep(paste0("^", etichetta, "\\s*:"), righe, ignore.case = TRUE)
    if (length(i) == 0) return(NA_character_)
    trimws(sub(paste0("^", etichetta, "\\s*:"), "", righe[i[1]], ignore.case = TRUE))
  }
  
  luogo    <- prendi("Nome del luogo di consumazione del reato")
  reato    <- prendi("Tipo di reato")
  tortura  <- prendi("Applicazione della tortura")
  sentenza <- prendi("Sentenza o decreto formale di spedizione")
  
  if (all(is.na(c(luogo, reato, tortura, sentenza)))) return(vuoto)
  
  norm <- function(v) {
    if (is.na(v) || !nzchar(v)) return("n.d.")
    v <- tolower(trimws(v))
    if (v %in% c("sì", "si", "sí")) "sì" else if (v == "no") "no" else "n.d."
  }
  
  # `residui`: il regesto dice più di ciò che i quattro campi rappresentano (altra etichetta, tortura o sentenza non sì/no/n.d.): ricomporlo lo mutilerebbe, chi legge decide di tenerlo («scritto a mano»).
  etichette <- c("Nome del luogo di consumazione del reato", "Tipo di reato", "Applicazione della tortura", "Sentenza o decreto formale di spedizione")
  riconosciuta <- function(r) any(vapply(etichette, function(e) grepl(paste0("^", e, "\\s*:"), r, ignore.case = TRUE), TRUE))
  altre <- righe[nzchar(righe) & !vapply(righe, riconosciuta, TRUE)]
  strano <- function(v) !is.na(v) && nzchar(v) && !(tolower(trimws(v)) %in% c("sì", "si", "sí", "no", "n.d.", "n.d", "nd"))
  residui <- length(altre) > 0L || strano(tortura) || strano(sentenza)
  
  list(strutturato = TRUE, residui = residui,
       luogo_reato = if (is.na(luogo) || !nzchar(luogo)) NA_character_ else luogo,
       reati       = if (is.na(reato) || !nzchar(reato)) character(0)
       else trimws(strsplit(reato, "[,;|]")[[1]]),   # ASMo separa con ";" (18/09)
       tortura     = norm(tortura),
       sentenza    = norm(sentenza))
}