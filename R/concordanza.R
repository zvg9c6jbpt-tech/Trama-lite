# ─────────────────────────────────────────────────────────────
# concordanza.R: i nomi di una riga devono concordare
# Un foglio fatto a mano nomina la stessa persona in più colonne della STESSA riga (titolo, regesto, persone citate, soggetti citati):
# se una scrive «Giovanni Violo» e le altre «Giovanni Viola» è un refuso, e la riga dice qual è la forma giusta.
#  - Spazi e virgole: doppi spazi, spazio prima della virgola, nessuno dopo («Due,Nicola»).
#  - Maiuscole dei nomi: «PAolo», «vIola», «tre» → «Paolo», «Viola», «Tre» (persone, soggetti citati, nomi del titolo/regesto nella grammatica «Atti circa A, B e C»).
#  - Concordanza: le persone citate sono il perno; gli altri nomi si confrontano uno a uno. Nomi quasi uguali NON si correggono mai per voti (D11):
#    ogni discordanza oltre le maiuscole va in «Da decidere», con la forma più scritta come suggerimento.
#  - Connettore del titolo: «Atti cicra X» → «Atti circa X», solo se il titolo comincia con un tipo noto (TIPI_TITOLO).
#  - Luoghi, contro la lista: maiuscole, accenti, spazi; una lettera in più o in meno solo se il luogo noto è uno e la grafia sbagliata compare al più due volte;
#    un «!» fra due luoghi noti. Mai sostituzioni di lettera, mai ciò che sta fra parentesi.
# La grammatica dei nomi nel testo è STRETTA (regesti strutturati, titoli in latino e nomi con qualifiche restano com'è).
# Funzioni pure sui dati del foglio (`a$dati`), senza database; le usano «Pulisci e scarica» (arianna_pulisci.R) e l'anteprima. Collaudo: verifiche/normalizzazione_prova.R.
# ─────────────────────────────────────────────────────────────

.chiave_nome <- function(x) gsub("[^a-z0-9]", "", .senza_accenti(tolower(as.character(x))))

# Il «|» non vuole mai spazi; il «;» mai prima e sempre dopo; la virgola come il «;». Inoltre doppi spazi, spazi ai bordi, spazio prima del punto (non dei due punti, né fra due cifre). Non tocca gli a capo.
.pulisci_spazi <- function(x) {
  x <- as.character(x)
  x <- gsub("[ \t\u00a0]+", " ", x); x <- gsub(" +([,;.])", "\\1", x, perl = TRUE)
  x <- gsub(" *\\| *", "|", x)
  x <- gsub("([,;])(?=[^ \n|[:digit:]])", "\\1 ", x, perl = TRUE)
  trimws(x)
}

# Un testo che può essere un nome di persona: da 1 a 5 pezzi di lettere (apostrofo, punto, trattino), niente cifre, parentesi o due punti.
.sembra_nome <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & nzchar(x) & nchar(x) <= 60 & !grepl("[0-9()\\[\\]:\"]", x)
  ok[ok] <- vapply(strsplit(trimws(x[ok]), "[[:space:]]+"), function(p)
    length(p) >= 1 && length(p) <= 5 && all(grepl("^[[:alpha:]][[:alpha:]'’.-]*$", p)), TRUE)
  ok
}

# Una parola è «sbagliata di maiuscole» se è tutta maiuscola o ha una maiuscola dopo la prima lettera. Una parola solo minuscola («capitano») la sistema la concordanza; con l'apostrofo il cognome è uno solo.
.parola_nome_da_sistemare <- function(w) {
  if (grepl("['’]", w)) return(FALSE)
  if (tolower(w) %in% c(PARTICELLE_COGNOME, .MINUSCOLE_NOME)) return(FALSE)
  l <- gsub("[^[:alpha:]]", "", w)
  if (nchar(l) < 2) return(FALSE)
  if (grepl("^[[:lower:]][[:alpha:]]*[[:upper:]]", w)) return(TRUE)   # «vIola»
  if (grepl("^[[:lower:]]", w)) return(FALSE)   # solo minuscola («capitano», «doratore»): può essere vera, la sistema la concordanza
  if (nchar(l) >= 3 && l == toupper(l)) return(TRUE)
  grepl("^.[[:alpha:]]*[[:upper:]]", w)
}

# «Tommaso d'Auria» e «D'Auria, Tommaso»: stessa grafia, la particella con l'apostrofo ha la maiuscola solo a inizio cognome; non è un errore.
.solo_particella_apostrofo <- function(a, b) {
  if (is.na(a) || is.na(b) || identical(a, b)) return(FALSE)
  # Anche lo spazio dopo l'apostrofo è solo grafia («de'Lucis» / «de' Lucis»): nei dati è senza spazio.
  senza_spazio <- function(x) gsub("(^|[ ,(])(d|de|dell|dall|dal|l|sull|nell|all)(['\u2019]) +", "\\1\\2\\3", x, ignore.case = TRUE, perl = TRUE)
  a <- senza_spazio(a); b <- senza_spazio(b)
  if (identical(a, b)) return(TRUE)
  pa <- strsplit(a, " ", fixed = TRUE)[[1]]; pb <- strsplit(b, " ", fixed = TRUE)[[1]]
  if (length(pa) != length(pb)) return(FALSE)
  diversi <- which(pa != pb)
  # La differenza sta solo nella particella: ciò che segue l'apostrofo è identico («De'cefali» / «De'Cefali» no).
  dopo_ap <- function(x) sub("^[^'\u2019]*['\u2019]", "", x)
  all(vapply(diversi, function(i) tolower(pa[i]) == tolower(pb[i]) && grepl("^(d|de|dell|dall|dal|l|sull|nell|all)['\u2019]", pa[i], ignore.case = TRUE, perl = TRUE) &&
                                  identical(dopo_ap(pa[i]), dopo_ap(pb[i])), TRUE))
}

# La maiuscola della particella con l'apostrofo è quella che la cella ha già: mai cambiata.
.conserva_particella <- function(nuovo, originale) {
  if (is.na(nuovo) || is.na(originale)) return(nuovo)
  pn <- strsplit(nuovo, " ", fixed = TRUE)[[1]]; po <- strsplit(originale, " ", fixed = TRUE)[[1]]
  if (length(pn) != length(po)) return(nuovo)
  rx <- "^(d|de|dell|dall|dal|l|sull|nell|all)['\u2019]"
  for (i in seq_along(pn)) if (grepl(rx, po[i], ignore.case = TRUE, perl = TRUE) && grepl(rx, pn[i], ignore.case = TRUE, perl = TRUE))
    substr(pn[i], 1, 1) <- substr(po[i], 1, 1)
  paste(pn, collapse = " ")
}

# «PAolo Uno» → «Paolo Uno»; lascia com'è ciò che è già giusto o non è un nome.
.nome_proprio <- function(x) {
  vapply(as.character(x), function(n) {
    if (is.na(n) || !nzchar(n) || !.sembra_nome(n)) return(n)
    p <- strsplit(n, " ", fixed = TRUE)[[1]]
    q <- vapply(p, function(w) if (.parola_nome_da_sistemare(w)) .maiuscola_parola(tolower(w)) else .maiuscola_dopo_apostrofo(w), "")
    # Il cognome (ultima parola di «Nome Cognome») in minuscolo è un errore («Nicola tre»), salvo particelle e titoli: lo si alza.
    k <- length(q)
    if (k == 2L && grepl("^[[:upper:]]", q[1]) && !tolower(q[1]) %in% .MINUSCOLE_NOME && grepl("^[[:lower:]]", q[k]) && nchar(q[k]) >= 2L && !tolower(q[k]) %in% c(PARTICELLE_COGNOME, .MINUSCOLE_NOME) && !grepl("['\u2019]", q[k]))
      q[k] <- .maiuscola_parola(q[k])
    paste(q, collapse = " ")
  }, "", USE.NAMES = FALSE)
}

# Nomi che un titolo o regesto elenca, solo con la grammatica «<tipo> circa|contro A, B e C [per …]»: tipo di TIPI_TITOLO (anche con refuso nel connettore), nomi di sole lettere.
.TIPI_ORDINATI <- function() names(TIPI_TITOLO)[order(-nchar(names(TIPI_TITOLO)))]
.tipo_all_inizio <- function(t) { for (x in .TIPI_ORDINATI()) if (startsWith(tolower(t), tolower(paste0(x, " ")))) return(x); NULL }
# `nudo = TRUE` (solo titolo): il titolo con i soli nomi, senza tipo né connettore; non vale se comincia con un genere («Informazioni», «Atti per diversi»).
.GENERI_DA_SOLI <- c("Atti", "Denuncia", "Denunce", "Processo", "Processi", "Informazioni", "Copia", "Atti difensivi")
.nomi_nel_testo <- function(testo, nudo = FALSE) {
  if (.vuoto(testo)) return(character(0))
  t <- as.character(testo)[1]
  if (grepl("[\n:]", t)) return(character(0))
  t <- sub("[.;]+\\s*$", "", trimws(t))
  tipo <- .tipo_all_inizio(t)
  if (is.null(tipo) && isTRUE(nudo)) {
    prima <- sub("\\s.*$", "", t)
    if (prima %in% .GENERI_DA_SOLI || tolower(t) %in% tolower(.GENERI_DA_SOLI) ||
        grepl("\\b(per|diversi|ignoti|soggetti)\\b", t, ignore.case = TRUE)) return(character(0))
    v <- trimws(unlist(strsplit(t, ",\\s*|\\s+e\\s+"))); v <- v[nzchar(v)]
    return(if (length(v) && all(.sembra_nome(v))) v else character(0))
  }
  if (is.null(tipo)) return(character(0))
  resto <- trimws(substring(t, nchar(tipo) + 2L))
  # Il regesto che Trama stessa compone («Processo per eresia; magia contro A e B»): i reati prima del connettore si saltano.
  if (grepl("^per\\s", resto, ignore.case = TRUE)) {
    m2 <- regexpr("\\s(circa|contro)\\s", resto, perl = TRUE, ignore.case = TRUE); if (m2[1] == -1) return(character(0))
    resto <- trimws(substring(resto, m2[1] + 1L))
  }
  m <- regexpr("^\\S+\\s+", resto, perl = TRUE); if (m[1] == -1) return(character(0))
  w <- tolower(trimws(regmatches(resto, m)))
  if (!w %in% c("circa", "contro") && !(nchar(w) >= 4 && min(drop(utils::adist(w, c("circa", "contro")))) <= 2)) return(character(0))
  resto <- trimws(substring(resto, attr(m, "match.length") + 1L))
  resto <- sub("\\s+per\\s+.*$", "", resto, ignore.case = TRUE, perl = TRUE)
  if (grepl("\\bdiversi\\b|\\bignoti\\b|\\bsoggetti\\b", resto, ignore.case = TRUE)) return(character(0))
  v <- trimws(unlist(strsplit(resto, ",\\s*|\\s+e\\s+")))
  v <- v[nzchar(v)]
  if (!length(v) || !all(.sembra_nome(v))) return(character(0))
  v
}

# Voci di una cella-elenco (persone, soggetti, luoghi): separate da «|» o «;» fuori dalle parentesi; in soggetti anche dalla virgola se non è «Cognome, Nome».
# I due punti non sono un separatore: fuori dalle parentesi un «:» fra due valori è un errore con separatore proposto. Mai negli indirizzi.
.due_punti_a_separatore <- function(v, sep = "|") {
  if (.vuoto(v)) return(v)
  v <- as.character(v)[1]
  if (!grepl(":", v, fixed = TRUE) || grepl("://", v, fixed = TRUE)) return(v)
  car <- strsplit(v, "")[[1]]; due <- car == ":" & .livello_parentesi(car) <= 0
  if (!any(due)) return(v)
  car[due] <- "\u0001"
  pezzi <- trimws(strsplit(paste(car, collapse = ""), "\u0001", fixed = TRUE)[[1]])
  paste(pezzi[nzchar(pezzi)], collapse = sep)
}


# ── Separatori ammessi ──────────────────────────────────
# Nelle colonne a più valori gli unici separatori sono «|» (senza spazi) e «; » (lingue e tipologie). La virgola è ammessa solo nel testo semplice e dentro un nome.
# «/», «+», «&», «!» non sono mai parte di un nome; «e», «ed» e il trattino fra spazi sono connettori. Ogni altro connettore è un ERRORE con il separatore giusto proposto.
.sostituisci_fuori_parentesi <- function(v, ch, sep) {
  if (.vuoto(v)) return(v)
  v <- as.character(v)[1]; car <- strsplit(v, "")[[1]]
  w <- car %in% ch & .livello_parentesi(car) <= 0
  if (!any(w)) return(v)
  car[w] <- "\u0001"
  p <- trimws(strsplit(paste(car, collapse = ""), "\u0001", fixed = TRUE)[[1]])
  paste(p[nzchar(p)], collapse = sep)
}

# Un elenco di CLASSI (reati, lingue, tipologie): se tutti i pezzi sono classi e un separatore non è fra gli `ammessi` («|» e «;» per i reati; solo «;» per lingue e tipologie), si riscrive.
# Se non si legge per intero resta com'è (la decide il vocabolario).
.separatori_con_classi <- function(v, classi, sep, ammessi = c("|", ";")) {
  if (.vuoto(v) || grepl("[()]", v) || !length(classi)) return(v)
  v <- as.character(v)[1]; lo <- tolower(v); n <- nchar(v)
  cl <- classi[order(-nchar(classi))]; cll <- tolower(cl)
  pos <- 1L; pezzi <- character(0); estraneo <- FALSE
  repeat {
    resto <- substr(lo, pos, n)
    k <- which(startsWith(resto, cll))[1]
    if (is.na(k)) return(v)
    len <- nchar(cll[k]); pezzi <- c(pezzi, substr(v, pos, pos + len - 1L)); pos <- pos + len
    if (pos > n) break
    resto <- substr(lo, pos, n)
    m <- regexpr("^\\s*(\\||;|,|/|\\\\|\\+|&|:|!)\\s*|^\\s+(e|ed)\\s+|^\\s+-\\s+", resto, perl = TRUE)
    if (m[1] != 1L) return(v)
    if (!trimws(regmatches(resto, m)) %in% ammessi) estraneo <- TRUE
    pos <- pos + attr(m, "match.length")
  }
  if (estraneo && length(pezzi) >= 2L) paste(pezzi, collapse = sep) else v
}

# Luoghi: «/», «\», «+», «&», «!» e la virgola fuori dalle parentesi separano sempre; «e», «ed» e « - » solo se tutti i pezzi sono luoghi noti.
.separatori_luoghi <- function(v, noti) {
  if (.vuoto(v)) return(v)
  v <- .sostituisci_fuori_parentesi(as.character(v)[1], c("/", "\\", "+", "&", ",", "!"), "|")
  if (grepl("[()]", v)) return(v)
  seg <- strsplit(v, "|", fixed = TRUE)[[1]]
  nuovo <- vapply(trimws(seg), function(x) {
    if (!nzchar(x) || chiave_grafia(x) %in% noti) return(x)
    p <- trimws(strsplit(x, "\\s+(e|ed)\\s+|\\s+-\\s+", perl = TRUE)[[1]])
    if (length(p) >= 2L && all(chiave_grafia(p) %in% noti)) paste(p, collapse = "|") else x
  }, "")
  paste(nuovo[nzchar(nuovo)], collapse = "|")
}

# Persone: «/», «\», «+», «&», «!» separano sempre; «e»/«ed» solo se i due lati sono «Cognome, Nome». Con più di una virgola fuori dalle parentesi si segnala (`avviso`).
.separatori_persone <- function(v) {
  if (.vuoto(v)) return(list(v = v, avviso = FALSE))
  v <- .sostituisci_fuori_parentesi(as.character(v)[1], c("/", "\\", "+", "&", "!"), "|")
  conta <- function(x) { car <- strsplit(x, "")[[1]]; sum(car == "," & .livello_parentesi(car) <= 0) }
  if (!grepl("[()]", v)) {
    seg <- strsplit(v, "|", fixed = TRUE)[[1]]
    seg <- unlist(lapply(trimws(seg), function(x) {
      p <- trimws(strsplit(x, "\\s+(e|ed)\\s+", perl = TRUE)[[1]])
      if (length(p) >= 2L && all(vapply(p, conta, 0L) == 1L)) p else x
    }))
    v <- paste(seg[nzchar(seg)], collapse = "|")
  }
  list(v = v, avviso = any(vapply(strsplit(v, "|", fixed = TRUE)[[1]], conta, 0L) >= 2L))
}

.nomi_nella_cella <- function(cella, con_virgole = FALSE) {
  if (.vuoto(cella)) return(character(0))
  car <- strsplit(as.character(cella)[1], "")[[1]]
  sep <- car %in% c("|", ";") & .livello_parentesi(car) <= 0
  car[sep] <- "\u0001"; v <- trimws(strsplit(paste(car, collapse = ""), "\u0001", fixed = TRUE)[[1]])
  if (con_virgole) v <- trimws(unlist(lapply(v, function(x) {
    p <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    if (length(p) == 2 && all(lengths(strsplit(p, "[[:space:]]+")) == 1L)) x else p
  })))
  v[nzchar(v)]
}

# Una parola minuscola in un nome («marco Cerretoli») che un'altra colonna scrive maiuscola si alza, anche se il resto discorda. Mai per particelle e titoli.
.alza_come <- function(forma, altre) {
  if (is.na(forma)) return(forma)
  maiuscole <- unique(unlist(strsplit(altre[!is.na(altre)], " ", fixed = TRUE))); maiuscole <- maiuscole[grepl("^[[:upper:]]", maiuscole)]
  p <- strsplit(forma, " ", fixed = TRUE)[[1]]
  p <- vapply(p, function(w) if (grepl("^[[:lower:]]", w) && !tolower(w) %in% c(PARTICELLE_COGNOME, .MINUSCOLE_NOME) && tolower(w) %in% tolower(maiuscole)) .maiuscola_parola(w) else w, "")
  paste(p, collapse = " ")
}

# Abbina i nomi di due liste uno a uno, dal più vicino: coppia valida se differiscono di al più due lettere (e un quinto), con stesse iniziali e stesso numero di pezzi.
.abbina_nomi <- function(a, b) {
  vuoto <- data.frame(i = integer(0), j = integer(0), d = integer(0))
  if (!length(a) || !length(b)) return(vuoto)
  ka <- .chiave_nome(a); kb <- .chiave_nome(b)
  d  <- matrix(utils::adist(ka, kb), nrow = length(a))
  for (i in seq_along(a)) for (j in seq_along(b)) {
    pa <- strsplit(trimws(a[i]), "[[:space:]]+")[[1]]; pb <- strsplit(trimws(b[j]), "[[:space:]]+")[[1]]
    ok <- length(pa) == length(pb) &&
      identical(tolower(substr(pa[1], 1, 1)), tolower(substr(pb[1], 1, 1))) &&
      identical(tolower(substr(pa[length(pa)], 1, 1)), tolower(substr(pb[length(pb)], 1, 1))) &&
      d[i, j] <= 2 && d[i, j] <= max(1, floor(nchar(ka[i]) / 5))
    if (!ok) d[i, j] <- Inf
  }
  coppie <- list(); ambigui <- list()
  while (any(is.finite(d))) {
    k <- which(d == min(d), arr.ind = TRUE)[1, ]
    ri <- which(is.finite(d[, k[2]])); cj <- which(is.finite(d[k[1], ]))
    if (d[k[1], k[2]] > 0 && (length(ri) > 1L || length(cj) > 1L)) {
      # Più abbinamenti plausibili (13/10): non si sceglie per ordine, si segnala (va in «Da decidere»)
      ri <- union(k[1], ri); cj <- union(k[2], cj)
      ambigui[[length(ambigui) + 1L]] <- list(i = ri, j = cj)
      d[ri, ] <- Inf; d[, cj] <- Inf
      next
    }
    coppie[[length(coppie) + 1]] <- data.frame(i = k[1], j = k[2], d = d[k[1], k[2]])
    d[k[1], ] <- Inf; d[, k[2]] <- Inf
  }
  r <- if (length(coppie)) do.call(rbind, coppie) else vuoto
  attr(r, "ambigui") <- ambigui
  r
}

# Il connettore del titolo con un refuso («Atti cicra X»): solo se il titolo comincia con un tipo noto.
.connettore_titolo <- function(testo) {
  if (.vuoto(testo)) return(testo)
  t <- as.character(testo)[1]; if (grepl("[\n:]", t)) return(t)
  tipo <- .tipo_all_inizio(t); if (is.null(tipo)) return(t)
  resto <- substring(t, nchar(tipo) + 2L)
  m <- regexpr("^\\S+", resto, perl = TRUE); if (m[1] == -1) return(t)
  lw <- tolower(regmatches(resto, m))
  if (lw %in% c("circa", "contro") || nchar(lw) < 4) return(t)
  d <- drop(utils::adist(lw, c("circa", "contro")))
  if (min(d) > 2 || sum(d == min(d)) != 1L) return(t)
  paste0(substr(t, 1, nchar(tipo) + 1L), c("circa", "contro")[which.min(d)], substring(resto, attr(m, "match.length") + 1L))
}

# Un nome del titolo che somiglia (una o due lettere) a una persona della riga senza essere uguale è un refuso da confrontare; «Lettera di Paolo a Pietro» non somiglia a nessuno.
.quasi_una_persona <- function(nomi, persone) {
  p <- .estendi_nome(persone); p <- p[!is.na(p) & nzchar(p)]; nomi <- nomi[!is.na(nomi) & nzchar(nomi)]
  if (!length(p) || !length(nomi)) return(FALSE)
  ab <- .abbina_nomi(nomi, p)
  nrow(ab) > 0L || length(attr(ab, "ambigui")) > 0L
}

# Un nome senza spazio nel titolo o nel regesto («Domenicodi Cineda») che la colonna Persone della stessa riga scrive giusto: si ripropone. Solo se, tolti gli spazi, il testo è uguale al nome; escluse parole intere, apostrofi e parentesi.
.nomi_staccati_male <- function(testo, persone) {
  if (.vuoto(testo) || .vuoto(persone)) return(testo)
  t <- as.character(testo)[1]
  for (p in .nomi_nella_cella(persone)) {
    if (grepl("[()'\u2019]", p)) next
    pz <- trimws(strsplit(p, ",", fixed = TRUE)[[1]]); if (length(pz) != 2L) next
    par <- c(strsplit(pz[2], "[[:space:]]+")[[1]], strsplit(pz[1], "[[:space:]]+")[[1]]); par <- par[nzchar(par)]
    if (length(par) < 3L || !any(tolower(par) %in% c("di", "da", "de", "del", "della", "dei", "degli", "dal", "dalla", "van", "von"))) next   # solo se c'è una particella: «Adamo» non è «Ada Mo»
    giusto <- paste(par, collapse = " ")
    re <- paste0("(?<![[:alpha:]])", paste(gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", par), collapse = "\\s*"), "(?![[:alpha:]])")
    m <- gregexpr(re, t, perl = TRUE, ignore.case = TRUE)[[1]]; if (m[1] == -1L) next
    trovati <- regmatches(t, list(m))[[1]]
    for (x in unique(trovati[tolower(trovati) != tolower(giusto) & tolower(gsub("[[:space:]]+", "", trovati)) == tolower(gsub("[[:space:]]+", "", giusto))]))
      t <- gsub(x, giusto, t, fixed = TRUE)
  }
  t
}

# `avvisi_luoghi`: solo «Pulisci un foglio» (la finestra «Controlla prima di importare» chiede già dei luoghi ignoti, D9).
# Concordanza dei nomi, riga per riga: restituisce i dati corretti, il registro (una riga per cella toccata) e i casi da decidere. `a` è il risultato di leggi_arianna().
concordanza_righe <- function(a, lista_luoghi = NULL, avvisi_luoghi = FALSE) {
  a$colonna <- .con_nomi_modello(a$colonna, a$profilo)
  d <- a$dati; reg <- list(); dec <- list(); decisioni <- list()
  col <- function(nome) { x <- unname(a$colonna[nome]); if (!length(x) || is.na(x) || !x %in% names(d)) NA_character_ else x }
  c_tit <- col("Titolo"); c_reg <- col("Regesto o descrizione sintetica")
  c_per <- col("Persone citate o indicizzate"); c_sog <- col("Soggetti citati o indicizzati")
  c_seg <- col("Segnatura"); c_luo <- col("Luoghi citati o indicizzati"); c_dt <- col("Luogo di produzione (data topica)")
  dove <- function(i) { s <- if (!is.na(c_seg)) as.character(d[[c_seg]][i]) else ""; paste0("riga ", i + 1L, if (nzchar(s) && !is.na(s)) paste0(" (", trimws(sub("^.*,", "", s)), ")") else "") }
  log_ <- function(i, colonna, prima, dopo, perche)
    reg[[length(reg) + 1L]] <<- data.frame(riga = i + 1L, colonna = colonna, prima = prima, dopo = dopo, perche = perche, stringsAsFactors = FALSE)
  # Un caso da decidere: il testo, le OPZIONI e i BERSAGLI (le celle dove la scelta va applicata).
  decidi <- function(i, testo, opzioni = character(0), bersagli = NULL) {
    dec[[length(dec) + 1L]] <<- data.frame(riga = i + 1L, testo = sprintf("%s: %s", dove(i), testo), stringsAsFactors = FALSE)
    if (length(opzioni) && !is.null(bersagli))
      decisioni[[length(decisioni) + 1L]] <<- list(riga = i + 1L, segnatura = { sg_ <- if (!is.na(c_seg)) as.character(d[[c_seg]][i]) else ""; if (nzchar(sg_) && !is.na(sg_)) trimws(if (grepl(",\\s*b\\.", sg_)) sub("^.*?,\\s*(b\\.)", "\\1", sg_, perl = TRUE) else sub("^.*,", "", sg_)) else "" }, testo = testo, opzioni = unique(opzioni), bersagli = bersagli)
  }

  # Un avviso: compare in «Da decidere», senza opzioni e senza cambiare la cella.
  avvisa <- function(i, testo) {
    dec[[length(dec) + 1L]] <<- data.frame(riga = i + 1L, testo = sprintf("%s: %s", dove(i), testo), stringsAsFactors = FALSE)
    decisioni[[length(decisioni) + 1L]] <<- list(riga = i + 1L, segnatura = segn_(i), testo = testo, opzioni = character(0), bersagli = NULL, tipo = "avviso")
  }

  # 0. Spazi e virgole scritti male (solo nelle colonne di nomi, luoghi e nel titolo)
  for (cc in na.omit(c(c_tit, c_reg, c_per, c_sog, c_luo, c_dt, col("Soggettario"), col("Soggetto produttore"),
                       col("Luogo di conservazione"), col("Note libere"), col("Lingua")))) {
    for (i in which(!is.na(d[[cc]]) & nzchar(d[[cc]]))) {
      v <- as.character(d[[cc]][i])
      # Il regesto composto sì; quello strutturato dei cataloghi («Tipo di reato: …») no.
      if (identical(cc, c_reg) && (grepl("[\n:]", v) || is.null(.tipo_all_inizio(v)))) next
      n <- .pulisci_spazi(v)
      if (!identical(n, v)) { d[[cc]][i] <- n; log_(i, cc, v, n, "spazi e virgole") }
    }
  }

  # 0b. Punteggiatura finale: una virgola o un punto e virgola lasciati in fondo, o due punti di fila («..»; tre sono i puntini di sospensione).
  # Il punto finale lo mette l'export (L3): una virgola in fondo diventerebbe «,.».
  for (cc in na.omit(c(c_reg, col("Note libere")))) for (i in which(!is.na(d[[cc]]) & nzchar(d[[cc]]))) {
    v <- as.character(d[[cc]][i]); if (identical(cc, c_reg) && (grepl("[\n:]", v) || is.null(.tipo_all_inizio(v)))) next
    n <- sub("(?<!\\.)\\.\\.$", ".", sub("[[:space:]]*[,;]+$", "", v), perl = TRUE)
    if (!identical(n, v)) { d[[cc]][i] <- n; log_(i, cc, v, n, "punteggiatura finale (virgola in fondo o punto doppio)") }
  }

  # 0a. Un nome scritto senza spazio nel titolo o nel regesto, giusto nella colonna Persone della stessa riga.
  if (!is.na(c_per)) for (cc in na.omit(c(c_tit, c_reg))) for (i in which(!is.na(d[[cc]]) & nzchar(d[[cc]]) & !is.na(d[[c_per]]))) {
    v <- as.character(d[[cc]][i]); if (identical(cc, c_reg) && (grepl("[\n:]", v) || is.null(.tipo_all_inizio(v)))) next
    n <- .nomi_staccati_male(v, d[[c_per]][i])
    if (!identical(n, v)) { d[[cc]][i] <- n; log_(i, cc, v, n, "nome senza spazio, come nella colonna Persone") }
  }

  # 0b. Il «;» al posto del «|» nelle colonne di voci che non sono nomi (soggettario, soggetto produttore).
  for (cc in na.omit(c(col("Soggettario"), col("Soggetto produttore"), c_per, c_sog, c_luo, c_dt))) for (i in which(!is.na(d[[cc]]) & grepl(";", d[[cc]], fixed = TRUE))) {
    v <- as.character(d[[cc]][i]); n <- paste(.nomi_nella_cella(v), collapse = "|")
    if (!identical(n, v) && !grepl("[()]", v)) { d[[cc]][i] <- n; log_(i, cc, v, n, "«;» al posto di «|» fra le voci") }
  }

  # 0c. Il «:» al posto del «|» (o del «; » di lingue e tipologie) fra i valori: errore, in tutte le colonne a più valori.
  for (cc in na.omit(c(col("Soggettario"), col("Soggetto produttore"), c_per, c_sog, c_luo, c_dt))) for (i in which(!is.na(d[[cc]]) & grepl(":", d[[cc]], fixed = TRUE))) {
    v <- as.character(d[[cc]][i]); n <- .due_punti_a_separatore(v, "|")
    if (!identical(n, v)) { d[[cc]][i] <- n; log_(i, cc, v, n, "«:» non è un separatore: al suo posto «|»") }
  }
  for (cc in na.omit(c(col("Lingua"), col("Tipologia documentaria")))) for (i in which(!is.na(d[[cc]]) & grepl(":", d[[cc]], fixed = TRUE))) {
    v <- as.character(d[[cc]][i]); n <- .due_punti_a_separatore(v, "; ")
    if (!identical(n, v)) { d[[cc]][i] <- n; log_(i, cc, v, n, "«:» non è un separatore: al suo posto «; »") }
  }

  # 0d. Separatori ammessi: solo «|» e «; »; ogni altro connettore è un errore, con il separatore giusto proposto.
  segn_ <- function(i) { sg_ <- if (!is.na(c_seg)) as.character(d[[c_seg]][i]) else ""; if (nzchar(sg_) && !is.na(sg_)) trimws(if (grepl(",\\s*b\\.", sg_)) sub("^.*?,\\s*(b\\.)", "\\1", sg_, perl = TRUE) else sub("^.*,", "", sg_)) else "" }
  noti_lista <- tryCatch(chiave_grafia(leggi_lista_luoghi()$nome), error = function(e) character(0))
  riscrivi_ <- function(cc, f, perche) for (i in which(!is.na(d[[cc]]) & nzchar(d[[cc]]))) {
    v <- as.character(d[[cc]][i]); n <- f(v)
    if (!identical(n, v)) { d[[cc]][i] <<- n; log_(i, cc, v, n, perche) }
  }
  for (cc in na.omit(col("Soggettario"))) riscrivi_(cc, function(v) .separatori_con_classi(v, vocabolario("reato"), "|"), "separatore non ammesso (solo «|» e «; »): corretto in «|»")
  for (cc in na.omit(col("Lingua"))) riscrivi_(cc, function(v) .separatori_con_classi(v, vocabolario("lingua"), "; ", ";"), "separatore non ammesso (solo «; » in questa colonna): corretto in «; »")
  for (cc in na.omit(col("Tipologia documentaria"))) riscrivi_(cc, function(v) .separatori_con_classi(v, vocabolario("tipologia_documentaria"), "; ", ";"), "separatore non ammesso (solo «; » in questa colonna): corretto in «; »")
  if (length(noti_lista)) for (cc in na.omit(c(c_luo, c_dt))) riscrivi_(cc, function(v) .separatori_luoghi(v, noti_lista), "separatore non ammesso (solo «|» e «; »): corretto in «|»")
  for (cc in na.omit(c(c_per, c_sog))) {
    riscrivi_(cc, function(v) .separatori_persone(v)$v, "separatore non ammesso (solo «|» e «; »): corretto in «|»")
    for (i in which(!is.na(d[[cc]]) & nzchar(d[[cc]]))) if (isTRUE(.separatori_persone(d[[cc]][i])$avviso)) {
      t_ <- sprintf("«%s»: la virgola non separa le persone (solo «|» e «; »): scrivi «Cognome, Nome|Cognome, Nome»", as.character(d[[cc]][i]))
      dec[[length(dec) + 1L]] <- data.frame(riga = i + 1L, testo = sprintf("%s: %s", dove(i), t_), stringsAsFactors = FALSE)
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn_(i), testo = t_, opzioni = character(0), bersagli = NULL, tipo = "avviso")
    }
  }

  # 0e. Giorno e mese negli estremi: la data intera è «aaaa-mm-gg» o «mese aaaa» (minuscolo); «1625-1-9», «Marzo 1644», «1644-03» si riscrivono così.
  for (cc in na.omit(c(col("Estremo remoto"), col("Estremo recente")))) riscrivi_(cc, function(v) { dp <- .data_precisa(v); if (is.null(dp)) v else dp$canonica },
                                                                                  "data scritta come nella guida (aaaa-mm-gg o «mese aaaa»)")
  # 0e bis. Estremo recente uguale al remoto: la data è una sola, il recente si toglie. Vale per anni e date uguali; un anno e una data dentro quell'anno restano.
  c_rem_ <- col("Estremo remoto"); c_rec_ <- col("Estremo recente")
  if (!is.na(c_rem_) && !is.na(c_rec_)) for (i in which(!is.na(d[[c_rem_]]) & !is.na(d[[c_rec_]]))) {
    r1_ <- trimws(as.character(d[[c_rem_]][i])); r2_ <- trimws(as.character(d[[c_rec_]][i]))
    if (nzchar(r1_) && identical(r1_, r2_) && !is.null(.data_o_anno(r1_))) {
      d[[c_rec_]][i] <- NA_character_; log_(i, c_rec_, r2_, NA_character_, "estremo recente uguale al remoto: si toglie (la data è una sola)")
    }
  }
  # 0f. Ciò che l'export scriverebbe comunque ma il foglio non diceva (nessuna modifica non dichiarata): particella dopo il nome («da Valle, Giacoma» → «Valle, Giacoma da»),
  # «Inquisizione di Roma» → «Sant'Uffizio» e le lingue in minuscolo. (Il punto finale di regesto e note è una convenzione di sola uscita, contratto L3.)
  if (!is.na(c_per)) riscrivi_(c_per, function(v) {
    voci <- strsplit(v, "|", fixed = TRUE)[[1]]
    paste(vapply(voci, function(x) if (grepl(",", x, fixed = TRUE)) .nome_persona_export(.nome_persona_import(trimws(x))) else x, ""), collapse = "|") },
    "particella dopo il nome, come nella guida al Tab")
  # Un cognome con particella e nient'altro, senza virgola («Della Cioppa»): l'import lo scrive «della Cioppa» (particella sempre minuscola, guida al Tab);
  # senza questo passo la modifica restava «non dichiarata» (Capua, riga 202, 19/11/2026).
  if (!is.na(c_per)) riscrivi_(c_per, function(v) {
    voci <- strsplit(v, "|", fixed = TRUE)[[1]]
    paste(vapply(voci, function(x) {
      x <- trimws(x)
      if (!grepl(",", x, fixed = TRUE) && grepl("^(Di|De|Da|Del|Della|Dello|Delle|Degli|Dei|Dal|Dalla|Dalle|Dai)[[:space:]]+[^[:space:]]+$", x)) .nome_persona_import(x) else x }, ""), collapse = "|") },
    "particella del cognome in minuscolo, come nella guida al Tab")
  # Il nome senza virgola («Mario Rossi») si indicizza «Rossi, Mario»: correzione che si vede e si può rifiutare. Non si indovina dove il cognome non è certo (qualifica, «di/da», un nome solo, sovrano).
  # Se titolo o regesto della riga scrivono le stesse parole in altro ordine («Matteo Gallo»), l'ordine lo dà il testo.
  if (!is.na(c_per)) for (i in which(!is.na(d[[c_per]]) & nzchar(d[[c_per]]))) {
    v <- as.character(d[[c_per]][i])
    voci <- strsplit(v, "|", fixed = TRUE)[[1]]
    if (any(grepl(",", voci, fixed = TRUE))) next          # una cella con virgole e un nome senza: è una decisione (revisione_import.R), non si indovina
    testi <- unique(c(if (!is.na(c_tit)) .nomi_nel_testo(d[[c_tit]][i], nudo = TRUE), if (!is.na(c_reg)) .nomi_nel_testo(d[[c_reg]][i])))
    parole <- function(x) sort(tolower(strsplit(trimws(x), "[[:space:]]+")[[1]]))
    n <- paste(vapply(voci, function(x) { x <- trimws(x)
      if (!nzchar(x) || .formula_sovrano(x) || .cognome_incerto(x) || senza_cognome(x) || !grepl("[[:space:]]", x)) return(x)
      k <- which(vapply(testi, function(tt) identical(parole(tt), parole(x)) && !identical(tolower(trimws(tt)), tolower(x)), TRUE))
      if (length(k) && length(unique(tolower(testi[k]))) == 1L) indicizza_nome(.nome_proprio(testi[k[1]])) else x }, ""), collapse = "|")
    if (!identical(n, v)) { d[[c_per]][i] <- n; log_(i, c_per, v, n, "nome senza virgola: l'ordine delle parole è quello del titolo o del regesto") }
  }
  if (!is.na(c_per)) riscrivi_(c_per, function(v) {
    voci <- strsplit(v, "|", fixed = TRUE)[[1]]
    if (any(grepl(",", voci, fixed = TRUE))) return(v)          # una cella con virgole e un nome senza: è una decisione (revisione_import.R), non si indovina
    paste(vapply(voci, function(x) { x <- trimws(x)
      if (!nzchar(x) || grepl(",", x, fixed = TRUE) || .formula_sovrano(x) || .cognome_incerto(x) || senza_cognome(x) || !grepl("[[:space:]]", x)) x else indicizza_nome(x) }, ""), collapse = "|") },
    "nome senza virgola indicizzato «Cognome, Nome» (l'ultima parola è il cognome)")
  # Una variante di un ente noto dell'anagrafica del profilo («Conseil del Commis») si riscrive nel nome dell'ente.
  for (cc in na.omit(col("Soggetto produttore"))) riscrivi_(cc, function(v) paste(vapply(strsplit(v, "|", fixed = TRUE)[[1]], function(x) { e <- anagrafica_ente(x); if (!is.null(e) && e$variante) e$nome else x }, ""), collapse = "|"),
                                                              "produttore: variante di un ente dell'anagrafica del profilo")
  for (cc in na.omit(col("Soggetto produttore"))) riscrivi_(cc, function(v) paste(vapply(strsplit(v, "|", fixed = TRUE)[[1]], function(x) { r <- .produttore_canonico(x); if (length(r)) r[1] else x }, ""), collapse = "|"),
                                                              "produttore nella forma della guida (solo Roma è «Sant'Uffizio»)")
  # Il regesto composto comincia con la maiuscola; quello strutturato dei cataloghi non si tocca.
  if (!is.na(c_reg)) riscrivi_(c_reg, function(v) if (!grepl("[\n:]", v) && !is.null(.tipo_all_inizio(v)) && grepl("^[[:lower:]]", v)) paste0(toupper(substr(v, 1, 1)), substring(v, 2)) else v,
                               "regesto: iniziale maiuscola")
  for (cc in na.omit(col("Lingua"))) riscrivi_(cc, function(v) tolower(gsub("\\s*[|]\\s*", "; ", v)), "lingua: minuscolo e «; » fra le voci, come nel modello")
  # La tipologia della guida è Atti, Lettere o Decreti; ogni altro valore («Unità archivistiche», «Processo») esce «Atti», ma ora si vede.
  for (cc in na.omit(col("Tipologia documentaria"))) riscrivi_(cc, function(v) {
    voci <- trimws(strsplit(v, "[;|]")[[1]]); voci <- voci[nzchar(voci)]
    if (length(voci) && all(tolower(voci) %in% tolower(TIPOLOGIE_FOGLIO))) v else "Atti" },
    "tipologia documentaria fuori dalla guida (Atti, Lettere, Decreti): esce «Atti», come in tutti i fogli del modello")

  # 0g. «S.» sciolto sempre in San, Santo, Santa o Sant' (luoghi, persone, soggetti, titolo, regesto).
  for (cc in na.omit(c(c_luo, c_dt, col("Luogo di conservazione")))) riscrivi_(cc, function(v) sciogli_santi(v, guardia = FALSE), "«S.» sciolto in San, Santo, Santa o Sant'")
  for (cc in na.omit(c_sog)) riscrivi_(cc, function(v) sciogli_santi(v, ignoti = "lascia"), "«S.» sciolto in San, Santo, Santa o Sant' (santo del dizionario)")
for (cc in na.omit(c(c_tit, c_reg))) riscrivi_(cc, sciogli_santi, "«S.» sciolto in San, Santo, Santa o Sant'")      # dopo un nome maiuscolo resta, e si propone (revisione_import.R)

  # 0d2. Spazi doppi dentro la cella nelle colonne della numerazione, che non hanno un campo proprio (e quindi non passano da normalizza_fascicolo): un errore, si porta a uno (Stefano, 18/11).
  for (nm in c("tipo di numerazione", "Numerazione", "epoca numerazione")) { cc <- col(nm); if (!is.na(cc)) riscrivi_(cc, function(v) gsub("[ \t]{2,}", " ", v), "spazi doppi portati a uno") }

  # 0e. «|» nei campi a un valore solo o di testo (segnatura, titolo, regesto, note…): errore; non si indovina il rimedio, si segnala.
  semplici_ <- c("Segnatura", "Altra segnatura", "Titolo", "Estremo remoto", "Estremo recente", "definizione.tipologia", "Supporto (materiale)", "dimensioni/altezza",
                 "dimensioni/larghezza", "tipo di numerazione", "Numerazione", "epoca numerazione", "consistenza.quantità", "consistenza.unitàMisura",
                 "Regesto o descrizione sintetica", "Luogo di conservazione", "Note libere", "Manifest IIIF", "Identificativo immagine o cartella")
  for (nm in semplici_) { cc <- col(nm); if (is.na(cc)) next
    for (i in which(!is.na(d[[cc]]) & grepl("|", d[[cc]], fixed = TRUE))) {
      t_ <- sprintf("%s: «%s» contiene «|», che separa solo i valori delle colonne a più valori", nm, substr(as.character(d[[cc]][i]), 1, 80))
      dec[[length(dec) + 1L]] <- data.frame(riga = i + 1L, testo = sprintf("%s: %s", dove(i), t_), stringsAsFactors = FALSE)
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn_(i), testo = t_, opzioni = character(0), bersagli = NULL, tipo = "avviso")
    }
  }

  nome_fonte <- c(persone = "Persone", soggetti = "Soggetti citati", titolo = "Titolo", regesto = "Regesto")
  for (i in seq_len(nrow(d))) {
    # 1. Il connettore del titolo e del regesto
    for (cc in na.omit(c(c_tit, c_reg))) {
      v <- d[[cc]][i]; if (.vuoto(v)) next
      n <- .connettore_titolo(v)
      if (!identical(n, as.character(v))) { d[[cc]][i] <- n; log_(i, cc, as.character(v), n, "connettore con un refuso") }
    }
    # 2. Persone, soggetti, nomi del titolo e del regesto. I nomi stanno in Titolo, Regesto e Persone; i «Soggetti citati» potrebbero esserlo, ma nei fogli veri sono vuoti.
    per <- if (!is.na(c_per)) .nomi_nella_cella(d[[c_per]][i]) else character(0)
    sog <- if (!is.na(c_sog)) .nomi_nella_cella(d[[c_sog]][i], con_virgole = TRUE) else character(0)
    tit <- if (!is.na(c_tit)) .nomi_nel_testo(d[[c_tit]][i], nudo = TRUE) else character(0)
    # Un titolo di soli nomi è un elenco di nomi solo se almeno uno è già fra le persone della riga («Catalogus causarum» non è una discordanza).
    if (length(tit) && is.null(.tipo_all_inizio(sub("[.;]+\\s*$", "", trimws(as.character(d[[c_tit]][i]))))) &&
        !any(.chiave_nome(tit) %in% .chiave_nome(.estendi_nome(per))) && !.quasi_una_persona(tit, per)) tit <- character(0)
    rgs <- if (!is.na(c_reg)) .nomi_nel_testo(d[[c_reg]][i]) else character(0)
    fonti <- list(persone = list(c = c_per, orig = per, f = .estendi_nome(per)),
                  soggetti = list(c = c_sog, orig = sog, f = .estendi_nome(sog)),
                  titolo   = list(c = c_tit, orig = tit, f = tit),
                  regesto  = list(c = c_reg, orig = rgs, f = rgs))
    # una voce che non sembra un nome (parentesi, cifre…) non entra: resta com'è
    for (nf in c("persone", "soggetti")) { ok <- .sembra_nome(fonti[[nf]]$f); fonti[[nf]]$f[!ok] <- NA_character_ }
    piene <- names(fonti)[vapply(fonti, function(x) any(!is.na(x$f)), TRUE)]
    if (!length(piene)) next
    nuovi <- lapply(fonti, function(x) .nome_proprio(x$f))       # la forma con le maiuscole a posto, per ogni nome
    scelta <- nuovi
    if (length(piene) >= 2L) {
      hub <- piene[1]; kh <- .chiave_nome(fonti[[hub]]$f)
      voti <- lapply(seq_along(kh), function(h) data.frame(fonte = hub, k = h, forma = nuovi[[hub]][h], stringsAsFactors = FALSE))
      for (nf in setdiff(piene, hub)) {
        kn <- .chiave_nome(fonti[[nf]]$f); presi <- integer(0)
        for (j in seq_along(kn)) {                       # prima i nomi uguali (a meno delle maiuscole), uno a uno
          h <- which(kh == kn[j] & !seq_along(kh) %in% presi)[1]
          if (!is.na(kn[j]) && !is.na(h)) { presi <- c(presi, h); voti[[h]] <- rbind(voti[[h]], data.frame(fonte = nf, k = j, forma = nuovi[[nf]][j], stringsAsFactors = FALSE)) }
        }
        resto_h <- setdiff(seq_along(kh), presi)
        resto_j <- which(!seq_along(kn) %in% unlist(lapply(voti, function(g) g$k[g$fonte == nf])))
        if (length(resto_h) && length(resto_j)) {        # poi i quasi uguali (un refuso)
          ab <- .abbina_nomi(fonti[[hub]]$f[resto_h], fonti[[nf]]$f[resto_j])
          for (r in seq_len(nrow(ab))) { h <- resto_h[ab$i[r]]; j <- resto_j[ab$j[r]]
            voti[[h]] <- rbind(voti[[h]], data.frame(fonte = nf, k = j, forma = nuovi[[nf]][j], stringsAsFactors = FALSE)) }
          for (am in attr(ab, "ambigui")) {                # più candidati plausibili: si decide, non si indovina
            hh <- resto_h[am$i]; jj <- resto_j[am$j]
            decidi(i, paste(c(sprintf("«%s» (%s)", fonti[[hub]]$f[hh], nome_fonte[hub]), sprintf("«%s» (%s)", fonti[[nf]]$f[jj], nome_fonte[nf])),
                            collapse = " ≈ "),
                   opzioni = unique(c(nuovi[[hub]][hh], nuovi[[nf]][jj])),
                   bersagli = data.frame(colonna = c(rep(fonti[[hub]]$c, length(hh)), rep(fonti[[nf]]$c, length(jj))),
                                         da = c(fonti[[hub]]$orig[hh], fonti[[nf]]$orig[jj]), stringsAsFactors = FALSE))
          }
        }
      }
      # La forma meglio scritta di una persona: la più piena di iniziali maiuscole («Nicola Tre»); le particelle non contano.
      migliore <- function(forme) forme[order(-vapply(forme, function(f) { w <- strsplit(f, " ")[[1]]; sum(grepl("^[[:upper:]]", w[!tolower(w) %in% c(PARTICELLE_COGNOME, .MINUSCOLE_NOME)])) }, 1L), seq_along(forme))][1]
      for (g in voti) {
        g <- g[!is.na(g$forma), , drop = FALSE]
        if (nrow(g) < 2L) next
        # La maiuscola si corregge comunque, anche se resta una decisione sul resto del nome.
        g$forma <- vapply(g$forma, .alza_come, "", altre = g$forma)
        for (r in seq_len(nrow(g))) scelta[[g$fonte[r]]][g$k[r]] <- g$forma[r]
        # Ogni discordanza fra le colonne è una DECISIONE, mai una correzione per voti: si scrive da soli solo la differenza di maiuscole fra le stesse lettere.
        stesse_lettere <- length(unique(tolower(gsub("[[:space:]]+", " ", trimws(g$forma))))) == 1L
        if (!stesse_lettere) {
          # la forma più scritta è solo un SUGGERIMENTO (prima fra le scelte, mai applicata da sola)
          g$chiave <- .chiave_nome(g$forma)
          conta <- sort(table(g$chiave), decreasing = TRUE)
          piu_scritta <- if (length(conta) == 1L || conta[1] > conta[2]) migliore(g$forma[g$chiave == names(conta)[1]]) else NA_character_
          per_forma <- vapply(unique(g$forma), function(f) sprintf("«%s» (%s)", f, paste(nome_fonte[g$fonte[g$forma == f]], collapse = ", ")), "")
          decidi(i, paste0(paste(per_forma, collapse = " ≠ "),
                           if (!is.na(piu_scritta)) sprintf(" · la più scritta: «%s»", piu_scritta) else ""),
                 opzioni = unique(c(piu_scritta[!is.na(piu_scritta)], g$forma)),
                 bersagli = data.frame(colonna = vapply(g$fonte, function(f) fonti[[f]]$c, ""),
                                       # Il testo com'è dopo la correzione della maiuscola: la scelta lo cerca nella cella già corretta.
                                       da = vapply(seq_len(nrow(g)), function(r) { f <- g$fonte[r]; o <- fonti[[f]]$orig[g$k[r]]
                                         if (f == "persone" && grepl(",", o, fixed = TRUE)) indicizza_nome(g$forma[r]) else if (f %in% c("persone", "soggetti", "titolo")) g$forma[r] else o }, ""),
                                       stringsAsFactors = FALSE))
          next
        }
        scrivi <- migliore(g$forma)
        for (r in seq_len(nrow(g))) scelta[[g$fonte[r]]][g$k[r]] <- scrivi
      }
    }
    # 3. Si riscrivono le celle dove una forma è cambiata
    for (nf in piene) {
      x <- fonti[[nf]]; if (is.na(x$c)) next
      # «d'Auria» e «D'Auria» restano come sono: una cella tiene la maiuscola della sua particella.
      scelta[[nf]] <- unname(mapply(.conserva_particella, scelta[[nf]], x$f))
      cambiati <- which(!is.na(x$f) & scelta[[nf]] != x$f & !mapply(.solo_particella_apostrofo, x$f, scelta[[nf]]))
      if (!length(cambiati)) next
      resa <- function(k) if (nf == "persone" && grepl(",", x$orig[k], fixed = TRUE)) indicizza_nome(scelta[[nf]][k]) else scelta[[nf]][k]
      for (k in cambiati)
        log_(i, x$c, x$orig[k], resa(k),
             if (identical(.nome_proprio(x$f[k]), scelta[[nf]][k])) "maiuscole del nome" else "il nome concorda con le altre colonne della riga")
      if (nf %in% c("persone", "soggetti")) {
        d[[x$c]][i] <- paste(vapply(seq_along(x$orig), function(k) if (k %in% cambiati) resa(k) else x$orig[k], ""), collapse = "|")
      } else {
        v <- as.character(d[[x$c]][i])
        for (k in cambiati) v <- sub(x$orig[k], resa(k), v, fixed = TRUE)
        d[[x$c]][i] <- v
      }
    }
  }

  # 3a. Luogo di conservazione e soggetto produttore con un refuso o una maiuscola diversa: un valore raro che somiglia a uno scritto almeno tre volte, e tre volte più spesso,
  # è forse un refuso; non si corregge da solo, si decide.
  for (nc in c("Luogo di conservazione", "Soggetto produttore")) {
    c_lc <- col(nc); if (is.na(c_lc)) next
    et <- if (identical(nc, "Luogo di conservazione")) "luogo di conservazione" else "soggetto produttore"
    lc <- trimws(as.character(d[[c_lc]])); lc[is.na(lc)] <- ""; tab <- table(lc[nzchar(lc)])
    for (i in which(nzchar(lc))) {
      ni <- as.integer(tab[[lc[i]]]); cand <- names(tab)[as.integer(tab) >= pmax(3L, 3L * ni) & names(tab) != lc[i]]
      cand <- cand[vapply(cand, function(w) nchar(w) >= 8L && identical(tolower(substr(w, 1, 1)), tolower(substr(lc[i], 1, 1))) &&
                                              utils::adist(.chiave_nome(lc[i]), .chiave_nome(w))[1, 1] <= 2, TRUE)]
      if (length(cand) == 1L)
        decidi(i, sprintf("%s «%s»: forse «%s» (scritto %d volte)", et, lc[i], cand, as.integer(tab[[cand]])),
               opzioni = cand, bersagli = data.frame(colonna = c_lc, da = lc[i], stringsAsFactors = FALSE))
    }
  }
  # 3b. Due voci della stessa cella di persone che somigliano a una persona sola: si segnala, non si fonde.
  if (!is.na(c_per)) for (i in which(!is.na(d[[c_per]]) & grepl("|", d[[c_per]], fixed = TRUE))) {
    pv <- .nomi_nella_cella(d[[c_per]][i]); pv <- pv[grepl(",", pv, fixed = TRUE)]
    if (length(pv) < 2L) next
    cg <- .chiave_nome(sub(",.*$", "", pv)); nm <- .chiave_nome(sub("^[^,]*,", "", pv))
    for (x in seq_len(length(pv) - 1L)) for (y in (x + 1L):length(pv))
      if (cg[x] == cg[y] && nchar(nm[x]) >= 6L && nm[x] != nm[y] && utils::adist(nm[x], nm[y])[1, 1] <= 2L) {
        t_ <- sprintf("«%s» e «%s» nella stessa cella: forse la stessa persona scritta in due modi", pv[x], pv[y])
        dec[[length(dec) + 1L]] <- data.frame(riga = i + 1L, testo = sprintf("%s: %s", dove(i), t_), stringsAsFactors = FALSE)
        decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn_(i), testo = t_, opzioni = character(0), bersagli = NULL, tipo = "avviso")
      }
  }

  # 4. I luoghi, contro la lista dei luoghi
  gaz <- lista_luoghi
  if (is.null(gaz)) gaz <- tryCatch(leggi_lista_luoghi(), error = function(e) NULL)
  if (!is.null(gaz) && nrow(gaz)) {
    nomi <- gaz$nome; chiavi <- chiave_grafia(nomi)
    for (cc in na.omit(c(c_luo, c_dt))) {
      # Quante volte compare ogni grafia nella colonna: un luogo vero compare spesso, un refuso una o due volte.
      freq <- table(unlist(lapply(d[[cc]], function(v) if (.vuoto(v)) character(0) else .nomi_nella_cella(v))))
      # Omonimi: più voci della lista con lo stesso nome. La cella non cambia (non si può scrivere la provincia): si segnala, con prima proposta il più vicino agli altri luoghi della riga.
      molt <- table(nomi); coord <- .coordinate_lista(if (is.null(gaz$coordinate)) rep(NA_character_, nrow(gaz)) else gaz$coordinate)
      colonne_luoghi <- na.omit(c(c_luo, c_dt, col("Luogo di conservazione")))
      # Il luogo di ogni voce dell'omonimo, per mostrarlo: il comune dalla chiave («tarantomontelibretti» → Montelibretti), altrimenti «comune». Mai pezzi di chiave: se il nome del comune non si trova, si dice solo «frazione».
      province_composte <- c(pesaroeurbino = "Pesaro e Urbino")
      etichetta <- function(j) {
        suf <- sub(paste0("^", chiave_grafia(nomi[j])), "", gaz$chiave[j])
        if (!nzchar(suf)) return("comune")
        if (startsWith(suf, "prov")) {
          kp <- sub("^prov", "", suf); np <- if (kp %in% names(province_composte)) province_composte[[kp]] else nomi[match(kp, chiavi)]
          return(if (is.na(np)) "comune in un'altra provincia" else paste("comune in provincia di", np))
        }
        k <- match(suf, chiavi); if (is.na(k)) "frazione" else paste("frazione di", nomi[k])
      }
      omonimo <- function(i, x) {
        j <- which(nomi == x)
        # Stesso identificativo Wikidata = lo stesso luogo scritto in più modi («Londra» / «London»), non un omonimo.
        wd <- if (is.null(gaz$wikidata)) rep("", nrow(gaz)) else gaz$wikidata
        j <- j[!(nzchar(wd[j]) & duplicated(wd[j]))]
        # Una voce «di prima» (chiave semplice, né identificativo né coordinate) accanto a una frazione con identificativo è la stessa frazione scritta senza il comune («Limidi» / «Limidi, Soliera»): non è un comune a sé.
        coo <- if (is.null(gaz$coordinate)) rep("", nrow(gaz)) else gaz$coordinate
        j <- j[!(vapply(j, function(k) etichetta(k) == "comune", TRUE) & !nzchar(wd[j]) & !nzchar(coo[j]) & any(nzchar(wd[j])))]
        # Si avvisa solo se almeno uno dei luoghi è un comune: più frazioni con lo stesso nome, senza un comune così chiamato, sarebbero solo rumore.
        if (length(j) < 2L || !any(vapply(j, function(k) etichetta(k) %in% "comune" || startsWith(etichetta(k), "comune in provincia"), TRUE))) return(NULL)
        rif <- NULL
        for (cc2 in colonne_luoghi) for (y in .nomi_nella_cella(as.character(d[[cc2]][i]))) {
          jy <- which(nomi == y); if (length(jy) == 1L && !is.na(coord[jy, 1]) && y != x) rif <- rbind(rif, coord[jy, ])
        }
        dist <- if (is.null(rif)) rep(NA_real_, length(j)) else vapply(j, function(k) if (is.na(coord[k, 1])) NA_real_ else min(.km(coord[k, 1], coord[k, 2], rif[, 1], rif[, 2])), 0)
        ord <- order(dist, na.last = TRUE)
        eti <- sprintf("%s (%s)", x, vapply(j[ord], etichetta, ""))
        elenco <- paste(sprintf("«%s»", eti), collapse = ", ")
        testo <- if (!is.na(dist[ord[1]])) sprintf("luogo «%s» esiste in più luoghi: %s. Per vicinanza agli altri luoghi della riga la prima proposta è «%s» (circa %d km); la cella resta com'è",
                                                   x, elenco, eti[1], as.integer(round(dist[ord[1]])))
                 else sprintf("luogo «%s» esiste in più luoghi: %s; la riga non ha altri luoghi con cui scegliere, la cella resta com'è", x, elenco)
        list(testo = testo, opzioni = eti, info = data.frame(scelta = eti, nome = x, wikidata = wd[j[ord]], coordinate = if (is.null(gaz$coordinate)) "" else gaz$coordinate[j[ord]], stringsAsFactors = FALSE))
      }
      # Un luogo è «uguale», «noto» (maiuscole/accenti/spazi diversi), «vicino» (una lettera in più o in meno, uno solo) o «ignoto».
      risolvi <- function(x) {
        kx <- chiave_grafia(x); if (!nzchar(kx)) return(list(tipo = "ignoto"))
        # Un nome scritto identico a una voce della lista è quella voce, anche se un'altra ha la stessa grafia senza accenti («Tarantò» / «Taranto»).
        if (x %in% nomi) return(list(tipo = "uguale", nome = x))
        # Più nomi diversi con la stessa grafia senza accenti («Paterno» / «Paternò», «Taranto» / «Tarantò»): non si sa quale sia, quindi non si riscrive; si propone.
        if (!is.na(match(kx, chiavi)) && length(uniche <- unique(nomi[chiavi == kx])) > 1L) return(list(tipo = "proposta", nome = uniche))
        j <- match(kx, chiavi)
        # Se cambia il numero di parole («San Zeno» contro il comune «Sanzeno») sono forse due luoghi: non si riscrive, si propone.
        if (!is.na(j) && !identical(nomi[j], x) && lengths(strsplit(trimws(x), "[[:space:]]+")) != lengths(strsplit(nomi[j], "[[:space:]]+")))
          return(list(tipo = "proposta", nome = nomi[j]))
        if (!is.na(j)) return(list(tipo = if (identical(nomi[j], x)) "uguale" else "noto", nome = nomi[j]))
        # La chiave è quella di una voce della lista scritta in altro modo. Per un luogo straniero («Berlin» per «Berlino», «Sassonia») la forma scritta è la buona e resta;
        # per uno italiano («Castel de'Britti» per «Castel dei Britti», «Racalbuto» per «Racalmuto») si propone il nome della lista.
        if (is.na(j) && (ia <- match(kx, gaz$chiave)) > 0L && !is.na(ia))
          return(if (gaz$paese[ia] %in% c("Italia", "Regno d'Italia")) list(tipo = "proposta", nome = nomi[ia]) else list(tipo = "uguale", nome = x))
        if (nchar(kx) < 6 || (!is.na(freq[x]) && freq[x] > 2L)) return(list(tipo = "ignoto"))
        # Un refuso a un passo si corregge solo con una prova (maiuscola dentro la parola, o lettera raddoppiata che nel luogo noto è una sola: «Trapaani»);
        # una lettera in più o in meno senza prove («Avignon», «Berlin») si propone: va in «Da decidere».
        dist <- drop(utils::adist(kx, chiavi))
        # Per i luoghi stranieri la forma giusta è quella locale: non si propone mai la forma italiana della lista.
        if (!is.null(gaz$paese)) dist[nzchar(gaz$paese) & !(gaz$paese %in% c("Italia", "Regno d'Italia"))] <- Inf
        ok <- if (grepl("[[:lower:]][[:upper:]]", x)) which(dist == 1L & abs(nchar(chiavi) - nchar(kx)) <= 1L)
              else if (grepl("(.)\\1", kx)) which(chiavi %in% .senza_una_doppia(kx))
              else integer(0)
        if (length(ok) == 1L) return(list(tipo = "vicino", nome = nomi[ok]))
        prop <- which(dist == 1L & abs(nchar(chiavi) - nchar(kx)) == 1L)
        # Due lettere vicine scambiate («Otarnto»). Una lettera sbagliata («Valenza Po» / «Valenzano») no: con migliaia di nomi somiglia a troppi luoghi veri.
        prop <- union(prop, which(chiavi %in% .con_scambio(kx)))
        if (length(prop)) return(list(tipo = "proposta", nome = unique(nomi[prop])))
        list(tipo = "ignoto")
      }
      for (i in which(!is.na(d[[cc]]) & nzchar(d[[cc]]))) {
        v <- as.character(d[[cc]][i]); if (grepl("[()]", v)) next          # mai ciò che sta fra parentesi
        voci <- .nomi_nella_cella(v)
        nuove <- character(0); cambiata <- FALSE
        for (x in voci) {
          # Un alias del profilo (come quel luogo è scritto in quell'archivio) vale prima di ogni confronto con la lista.
          al <- alias_luogo(x, if (!is.na(c_seg)) as.character(d[[c_seg]][i]) else "")
          if (!is.null(al)) { log_(i, cc, x, al$luogo, paste0("alias del luogo nel profilo: ", al$perche)); nuove <- c(nuove, al$luogo); cambiata <- TRUE; next }
          pezzi <- if (grepl("!", x, fixed = TRUE)) trimws(unlist(strsplit(x, "!", fixed = TRUE))) else x
          res <- lapply(pezzi, risolvi)
          if (length(pezzi) > 1L && any(vapply(res, function(r) r$tipo == "ignoto", TRUE))) { pezzi <- x; res <- list(risolvi(x)) }
          else if (length(pezzi) > 1L) { log_(i, cc, x, paste(vapply(res, `[[`, "", "nome"), collapse = "|"), "separatore «!» fra luoghi noti"); cambiata <- TRUE }
          for (q in seq_along(pezzi)) {
            r <- res[[q]]
            if (r$tipo %in% c("uguale", "noto") && !is.na(molt[r$nome]) && molt[r$nome] > 1L) {
              t_om <- omonimo(i, r$nome)
              if (r$tipo == "noto") { log_(i, cc, pezzi[q], r$nome, "grafia del luogo come nella lista dei luoghi"); cambiata <- TRUE }
              nuove <- c(nuove, r$nome)
              if (!is.null(t_om)) {
                dec[[length(dec) + 1L]] <- data.frame(riga = i + 1L, testo = sprintf("%s: %s", dove(i), t_om$testo), stringsAsFactors = FALSE)
                decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn_(i), testo = t_om$testo, opzioni = t_om$opzioni, bersagli = NULL, tipo = "omonimo", info = t_om$info, colonna = cc)
              }
            }
            else if (r$tipo == "noto")        { log_(i, cc, pezzi[q], r$nome, "grafia del luogo come nella lista dei luoghi"); nuove <- c(nuove, r$nome); cambiata <- TRUE }
            else if (r$tipo == "vicino") { log_(i, cc, pezzi[q], r$nome, "refuso del luogo (una maiuscola o una lettera fuori posto)"); nuove <- c(nuove, r$nome); cambiata <- TRUE }
            else {
              if (r$tipo == "proposta")
                decidi(i, sprintf("luogo «%s»: forse %s", pezzi[q], paste(sprintf("«%s»", r$nome), collapse = " o ")),
                       opzioni = r$nome, bersagli = data.frame(colonna = cc, da = pezzi[q], stringsAsFactors = FALSE))
              # Un luogo che non è nella lista, non somiglia a nessuno e compare una volta sola nella colonna: forse un refuso o una forma antica; si segnala, non si cambia.
              else if (avvisi_luoghi && r$tipo == "ignoto" && nchar(pezzi[q]) >= 4L && !is.na(freq[pezzi[q]]) && freq[pezzi[q]] == 1L)
                avvisa(i, sprintf("luogo «%s» non è nella lista dei luoghi e non somiglia a nessuno: controlla la grafia (forma antica o locale?)", pezzi[q]))
              nuove <- c(nuove, pezzi[q])
            }
          }
        }
        if (cambiata) d[[cc]][i] <- paste(nuove, collapse = "|")
      }
    }
  }

  list(dati = d,
       registro = if (length(reg)) do.call(rbind, reg) else data.frame(riga = integer(0), colonna = character(0), prima = character(0), dopo = character(0), perche = character(0)),
       da_decidere = if (length(dec)) do.call(rbind, dec) else data.frame(riga = integer(0), testo = character(0)),
       decisioni = decisioni)
}

# Coordinate «lat, lon» della lista come matrice (NA dove vuote); distanza in km (haversine).
.coordinate_lista <- function(x) {
  m <- matrix(NA_real_, length(x), 2L)
  ok <- !is.na(x) & grepl("^-?[0-9.]+, -?[0-9.]+$", x)
  if (any(ok)) m[ok, ] <- do.call(rbind, lapply(strsplit(x[ok], ", ", fixed = TRUE), as.numeric))
  m
}
.km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  12742 * asin(pmin(1, sqrt(a)))
}

# Le grafie che si ottengono togliendo una lettera da una coppia di lettere uguali («trapaani» → «trapani»).
# Le grafie che si ottengono scambiando due lettere vicine («otarnto» → «otranto»).
.con_scambio <- function(k) {
  ch <- strsplit(k, "")[[1]]; n <- length(ch); if (n < 2L) return(character(0))
  unique(vapply(seq_len(n - 1L), function(j) { v <- ch; v[c(j, j + 1L)] <- v[c(j + 1L, j)]; paste(v, collapse = "") }, ""))
}

.senza_una_doppia <- function(k) {
  ch <- strsplit(k, "")[[1]]; i <- which(ch[-1] == ch[-length(ch)])
  unique(vapply(i, function(j) paste(ch[-j], collapse = ""), ""))
}

# Ogni estremo è un anno, una data intera della guida, «n.d.» o una datazione per secolo; il resto esce com'è scritto, ma lo si segnala.
avvisi_date <- function(a) {
  a$colonna <- .con_nomi_modello(a$colonna, a$profilo)
  d <- a$dati; out <- data.frame(riga = integer(0), testo = character(0), stringsAsFactors = FALSE)
  col <- function(nome) { x <- unname(a$colonna[nome]); if (!length(x) || is.na(x) || !x %in% names(d)) NA_character_ else x }
  c_seg <- col("Segnatura")
  dove <- function(i) { s <- if (!is.na(c_seg)) as.character(d[[c_seg]][i]) else ""; paste0("riga ", i + 1L, if (nzchar(s) && !is.na(s)) paste0(" (", trimws(sub("^.*,", "", s)), ")") else "") }
  avvisa <- function(i, testo) out[nrow(out) + 1L, ] <<- list(i + 1L, sprintf("%s: %s", dove(i), testo))
  c_er <- col("Estremo remoto"); c_ec <- col("Estremo recente")
  leggi_estremo <- function(v) {
    v <- trimws(as.character(v)); if (is.na(v) || !nzchar(v)) return(list(stato = "vuoto"))
    if (tolower(gsub("[ .]", "", v)) %in% c("nd", "nondisponibile") || .datazione_secolo(v)) return(list(stato = "dichiarato"))
    dp <- .data_precisa(v); n <- if (!is.null(dp)) dp$anno else suppressWarnings(as.integer(if (grepl("^[0-9]{1,4}(\\.0+)?$", v)) sub("\\.0+$", "", v) else NA))
    if (is.na(n)) return(list(stato = "illeggibile"))
    if (n < ANNI_AMMESSI[1] || n > ANNI_AMMESSI[2]) return(list(stato = "fuori", anno = n))
    list(stato = "anno", anno = n)
  }
  for (i in seq_len(nrow(d))) {
    e <- list(remoto = if (!is.na(c_er)) leggi_estremo(d[[c_er]][i]) else list(stato = "vuoto"),
              recente = if (!is.na(c_ec)) leggi_estremo(d[[c_ec]][i]) else list(stato = "vuoto"))
    for (k in names(e)) {
      et <- paste("estremo", k); v <- trimws(as.character(d[[if (k == "remoto") c_er else c_ec]][i]))
      if (e[[k]]$stato == "illeggibile")
        avvisa(i, sprintf("%s «%s» non è un anno leggibile: resta com'è scritto, senza anno nel campo numerico (scrivi l'anno, «n.d.» o «sec. XVI»)", et, v))
      else if (e[[k]]$stato == "fuori")
        avvisa(i, sprintf("%s «%s» è fuori dall'intervallo ammesso (%d–%d): controlla la cifra", et, v, ANNI_AMMESSI[1], ANNI_AMMESSI[2]))
    }
    if (identical(e$remoto$stato, "anno") && identical(e$recente$stato, "anno") && e$recente$anno < e$remoto$anno)
      avvisa(i, sprintf("estremo recente (%d) precedente all'estremo remoto (%d): sono scambiati?", e$recente$anno, e$remoto$anno))
  }
  out
}

