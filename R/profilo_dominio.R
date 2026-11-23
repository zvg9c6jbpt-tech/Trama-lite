# ─────────────────────────────────────────────────────────────
# profilo_dominio.R: il profilo del materiale (vocabolari, convenzioni, parametri)
# Il metodo di normalizzazione è lo stesso per ogni materiale (docs/METODO.md); ciò che dice cosa è «corretto» per un materiale sta in
# profili/<nome>/profilo.json (docs/PROFILO.md). Qui lo si legge, lo si controlla e lo si offre al resto del programma.
# Il profilo si sceglie con TRAMA_PROFILO (di partenza «inquisizione»). Se manca o è incompleto il programma NON parte con valori di ripiego:
# un vocabolario sbagliato in silenzio falsificherebbe i dati.
# Non dipende da altri file di R/: va letto prima di compose.R, profili.R, arianna.R e arianna_import.R.
# ─────────────────────────────────────────────────────────────

PROFILO_DOMINIO_DI_PARTENZA <- "inquisizione"

# Tutto ciò che il codice si aspetta di trovare: una chiave mancante ferma l'avvio e dice quale.
.PROFILO_CHIAVI <- list(
  profilo = c("nome", "titolo", "versione"),
  produttore = c("ente", "preposizione", "preposizione_elisa", "prefisso_lungo", "sinonimi", "eccezioni"),
  vocabolari = c("reato", "supporto", "tipologia_documentaria", "soggetto", "lingua", "definizione"),
  titolo = c("tipi_titolo", "connettore_fisso", "plurale_tipo", "parola_tipologia", "tipologia_per_diversi"),
  sovrani = "titoli",
  abbreviazione_santo = c("non_santo", "sant_apostrofo", "santa", "santo", "s_t_prima_di"),
  anagrafiche = c("archivi", "luoghi_alias", "enti"),
  tesauro = "reato",
  traduzioni_en = "vocabolari",
  prova = c("archivio", "fondo", "serie")
)

# Un elenco JSON di stringhe → vettore di caratteri; un oggetto JSON di stringhe → vettore con i nomi.
.pd_chr <- function(x) as.character(unlist(x, use.names = FALSE))
.pd_vett <- function(x) { v <- unlist(x, use.names = TRUE); if (is.null(v)) character(0) else v }

# Il tesauro dei reati: ogni termine sta sotto una classe del vocabolario, ha un identificativo stabile e dice da dove viene la corrispondenza.
# Un termine che non rientra in una classe nota, un identificativo o un termine ripetuti, un termine uguale a una classe fermano l'avvio.
.controlla_tesauro_reato <- function(p, percorso) {
  tt <- p$tesauro$reato$termini
  if (!length(tt)) return(invisible())
  per <- function(k) vapply(tt, function(x) { v <- x[[k]]; if (is.null(v) || !nzchar(trimws(as.character(v)[1]))) NA_character_ else as.character(v)[1] }, "")
  guasto <- function(msg) stop("Il profilo ", percorso, ": tesauro dei reati: ", msg, call. = FALSE)
  for (k in c("id", "termine", "classe", "origine")) if (anyNA(per(k))) guasto(sprintf("un termine non ha «%s».", k))
  classi <- .pd_chr(p$vocabolari$reato)
  fuori <- setdiff(per("classe"), classi)
  if (length(fuori)) guasto(paste0("classe non nel vocabolario: ", paste0("«", fuori, "»", collapse = ", "), "."))
  if (anyDuplicated(per("id"))) guasto(paste0("identificativo ripetuto: «", per("id")[duplicated(per("id"))][1], "»."))
  nomi <- unlist(lapply(tt, function(x) c(x$termine, .pd_chr(x$varianti))))
  rip <- tolower(nomi)[duplicated(tolower(nomi))]
  if (length(rip)) guasto(paste0("termine o variante ripetuti: «", rip[1], "»."))
  uguali <- intersect(tolower(nomi), tolower(classi))
  if (length(uguali)) guasto(paste0("un termine ha il nome di una classe: «", uguali[1], "»."))
  invisible()
}

# I codici delle lingue (ISO 639): ogni lingua del vocabolario ha il suo codice di tre lettere e dice da quale norma viene (639-3 una lingua, 639-5 un gruppo).
# Una lingua senza codice, un codice di forma sbagliata o dato a due lingue, una norma sconosciuta fermano l'avvio. Il foglio resta con i nomi italiani.
.controlla_codici_lingua <- function(p, percorso) {
  cc <- p$tesauro$lingua$codici
  if (is.null(p$tesauro$lingua)) return(invisible())
  guasto <- function(msg) stop("Il profilo ", percorso, ": codici delle lingue: ", msg, call. = FALSE)
  lingue <- .pd_chr(p$vocabolari$lingua)
  manca <- setdiff(lingue, names(cc))
  if (length(manca)) guasto(paste0("lingua senza codice: ", paste0("«", manca, "»", collapse = ", "), "."))
  extra <- setdiff(names(cc), lingue)
  if (length(extra)) guasto(paste0("codice per una lingua che non è nel vocabolario: «", extra[1], "»."))
  cod <- vapply(cc, function(x) { v <- x$codice; if (is.null(v)) NA_character_ else as.character(v)[1] }, "")
  if (anyNA(cod) || any(!grepl("^[a-z]{3}$", cod))) guasto(paste0("codice non di tre lettere minuscole: «", names(cod)[is.na(cod) | !grepl("^[a-z]{3}$", cod)][1], "»."))
  if (anyDuplicated(cod)) guasto(paste0("codice dato a due lingue: «", cod[duplicated(cod)][1], "»."))
  norma <- vapply(cc, function(x) { v <- x$norma; if (is.null(v)) NA_character_ else as.character(v)[1] }, "")
  if (anyNA(norma) || any(!norma %in% c("ISO 639-3", "ISO 639-5"))) guasto(paste0("norma mancante o sconosciuta per «", names(norma)[is.na(norma) | !norma %in% c("ISO 639-3", "ISO 639-5")][1], "»."))
  coll <- names(cc)[norma == "ISO 639-5"]
  senza <- coll[!vapply(cc[coll], function(x) nzchar(as.character(x$nota)[1]), NA)]
  if (length(senza)) guasto(paste0("«", senza[1], "» è un gruppo di lingue (639-5): serve una nota che lo dica."))
  invisible()
}

# Gli alias dei luoghi: come un luogo è scritto in un archivio e quale luogo della lista è («Casale» a Torino è Casale Monferrato). Ogni alias dice perché; se limitato a un archivio,
# l'archivio è uno dell'anagrafica. Che il luogo esista nella lista lo controlla il collaudo, la lista non è ancora letta quando si legge il profilo.
.controlla_alias_luoghi <- function(p, percorso, s1) {
  al <- p$anagrafiche$luoghi_alias
  if (!length(al)) return(invisible())
  guasto <- function(msg) stop("Il profilo ", percorso, ": alias dei luoghi: ", msg, call. = FALSE)
  for (x in al) for (k in c("scritto", "luogo", "perche")) if (!nzchar(s1(x, k))) guasto(sprintf("un alias non ha «%s».", k))
  arch <- unlist(lapply(p$anagrafiche$archivi, function(x) s1(x, "nome")))
  for (x in al) if (nzchar(s1(x, "archivio")) && !tolower(s1(x, "archivio")) %in% tolower(arch)) guasto(paste0("archivio non nell'anagrafica: «", s1(x, "archivio"), "»."))
  k <- vapply(al, function(x) paste(tolower(s1(x, "scritto")), tolower(s1(x, "archivio")), sep = "\r"), "")
  if (anyDuplicated(k)) guasto(paste0("alias ripetuto: «", s1(al[[which(duplicated(k))[1]]], "scritto"), "»."))
  invisible()
}

# Gli enti produttori noti che non seguono la regola del profilo («Senato di Nizza»): nome, varianti (una variante si riscrive nel nome), motivo, Wikidata se verificato.
.controlla_enti <- function(p, percorso, s1) {
  ee <- p$anagrafiche$enti
  if (!length(ee)) return(invisible())
  guasto <- function(msg) stop("Il profilo ", percorso, ": anagrafiche degli enti: ", msg, call. = FALSE)
  for (x in ee) for (k in c("nome", "perche")) if (!nzchar(s1(x, k))) guasto(sprintf("un ente non ha «%s».", k))
  wd <- vapply(ee, s1, "", "wikidata"); wd <- wd[nzchar(wd)]
  if (length(wd) && any(!grepl("^Q[0-9]+$", wd))) guasto(paste0("identificativo Wikidata malformato: «", wd[!grepl("^Q[0-9]+$", wd)][1], "»."))
  if (anyDuplicated(wd)) guasto(paste0("identificativo ripetuto: «", wd[duplicated(wd)][1], "»."))
  nomi <- unlist(lapply(ee, function(x) c(x$nome, .pd_chr(x$varianti))))
  rip <- tolower(nomi)[duplicated(tolower(nomi))]
  if (length(rip)) guasto(paste0("nome o variante ripetuti: «", rip[1], "»."))
  invisible()
}

# Le anagrafiche (authority files): ogni archivio ha un nome, varianti, un luogo e identificativi esterni. Wikidata è «Q» e cifre; ISIL è paese, trattino, codice (IT-MO0172).
# Nome o variante ripetuti, identificativo malformato o ripetuto fermano l'avvio. L'identificativo si verifica a monte e non si inventa: se manca resta vuoto.
.controlla_anagrafiche <- function(p, percorso) {
  s1 <- function(x, k) { v <- x[[k]]; if (is.null(v)) "" else trimws(as.character(v)[1]) }
  .controlla_alias_luoghi(p, percorso, s1)
  .controlla_enti(p, percorso, s1)
  aa <- p$anagrafiche$archivi
  if (!length(aa)) return(invisible())
  guasto <- function(msg) stop("Il profilo ", percorso, ": anagrafiche degli archivi: ", msg, call. = FALSE)
  for (x in aa) if (!nzchar(s1(x, "nome"))) guasto("un archivio non ha «nome».")
  wd <- vapply(aa, s1, "", "wikidata"); isil <- vapply(aa, s1, "", "isil")
  mal <- wd[nzchar(wd) & !grepl("^Q[0-9]+$", wd)]
  if (length(mal)) guasto(paste0("identificativo Wikidata malformato: «", mal[1], "»."))
  mal <- isil[nzchar(isil) & !grepl("^[A-Z]{2}-[A-Za-z0-9:/-]+$", isil)]
  if (length(mal)) guasto(paste0("codice ISIL malformato: «", mal[1], "»."))
  for (id in list(wd, isil)) { id <- id[nzchar(id)]; if (anyDuplicated(id)) guasto(paste0("identificativo ripetuto: «", id[duplicated(id)][1], "».")) }
  nomi <- unlist(lapply(aa, function(x) c(x$nome, .pd_chr(x$varianti))))
  rip <- tolower(nomi)[duplicated(tolower(nomi))]
  if (length(rip)) guasto(paste0("nome o variante ripetuti: «", rip[1], "»."))
  invisible()
}

leggi_profilo_dominio <- function(nome = Sys.getenv("TRAMA_PROFILO", PROFILO_DOMINIO_DI_PARTENZA), cartella = Sys.getenv("TRAMA_PROFILI", "profili")) {
  if (!grepl("^[a-z0-9_-]+$", nome)) stop("Nome del profilo non valido: «", nome, "».", call. = FALSE)
  percorso <- file.path(cartella, nome, "profilo.json")
  if (!file.exists(percorso)) stop("Profilo «", nome, "» non trovato: manca ", percorso, ".", call. = FALSE)
  testo <- paste(readLines(percorso, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  p <- tryCatch(jsonlite::fromJSON(testo, simplifyVector = FALSE),
                error = function(e) stop("Il profilo ", percorso, " non è un JSON valido: ", conditionMessage(e), call. = FALSE))
  for (sez in names(.PROFILO_CHIAVI)) {
    manca <- setdiff(.PROFILO_CHIAVI[[sez]], names(p[[sez]]))
    if (length(manca)) stop("Il profilo ", percorso, " è incompleto: in «", sez, "» mancano ", paste0("«", manca, "»", collapse = ", "), ".", call. = FALSE)
  }
  .controlla_tesauro_reato(p, percorso)
  .controlla_codici_lingua(p, percorso)
  .controlla_anagrafiche(p, percorso)
  anni <- as.integer(.pd_chr(p$anni_ammessi))
  if (length(anni) != 2L || anyNA(anni) || anni[1] > anni[2]) stop("Il profilo ", percorso, ": «anni_ammessi» vuole due anni, il primo non dopo il secondo.", call. = FALSE)
  p$anni_ammessi <- anni
  p$percorso <- percorso
  p
}

PROFILO_DOMINIO <- leggi_profilo_dominio()


# Una frase del profilo come espressione regolare: spazi = uno o più spazi, apostrofo = dritto o tipografico, il resto letterale.
.re_frase <- function(x) {
  x <- gsub("([][.|()^${}+*?\\\\])", "\\\\\\1", x, perl = TRUE)
  gsub(" ", "\\\\s+", gsub("'", "['’]", x, fixed = TRUE))
}

# L'archivio dell'anagrafica per nome o variante (senza badare a maiuscole e spazi doppi): una riga {nome, luogo, wikidata, isil}, o NULL se non è nota.
# Il codice ISO 639 di una lingua del vocabolario (NA se la lingua non c'è); con norma = TRUE restituisce anche la norma.
codice_lingua <- function(nome, norma = FALSE) {
  cc <- PROFILO_DOMINIO$tesauro$lingua$codici
  vapply(nome, function(n) {
    x <- cc[[n]]
    if (is.null(x)) NA_character_ else if (norma) paste(x$codice, x$norma) else as.character(x$codice)[1]
  }, "", USE.NAMES = FALSE)
}

anagrafica_archivio <- function(nome) {
  k <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x)[1])))
  if (is.null(nome) || is.na(nome) || !nzchar(trimws(nome))) return(NULL)
  for (x in PROFILO_DOMINIO$anagrafiche$archivi)
    if (k(nome) %in% vapply(c(x$nome, .pd_chr(x$varianti)), k, "")) {
      s1 <- function(a) if (is.null(x[[a]])) "" else as.character(x[[a]])[1]
      return(data.frame(nome = x$nome, luogo = s1("luogo"), wikidata = s1("wikidata"), isil = s1("isil"), stringsAsFactors = FALSE))
    }
  NULL
}

# Il luogo della lista che un alias del profilo dà a un nome scritto nella scheda: una lista {luogo, perche}, o NULL. Un alias limitato a un archivio vale solo se la segnatura
# della riga comincia con quell'archivio (nome o variante dell'anagrafica).
alias_luogo <- function(x, segnatura = "") {
  al <- PROFILO_DOMINIO$anagrafiche$luoghi_alias
  if (!length(al) || is.na(x) || !nzchar(trimws(x))) return(NULL)
  k <- function(v) tolower(gsub("\\s+", " ", trimws(as.character(v)[1])))
  arch <- anagrafica_archivio(trimws(sub(",.*$", "", if (is.na(segnatura)) "" else segnatura)))
  for (a in al) {
    if (k(a$scritto) != k(x)) next
    if (!is.null(a$archivio) && nzchar(a$archivio) && (is.null(arch) || tolower(arch$nome) != tolower(a$archivio))) next
    return(list(luogo = as.character(a$luogo)[1], perche = as.character(a$perche)[1]))
  }
  NULL
}

# L'ente noto dell'anagrafica per nome o variante: una lista {nome, variante, perche}, o NULL. `variante` è TRUE se il testo non è il nome ma una sua variante (da riscrivere).
anagrafica_ente <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) return(NULL)
  k <- function(v) tolower(gsub("\\s+", " ", trimws(as.character(v)[1])))
  for (e in PROFILO_DOMINIO$anagrafiche$enti) {
    if (k(x) == k(e$nome)) return(list(nome = e$nome, variante = FALSE, perche = as.character(e$perche)[1]))
    if (k(x) %in% vapply(.pd_chr(e$varianti), k, "")) return(list(nome = e$nome, variante = TRUE, perche = as.character(e$perche)[1]))
  }
  NULL
}

# Un produttore che è una frase, non un nome: almeno 6 parole, nessun ente noto, non nella forma «Inquisizione di <città>». Ritorna il produttore proposto (la forma d'eccezione del profilo,
# «Sant'Uffizio», se la frase nomina l'ente con uno dei suoi sinonimi) o NA se la frase non dice di chi si tratta; NULL se il valore è un nome.
produttore_descrizione <- function(x) {
  if (is.null(x) || is.na(x) || grepl("|", x, fixed = TRUE) || lengths(strsplit(trimws(x), "\\s+")) < 6L || !is.null(anagrafica_ente(x))) return(NULL)
  pr <- PROFILO_DOMINIO$produttore
  if (grepl(paste0("^", .re_frase(pr$ente), "\\s+", .re_frase(pr$preposizione), "\\s+"), trimws(x), ignore.case = TRUE)) return(NULL)
  parole <- c(.pd_chr(pr$sinonimi), unlist(pr$eccezioni, use.names = FALSE), "S. Uffizio", "S. Ufficio", "S. Officio", "S.Uffizio", "S.Ufficio", "S.Officio")
  if (length(pr$eccezioni) && any(vapply(parole, function(w) grepl(.re_frase(sub("\\.(?=\\S)", ". ", w, perl = TRUE)), gsub("\\.(?=\\S)", ". ", x, perl = TRUE), ignore.case = TRUE), TRUE)))
    return(as.character(unlist(pr$eccezioni, use.names = FALSE))[1])
  NA_character_
}
