# ─────────────────────────────────────────────────────────────
# lista_luoghi.R: la lista dei luoghi ammessi
# Dipende da percorsi.R (percorso_lista_luoghi) e qualita.R (chiave_grafia, letto dopo: si usa a app avviata)
# Una lista piatta di NOMI (chiave, nome, paese) con, dove si sono trovati, l'identificativo Wikidata e le coordinate (formato Google, «lat, lon»): sono dati accanto al nome, Lite non ha carta né servizi esterni. Riconosce un luogo già scritto bene («TRapani» → «Trapani»),
# distingue un luogo nuovo da un refuso e scioglie «S. Severo». `dati/luoghi.csv` è la base distribuita col programma; la copia personale (`luoghi/luoghi.csv` nella cartella dati)
# è l'unica che cresce, quando in un import si sceglie «Aggiungi alla lista».
# ─────────────────────────────────────────────────────────────

LISTA_COLONNE <- c("chiave", "nome", "paese", "wikidata", "coordinate")

leggi_lista_luoghi <- function(percorso = percorso_lista_luoghi()) {
  d <- utils::read.csv2(percorso, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8", na.strings = character(0))
  if (!all(c("chiave", "nome") %in% names(d))) stop("La lista dei luoghi non ha le colonne «chiave» e «nome».")
  for (c_ in setdiff(LISTA_COLONNE, names(d))) d[[c_]] <- ""     # una lista di prima ha solo chiave, nome e paese
  d$chiave <- ifelse(nzchar(trimws(d$chiave)), d$chiave, chiave_grafia(d$nome))
  if (anyDuplicated(d$chiave)) stop("La lista dei luoghi ha chiavi duplicate: correggi il CSV prima di continuare.")
  d[, LISTA_COLONNE, drop = FALSE]
}

.scrivi_lista_luoghi <- function(d, percorso)
  write.table(d[, LISTA_COLONNE, drop = FALSE], percorso, sep = ";", row.names = FALSE, na = "", qmethod = "double", fileEncoding = "UTF-8")

# I luoghi scelti con «Aggiungi alla lista» (applica_proposte → attr "luoghi_da_aggiungere"): una voce nuova per nome; un nome già presente (stessa chiave di grafia) non si riscrive. Ritorna i nomi entrati.
# Scrittura atomica (file temporaneo e rinomina), con copia della lista di prima in `backup-luoghi/`; un solo scrittore alla volta (cartella di blocco).
aggiungi_luoghi_alla_lista <- function(nomi, percorso = percorso_lista_luoghi()) {
  nomi <- unique(trimws(as.character(nomi))); nomi <- nomi[nzchar(nomi)]
  if (!length(nomi)) return(character(0))
  blocco <- paste0(percorso, ".lock")
  # Un blocco rimasto da una sessione interrotta non scade da solo: oltre i 2 minuti si toglie.
  if (dir.exists(blocco) && isTRUE(difftime(Sys.time(), file.info(blocco)$mtime, units = "secs") > 120)) unlink(blocco, recursive = TRUE)
  if (!dir.create(blocco, showWarnings = FALSE)) return(character(0))     # occupata da un'altra modifica: i luoghi restano nei dati
  on.exit(unlink(blocco, recursive = TRUE), add = TRUE)
  tryCatch({
    d <- leggi_lista_luoghi(percorso)
    nuovi <- nomi[!chiave_grafia(nomi) %in% d$chiave & nzchar(chiave_grafia(nomi))]
    nuovi <- nuovi[!duplicated(chiave_grafia(nuovi))]      # due grafie con la stessa chiave: ne entra una sola, altrimenti la lista non si rilegge più
    if (!length(nuovi)) return(character(0))
    nuove_righe <- data.frame(chiave = chiave_grafia(nuovi), nome = nuovi, paese = "", wikidata = "", coordinate = "", stringsAsFactors = FALSE)
    # Una regione nota («Sassonia») entra con paese, identificativo e coordinate del suo capoluogo, se il capoluogo è nella lista.
    reg <- regioni_luoghi(); k <- match(chiave_grafia(nuovi), reg$chiave); kc <- match(reg$chiave_capoluogo[k], chiave_grafia(d$nome))     # il capoluogo si cerca per nome («Dresda»), la sua chiave può essere un'altra («dresden»)
    for (q in which(!is.na(k) & !is.na(kc))) nuove_righe[q, c("paese", "wikidata", "coordinate")] <- d[kc[q], c("paese", "wikidata", "coordinate")]
    d <- rbind(d, nuove_righe)
    dir_copie <- file.path(dirname(percorso), "backup-luoghi")
    dir.create(dir_copie, showWarnings = FALSE)
    file.copy(percorso, file.path(dir_copie, paste0("luoghi-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", uuid::UUIDgenerate(), ".csv")))
    tmp <- tempfile("luoghi-", tmpdir = dirname(percorso), fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    .scrivi_lista_luoghi(d, tmp)
    if (!file.rename(tmp, percorso)) stop("Impossibile aggiornare la lista dei luoghi: controlla che il file non sia aperto in un altro programma.")
    nuovi
  }, error = function(e) character(0))
}

# `dati/luoghi_regioni.csv`: le regioni (storiche o straniere) che non sono luoghi ma si scrivono come tali, col loro capoluogo («Sassonia» → Dresda). Se si sceglie «Aggiungi alla lista» la regione entra con le coordinate del capoluogo.
regioni_luoghi <- function(percorso = "dati/luoghi_regioni.csv") {
  vuoto <- data.frame(regione = character(0), capoluogo = character(0), paese = character(0), chiave = character(0), chiave_capoluogo = character(0))
  if (!file.exists(percorso)) return(vuoto)
  d <- utils::read.csv2(percorso, colClasses = "character", check.names = FALSE, fileEncoding = "UTF-8", na.strings = character(0))
  d$chiave <- chiave_grafia(d$regione); d$chiave_capoluogo <- chiave_grafia(d$capoluogo); d
}
