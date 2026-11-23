# ──────────────────────────────────────────────
# verifiche/tab_reali.R: i tab veri come corpus permanente (22/10/2026, dal parere di Codex)
# I tab d'archivio (Torino, Bologna, Dublino, Napoli; ASMo è escluso) stanno fuori dal progetto, in `Tab Completi`: qui NON si copia
# nessun dato. Se la cartella c'è (o se TRAMA_TAB_REALI la indica) ogni tab fa il percorso vero e si controlla:
#   1. nessuna riga sparisce: letti = importati + respinti, e «Pulisci e scarica» (come nell'app: correzioni spuntate, poi pulisci) produce un
#      foglio nel modello;
#   2. il foglio pulito attraversa di nuovo l'import con 0 correzioni automatiche (neutro sul corretto) e le stesse righe;
#   3. il foglio pulito, importato in un database vuoto e riesportato, torna uguale cella per cella;
#   3b. ogni differenza fra il foglio originale e il pulito è nel registro (proposta accettata o convenzione dichiarata, nessuna «non dichiarata»);
#   4. i conteggi (righe, correzioni automatiche per colonna, proposte, decisioni) sono quelli dell'istantanea
#      `verifiche/corpus/tab_reali.csv`: chi cambia una regola e sposta i conteggi lo dichiara rigenerandola:
#      `TRAMA_TAB_REALI_SALVA=1 Rscript verifiche/tab_reali.R`. L'istantanea ha solo numeri, mai dati d'archivio.
# Senza la cartella dei tab, o senza qualcuno dei tab, il collaudo stampa `SALTATO: …` (esce 0, ma `tutte.R` non lo conta fra i riusciti).
#     Rscript verifiche/tab_reali.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
# Isolato dal database vero (19/11/2026): senza questo le decisioni (omonimi, luoghi noti) dipendevano dalle anagrafiche di ~/Documents/Trama Lite
# e lo stesso tab dava 7 decisioni su un Mac e 6 su un altro.
Sys.setenv(TRAMA_HOME = local({ d <- tempfile("home-tab-reali-"); dir.create(d); d }))
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
BASE <- Sys.getenv("TRAMA_TAB_REALI", file.path(Sys.getenv("HOME"), "Desktop", "Digital Inquisition", "Tab Completi"))   # i tab veri non sono nel repository: senza la cartella il collaudo è saltato
ISTANTANEA <- "verifiche/corpus/tab_reali.csv"
# I tab: il percorso sotto BASE e, per quelli la cui segnatura non dice archivio e fondo, i nomi con cui entrano (come li darebbe chi li carica).
TAB <- list(
  list(f = "Torino/Tab ASTo.xlsx"),
  list(f = "Bologna/ap 2.xlsx"),
  list(f = "Bologna/Tab Bologna - AGABo, aa.164-168.xlsx"),
  list(f = "Bologna/Tab Bologna - Archiginnasio.xlsx", archivio = "Biblioteca dell'Archiginnasio", fondo = "Manoscritti"),
  list(f = "Dublino - Trinity College/TAB Trinity College.xlsx", archivio = "Trinity College", fondo = "Inquisizione"),
  list(f = "Napoli/ASDNa buste 1-32.xlsx"),
  # Capua sta fuori da «Tab Completi» (Handoff/Tab di prova): un percorso assoluto vale com'è (19/11/2026).
  list(f = file.path(Sys.getenv("HOME"), "Desktop", "Dh Progetti", "Handoff", "Tab di prova", "Pilota premium (stampa)", "capua_modello.xlsx")))
if (!dir.exists(BASE)) { cat("SALTATO: la cartella dei tab veri non c'è (", BASE, "): le correzioni che spostano i conteggi dei tab veri NON sono state controllate; indica la cartella con TRAMA_TAB_REALI\nESITO: ok - saltato\n"); quit(status = 0) }

celle_diverse <- function(x, y) { x <- as.matrix(x); y <- as.matrix(y); if (!identical(dim(x), dim(y))) return(Inf); sum(!(is.na(x) & is.na(y)) & (is.na(x) != is.na(y) | x != y), na.rm = TRUE) }
conta <- list()
mis <- function(tab, metrica, valore) conta[[length(conta) + 1L]] <<- data.frame(tab = tab, metrica = metrica, valore = as.integer(valore), stringsAsFactors = FALSE)
lettura <- function(p) as.data.frame(readxl::read_excel(p, sheet = 1, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
lettura_foglio <- function(p, foglio) as.data.frame(readxl::read_excel(p, sheet = foglio, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)

non_letti <- character()
for (t in TAB) {
  percorso <- if (startsWith(t$f, "/")) t$f else file.path(BASE, t$f); nome <- sub("[.]xlsx$", "", basename(t$f))
  if (!file.exists(percorso)) { cat("saltato:", t$f, "(non c'è)\n"); non_letti <- c(non_letti, basename(t$f)); next }
  a <- leggi_arianna(percorso)
  pr <- proposte_import(a, NULL, NULL)
  auto <- pr$celle[pr$celle$consigliata, , drop = FALSE]
  mis(nome, "righe", nrow(a$dati)); mis(nome, "auto", nrow(auto)); mis(nome, "proposte", nrow(pr$celle) - nrow(auto)); mis(nome, "decisioni", length(pr$decisioni))
  for (cl in sort(unique(auto$colonna))) mis(nome, paste0("auto: ", cl), sum(auto$colonna == cl))
  # il percorso dell'app: la finestra «Controlla prima di pulire» (correzioni spuntate di partenza, nessuna decisione presa), poi «Pulisci» revisionato
  pr0 <- proposte_import(a, NULL, NULL, luoghi_ignoti = FALSE)
  a1 <- applica_proposte(a, pr0, ok = pr0$celle$id[pr0$celle$consigliata], scelte = list())
  p <- tryCatch(do.call(pulisci_foglio, c(list(a1, nome = basename(t$f), revisionato = TRUE), t[c("archivio", "fondo")[c("archivio", "fondo") %in% names(t)]])),
                error = function(e) conditionMessage(e))
  if (is.character(p)) {
    # Nessuna riga entra (un tab con un'altra forma di segnatura non ammessa: i doppi spazi, invece, ora sono una correzione proposta, 23/10): è un esito
    # noto, non un difetto. Si registra quante righe sono respinte e perché, e il tab non va oltre.
    tmp <- apri_db(tempfile(fileext = ".sqlite"))
    e0 <- if (!is.null(t$archivio)) importa_nuovo_archivio(tmp, a1, t$archivio, t$fondo, NULL) else importa_arianna(tmp, a1, modo = "accoda")
    v(sprintf("%s: nessuna riga entra, e il motivo è dichiarato (%d respinte: %s)", nome, nrow(e0$respinti), paste(unique(sub("\\(.*$", "", e0$respinti$motivo)), collapse = "; ")),
      grepl("^Nessuna riga è entrata", p) && e0$importati == 0L && all(nzchar(e0$respinti$motivo)))
    mis(nome, "importati", e0$importati); mis(nome, "respinti", nrow(e0$respinti)); DBI::dbDisconnect(tmp); next
  }
  v(sprintf("%s: nessuna riga sparisce (letti %d = importati %d + respinti %d)", nome, p$letti, p$importati, nrow(p$respinti)), p$letti == p$importati + nrow(p$respinti))
  mis(nome, "respinti", nrow(p$respinti))
  # Ogni differenza fra il foglio originale e il pulito ha un nome (22/10, parere di Codex): proposta accettata, convenzione di uscita contata in
  # anteprima, mai «non dichiarata». Il foglio «Registro modifiche» è il registro intero.
  reg <- p$registro; reg <- if (is.null(reg)) data.frame(Dichiarata = character(0)) else reg
  v(sprintf("%s: ogni modifica fra originale e pulito è dichiarata (%d modifiche, 0 «no»)", nome, nrow(reg)), !any(reg$Dichiarata == "no"))
  v(sprintf("%s: il foglio «Registro modifiche» ha tutte le righe del registro", nome), nrow(lettura_foglio(p$percorso, "Registro modifiche")) == max(1L, nrow(reg)))
  mis(nome, "registro: sì", sum(reg$Dichiarata == "sì")); mis(nome, "registro: convenzione", sum(reg$Dichiarata == "convenzione")); mis(nome, "registro: no", sum(reg$Dichiarata == "no"))
  pul <- lettura(p$percorso)
  v(sprintf("%s: il foglio pulito è nel modello a 28 colonne", nome), identical(names(pul), names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))))
  a2 <- leggi_arianna(p$percorso); pr2 <- proposte_import(a2, NULL, NULL)
  v(sprintf("%s: riletto, ha le stesse righe e 0 correzioni automatiche (neutro sul corretto)", nome), nrow(a2$dati) == p$importati && sum(pr2$celle$consigliata) == 0L)
  # database vuoto: import e riesport tornano uguali
  con <- apri_db(tempfile(fileext = ".sqlite")); e <- importa_arianna(con, a2, modo = "accoda")
  v(sprintf("%s: importato in un database vuoto senza respinte", nome), e$importati == nrow(a2$dati) && nrow(e$respinti) == 0L)
  fondi <- elenco_fondi(con)
  if (nrow(fondi) == 1L) {
    x <- tempfile(fileext = ".xlsx"); esporta_arianna(con, x, fondo = fondi$id[1]); y <- lettura(x)
    o1 <- order(pul[["Segnatura"]]); o2 <- order(y[["Segnatura"]])
    nd <- celle_diverse(pul[o1, , drop = FALSE], y[o2, , drop = FALSE])
    v(sprintf("%s: riesportato dal database è uguale al foglio pulito (celle diverse: %s)", nome, nd), identical(nd, 0L) || nd == 0)
  } else cat("   (", nrow(fondi), "fondi: il riesport cella per cella non si prova)\n")
  DBI::dbDisconnect(con)
}
ora <- do.call(rbind, conta); rownames(ora) <- NULL
if (nzchar(Sys.getenv("TRAMA_TAB_REALI_SALVA"))) {
  utils::write.table(ora, ISTANTANEA, sep = ";", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8"); cat("istantanea riscritta:", ISTANTANEA, "\n")
} else if (file.exists(ISTANTANEA)) {
  prima <- utils::read.csv2(ISTANTANEA, colClasses = c("character", "character", "integer"), fileEncoding = "UTF-8")
  k <- function(d) paste(d$tab, d$metrica, sep = " | ")
  tab_provati <- unique(ora$tab); prima <- prima[prima$tab %in% tab_provati, , drop = FALSE]
  m <- merge(data.frame(k = k(prima), prima = prima$valore), data.frame(k = k(ora), ora = ora$valore), by = "k", all = TRUE)
  diff <- m[is.na(m$prima) | is.na(m$ora) | m$prima != m$ora, , drop = FALSE]
  if (nrow(diff)) { cat("differenze dall'istantanea:\n"); print(diff, row.names = FALSE) }
  v(sprintf("i conteggi sono quelli dell'istantanea (%d misure)", nrow(prima)), nrow(diff) == 0L)
} else v("c'è l'istantanea (TRAMA_TAB_REALI_SALVA=1 per crearla)", FALSE)
if (length(non_letti) && !length(falliti)) cat("SALTATO: tab non letti (", paste(non_letti, collapse = ", "), "): gli altri sono stati controllati\n")
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
