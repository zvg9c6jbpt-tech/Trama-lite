# ─────────────────────────────────────────────────────────────
# verifiche/backup_completo.R: copie di sicurezza e ripristino (2/10)
#
# Copre: lo stato della striscia del backup (stato_backup, elenco_copie),
# la copia al giorno (che ora riporta l'errore invece di tacerlo), le copie
# periodiche (backup_periodico: solo se il database è cambiato, con un minimo
# di tempo, con le ultime N) e la copia completa in zip (database, JSON,
# lista dei luoghi personale, LEGGIMI) con il ripristino su una cartella vuota, su
# una cartella con dati (che si salvano prima) e con zip rotti (che non
# toccano nulla). Lavora su copie temporanee: non apre MAI il database vero
# né ~/Documents/Trama Lite.
#
# Atteso ESITO: ok; altrimenti stampa quale NON TORNA ed esce con 1.
#     Rscript verifiche/backup_completo.R
# ─────────────────────────────────────────────────────────────
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
source("verifiche/aiuti.R")
ko <- 0L
verifica <- function(cond, msg) if (!isTRUE(cond)) { cat("NON TORNA:", msg, "\n"); ko <<- ko + 1L }
Sys.unsetenv("TRAMA_DB"); Sys.unsetenv("TRAMA_HOME")

# Un database vero di partenza (copia) e una lista dei luoghi personale (copia della base).
lavoro <- tempfile("lavoro-"); dir.create(lavoro)
db <- file.path(lavoro, "trama.sqlite"); stopifnot(file.copy(database_di_prova(), db))
con <- apri_db(db)
gaz <- file.path(lavoro, "luoghi"); dir.create(gaz); stopifnot(file.copy(LISTA_LUOGHI_BASE, file.path(gaz, "luoghi.csv")))
n0 <- dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n
verifica(n0 >= 20, "database di partenza troppo piccolo")

# 1. stato della striscia (funzioni pure)
ora <- as.POSIXct("2026-10-02 12:00:00")
copie <- function(...) { q <- as.POSIXct(as.character(c(...)), tz = ""); data.frame(file = character(length(q)), tipo = rep("periodica", length(q)), quando = q, stringsAsFactors = FALSE) }
s <- stato_backup(copie("2026-10-02 11:30:00"), ora)
verifica(s$livello == "ok" && grepl("^Ultima copia", s$testo), "stato ok")
s <- stato_backup(copie("2026-09-30 08:00:00"), ora)
verifica(s$livello == "vecchia" && length(s$parti) == 3, "una copia di due giorni fa è «vecchia»")
s <- stato_backup(copie(), ora)
verifica(s$livello == "nessuna" && grepl("Nessuna copia", s$testo), "nessuna copia")
s <- stato_backup(copie("2026-10-02 11:59:00"), ora, errore = "disco pieno")
verifica(s$livello == "errore" && grepl("disco pieno", s$testo), "l'errore passa davanti a una copia recente")
verifica(grepl("^2 copie conservate", stato_backup(copie("2026-10-02 11:59:00", "2026-10-01 08:00:00"), ora)$dettaglio), "il dettaglio conta le copie")

# 2. tipi di copia dai nomi dei file
cart <- file.path(lavoro, "backup"); dir.create(cart, showWarnings = FALSE)
unlink(list.files(cart, full.names = TRUE))      # la copia fatta dalla migrazione dei fondi (19/10/2026) non è parte di questa prova
for (n in c("regestum-2026-10-01.sqlite", "regestum-2026-10-02-0930.sqlite", "regestum-prima-manuale-20261002-0900-x.sqlite", "regestum-prima-elimina-20261002-0901-y.sqlite", "altro.txt"))
  file.create(file.path(cart, n))
e <- elenco_copie(cart)
verifica(nrow(e) == 4, "elenco_copie: solo i regestum-*.sqlite")
verifica(setequal(e$tipo, c("automatica del giorno", "periodica", "manuale", "prima di un'azione")), "elenco_copie: i quattro tipi")
unlink(cart, recursive = TRUE)

# 3. copia al giorno: riporta l'errore (un percorso che non è una cartella)
blocco <- file.path(lavoro, "non-una-cartella"); writeLines("x", blocco)
r <- backup_automatico(con, cartella = blocco)
verifica(is.null(r$percorso) && !is.null(r$errore) && nzchar(r$errore), "backup_automatico non riporta l'errore")
verifica(!grepl("Could not connect|unable to open", r$errore), "l'errore va detto in italiano, non con il messaggio di SQLite")
verifica(.spiega_errore_copia("database or disk is full") == "il disco è pieno", "errore: disco pieno")
verifica(.spiega_errore_copia("qualcosa di nuovo") == "qualcosa di nuovo", "errore sconosciuto: si riporta com'è")
verifica(grepl("^Una copia conservata", stato_backup(copie("2026-10-02 11:59:00"), ora)$dettaglio), "una sola copia: singolare")
cart <- file.path(lavoro, "backup")
r <- backup_automatico(con, cartella = cart)
verifica(file.exists(r$percorso) && is.null(r$errore) && r$nuovo, "backup_automatico: prima copia del giorno")
r2 <- backup_automatico(con, cartella = cart)
verifica(!r2$nuovo, "backup_automatico: una sola al giorno")

# 4. copie periodiche
ora_reale <- Sys.time()
p <- backup_periodico(con, cartella = cart, ora = ora_reale)
verifica(!p$nuovo, "periodica: nessuna modifica, nessuna copia")
# portiamo indietro l'orario della copia del giorno: «una copia vecchia di un'ora»
Sys.setFileTime(r$percorso, ora_reale - 3600)
invisible(dbExecute(con, "UPDATE fascicoli SET note = note WHERE id = (SELECT id FROM fascicoli LIMIT 1)"))
Sys.setFileTime(db, ora_reale - 60)             # modificato dopo la copia (un'ora fa), un minuto fa
p <- backup_periodico(con, cartella = cart, minimo_minuti = 90, ora = ora_reale)
verifica(!p$nuovo, "periodica: modificato ma l'ultima copia è più recente del minimo")
p <- backup_periodico(con, cartella = cart, minimo_minuti = 30, ora = ora_reale)
verifica(isTRUE(p$nuovo) && file.exists(p$percorso) && grepl("-\\d{4}\\.sqlite$", p$percorso), "periodica: modificato e passato il minimo → copia")
chk <- dbConnect(RSQLite::SQLite(), p$percorso)
verifica(dbGetQuery(chk, "SELECT COUNT(*) n FROM fascicoli")$n == n0, "periodica: la copia ha tutti i fascicoli")
verifica(identical(dbGetQuery(chk, "PRAGMA integrity_check")[[1]], "ok"), "periodica: integrità")
dbDisconnect(chk)
Sys.setFileTime(db, ora_reale - 7200)
Sys.setFileTime(p$percorso, ora_reale - 3600)
p <- backup_periodico(con, cartella = cart, ora = ora_reale + 1)
verifica(!p$nuovo, "periodica: modifica più vecchia dell'ultima copia, nessuna copia")
# conservazione: solo le ultime `tieni`
unlink(list.files(cart, pattern = "^trama-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.sqlite$", full.names = TRUE))
for (k in 1:5) {
  Sys.setFileTime(db, ora_reale - 600 * (6 - k) + 1)
  dest <- file.path(cart, sprintf("trama-2026-09-2%d-1200.sqlite", k)); file.copy(db, dest); Sys.setFileTime(dest, ora_reale - 7200 - 600 * (6 - k))
}
Sys.setFileTime(db, ora_reale - 5)
p <- backup_periodico(con, cartella = cart, tieni = 3, ora = ora_reale)
per <- list.files(cart, pattern = "^trama-\\d{4}-\\d{2}-\\d{2}-\\d{4}\\.sqlite$")
verifica(isTRUE(p$nuovo) && length(per) == 3, sprintf("periodica: tieni 3 (restano %d)", length(per)))
verifica(basename(p$percorso) %in% per, "periodica: la più recente resta")
verifica(file.exists(r$percorso), "periodica: la pulizia non tocca la copia del giorno")
dbDisconnect(con); con <- apri_db(db)

# 5. copia completa
zipf <- file.path(lavoro, "completa.zip")
b <- backup_completo(con, zipf, cart_luoghi = gaz)
verifica(file.exists(zipf) && b$fascicoli == n0 && b$luoghi, "backup_completo: creato")
verifica(setequal(b$file, c("trama.sqlite", "trama-backup.json", "luoghi/luoghi.csv", "LEGGIMI.txt")), "backup_completo: i quattro file")
info <- leggi_backup_completo(zipf)
verifica(info$fascicoli == n0 && info$luoghi && info$json, "leggi_backup_completo")
j <- jsonlite::fromJSON(unzip_json <- { t <- tempfile(); dir.create(t); zip::unzip(zipf, files = "trama-backup.json", exdir = t); file.path(t, "trama-backup.json") }, simplifyVector = FALSE)
verifica(length(j$fascicoli) == n0, "backup_completo: il JSON ha tutti i fascicoli")
verifica(identical(j$formato, "trama-backup"), "backup_completo: il formato del JSON è «trama-backup»")
# una copia fatta prima del 31/10/2026 dice ancora «datatrama-backup»: si legge lo stesso
f_vecchio <- file.path(lavoro, "copia-vecchia.json")
writeLines(jsonlite::toJSON(modifyList(j, list(formato = "datatrama-backup")), auto_unbox = TRUE, null = "null", na = "null"), f_vecchio, useBytes = TRUE)
verifica(!inherits(try(leggi_backup_json(f_vecchio), silent = TRUE), "try-error"), "backup_completo: un JSON col nome di prima si legge ancora")
f_strano <- file.path(lavoro, "copia-strana.json")
writeLines(jsonlite::toJSON(modifyList(j, list(formato = "altro")), auto_unbox = TRUE, null = "null", na = "null"), f_strano, useBytes = TRUE)
verifica(inherits(try(leggi_backup_json(f_strano), silent = TRUE), "try-error"), "backup_completo: un formato sconosciuto si rifiuta")

# 6. ripristino su una cartella vuota: tutto identico
nuova <- file.path(lavoro, "nuova"); dir.create(nuova)
rp <- ripristina_backup_completo(zipf, dove = nuova)
verifica(length(rp$copie_di_prima) == 0, "ripristino su vuoto: niente da salvare")
c2 <- dbConnect(RSQLite::SQLite(), file.path(nuova, "trama.sqlite"))
uguali <- vapply(dbListTables(con), function(t) isTRUE(all.equal(dbReadTable(con, t), dbReadTable(c2, t), check.attributes = FALSE)), logical(1))
verifica(all(uguali), paste("ripristino: tabelle diverse:", paste(names(uguali)[!uguali], collapse = ", ")))
verifica(setequal(dbListTables(con), dbListTables(c2)), "ripristino: stesse tabelle")
dbDisconnect(c2)
verifica(identical(unname(tools::md5sum(file.path(nuova, "luoghi", "luoghi.csv"))), unname(tools::md5sum(file.path(gaz, "luoghi.csv")))), "ripristino: lista dei luoghi identica")

# 7. ripristino su una cartella con dati: si salvano prima
altra <- file.path(lavoro, "altra"); dir.create(file.path(altra, "luoghi"), recursive = TRUE)
writeLines("db-vecchio", file.path(altra, "trama.sqlite")); writeLines("luoghi-vecchi", file.path(altra, "luoghi", "luoghi.csv"))
rp <- ripristina_backup_completo(zipf, dove = altra)
verifica(length(rp$copie_di_prima) == 2 && all(file.exists(rp$copie_di_prima)), "ripristino su dati: copie di prima")
verifica(any(readLines(rp$copie_di_prima[1]) == "db-vecchio" | readLines(rp$copie_di_prima[2]) == "db-vecchio"), "ripristino su dati: la copia di prima è il vecchio database")
verifica(identical(unname(tools::md5sum(file.path(altra, "luoghi", "luoghi.csv"))), unname(tools::md5sum(file.path(gaz, "luoghi.csv")))), "ripristino su dati: lista dei luoghi sostituita")
c3 <- dbConnect(RSQLite::SQLite(), file.path(altra, "trama.sqlite"))
verifica(dbGetQuery(c3, "SELECT COUNT(*) n FROM fascicoli")$n == n0, "ripristino su dati: database sostituito")
dbDisconnect(c3)

# 8. zip che non vanno: errore chiaro, cartella intatta
intatta <- file.path(lavoro, "intatta"); dir.create(intatta); writeLines("com'era", file.path(intatta, "trama.sqlite"))
rotto <- file.path(lavoro, "rotto.zip"); writeBin(as.raw(sample(0:255, 500, replace = TRUE)), rotto)
e1 <- tryCatch({ ripristina_backup_completo(rotto, dove = intatta); "" }, error = function(e) conditionMessage(e))
verifica(nzchar(e1), "zip rotto: deve dare errore")
verifica(identical(readLines(file.path(intatta, "trama.sqlite")), "com'era"), "zip rotto: la cartella non va toccata")
senza <- file.path(lavoro, "senza.zip"); t <- tempfile(); dir.create(t); writeLines("x", file.path(t, "altro.txt")); zip::zipr(senza, "altro.txt", root = t)
e2 <- tryCatch({ ripristina_backup_completo(senza, dove = intatta); "" }, error = function(e) conditionMessage(e))
verifica(grepl("manca trama.sqlite", e2), "zip senza database: messaggio chiaro")
falso <- file.path(lavoro, "falso.zip"); t <- tempfile(); dir.create(t); writeLines("non sono un database", file.path(t, "trama.sqlite")); zip::zipr(falso, "trama.sqlite", root = t)
e3 <- tryCatch({ ripristina_backup_completo(falso, dove = intatta); "" }, error = function(e) conditionMessage(e))
verifica(nzchar(e3), "database finto nello zip: deve dare errore")
verifica(identical(readLines(file.path(intatta, "trama.sqlite")), "com'era"), "database finto: la cartella non va toccata")
verifica(nzchar(tryCatch({ leggi_backup_completo(file.path(lavoro, "non-esiste.zip")); "" }, error = function(e) conditionMessage(e))), "zip inesistente: errore")


# 14/10: le copie «prima di un'azione» ruotano (ultime 20); manuali, giornaliere e quelle prima di una cancellazione restano
rot <- tempfile("rot-"); dir.create(rot)
for (i in 1:25) { f <- file.path(rot, sprintf("trama-prima-correzione-%02d.sqlite", i)); writeLines("x", f); Sys.setFileTime(f, Sys.time() - (30 - i) * 3600) }
for (n in c("trama-prima-manuale-1.sqlite", "trama-prima-eliminazione-fondo-1.sqlite", "trama-prima-sostituzione-1.sqlite", "trama-2026-10-01.sqlite")) {
  f <- file.path(rot, n); writeLines("x", f); Sys.setFileTime(f, Sys.time() - 900 * 3600) }
verifica(ruota_copie_prima(rot, tieni = 20L) == 5L, "ruota_copie_prima: toglie le 5 più vecchie su 25")
resto <- list.files(rot)
verifica(sum(grepl("prima-correzione", resto)) == 20L && all(sprintf("trama-prima-correzione-%02d.sqlite", 6:25) %in% resto), "ruota_copie_prima: restano le 20 più recenti")
verifica(all(c("trama-prima-manuale-1.sqlite", "trama-prima-eliminazione-fondo-1.sqlite", "trama-prima-sostituzione-1.sqlite", "trama-2026-10-01.sqlite") %in% resto), "ruota_copie_prima: manuali, cancellazioni e giornaliere non si toccano")
unlink(rot, recursive = TRUE)
dbDisconnect(con); unlink(lavoro, recursive = TRUE)
cat(if (ko == 0L) "ESITO: ok\n" else sprintf("ESITO: %d NON TORNA\n", ko))
if (ko) quit(status = 1)
