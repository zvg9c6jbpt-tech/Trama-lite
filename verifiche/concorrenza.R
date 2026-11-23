# ──────────────────────────────────────────────
# verifiche/concorrenza.R: due connessioni sullo stesso database (13/10)
# Trama è pensata per una persona, ma due finestre o due istanze possono scrivere insieme:
# nessuno deve vedere «database is locked» né perdere una scheda, e un backup fatto mentre
# un'altra connessione scrive deve restare leggibile. Lavora su una copia temporanea.
#     Rscript verifiche/concorrenza.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
suppressMessages({ library(DBI); library(RSQLite) })
Sys.setenv(TRAMA_COLLAUDO = "1")
for (f in c("percorsi", "schema")) source(file.path("R", paste0(f, ".R")))
falliti <- character(); v <- function(nome, ok) { cat(if (ok) "ok  " else "NO  ", nome, "\n"); if (!ok) falliti <<- c(falliti, nome) }
db <- tempfile(fileext = ".sqlite")
a <- apri_db(db); b <- apri_db(db)
v("il journal NON è WAL (le copie copiano il solo file .sqlite)", !identical(tolower(dbGetQuery(a, "PRAGMA journal_mode")[[1]]), "wal"))
v("c'è un'attesa prima di dire «locked» (busy_timeout)", dbGetQuery(a, "PRAGMA busy_timeout")[[1]] >= 3000)
ins <- function(con, n) dbExecute(con, "INSERT INTO fascicoli (archivio, fondo, busta, fascicolo, titolo, creato, modificato) VALUES ('A', ?, ?, ?, 'x', 'now', 'now')", params = list("Prova", n, 1L))
# A tiene aperta una scrittura; B ne prova un'altra: deve attendere o riuscire, non cadere subito
dbExecute(a, "BEGIN IMMEDIATE"); ins(a, 1L)
r <- system.time(e <- tryCatch({ later <- FALSE; dbExecute(b, "BEGIN IMMEDIATE"); "ok" }, error = function(e) conditionMessage(e)))
dbExecute(a, "COMMIT")
v("una seconda scrittura contemporanea dà «locked» solo dopo avere atteso", !grepl("locked", e) || r[["elapsed"]] >= 2.5)
if (identical(e, "ok")) dbExecute(b, "ROLLBACK")
# letture durante una scrittura aperta
dbExecute(a, "BEGIN IMMEDIATE"); ins(a, 2L)
n <- tryCatch(dbGetQuery(b, "SELECT count(*) n FROM fascicoli")$n, error = function(e) NA)
dbExecute(a, "COMMIT")
v("B legge mentre A scrive (e vede solo il già salvato)", identical(n, 1L))
# scritture una dopo l'altra: nessuna perduta
for (i in 3:6) { ins(a, i); ins(b, i + 10L) }
v("le scritture di entrambe le connessioni ci sono tutte", dbGetQuery(a, "SELECT count(*) n FROM fascicoli")$n == 10L)
# copia di sicurezza mentre B scrive
dest <- tempfile(fileext = ".sqlite")
dbExecute(b, "BEGIN IMMEDIATE"); ins(b, 99L)
ok <- tryCatch({ dbExecute(a, "VACUUM INTO ?", params = list(dest)); TRUE }, error = function(e) FALSE)
dbExecute(b, "COMMIT")
cp <- tryCatch({ z <- dbConnect(SQLite(), dest); on.exit(dbDisconnect(z)); dbGetQuery(z, "PRAGMA integrity_check")[[1]] }, error = function(e) "errore")
v("una copia fatta durante la scrittura di un'altra connessione è integra o dice chiaramente di no", !ok || identical(cp, "ok"))
dbDisconnect(a); dbDisconnect(b)
cat(if (length(falliti)) "ESITO: NON TORNA\n" else "ESITO: ok\n"); if (length(falliti)) quit(status = 1)
