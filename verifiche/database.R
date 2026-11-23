# ─────────────────────────────────────────────────────────────
# verifiche/database.R: la copia del database in SQL e SQLite, elimina un fondo, svuota tutto (7/10)
# Copia in SQLite (tutto e di un fondo solo), SQL che si ricarica con sqlite3 e rende le stesse tabelle
# (apici, a capo e «;» dentro i testi compresi), licenza e data nel file, eliminazione di un fondo, svuotamento
# (le impostazioni che restano), ritorno dal backup JSON. Lavora su database temporanei.
#
#     Rscript verifiche/database.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny)
  for (f in c("percorsi", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo", "analisi", "qualita",
              "attribuzione", "ricerca", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "colonne", "lista_luoghi", "luoghi", "fusioni", "luoghi_citati", "elenco",
              "backup_electron", "backup"))
    source(file.path("R", paste0(f, ".R")))
})
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else {
    .esiti$falliti <- c(.esiti$falliti, nome)
    cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto)
  }
}

con <- apri_db(tempfile(fileext = ".sqlite"))
nuova <- function(fondo, busta, num, persone, reati = "Magia", note = NA_character_) {
  f <- fascicolo_vuoto(); f$fondo <- fondo; f$archivio <- paste("Archivio di", fondo); f$busta <- busta; f$fascicolo <- num
  f$tipologia_documentaria <- "Processi"; f$persone <- persone; f$reati <- reati; f$estremo_remoto <- 1600L
  f$lingue <- c("Italiano", "Latino"); f$luoghi <- "Capua"
  if (!is.na(note)) f$note <- note
  salva_fascicolo(con, f)
}
dura <- "L'inquisitore disse: «non è vero»; poi\nsi corresse. Altro; \nrigo 'tra apici' e un backslash \\ e %s"
nuova("Alfa", 1L, 1L, c("Rossi, Mario", "de Lama, Anna"), note = dura)
nuova("Alfa", 1L, 2L, "Bianchi, Luca", "Eresia")
nuova("Beta", 1L, 1L, c("Verdi, Paolo", "Rossi, Mario"))
nuova("Beta", 2L, 1L, "Neri, Carla")
invisible(dbExecute(con, "INSERT INTO schede_persona (id, forma_canonica, note, creato, modificato) VALUES ('p1', 'Rossi, Mario', 'nota', 'x', 'x'), ('p2', 'Verdi, Paolo', '', 'x', 'x'), ('p3', 'Mai citato, Tizio', '', 'x', 'x')"))
id_alfa <- fondo_id_da(con, "Archivio di Alfa", "Alfa"); id_beta <- fondo_id_da(con, "Archivio di Beta", "Beta")
invisible(dbExecute(con, "INSERT INTO piani_busta (fondo_id, busta, primo, ultimo, creato) VALUES (?, 1, 1, 5, 'x'), (?, 2, 1, 3, 'x')", list(id_alfa, id_beta)))
invisible(dbExecute(con, "UPDATE fondi SET dichiarato = 1"))
invisible(dbExecute(con, "INSERT INTO coorti (nome, query, creato) VALUES ('magia', 'reato:magia', 'x')"))
registra_valore(con, "Alfa", "reato", "Contrabbando"); registra_valore(con, "Beta", "lingua", "Tedesco")
scrivi_impostazione(con, "attribuzione_schedatore", "Stefano Colavecchia"); scrivi_impostazione(con, "attribuzione_progetto", "Digital Inquisition")
scrivi_impostazione(con, "fondo_attivo", id_alfa)
TABELLE <- setdiff(dbListTables(con), c("storia_fascicoli", "impostazioni", "sqlite_sequence"))
contenuto <- function(db, t) { d <- dbGetQuery(db, sprintf('SELECT * FROM "%s"', t)); if (!nrow(d)) return(character(0)); sort(do.call(paste, c(d, sep = "\r"))) }
uguali <- function(a, b) all(vapply(TABELLE, function(t) identical(contenuto(a, t), contenuto(b, t)), TRUE))

# --- SQLite -------------------------------------------------------------------
p <- tempfile(fileext = ".sqlite"); esporta_sqlite(con, p)
x <- dbConnect(RSQLite::SQLite(), p)
atteso("sqlite: stesse tabelle e stessi dati", uguali(con, x), TRUE)
atteso("sqlite: integrità", dbGetQuery(x, "PRAGMA integrity_check")[[1]], "ok")
atteso("sqlite: la storia delle schede non c'è", dbGetQuery(x, "SELECT COUNT(*) n FROM storia_fascicoli")$n, 0L)
atteso("sqlite: la licenza e l'attribuzione viaggiano nel file",
       c(grepl("CC BY", leggi_impostazione(x, "licenza_dati")), grepl("Digital Inquisition", leggi_impostazione(x, "da_citare"))), c(TRUE, TRUE))
atteso("sqlite: un solo file, niente -wal", file.exists(paste0(p, "-wal")), FALSE)
dbDisconnect(x)
atteso("sqlite: il database vero non cambia", dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n, 4L)

# --- SQL ------------------------------------------------------------------------
q <- tempfile(fileext = ".sql"); esporta_sql(con, q)
testa <- readLines(q, n = 6, encoding = "UTF-8")
atteso("sql: testata con data, fondi e licenza", c(grepl("tutti i fondi", testa[2]), grepl("CC BY", testa[3]), grepl("Digital Inquisition", testa[4])), c(TRUE, TRUE, TRUE))
if (nzchar(Sys.which("sqlite3"))) {
  nuovo <- tempfile(fileext = ".sqlite")
  stato <- system2("sqlite3", shQuote(nuovo), stdin = q, stdout = FALSE, stderr = FALSE)
  atteso("sql: si carica con sqlite3", stato, 0L)
  y <- dbConnect(RSQLite::SQLite(), nuovo)
  atteso("sql: stesse tabelle e stessi dati", uguali(con, y), TRUE)
  atteso("sql: apici, a capo, punti e virgola e backslash nei testi", dbGetQuery(y, "SELECT note FROM fascicoli WHERE note IS NOT NULL")$note, dura)
  atteso("sql: gli indici ci sono", nrow(dbGetQuery(y, "SELECT name FROM sqlite_master WHERE type = 'index' AND name LIKE 'idx_%'")) >= 5L, TRUE)
  atteso("sql: i collegamenti reggono", nrow(dbGetQuery(y, "PRAGMA foreign_key_check")), 0L)
  dbDisconnect(y)
} else cat("(sqlite3 non c'è: la prova di ricarica dell'SQL è saltata)\n")

# --- di un fondo solo ----------------------------------------------------------
p2 <- tempfile(fileext = ".sqlite"); esporta_sqlite(con, p2, fondo = "Alfa")
z <- dbConnect(RSQLite::SQLite(), p2)
atteso("fondo: solo le schede di Alfa", dbGetQuery(z, "SELECT DISTINCT fondo FROM fascicoli")$fondo, "Alfa")
atteso("fondo: archivio, piano e vocabolario di Beta non ci sono",
       c(dbGetQuery(z, "SELECT COUNT(*) n FROM fondi WHERE denominazione = 'Beta'")$n, dbGetQuery(z, "SELECT COUNT(*) n FROM piani_busta WHERE fondo_id IN (SELECT id FROM fondi WHERE denominazione = 'Beta')")$n,
         dbGetQuery(z, "SELECT COUNT(*) n FROM vocabolario_fondo WHERE fondo_id IN (SELECT id FROM fondi WHERE denominazione = 'Beta')")$n), c(0L, 0L, 0L))
atteso("fondo: le schede persona che non lo toccano non ci sono", sort(dbGetQuery(z, "SELECT forma_canonica FROM schede_persona")$forma_canonica), "Rossi, Mario")
atteso("fondo: ricerche salvate fuori", dbGetQuery(z, "SELECT COUNT(*) n FROM coorti")$n, 0L)
atteso("fondo: la copia di Alfa dice di chi è", leggi_impostazione(z, "copia_di"), "Alfa · Archivio di Alfa")
dbDisconnect(z)
q2 <- tempfile(fileext = ".sql"); esporta_sql(con, q2, fondo = "Beta")
atteso("fondo: la testata dice il fondo", grepl("solo il fondo «Beta · Archivio di Beta»", readLines(q2, n = 3, encoding = "UTF-8")[2], fixed = TRUE), TRUE)
atteso("fondo: nell'SQL non c'è niente di Alfa", any(grepl("Alfa", readLines(q2, encoding = "UTF-8"), fixed = TRUE)), FALSE)
atteso("export di un fondo che non c'è: errore chiaro", inherits(try(esporta_sql(con, tempfile(), fondo = ""), silent = TRUE), "try-error"), TRUE)

# --- che cosa sparirebbe --------------------------------------------------------
r <- riepilogo_cancellazione(con)
atteso("riepilogo: tutto", unlist(r[c("fondi", "fascicoli", "persone", "piani", "ricerche")]), c(fondi = 2L, fascicoli = 4L, persone = 3L, piani = 2L, ricerche = 1L))
atteso("riepilogo: un fondo", unlist(riepilogo_cancellazione(con, "Alfa")[c("fascicoli", "piani")]), c(fascicoli = 2L, piani = 1L))

# --- elimina un fondo -----------------------------------------------------------
json <- tempfile(fileext = ".json"); esporta_backup_json(con, json)
cartella <- tempfile("sicurezza-"); copia <- backup_prima_ripristino(con, cartella, "eliminazione-fondo")
atteso("elimina fondo: la copia di sicurezza c'è", file.exists(copia), TRUE)
e <- elimina_fondo(con, "Alfa")
atteso("elimina fondo: due schede in meno", e$fascicoli, 2L)
atteso("elimina fondo: Beta resta com'era", dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE fondo = 'Beta'")$n, 2L)
atteso("elimina fondo: archivio, piano e decisioni di Alfa spariti",
       c(dbGetQuery(con, "SELECT COUNT(*) n FROM fondi WHERE denominazione = 'Alfa'")$n, dbGetQuery(con, "SELECT COUNT(*) n FROM piani_busta WHERE fondo_id IN (SELECT id FROM fondi WHERE denominazione = 'Alfa')")$n,
         dbGetQuery(con, "SELECT COUNT(*) n FROM vocabolario_fondo WHERE fondo_id IN (SELECT id FROM fondi WHERE denominazione = 'Alfa')")$n), c(0L, 0L, 0L))
atteso("elimina fondo: le tabelle figlie seguono", dbGetQuery(con, "SELECT COUNT(*) n FROM reati WHERE fascicolo_id NOT IN (SELECT id FROM fascicoli)")$n, 0L)
atteso("elimina fondo: il vocabolario di Beta resta", dbGetQuery(con, "SELECT COUNT(*) n FROM vocabolario_fondo WHERE fondo_id IN (SELECT id FROM fondi WHERE denominazione = 'Beta')")$n, 1L)
atteso("elimina fondo: senza nome, errore", inherits(try(elimina_fondo(con, ""), silent = TRUE), "try-error"), TRUE)

# --- svuota tutto ----------------------------------------------------------------
v <- svuota_database(con)
atteso("svuota: nessuna scheda", v$fascicoli, 2L)
conta <- function(t) dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s", t))$n
atteso("svuota: tutto a zero", vapply(c("fascicoli", "reati", "persone", "luoghi", "lingue", "schede_persona", "piani_busta", "fondi", "coorti", "vocabolario_fondo", "omonimie_verificate"), conta, 1L),
       stats::setNames(rep(0L, 11), c("fascicoli", "reati", "persone", "luoghi", "lingue", "schede_persona", "piani_busta", "fondi", "coorti", "vocabolario_fondo", "omonimie_verificate")))
atteso("svuota: l'attribuzione resta, il fondo di lavoro no",
       c(leggi_impostazione(con, "attribuzione_schedatore"), is.null(leggi_impostazione(con, "fondo_attivo"))), c("Stefano Colavecchia", "TRUE"))
atteso("svuota: integrità", dbGetQuery(con, "PRAGMA integrity_check")[[1]], "ok")
invisible(svuota_database(con, tieni_impostazioni = FALSE))
atteso("svuota senza tenere le impostazioni: vuote", conta("impostazioni"), 0L)

# --- la strada del ritorno: dal backup JSON fatto prima ------------------------------
invisible(ripristina_backup_json(con, json, "accoda"))
atteso("ritorno: le quattro schede tornano dal JSON", conta("fascicoli"), 4L)
atteso("ritorno: i due fondi con le loro decisioni", c(conta("fondi"), conta("vocabolario_fondo")), c(2L, 2L))
dbDisconnect(con)

cat(sprintf("\nverifiche: %d passate, %d fallite\n", .esiti$ok, length(.esiti$falliti)))
if (length(.esiti$falliti)) { cat("ESITO: NON TORNA\n"); quit(status = 1) }
cat("ESITO: ok\n")
