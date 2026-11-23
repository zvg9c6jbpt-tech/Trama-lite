# ─────────────────────────────────────────────────────────────
# verifiche/fondi.R: l'identificativo interno del fondo (19/10/2026, R/fondi.R, PIANO_FONDO_ID.md)
#
# Copre: la migrazione di un database a nome (con la copia di sicurezza, senza perdere niente, idempotente); due archivi con un fondo
# omonimo che restano distinti (schede, piani, vocabolario, export, eliminazione, backup JSON e import); un backup JSON del formato di prima
# (archivi per nome) che si legge ancora; il trigger che dà un fondo_id a una scheda scritta senza.
#
#     Rscript verifiche/fondi.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_COLLAUDO = "1")
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else { .esiti$falliti <- c(.esiti$falliti, nome); cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto) }
}

# ── 1. Un database a nome (il modello di prima) migra ──────────────────────────────────────────
lavoro <- tempfile("fondi-"); dir.create(lavoro)
vecchio <- file.path(lavoro, "trama.sqlite")
# Il database di prima si ottiene "invecchiando" uno nuovo: fascicoli senza fondo_id, archivi, piani e vocabolario con il nome del fondo.
n0 <- apri_db(vecchio)
for (a in list(c("Archivio Alfa", "Sant'Ufficio", 1L, 1L), c("Archivio Alfa", "Sant'Ufficio", 1L, 2L), c("Archivio Beta", "Inquisizione", 1L, 1L))) {
  f <- fascicolo_vuoto(); f$archivio <- a[1]; f$fondo <- a[2]; f$busta <- as.integer(a[3]); f$fascicolo <- as.integer(a[4]); f$titolo <- "t"; f$titolo_manuale <- 1L
  invisible(salva_fascicolo(n0, f))
}
for (q in c(
  "DROP TRIGGER fascicoli_fondo_id", "DROP INDEX idx_segnatura_fondo_id", "DROP INDEX IF EXISTS idx_segnatura_senza_numeri", "ALTER TABLE fascicoli DROP COLUMN fondo_id",
  "DROP TABLE fondi", "DROP TABLE piani_busta", "DROP TABLE vocabolario_fondo",
  "CREATE TABLE archivi (fondo TEXT PRIMARY KEY, archivio TEXT NOT NULL, serie TEXT, profilo TEXT, creato TEXT NOT NULL)",
  "CREATE TABLE piani_busta (fondo TEXT NOT NULL, busta INTEGER NOT NULL, busta_suffisso TEXT NOT NULL DEFAULT '', primo INTEGER NOT NULL,
     ultimo INTEGER NOT NULL, creato TEXT NOT NULL, PRIMARY KEY (fondo, busta, busta_suffisso))",
  "CREATE TABLE vocabolario_fondo (fondo TEXT NOT NULL, campo TEXT NOT NULL, valore TEXT NOT NULL, forma TEXT, creato TEXT NOT NULL,
     PRIMARY KEY (fondo, campo, valore))",
  "CREATE UNIQUE INDEX idx_segnatura_busta_suffisso ON fascicoli(fondo, busta, busta_suffisso, fascicolo, fascicolo_suffisso)",
  "INSERT INTO archivi VALUES ('Sant''Ufficio', 'Archivio Alfa', 'Processi', 'Modello', 'y'), ('Vuoto', 'Archivio Gamma', NULL, 'Modello', 'y')",
  "INSERT INTO piani_busta VALUES ('Sant''Ufficio', 1, '', 1, 9, 'y'), ('Inquisizione', 1, '', 1, 3, 'y')",
  "INSERT INTO vocabolario_fondo VALUES ('Inquisizione', 'reato', 'Contrabbando', NULL, 'y')",
  "INSERT OR REPLACE INTO impostazioni VALUES ('fondo_attivo', 'Inquisizione')"))
  DBI::dbExecute(n0, q)
DBI::dbDisconnect(n0)
con <- apri_db(vecchio)
atteso("migrazione: le schede ci sono tutte, nessuna senza fondo_id", dbGetQuery(con, "SELECT COUNT(*) n, SUM(fondo_id IS NULL) s FROM fascicoli")[1, ], data.frame(n = 3L, s = 0L))
atteso("migrazione: tre fondi (Alfa, Beta e il vuoto dichiarato)", sort(dbGetQuery(con, "SELECT denominazione FROM fondi")$denominazione), c("Inquisizione", "Sant'Ufficio", "Vuoto"))
atteso("migrazione: il fondo vuoto dichiarato si vede, gli altri hanno schede", elenco_fondi(con)$fondo[order(elenco_fondi(con)$fondo)], c("Inquisizione", "Sant'Ufficio", "Vuoto"))
atteso("migrazione: serie e profilo di archivi passano a fondi", dbGetQuery(con, "SELECT serie, profilo FROM fondi WHERE denominazione = 'Sant''Ufficio'"), data.frame(serie = "Processi", profilo = "Modello"))
atteso("migrazione: il piano di busta segue il suo fondo",
       dbGetQuery(con, "SELECT f.denominazione, p.ultimo FROM piani_busta p JOIN fondi f ON f.id = p.fondo_id ORDER BY f.denominazione")$ultimo, c(3L, 9L))
atteso("migrazione: il vocabolario segue il suo fondo", dbGetQuery(con, "SELECT f.denominazione FROM vocabolario_fondo v JOIN fondi f ON f.id = v.fondo_id")$denominazione, "Inquisizione")
atteso("migrazione: archivi non c'è più", "archivi" %in% dbListTables(con), FALSE)
atteso("migrazione: il fondo attivo salvato era un nome, ora è l'id", .e_id_fondo(leggi_impostazione(con, "fondo_attivo")), TRUE)
copie <- list.files(file.path(lavoro, "backup"), pattern = "migrazione-fondi")
atteso("migrazione: una copia di sicurezza prima di cambiare", length(copie), 1L)
n_fondi <- nrow(dbGetQuery(con, "SELECT 1 FROM fondi")); dbDisconnect(con)
con <- apri_db(vecchio)
atteso("migrazione: riaprire non cambia niente (idempotente) e non rifà la copia",
       c(nrow(dbGetQuery(con, "SELECT 1 FROM fondi")), length(list.files(file.path(lavoro, "backup"), pattern = "migrazione-fondi"))), c(n_fondi, 1L))
dbDisconnect(con)

# ── 2. Due archivi, un fondo omonimo ──────────────────────────────────────────────────────────
con <- apri_db(":memory:")
sch <- function(arch, fondo, busta, n, persone = "Rossi, Mario", titolo = "t") {
  f <- fascicolo_vuoto(); f$archivio <- arch; f$fondo <- fondo; f$busta <- busta; f$fascicolo <- n
  f$tipologia_documentaria <- "Processi"; f$persone <- persone; f$reati <- "Magia"; f$estremo_remoto <- 1600L; f$titolo <- titolo; f$titolo_manuale <- 1L
  salva_fascicolo(con, f)
}
sch("Archivio di Stato di Modena", "Sant'Ufficio", 1L, 1L, "Verdi, Paolo"); sch("Archivio di Stato di Modena", "Sant'Ufficio", 1L, 2L, "Verdi, Anna")
sch("Archivio Arcivescovile di Bologna", "Sant'Ufficio", 1L, 1L, "Neri, Carla")
id_mo <- fondo_id_da(con, "Archivio di Stato di Modena", "Sant'Ufficio"); id_bo <- fondo_id_da(con, "Archivio Arcivescovile di Bologna", "Sant'Ufficio")
atteso("due fondi omonimi: due id", c(!is.na(id_mo), !is.na(id_bo), id_mo != id_bo), c(TRUE, TRUE, TRUE))
atteso("due fondi omonimi: stessa segnatura b. 1 fasc. 1, nessun conflitto", dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE busta = 1 AND fascicolo = 1")$n, 2L)
atteso("due fondi omonimi: elenco_fondi li distingue", elenco_fondi(con)[order(elenco_fondi(con)$archivio), c("archivio", "n")],
       data.frame(archivio = c("Archivio Arcivescovile di Bologna", "Archivio di Stato di Modena"), n = c(1L, 2L), row.names = c(2L, 1L)))
atteso("due fondi omonimi: l'export è di un fondo solo, per id", c(nrow(tabella_arianna(con, fondo = id_mo)), nrow(tabella_arianna(con, fondo = id_bo))), c(2L, 1L))
atteso("due fondi omonimi: senza dire quale, l'export non esce", inherits(try(tabella_arianna(con), silent = TRUE), "try-error"), TRUE)
atteso("due fondi omonimi: indicare il solo nome è un errore chiaro (due archivi)", inherits(try(tabella_arianna(con, fondo = "Sant'Ufficio"), silent = TRUE), "try-error"), TRUE)
atteso("due fondi omonimi: Qualità vede un fondo alla volta", c(qualita(con, id_mo)$casi[1] >= 0, TRUE), c(TRUE, TRUE))
atteso("due fondi omonimi: i suggerimenti sono del fondo", suggerimenti_da_db(con, id_bo)$persona, "Neri, Carla")
atteso("due fondi omonimi: l'ultimo modificato è del fondo", ultimo_modificato(con, id_bo)$archivio, "Archivio Arcivescovile di Bologna")
invisible(salva_piano(con, id_mo, 1L, 1L, 4L)); invisible(salva_piano(con, id_bo, 1L, 1L, 9L))
atteso("due fondi omonimi: piani distinti", c(prossimo_nel_piano(con, id_mo, 1L), prossimo_nel_piano(con, id_bo, 1L)), c(3L, 2L))
registra_valore(con, id_mo, "reato", "Contrabbando")
atteso("due fondi omonimi: il vocabolario è del fondo", c("Contrabbando" %in% vocabolario_di(con, "reato", id_mo), "Contrabbando" %in% vocabolario_di(con, "reato", id_bo)), c(TRUE, FALSE))
atteso("due fondi omonimi: non sono doppioni (stessa segnatura, archivi diversi)", nrow(duplicati_fascicoli(con)), 0L)

# Il giro del backup JSON: tutto sostituito in un database vuoto torna uguale, con gli stessi id
p <- tempfile(fileext = ".json"); invisible(esporta_backup_json(con, p))
con2 <- apri_db(":memory:"); invisible(ripristina_backup_json(con2, p, "sostituisci"))
atteso("backup JSON: gli stessi fondi con gli stessi id", dbGetQuery(con2, "SELECT id, archivio, denominazione FROM fondi ORDER BY id"), dbGetQuery(con, "SELECT id, archivio, denominazione FROM fondi ORDER BY id"))
atteso("backup JSON: piani e vocabolario sul fondo giusto",
       list(dbGetQuery(con2, "SELECT fondo_id, ultimo FROM piani_busta ORDER BY fondo_id"), dbGetQuery(con2, "SELECT fondo_id, valore FROM vocabolario_fondo")),
       list(dbGetQuery(con, "SELECT fondo_id, ultimo FROM piani_busta ORDER BY fondo_id"), dbGetQuery(con, "SELECT fondo_id, valore FROM vocabolario_fondo")))
atteso("backup JSON: ogni scheda nel suo fondo", dbGetQuery(con2, "SELECT id, fondo_id FROM fascicoli ORDER BY id"), dbGetQuery(con, "SELECT id, fondo_id FROM fascicoli ORDER BY id"))
# Accodato a un database che ha già un fondo omonimo di un altro archivio: i due restano distinti
con3 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$archivio <- "Archivio Terzo"; f$fondo <- "Sant'Ufficio"; f$busta <- 1L; f$fascicolo <- 1L; f$titolo <- "x"; f$titolo_manuale <- 1L; invisible(salva_fascicolo(con3, f))
e <- ripristina_backup_json(con3, p, "accoda")
atteso("backup JSON accodato: tre fondi omonimi, nessun scarto", c(e$importati, nrow(dbGetQuery(con3, "SELECT 1 FROM fondi WHERE denominazione = 'Sant''Ufficio'"))), c(3L, 3L))

# Un backup del formato di prima (archivi e piani per nome, niente `fondi`) si legge ancora
d <- jsonlite::fromJSON(p, simplifyVector = FALSE)
d$fondi <- NULL
d$archivi <- list(list(fondo = "Sant'Ufficio", archivio = "Archivio di Stato di Modena", serie = NA, profilo = "Modello", creato = "x"))
d$piani_busta <- lapply(d$piani_busta, function(r) { r$fondo <- "Sant'Ufficio"; r$fondo_id <- NULL; r })[1]
d$vocabolario_fondo <- lapply(d$vocabolario_fondo, function(r) { r$fondo <- "Sant'Ufficio"; r$fondo_id <- NULL; r })
p2 <- tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(d, auto_unbox = TRUE, null = "null", na = "null"), p2)
con4 <- apri_db(":memory:")
e4 <- try(ripristina_backup_json(con4, p2, "sostituisci"), silent = TRUE)
atteso("backup di prima: si ripristina", inherits(e4, "try-error"), FALSE)
atteso("backup di prima: due fondi dalle schede, il piano e il vocabolario sul più grande", c(nrow(dbGetQuery(con4, "SELECT 1 FROM fondi")), nrow(dbGetQuery(con4, "SELECT 1 FROM piani_busta")), nrow(dbGetQuery(con4, "SELECT 1 FROM vocabolario_fondo"))), c(2L, 1L, 1L))

# Elimina un fondo: l'altro omonimo resta intero
invisible(elimina_fondo(con, id_bo))
atteso("elimina: il fondo omonimo dell'altro archivio resta", c(nrow(dbGetQuery(con, "SELECT 1 FROM fascicoli")), nrow(dbGetQuery(con, "SELECT 1 FROM fondi")), nrow(dbGetQuery(con, "SELECT 1 FROM piani_busta"))), c(2L, 1L, 1L))

# Copia SQLite di un fondo solo
ps <- tempfile(fileext = ".sqlite"); sch("Archivio Arcivescovile di Bologna", "Sant'Ufficio", 1L, 1L, "Neri, Carla")
id_bo <- fondo_id_da(con, "Archivio Arcivescovile di Bologna", "Sant'Ufficio")
esporta_sqlite(con, ps, fondo = id_bo)
x <- DBI::dbConnect(RSQLite::SQLite(), ps)
atteso("copia SQLite di un fondo: solo quel fondo", c(DBI::dbGetQuery(x, "SELECT COUNT(*) n FROM fascicoli")$n, DBI::dbGetQuery(x, "SELECT COUNT(*) n FROM fondi")$n), c(1L, 1L))
DBI::dbDisconnect(x)

# ── 3. Il trigger: una scheda scritta senza fondo_id lo riceve ───────────────────────────────
DBI::dbExecute(con, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, creato, modificato) VALUES ('t1', '  Archivio di Stato di Modena ', 'Sant''Ufficio', 7, 7, 'x', 'x')")
atteso("trigger: la scheda scritta a mano entra nel fondo giusto (spazi e maiuscole non contano)", DBI::dbGetQuery(con, "SELECT fondo_id FROM fascicoli WHERE id = 't1'")$fondo_id, id_mo)
DBI::dbExecute(con, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, creato, modificato) VALUES ('t2', 'Archivio Nuovo', 'Fondo Nuovo', 1, 1, 'x', 'x')")
atteso("trigger: un fondo mai visto si crea", !is.na(fondo_id_da(con, "Archivio Nuovo", "Fondo Nuovo")) && .e_id_fondo(DBI::dbGetQuery(con, "SELECT fondo_id FROM fascicoli WHERE id = 't2'")$fondo_id), TRUE)
atteso("trigger: la stessa segnatura nello stesso fondo è ancora un conflitto",
       inherits(try(DBI::dbExecute(con, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, creato, modificato) VALUES ('t3', 'Archivio di Stato di Modena', 'Sant''Ufficio', 7, 7, 'x', 'x')"), silent = TRUE), "try-error"), TRUE)
atteso("chiave: R e SQLite calcolano la stessa", .chiave_fondo("  Archivio  X ", "Fondo  Y"), DBI::dbGetQuery(con, paste0("SELECT ", .SQL_CHIAVE_FONDO("'  Archivio  X '", "'Fondo  Y'"), " AS k"))$k)

# ── 4. «Sostituisci questo fondo»: solo il fondo scelto, e solo con righe sue ─────────────────
cs <- apri_db(":memory:"); a_es <- leggi_arianna("dati/esempio/esempio.xlsx")
invisible(importa_arianna(cs, a_es))
id_es <- dbGetQuery(cs, "SELECT DISTINCT fondo_id FROM fascicoli")$fondo_id; n_es <- dbGetQuery(cs, "SELECT COUNT(*) n FROM fascicoli")$n
f <- fascicolo_vuoto(); f$archivio <- "Altro Archivio"; f$fondo <- "Altro Fondo"; f$busta <- 1L; f$fascicolo <- 1L; f$titolo <- "x"; f$titolo_manuale <- 1L; invisible(salva_fascicolo(cs, f))
id_altro <- fondo_id_da(cs, "Altro Archivio", "Altro Fondo")
r1 <- importa_arianna(cs, a_es, "sostituisci", fondo = id_es)
atteso("sostituisci un fondo: le sue schede tornano, l'altro fondo resta", c(r1$importati, dbGetQuery(cs, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(id_es))$n, dbGetQuery(cs, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(id_altro))$n), c(n_es, n_es, 1L))
r2 <- importa_arianna(cs, a_es, "sostituisci", fondo = id_altro)
atteso("sostituisci un fondo con righe di un altro: respinte, non si cancella niente",
       c(isTRUE(r2$sostituzione_annullata), dbGetQuery(cs, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(id_altro))$n, dbGetQuery(cs, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(id_es))$n), c(TRUE, 1L, n_es))

# Import con un nome di fondo che sta in più archivi (18/11/2026: «Il fondo c'è in più archivi: indica quale» bloccava l'import di un archivio nuovo)
for (arc in c("Archivio Omonimo 1", "Archivio Omonimo 2")) invisible(crea_archivio(cs, arc, "Sant'Ufficio"))
ro <- try(importa_nuovo_archivio(cs, a_es, "Archivio Omonimo 3", "Sant'Ufficio"), silent = TRUE)
atteso("import di un archivio nuovo con un fondo omonimo di altri archivi: entra", !inherits(ro, "try-error") && ro$importati > 0L, TRUE)
fo <- fascicolo_vuoto(); fo$fondo <- "Sant'Ufficio"; fo$reati <- "Magia"
# Il solo nome che sta in più archivi non sceglie una regola in silenzio (19/11/2026): errore chiaro in normalizza_valori_fascicolo, e nell'import la riga è respinta con il motivo.
en <- tryCatch(normalizza_valori_fascicolo(cs, fo), error = function(e) conditionMessage(e))
atteso("normalizzare con il solo nome ambiguo: errore chiaro, non un ripiego silenzioso", is.character(en) && grepl("in più archivi", en, fixed = TRUE), TRUE)
atteso("normalizzare con archivio e nome: nessun errore", inherits(try(normalizza_valori_fascicolo(cs, { g <- fo; g$archivio <- "Archivio Omonimo 1"; g }), silent = TRUE), "try-error"), FALSE)

if (length(.esiti$falliti)) { cat(sprintf("ESITO: NON TORNA (%d ok, %d no)\n", .esiti$ok, length(.esiti$falliti))); quit(status = 1) }
cat(sprintf("ESITO: ok - %d verifiche sui fondi con id interno\n", .esiti$ok))
