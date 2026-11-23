# Fixture sintetiche; database su disco temporaneo, confronto di tutte le tabelle.
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1", TRAMA_COLLAUDO = "1")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
local({
  path <- tempfile(fileext = ".sqlite")
  con <- apri_db(path)
  on.exit({ if (dbIsValid(con)) dbDisconnect(con); unlink(path) })
  crea <- function(c, numero, fondo) {
    f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- numero; f$fondo <- fondo
    f$persone <- "Rossi, Antonio"; f$reati <- "Eresia"
    salva_fascicolo(c, f); f$id
  }
  old <- crea(con, 99L, "A"); other <- crea(con, 99L, "B")
  src <- apri_db(":memory:"); on.exit(dbDisconnect(src), add = TRUE)
  crea(src, 1L, "A"); crea(src, 2L, "A")
  x <- tempfile(fileext = ".xlsx"); on.exit(unlink(x), add = TRUE)
  esporta_arianna(src, x); a <- leggi_arianna(x)
  snapshot <- function() setNames(lapply(dbListTables(con), function(t) dbReadTable(con, t)), dbListTables(con))
  before <- snapshot()
  # Una riga valida seguita da una senza segnatura: prima salvava metà import.
  bad <- a; col <- unname(a$colonna["Segnatura"])
  stopifnot(length(col) == 1L, !is.na(col))
  bad$dati[2, col] <- ""
  r <- importa_arianna(con, bad, "sostituisci", fondo = "A")
  stopifnot(isTRUE(r$sostituzione_annullata), r$importati == 0L, nrow(r$respinti) == 1L)
  dbDisconnect(con); con <- apri_db(path)
  stopifnot(identical(before, snapshot()))
  # Rifiuto durante il salvataggio (trigger SQLite), dopo una riga valida.
  dbExecute(con, "CREATE TRIGGER rifiuto_prova BEFORE INSERT ON fascicoli WHEN NEW.fascicolo = 2 BEGIN SELECT RAISE(ABORT, 'rifiuto di prova'); END")
  r <- importa_arianna(con, a, "sostituisci", fondo = "A")
  stopifnot(isTRUE(r$sostituzione_annullata), r$importati == 0L, nrow(r$respinti) == 1L)
  dbExecute(con, "DROP TRIGGER rifiuto_prova")
  dbDisconnect(con); con <- apri_db(path)
  stopifnot(identical(before, snapshot()))
  # Tutte respinte; vale anche per la sostituzione globale.
  bad$dati[, col] <- ""
  r <- importa_arianna(con, bad, "sostituisci")
  stopifnot(isTRUE(r$sostituzione_annullata), r$importati == 0L, identical(before, snapshot()))
  # Tutte valide: sostituisce A e conserva integralmente B.
  b <- leggi_fascicolo(con, other)
  r <- importa_arianna(con, a, "sostituisci", fondo = "A")
  stopifnot(r$importati == 2L, nrow(r$respinti) == 0L, !isTRUE(r$sostituzione_annullata))
  dbDisconnect(con); con <- apri_db(path)
  stopifnot(!old %in% elenco_fascicoli(con)$id, identical(b, leggi_fascicolo(con, other)), nrow(elenco_fascicoli(con)) == 3L)
  # Righe senza segnatura (note in fondo al foglio): con il consenso della finestra di conferma restano fuori e non annullano; senza consenso annullano.
  nota <- a; nota$dati <- rbind(a$dati, a$dati[1, , drop = FALSE]); nota$dati[3, col] <- ""
  snap0 <- snapshot()
  r <- importa_arianna(con, nota, "sostituisci", fondo = "A")
  stopifnot(isTRUE(r$sostituzione_annullata), identical(snap0, snapshot()))
  r <- importa_arianna(con, nota, "sostituisci", fondo = "A", senza_segnatura_ok = TRUE)
  stopifnot(!isTRUE(r$sostituzione_annullata), r$importati == 2L, nrow(r$respinti) == 1L, identical(r$respinti$motivo, .MOTIVO_SENZA_SEGNATURA))
  # Il consenso vale solo per le righe senza segnatura: un altro rifiuto annulla comunque.
  dbExecute(con, "CREATE TRIGGER rifiuto_prova2 BEFORE INSERT ON fascicoli WHEN NEW.fascicolo = 2 BEGIN SELECT RAISE(ABORT, 'rifiuto di prova'); END")
  snap1 <- snapshot()
  r <- importa_arianna(con, nota, "sostituisci", fondo = "A", senza_segnatura_ok = TRUE)
  stopifnot(isTRUE(r$sostituzione_annullata), identical(snap1, snapshot()))
  dbExecute(con, "DROP TRIGGER rifiuto_prova2")
  # Accoda conserva il comportamento parziale dichiarato.
  empty <- apri_db(":memory:"); on.exit(dbDisconnect(empty), add = TRUE)
  bad <- a; bad$dati[2, col] <- ""
  r <- importa_arianna(empty, bad, "accoda")
  stopifnot(r$importati == 1L, nrow(r$respinti) == 1L, nrow(elenco_fascicoli(empty)) == 1L)
})
# Comportamento ATTUALE di «Sostituisci» sugli identificativi (23/10/2026): le schede del fondo si cancellano e si ricreano, quindi gli UUID cambiano,
# la storia di «Annulla» e la posizione nell'ordine di consultazione spariscono, e una scheda che il foglio non ha più si perde. Non è un difetto
# da correggere qui: è ciò che Lite fa oggi, e chi lo cambiasse deve cambiare questa prova di proposito.
local({
  con <- apri_db(":memory:"); on.exit(dbDisconnect(con))
  crea <- function(numero) { f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- numero; f$fondo <- "A"; f$persone <- "Rossi, Antonio"; f$reati <- "Eresia"; salva_fascicolo(con, f); f$id }
  id1 <- crea(1L); id2 <- crea(2L); id3 <- crea(3L)
  x <- tempfile(fileext = ".xlsx"); on.exit(unlink(x), add = TRUE)
  esporta_arianna(con, x, fondo = "A"); a <- leggi_arianna(x)
  col <- unname(a$colonna["Segnatura"]); a$dati <- a$dati[-3, , drop = FALSE]       # il foglio non ha più la scheda 3
  dbExecute(con, "INSERT INTO storia_fascicoli (fascicolo_id, quando, campi, prima) VALUES (?, '2026-10-23 10:00:00', 'titolo', '{}')", list(id1))
  dbExecute(con, "INSERT OR REPLACE INTO ordine_fascicoli (fascicolo_id, posizione) VALUES (?, 1)", list(id1))
  prima_ids <- elenco_fascicoli(con, fondo = "A")$id
  r <- importa_arianna(con, a, "sostituisci", fondo = "A")
  dopo <- elenco_fascicoli(con, fondo = "A")
  stopifnot(r$importati == 2L, nrow(dopo) == 2L)
  stopifnot("«Sostituisci» oggi rigenera gli UUID" = !any(dopo$id %in% prima_ids))
  stopifnot("la storia di Annulla sparisce con le schede" = dbGetQuery(con, "SELECT COUNT(*) n FROM storia_fascicoli")$n == 0L)
  stopifnot("la posizione nell'ordine sparisce con le schede" = dbGetQuery(con, "SELECT COUNT(*) n FROM ordine_fascicoli WHERE fascicolo_id = ?", list(id1))$n == 0L)
  stopifnot("la scheda assente dal foglio si perde" = !(id3 %in% dopo$id) && !any(dopo$fascicolo == 3L, na.rm = TRUE))
})
cat("Sostituzione atomica: 7 scenari superati, persistenza e tutte le tabelle verificate; identificativi, storia e ordine come oggi.\n")
