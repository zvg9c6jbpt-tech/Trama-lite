# ──────────────────────────────────────────────
# verifiche/export_import_fondi_veri.R: export e import su TUTTI i fondi del database di prova, in TUTTI i formati (14/10)
# «Import ed export devono essere perfetti» (regola del progetto): per ogni fondo
#   · xlsx e CSV: esporta, reimporta in un database vuoto, riesporta: 0 celle diverse, nessuna riga respinta;
#   · backup JSON: ripristina in un database vuoto: ogni fondo identico;
#   · schede .json: valido, tante schede quante quelle del fondo;
#   · SQL e SQLite: si aprono e hanno le schede del fondo;
#   · (le località e l'RDF non esistono in Trama Lite)
# Lavora su una copia del database vero (o dell'esempio con TRAMA_DB_COLLAUDI=esempio).
#     Rscript verifiche/export_import_fondi_veri.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
con <- apri_db(database_di_prova())
fondi <- dbGetQuery(con, "SELECT fondo_id, MIN(fondo) fondo, COUNT(*) n FROM fascicoli GROUP BY fondo_id ORDER BY fondo, fondo_id")
celle_diverse <- function(x, y) { x <- as.matrix(x); y <- as.matrix(y); if (!identical(dim(x), dim(y))) return(Inf); sum(!(is.na(x) & is.na(y)) & (is.na(x) != is.na(y) | x != y), na.rm = TRUE) }
vuoto_db <- function() apri_db(tempfile(fileext = ".sqlite"))
d <- tempfile("export_"); dir.create(d)
# backup JSON intero, ripristinato una volta sola
bj <- file.path(d, "backup.json"); esporta_backup_json(con, bj)
db_json <- vuoto_db(); ripristina_backup_json(db_json, bj, "accoda")
for (k in seq_len(nrow(fondi))) {
  fo <- fondi$fondo[k]; fid <- fondi$fondo_id[k]; n <- fondi$n[k]; nome <- paste0(k, "_", gsub("[^A-Za-z0-9]+", "_", fo))  # sul database vero si lavora per id: due archivi possono avere un fondo con lo stesso nome
  t0 <- tabella_arianna(con, fondo = fid)
  # xlsx
  fx <- file.path(d, paste0(nome, ".xlsx")); esporta_arianna(con, fx, fondo = fid)
  db1 <- vuoto_db(); e1 <- importa_arianna(db1, leggi_arianna(fx), modo = "accoda")
  v(sprintf("«%s» (%d): xlsx → import → xlsx, nessuna respinta e 0 celle diverse", fo, n), nrow(e1$respinti) == 0L && celle_diverse(t0, tabella_arianna(db1, fondo = fo)) == 0)
  # CSV
  fc <- file.path(d, paste0(nome, ".csv")); esporta_csv(con, fc, fondo = fid)
  db2 <- vuoto_db(); e2 <- importa_arianna(db2, leggi_arianna(fc), modo = "accoda")
  v(sprintf("«%s»: csv → import → xlsx, nessuna respinta e 0 celle diverse", fo), nrow(e2$respinti) == 0L && celle_diverse(t0, tabella_arianna(db2, fondo = fo)) == 0)
  # backup JSON
  v(sprintf("«%s»: backup JSON ripristinato, identico", fo), celle_diverse(t0, tabella_arianna(db_json, fondo = fid)) == 0)
  # schede .json
  fj <- file.path(d, paste0(nome, "-schede.json")); esporta_json_schede(con, fj, fondo = fid)
  j <- tryCatch(jsonlite::fromJSON(fj, simplifyVector = FALSE), error = function(e) NULL)
  v(sprintf("«%s»: schede .json valido con %d schede", fo, n), !is.null(j) && length(if (!is.null(j$schede)) j$schede else j) == n)
  # SQL e SQLite
  fs <- file.path(d, paste0(nome, ".sql")); esporta_sql(con, fs, fondo = fid)
  q <- tempfile(fileext = ".sqlite"); sql_ok <- tryCatch({ system2("sqlite3", c(shQuote(q)), input = readLines(fs, warn = FALSE), stdout = FALSE, stderr = FALSE) == 0 }, error = function(e) FALSE)
  nq <- tryCatch({ x <- dbConnect(RSQLite::SQLite(), q); on.exit(dbDisconnect(x)); dbGetQuery(x, "SELECT COUNT(*) n FROM fascicoli")$n }, error = function(e) NA)
  v(sprintf("«%s»: .sql si carica in SQLite con %d schede", fo, n), sql_ok && identical(as.integer(nq), as.integer(n)))
  fq <- file.path(d, paste0(nome, ".sqlite")); esporta_sqlite(con, fq, fondo = fid)
  nq2 <- tryCatch({ x <- dbConnect(RSQLite::SQLite(), fq); on.exit(dbDisconnect(x), add = TRUE); dbGetQuery(x, "SELECT COUNT(*) n FROM fascicoli")$n }, error = function(e) NA)
  v(sprintf("«%s»: .sqlite si apre con %d schede", fo, n), identical(as.integer(nq2), as.integer(n)))
}
cat(if (length(falliti)) sprintf("ESITO: NON TORNA (%d: %s)\n", length(falliti), paste(head(falliti, 4), collapse = "; ")) else "ESITO: ok\n"); if (length(falliti)) quit(status = 1)
