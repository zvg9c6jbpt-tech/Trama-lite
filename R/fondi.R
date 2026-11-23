# ─────────────────────────────────────────────────────────────
# fondi.R: l'identificativo interno del fondo (PIANO_FONDO_ID.md)
# Un fondo è una riga di `fondi`: `id` interno stabile (UUID, mai mostrato) con ARCHIVIO e DENOMINAZIONE; la coppia è unica, la stessa denominazione può stare in archivi diversi.
#   fondi(id, archivio, denominazione, serie, profilo, creato, chiave)
#   fascicoli.fondo_id, piani_busta.fondo_id, vocabolario_fondo.fondo_id → fondi.id
#   fascicoli.archivio e fascicoli.fondo restano come NOMI (si mostrano, entrano in segnature ed export), ma non identificano più.
# Le funzioni con `fondo` accettano l'id o, per script e collaudi, la denominazione se è di un solo fondo (`.fid(con, fondo)`); di più archivi è un errore chiaro.
# ─────────────────────────────────────────────────────────────

.UUID_FONDO <- "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$"
.FONDO_INESISTENTE <- "00000000-0000-0000-0000-000000000000"     # un id che non esiste mai: una denominazione sconosciuta non trova niente

.e_id_fondo <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && grepl(.UUID_FONDO, x)

# Un nome per confrontare: minuscolo (ASCII, come lower() di SQLite), spazi agli estremi tolti e ripetuti ridotti a uno.
# DEVE restare identico all'espressione del trigger in schema.R (.SQL_CHIAVE_FONDO): la chiave si calcola in R e in SQLite.
.nome_per_chiave <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz", x)
  x <- gsub(" {2,}", " ", x)
  gsub("^ +| +$", "", x)
}
.chiave_fondo <- function(archivio, denominazione) paste0(.nome_per_chiave(archivio), "\n", .nome_per_chiave(denominazione))

# L'id del fondo (archivio, denominazione). Con `crea = TRUE` lo crea se non c'è; senza, NA.
fondo_id_da <- function(con, archivio, denominazione, crea = FALSE, serie = NA_character_, profilo = "Modello", id = NULL, dichiarato = FALSE) {
  arc <- gsub("^ +| +$", "", as.character(archivio)[1]); if (is.na(arc)) arc <- ""
  den <- gsub("^ +| +$", "", as.character(denominazione)[1]); if (is.na(den)) den <- ""
  k <- .chiave_fondo(arc, den)
  trovato <- dbGetQuery(con, "SELECT id FROM fondi WHERE chiave = ?", list(k))$id
  if (length(trovato)) return(trovato[1])
  if (!isTRUE(crea)) return(NA_character_)
  id <- if (is.null(id)) UUIDgenerate() else id
  dbExecute(con, "INSERT INTO fondi (id, archivio, denominazione, serie, profilo, creato, chiave, dichiarato) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
            list(id, arc, den, if (.vuoto(serie)) NA_character_ else serie, if (.vuoto(profilo)) NA_character_ else profilo,
                 format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), k, as.integer(isTRUE(dichiarato))))
  id
}

# Da id o denominazione all'id. NULL o vuoto resta NULL («tutti i fondi»); un nome inesistente dà un id che non trova niente; una denominazione di PIÙ archivi è un errore.
.fid <- function(con, fondo) {
  if (is.null(fondo) || !length(fondo)) return(NULL)
  x <- as.character(fondo)[1]
  if (is.na(x) || !nzchar(trimws(x))) return(NULL)
  if (.e_id_fondo(x)) return(x)
  d <- dbGetQuery(con, "SELECT id, archivio, denominazione FROM fondi")
  i <- which(.nome_per_chiave(d$denominazione) == .nome_per_chiave(x))
  if (length(i) == 1L) return(d$id[i])
  if (length(i) > 1L)
    stop("Il fondo «", trimws(x), "» c'è in più archivi (", paste(unique(d$archivio[i]), collapse = "; "), "): indica quale.", call. = FALSE)
  .FONDO_INESISTENTE
}

# I nomi di un fondo, per mostrarli.
nome_fondo <- function(con, fondo) {
  id <- .fid(con, fondo); if (is.null(id)) return(NA_character_)
  r <- dbGetQuery(con, "SELECT denominazione FROM fondi WHERE id = ?", list(id))$denominazione
  if (length(r)) r[1] else as.character(fondo)[1]
}
# «Fondo Sant'Ufficio»: la parola «Fondo» si aggiunge solo se la denominazione non comincia già così.
con_parola_fondo <- function(nome) if (grepl("^fondo\\b", trimws(nome), ignore.case = TRUE)) nome else paste("Fondo", nome)
# «Denominazione · Archivio»: per dire quale fondo, quando il solo nome può stare in più archivi.
nome_fondo_esteso <- function(con, fondo) {
  id <- .fid(con, fondo); if (is.null(id)) return(NA_character_)
  r <- dbGetQuery(con, "SELECT denominazione, archivio FROM fondi WHERE id = ?", list(id))
  if (!nrow(r)) return(as.character(fondo)[1])
  if (.vuoto(r$archivio[1])) r$denominazione[1] else paste0(r$denominazione[1], " \u00b7 ", r$archivio[1])
}
# Il fondo di un NOME, quando è tutto ciò che si sa (piani e vocabolario di database e backup vecchi): quello con più schede, a pari merito il più vecchio. NA se nessuno.
.fondo_per_nome <- function(con, nome) {
  r <- dbGetQuery(con, "SELECT f.id, (SELECT COUNT(*) FROM fascicoli s WHERE s.fondo_id = f.id) AS n FROM fondi f
                         WHERE f.denominazione = ? COLLATE NOCASE ORDER BY n DESC, f.creato", list(trimws(as.character(nome))))$id
  if (length(r)) r[1] else NA_character_
}

# ── La migrazione dal modello a nome al modello a id ─────────
# Idempotente. Per ogni coppia (archivio, fondo) crea un fondo, riempie fascicoli.fondo_id, ricostruisce piani_busta e vocabolario_fondo e toglie `archivi`.
# Su un database vero prima una copia di sicurezza; mai silenziosa, scrive nel registro.
.migra_a_fondi <- function(con) {
  tabelle <- dbListTables(con)
  col <- function(t) if (t %in% tabelle) dbGetQuery(con, sprintf("PRAGMA table_info(%s)", t))$name else character(0)
  da_fare_schede <- "fascicoli" %in% tabelle && "fondo_id" %in% col("fascicoli") &&
    dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id IS NULL")$n > 0
  da_fare_piani  <- "piani_busta" %in% tabelle && "fondo" %in% col("piani_busta")
  da_fare_voc    <- "vocabolario_fondo" %in% tabelle && "fondo" %in% col("vocabolario_fondo")
  da_fare_archivi <- "archivi" %in% tabelle
  if (!(da_fare_schede || da_fare_piani || da_fare_voc || da_fare_archivi)) return(invisible(FALSE))

  # Copia di sicurezza (solo su un file vero con qualcosa da migrare).
  nome_file <- tryCatch(dbGetInfo(con)$dbname, error = function(e) "")
  n_schede <- dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n
  if (nzchar(nome_file) && !identical(nome_file, ":memory:") && file.exists(nome_file) && n_schede > 0)
    tryCatch(backup_prima_ripristino(con, file.path(dirname(nome_file), "backup"), "migrazione-fondi"),
             error = function(e) if (exists("registra", mode = "function")) registra("ERRORE", "copia prima della migrazione dei fondi non riuscita", conditionMessage(e)))

  dbExecute(con, "PRAGMA foreign_keys = OFF")
  on.exit(dbExecute(con, "PRAGMA foreign_keys = ON"), add = TRUE)
  .atomico(con, {
    # 1. i fondi: dalle righe di `archivi` (con serie e profilo) e dalle coppie (archivio, fondo) delle schede
    if (da_fare_archivi) {
      a <- dbGetQuery(con, "SELECT fondo, archivio, serie, profilo, creato FROM archivi")
      for (i in seq_len(nrow(a))) {
        id <- fondo_id_da(con, a$archivio[i], a$fondo[i], crea = TRUE, serie = a$serie[i], profilo = a$profilo[i], dichiarato = TRUE)
        if (!is.na(a$creato[i])) dbExecute(con, "UPDATE fondi SET creato = ? WHERE id = ?", list(a$creato[i], id))
      }
    }
    if ("fondo_id" %in% col("fascicoli")) {
      p <- dbGetQuery(con, "SELECT DISTINCT COALESCE(archivio, '') AS archivio, fondo FROM fascicoli WHERE fondo_id IS NULL")
      for (i in seq_len(nrow(p))) {
        id <- fondo_id_da(con, p$archivio[i], p$fondo[i], crea = TRUE)
        dbExecute(con, "UPDATE fascicoli SET fondo_id = ? WHERE fondo_id IS NULL AND COALESCE(archivio, '') = ? AND fondo = ?",
                  list(id, p$archivio[i], p$fondo[i]))
      }
    }
    # 2. il fondo di un NOME (per piani e vocabolario): quello con più schede, a pari merito il primo
    per_nome <- function(nome) .fondo_per_nome(con, nome)
    # 3. piani_busta e vocabolario_fondo con fondo_id
    if (da_fare_piani) {
      dbExecute(con, "CREATE TABLE piani_busta_nuova (
        fondo_id TEXT NOT NULL, busta INTEGER NOT NULL, busta_suffisso TEXT NOT NULL DEFAULT '',
        primo INTEGER NOT NULL, ultimo INTEGER NOT NULL, creato TEXT NOT NULL,
        PRIMARY KEY (fondo_id, busta, busta_suffisso), CHECK (busta >= 1 AND primo >= 1 AND ultimo >= primo))")
      v <- dbGetQuery(con, "SELECT fondo, busta, busta_suffisso, primo, ultimo, creato FROM piani_busta")
      for (i in seq_len(nrow(v))) {
        id <- per_nome(v$fondo[i]); if (is.na(id)) id <- fondo_id_da(con, "", v$fondo[i], crea = TRUE)
        dbExecute(con, "INSERT OR IGNORE INTO piani_busta_nuova (fondo_id, busta, busta_suffisso, primo, ultimo, creato) VALUES (?, ?, ?, ?, ?, ?)",
                  list(id, v$busta[i], v$busta_suffisso[i], v$primo[i], v$ultimo[i], v$creato[i]))
      }
      dbExecute(con, "DROP TABLE piani_busta"); dbExecute(con, "ALTER TABLE piani_busta_nuova RENAME TO piani_busta")
    }
    if (da_fare_voc) {
      dbExecute(con, "CREATE TABLE vocabolario_fondo_nuova (
        fondo_id TEXT NOT NULL, campo TEXT NOT NULL, valore TEXT NOT NULL, forma TEXT, creato TEXT NOT NULL,
        PRIMARY KEY (fondo_id, campo, valore))")
      v <- dbGetQuery(con, "SELECT fondo, campo, valore, forma, creato FROM vocabolario_fondo")
      for (i in seq_len(nrow(v))) {
        id <- per_nome(v$fondo[i]); if (is.na(id)) id <- fondo_id_da(con, "", v$fondo[i], crea = TRUE)
        dbExecute(con, "INSERT OR IGNORE INTO vocabolario_fondo_nuova (fondo_id, campo, valore, forma, creato) VALUES (?, ?, ?, ?, ?)",
                  list(id, v$campo[i], v$valore[i], v$forma[i], v$creato[i]))
      }
      dbExecute(con, "DROP TABLE vocabolario_fondo"); dbExecute(con, "ALTER TABLE vocabolario_fondo_nuova RENAME TO vocabolario_fondo")
    }
    # 4. `archivi` non serve più: è `fondi`
    if (da_fare_archivi) dbExecute(con, "DROP TABLE archivi")
  })
  # L'impostazione del fondo attivo era un nome: diventa l'id (se il nome è di un solo fondo)
  try({
    s <- dbGetQuery(con, "SELECT valore FROM impostazioni WHERE chiave = 'fondo_attivo'")$valore
    if (length(s) && nzchar(s[1]) && !.e_id_fondo(s[1])) {
      id <- tryCatch(.fid(con, s[1]), error = function(e) NULL)
      if (!is.null(id) && !identical(id, .FONDO_INESISTENTE)) dbExecute(con, "UPDATE impostazioni SET valore = ? WHERE chiave = 'fondo_attivo'", list(id))
    }
  }, silent = TRUE)
  if (exists("registra", mode = "function")) registra("INFO", "migrazione", sprintf("fondi: %d fondi creati, schede con fondo_id riempito", dbGetQuery(con, "SELECT COUNT(*) n FROM fondi")$n))
  invisible(TRUE)
}
