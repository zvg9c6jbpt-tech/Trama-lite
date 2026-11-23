# ─────────────────────────────────────────────────────────────
# schema.R: struttura del database
# ─────────────────────────────────────────────────────────────

library(DBI)
library(RSQLite)

# SAVEPOINT funziona anche dentro l'import, che ha già una transazione.
# Un errore annulla l'intera operazione locale, non solo l'ultima query.
.atomico <- function(con, codice) {
  nome <- paste0("operazione_", paste(sample(letters, 20, replace = TRUE), collapse = ""))
  dbExecute(con, paste("SAVEPOINT", nome))
  concluso <- FALSE
  on.exit(if (!concluso) {
    try(dbExecute(con, paste("ROLLBACK TO", nome)), silent = TRUE)
    try(dbExecute(con, paste("RELEASE", nome)), silent = TRUE)
  }, add = TRUE)
  risultato <- withVisible(force(codice))
  dbExecute(con, paste("RELEASE", nome))
  concluso <- TRUE
  if (risultato$visible) risultato$value else invisible(risultato$value)
}

DDL <- c(
  
  "CREATE TABLE IF NOT EXISTS fascicoli (
  id                      TEXT PRIMARY KEY,
  -- livello struttura: identità archivistica
  archivio                TEXT NOT NULL,
  fondo                   TEXT NOT NULL,
  serie                   TEXT,
  altra_segnatura         TEXT,
  busta                   INTEGER,
  fascicolo               INTEGER,
  -- Fascicoli tipo 16-bis, 1-ter, 1-2: il numero resta intero e il
  -- seguito sta qui com'è scritto nella fonte. Vuoto = nessun suffisso.
  fascicolo_suffisso      TEXT NOT NULL DEFAULT '',
  -- Buste tipo 232-bis, 5-ter, 7-quater (ASMo, 90 schede): come il fascicolo, il numero resta intero e il
  -- seguito sta qui com'è scritto. La numerazione dei fascicoli può ripartire da 1 nella busta «bis».
  busta_suffisso          TEXT NOT NULL DEFAULT '',
  sigla_busta             TEXT,
  sigla_fascicolo         TEXT,
  titolo                  TEXT,
  titolo_manuale          INTEGER NOT NULL DEFAULT 0,
  -- Tipo (Processo/Atti/Denuncia/...) e connettore (contro/circa/su/di)
  -- che compongono il titolo insieme agli imputati (TODO B10).
  tipo_titolo             TEXT,
  connettore              TEXT,
  estremo_remoto          INTEGER,
  estremo_recente         INTEGER,
  -- La data manca perché la fonte non la dà: dichiarato, non dimenticato.
  -- Senza data e senza questa spunta la maschera non salva (18/09).
  data_nd                 INTEGER NOT NULL DEFAULT 0,
  -- Il fascicolo non si può consultare (TODO 26): prima era una frase
  -- nelle Note libere, in tre grafie. All'export torna nelle Note.
  non_consultabile        INTEGER NOT NULL DEFAULT 0,
  -- Nel foglio la colonna Soggettario e i reati scritti nel regesto
  -- dicevano cose diverse: qui si tengono uniti, e questo segna che il
  -- caso va rivisto sulla fonte (Qualita, 21/09).
  soggettario_discorde    INTEGER NOT NULL DEFAULT 0,
  definizione             TEXT DEFAULT 'fascicolo',
  supporto                TEXT DEFAULT 'cartaceo',
  tipologia_documentaria  TEXT DEFAULT 'Atti',
  soggetto_conservatore   TEXT,                       -- eliminato (12/10): colonna morta, senza default
  luogo_conservazione     TEXT,
  luogo_produzione        TEXT,
  note                    TEXT,
  -- livello qualità: il regesto strutturato
  luogo_reato             TEXT,
  tortura                 TEXT DEFAULT 'n.d.',
  sentenza                TEXT DEFAULT 'n.d.',
  -- 'n.d.' inghiotte due stati opposti: fonte controllata e muta, o
  -- nessuno l'ha ancora guardata. Questi due flag li separano (TODO B15).
  tortura_esaminata       INTEGER NOT NULL DEFAULT 0,
  sentenza_esaminata      INTEGER NOT NULL DEFAULT 0,
  -- Chi risponde di quale capo, quando ci sono più imputati e più
  -- capi insieme (TODO B15/B16): 'n.d.' = non esaminato, mai un
  -- default. 'collettiva' = la fonte contesta i capi a tutti insieme
  -- (un'asserzione, non una scorciatoia); 'per persona' = dichiarato
  -- caso per caso in persona_reati (tabella a sé: l'editor per
  -- popolarla arriva con la maschera rifatta, non c'è ancora).
  attribuzione             TEXT NOT NULL DEFAULT 'n.d.',
  -- Specchio identico sull'asse capo->luogo, con reato_luoghi.
  attribuzione_luoghi      TEXT NOT NULL DEFAULT 'n.d.',
  -- L'elenco imputati è compilato ma per inclusione, non per lettura
  -- (tipicamente le raccolte 'Atti per diversi'): compaiono, ma
  -- l'identità dell'accusato non è accertata.
  identita_imputato_dubbia INTEGER NOT NULL DEFAULT 0,
  imputati_esaminati      INTEGER NOT NULL DEFAULT 0,
  -- Flag sulla fonte (12/10): reato o luogo del reato guardati e ignoti; scheda che non è un procedimento (carte
  -- sciolte, cataloghi, registri). Stato della scheda, non colonne del modello: non escono nell'export.
  reato_ignoto            INTEGER NOT NULL DEFAULT 0,
  luogo_ignoto            INTEGER NOT NULL DEFAULT 0,
  non_procedimento        INTEGER NOT NULL DEFAULT 0,
  regesto                 TEXT,
  regesto_manuale         INTEGER NOT NULL DEFAULT 0,
  -- tracciamento
  creato                  TEXT NOT NULL,
  modificato              TEXT NOT NULL
)",
  
  "CREATE TABLE IF NOT EXISTS imputati (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  ordine        INTEGER NOT NULL,
  esteso        TEXT,
  indicizzato   TEXT,
  PRIMARY KEY (fascicolo_id, ordine)
)",
  
  # `ordine`: la posizione nella fonte, senza cui reati e persone tornerebbero in ordine alfabetico e il regesto ricomposto cambierebbe.
  "CREATE TABLE IF NOT EXISTS reati (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  reato         TEXT NOT NULL,
  ordine        INTEGER,
  PRIMARY KEY (fascicolo_id, reato)
)",
  
  "CREATE TABLE IF NOT EXISTS persone (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  persona       TEXT NOT NULL,
  ordine        INTEGER,
  PRIMARY KEY (fascicolo_id, persona)
)",
  
  "CREATE TABLE IF NOT EXISTS luoghi (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  ordine        INTEGER NOT NULL,
  luogo         TEXT NOT NULL,
  PRIMARY KEY (fascicolo_id, ordine)
)",
  
  "CREATE TABLE IF NOT EXISTS lingue (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  lingua        TEXT NOT NULL,
  ordine        INTEGER,
  PRIMARY KEY (fascicolo_id, lingua)
)",
  
  "CREATE TABLE IF NOT EXISTS produttori (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  soggetto      TEXT NOT NULL,
  ordine        INTEGER,
  PRIMARY KEY (fascicolo_id, soggetto)
)",
  "CREATE TABLE IF NOT EXISTS campi_extra (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  campo         TEXT NOT NULL,
  ordine        INTEGER NOT NULL DEFAULT 1,
  valore        TEXT,
  PRIMARY KEY (fascicolo_id, campo, ordine)
)",
  
  # I fascicoli attesi in una busta. NON sono schede: nessun export e
  # nessuna statistica li legge come fascicoli.
  "CREATE TABLE IF NOT EXISTS piani_busta (
  fondo_id TEXT   NOT NULL,
  busta   INTEGER NOT NULL,
  busta_suffisso TEXT NOT NULL DEFAULT '',
  primo   INTEGER NOT NULL,
  ultimo  INTEGER NOT NULL,
  creato  TEXT    NOT NULL,
  PRIMARY KEY (fondo_id, busta, busta_suffisso),
  CHECK (busta >= 1 AND primo >= 1 AND ultimo >= primo)
)",

  # Anagrafe persone: un id stabile per persona, che nasce solo quando serve (fusione o nota biografica). Le persone senza scheda restano stringhe in persone.persona.
  "CREATE TABLE IF NOT EXISTS schede_persona (
  id              TEXT PRIMARY KEY,
  forma_canonica  TEXT NOT NULL UNIQUE,
  note            TEXT NOT NULL DEFAULT '',
  creato          TEXT NOT NULL,
  modificato      TEXT NOT NULL
)",

  # Chi è, davvero, il nome letto in QUESTO fascicolo: persone.persona resta la lettura della fonte; qui si dichiara l'identità (l'uuid; due omonimi sono due schede con la stessa etichetta).
  # `fondamento`: come si sa (dichiarata, derivata, non esaminata), lo stesso asse di persona_reati e reato_luoghi.
  "CREATE TABLE IF NOT EXISTS persona_identita (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  persona       TEXT NOT NULL,
  scheda_id     TEXT NOT NULL REFERENCES schede_persona(id) ON DELETE CASCADE,
  fondamento    TEXT NOT NULL DEFAULT 'dichiarata',
  creato        TEXT NOT NULL,
  PRIMARY KEY (fascicolo_id, persona)
)",

  # Grafie assorbite da una fusione: documentano le varianti storiche.
  "CREATE TABLE IF NOT EXISTS persona_varianti (
  scheda_id  TEXT NOT NULL REFERENCES schede_persona(id) ON DELETE CASCADE,
  variante   TEXT NOT NULL,
  PRIMARY KEY (scheda_id, variante)
)",

  # Uuid delle schede assorbite dalle fusioni: non si distruggono, un
  # id già pubblicato come URI (export RDF, TODO C3) non deve morire.
  "CREATE TABLE IF NOT EXISTS persona_assorbiti (
  scheda_id     TEXT NOT NULL REFERENCES schede_persona(id) ON DELETE CASCADE,
  id_assorbito  TEXT NOT NULL,
  PRIMARY KEY (scheda_id, id_assorbito)
)",

  # Capi dichiarati per singolo imputato, usata solo con attribuzione = 'per persona'. Si legge ma nessuna maschera la scrive ancora.
  "CREATE TABLE IF NOT EXISTS persona_reati (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  persona       TEXT NOT NULL,
  reato         TEXT NOT NULL,
  PRIMARY KEY (fascicolo_id, persona, reato)
)",

  # Il termine specifico sotto la classe (tesauro dei reati, D33): `reato` è la classe della Guida, `termine` il termine del profilo (Bigamia sotto
  # «Crimini sessuali»). Il foglio esce con la classe; il termine resta qui. Si scrive solo se chi salva lo porta (import, maschera).
  "CREATE TABLE IF NOT EXISTS reato_termini (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  reato         TEXT NOT NULL,
  termine       TEXT NOT NULL,
  PRIMARY KEY (fascicolo_id, reato, termine)
)",

  # Specchio di persona_reati sull'asse capo->luogo.
  "CREATE TABLE IF NOT EXISTS reato_luoghi (
  fascicolo_id  TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  reato         TEXT NOT NULL,
  luogo         TEXT NOT NULL,
  PRIMARY KEY (fascicolo_id, reato, luogo)
)",

  # Coorti: ricerche nominate da richiamare con un clic. Salva la QUERY del mini-linguaggio (dinamica), non un elenco di fascicoli; non tocca mai i fascicoli.
  "CREATE TABLE IF NOT EXISTS coorti (
  id      INTEGER PRIMARY KEY AUTOINCREMENT,
  nome    TEXT NOT NULL UNIQUE,
  query   TEXT NOT NULL,
  creato  TEXT NOT NULL
)",

  # Omonimie verificate a mano: un nome attestato in fascicoli distanti più di 5 anni viene segnalato; la spunta dichiara «stessa persona» o «omonimi».
  # `fascicoli` è l'insieme verificato (id ordinati, separati da virgola): se il nome compare in un fascicolo nuovo la verifica non vale più.
  "CREATE TABLE IF NOT EXISTS omonimie_verificate (
  persona    TEXT PRIMARY KEY,
  esito      TEXT NOT NULL CHECK (esito IN ('stessa persona', 'omonimi')),
  fascicoli  TEXT NOT NULL,
  creato     TEXT NOT NULL
)",

  # Impostazioni di lavoro (es. il soggetto produttore delle schede
  # nuove): configurazione, non dati - nessun export le legge.
  "CREATE TABLE IF NOT EXISTS impostazioni (
  chiave  TEXT PRIMARY KEY,
  valore  TEXT NOT NULL
)",

  # Un archivio dichiarato prima di avere schede (Home → Nuovo archivio): il fondo esiste subito e la prima scheda ne eredita archivio e serie.
  "CREATE TABLE IF NOT EXISTS fondi (
  id            TEXT PRIMARY KEY,
  archivio      TEXT NOT NULL,
  denominazione TEXT NOT NULL,
  serie         TEXT,
  profilo       TEXT,
  creato        TEXT NOT NULL,
  chiave        TEXT NOT NULL UNIQUE,
  dichiarato    INTEGER NOT NULL DEFAULT 0,  -- 1 = creato da Home → Nuovo archivio: esiste anche senza schede
  fonte         TEXT                         -- 24/10/2026: da dove vengono i dati del fondo, in una riga (facoltativa)
)",

"CREATE TABLE IF NOT EXISTS ordine_fascicoli (
  fascicolo_id TEXT PRIMARY KEY REFERENCES fascicoli(id) ON DELETE CASCADE,
  posizione INTEGER NOT NULL CHECK (posizione >= 1)
)",
  # Vocabolario per fondo e decisioni che si ricordano (R/colonne.R): `forma` vuota = valore ammesso per il fondo; `forma` piena = variante riscritta dall'import.
  # Sono decisioni, non derivabili dai dati: vanno nel backup JSON.
  "CREATE TABLE IF NOT EXISTS vocabolario_fondo (
  fondo_id TEXT NOT NULL,
  campo   TEXT NOT NULL,
  valore  TEXT NOT NULL,
  forma   TEXT,
  creato  TEXT NOT NULL,
  PRIMARY KEY (fondo_id, campo, valore)
)",
  # Storia di una scheda: a ogni Salva dalla maschera che cambia qualcosa, la scheda com'era (`prima`, leggi_fascicolo serializzato) e i campi cambiati.
  # Serve ad «Annulla l'ultima modifica»; è un dato di processo (né export né backup JSON) e sparisce con la scheda.
  "CREATE TABLE IF NOT EXISTS storia_fascicoli (
  id           INTEGER PRIMARY KEY AUTOINCREMENT,
  fascicolo_id TEXT NOT NULL REFERENCES fascicoli(id) ON DELETE CASCADE,
  quando       TEXT NOT NULL,
  campi        TEXT NOT NULL,
  prima        TEXT NOT NULL
)",
  "CREATE INDEX IF NOT EXISTS idx_storia_fascicolo ON storia_fascicoli(fascicolo_id, id)",
"CREATE INDEX IF NOT EXISTS idx_extra_campo ON campi_extra(campo)",
  "CREATE INDEX IF NOT EXISTS idx_fasc_busta  ON fascicoli(busta)",
  "CREATE INDEX IF NOT EXISTS idx_fasc_anno   ON fascicoli(estremo_remoto)",
  "CREATE INDEX IF NOT EXISTS idx_fasc_fondo  ON fascicoli(fondo)",
  "CREATE INDEX IF NOT EXISTS idx_reati        ON reati(reato)",
  "CREATE INDEX IF NOT EXISTS idx_luoghi       ON luoghi(luogo)",
  "CREATE INDEX IF NOT EXISTS idx_persone      ON persone(persona)"
)



# Colonne aggiunte dopo la prima versione: su un database esistente `CREATE TABLE IF NOT EXISTS` non le aggiunge. Ogni riga è idempotente.
.migrazioni <- list(
  c("fondi",      "fonte",  "TEXT"),          # 24/10/2026: da dove vengono i dati del fondo
  c("lingue",     "ordine", "INTEGER"),       # 22/10/2026: l'ordine delle voci è quello del foglio (D4), non l'alfabetico
  c("produttori", "ordine", "INTEGER"),
  c("fascicoli", "fondo_id",           "TEXT"),
  c("fascicoli", "imputati_esaminati", "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "reato_ignoto",        "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "luogo_ignoto",        "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "non_procedimento",    "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "tortura_esaminata",         "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "sentenza_esaminata",        "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "attribuzione",              "TEXT NOT NULL DEFAULT 'n.d.'"),
  c("fascicoli", "attribuzione_luoghi",       "TEXT NOT NULL DEFAULT 'n.d.'"),
  c("fascicoli", "identita_imputato_dubbia",  "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "tipo_titolo",               "TEXT"),
  c("fascicoli", "connettore",                "TEXT"),
  c("fascicoli", "data_nd",                   "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "non_consultabile",          "INTEGER NOT NULL DEFAULT 0"),
  c("fascicoli", "fascicolo_suffisso",        "TEXT NOT NULL DEFAULT ''"),
  c("fascicoli", "busta_suffisso",            "TEXT NOT NULL DEFAULT ''"),
  c("fascicoli", "sigla_busta",               "TEXT"),
  c("fascicoli", "sigla_fascicolo",           "TEXT"),
  c("fascicoli", "soggettario_discorde",      "INTEGER NOT NULL DEFAULT 0"),
  c("reati",     "ordine",                    "INTEGER"),
  c("persone",   "ordine",                    "INTEGER")
)

# schede_persona nasceva con forma_canonica UNIQUE (due omonimi non potevano esistere): si rifà la tabella senza il vincolo, con le righe.
# Le chiavi esterne si spengono durante la ricostruzione, o il DROP porterebbe via in cascata varianti e id assorbiti.
.rifai_schede_persona <- function(con) {
  sql <- dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE type='table' AND name='schede_persona'")$sql
  if (!length(sql) || !grepl("UNIQUE", sql)) return(invisible(FALSE))
  dbExecute(con, "PRAGMA foreign_keys = OFF")
  dbExecute(con, "CREATE TABLE schede_persona_nuova (
    id TEXT PRIMARY KEY, forma_canonica TEXT NOT NULL, note TEXT NOT NULL DEFAULT '',
    creato TEXT NOT NULL, modificato TEXT NOT NULL)")
  dbExecute(con, "INSERT INTO schede_persona_nuova SELECT id, forma_canonica, note, creato, modificato FROM schede_persona")
  dbExecute(con, "DROP TABLE schede_persona")
  dbExecute(con, "ALTER TABLE schede_persona_nuova RENAME TO schede_persona")
  dbExecute(con, "PRAGMA foreign_keys = ON")
  invisible(TRUE)
}

# piani_busta nasceva con la chiave (fondo, busta), che urtava su 232 e 232-bis: si rifà la tabella con il suffisso nella chiave.
.rifai_piani_busta <- function(con) {
  sql <- dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE type='table' AND name='piani_busta'")$sql
  if (!length(sql) || grepl("busta_suffisso", sql)) return(invisible(FALSE))
  dbExecute(con, "CREATE TABLE piani_busta_nuova (
    fondo TEXT NOT NULL, busta INTEGER NOT NULL, busta_suffisso TEXT NOT NULL DEFAULT '',
    primo INTEGER NOT NULL, ultimo INTEGER NOT NULL, creato TEXT NOT NULL,
    PRIMARY KEY (fondo, busta, busta_suffisso),
    CHECK (busta >= 1 AND primo >= 1 AND ultimo >= primo))")
  dbExecute(con, "INSERT INTO piani_busta_nuova (fondo, busta, busta_suffisso, primo, ultimo, creato)
                    SELECT fondo, busta, '', primo, ultimo, creato FROM piani_busta")
  dbExecute(con, "DROP TABLE piani_busta")
  dbExecute(con, "ALTER TABLE piani_busta_nuova RENAME TO piani_busta")
  invisible(TRUE)
}

# La chiave di un fondo in SQL: stessa regola di .nome_per_chiave (R/fondi.R); spazi ripetuti: cinque passaggi (fino a 32 di fila).
.SQL_CHIAVE_FONDO <- function(archivio, fondo) {
  n <- function(x) {
    y <- sprintf("trim(COALESCE(%s, ''), ' ')", x)
    for (i in 1:5) y <- sprintf("replace(%s, '  ', ' ')", y)
    sprintf("lower(trim(%s, ' '))", y)
  }
  sprintf("(%s || char(10) || %s)", n(archivio), n(fondo))
}

# Chi scrive una scheda senza fondo_id (un INSERT a mano, un SQL di Trama) lo riceve qui: si trova il fondo (archivio, denominazione) o lo si crea.
.trigger_fondo_id <- function(con) {
  chiave <- .SQL_CHIAVE_FONDO("NEW.archivio", "NEW.fondo")
  sql <- sprintf("CREATE TRIGGER fascicoli_fondo_id AFTER INSERT ON fascicoli WHEN NEW.fondo_id IS NULL
    BEGIN
      INSERT OR IGNORE INTO fondi (id, archivio, denominazione, creato, chiave, dichiarato)
        VALUES (lower(hex(randomblob(4)) || '-' || hex(randomblob(2)) || '-4' || substr(hex(randomblob(2)), 2) || '-' ||
                      substr('89ab', abs(random()) %% 4 + 1, 1) || substr(hex(randomblob(2)), 2) || '-' || hex(randomblob(6))),
                trim(COALESCE(NEW.archivio, ''), ' '), trim(NEW.fondo, ' '), strftime('%%Y-%%m-%%dT%%H:%%M:%%S', 'now'), %1$s, 0);
      UPDATE fascicoli SET fondo_id = (SELECT id FROM fondi WHERE chiave = %1$s) WHERE id = NEW.id;
    END", chiave)
  # Si riscrive solo se manca o è diverso: DROP e CREATE a ogni apertura cambierebbero il file anche senza dati nuovi.
  uniforma <- function(x) gsub("\\s+", " ", trimws(x))
  esistente <- dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE type = 'trigger' AND name = 'fascicoli_fondo_id'")$sql
  if (length(esistente) && identical(uniforma(esistente[1]), uniforma(sql))) return(invisible())
  dbExecute(con, "DROP TRIGGER IF EXISTS fascicoli_fondo_id")
  dbExecute(con, sql)
}

# D13: l'apostrofo si salva sempre dritto (solo `produttori.soggetto` aveva «Sant’Uffizio»). Idempotente; se la scheda ha già la forma dritta, la riga tipografica cade.
.migra_apostrofi <- function(con) {
  n <- dbGetQuery(con, "SELECT COUNT(*) n FROM produttori WHERE soggetto LIKE '%' || char(8217) || '%'")$n
  if (!n) return(invisible(0L))
  dbExecute(con, "UPDATE OR IGNORE produttori SET soggetto = replace(soggetto, char(8217), '''') WHERE soggetto LIKE '%' || char(8217) || '%'")
  dbExecute(con, "DELETE FROM produttori WHERE soggetto LIKE '%' || char(8217) || '%'")
  if (exists("registra", mode = "function")) registra("INFO", "migrazione", sprintf("apostrofo dritto in produttori.soggetto: %d celle", n))
  invisible(n)
}

# «Luogo del reato» è tolto: per le schede che ancora lo portano il luogo entra in coda ai citati e la colonna si svuota. Idempotente; la colonna resta, mai più scritta.
.migra_luogo_reato <- function(con) {
  d <- dbGetQuery(con, "SELECT id, luogo_reato FROM fascicoli WHERE TRIM(COALESCE(luogo_reato, '')) <> ''")
  if (!nrow(d)) return(invisible(0L))
  for (k in seq_len(nrow(d))) {
    qui <- dbGetQuery(con, "SELECT luogo FROM luoghi WHERE fascicolo_id = ? ORDER BY ordine", list(d$id[k]))$luogo
    nuovi <- setdiff(separa_luoghi(d$luogo_reato[k]), qui)
    n <- length(qui)
    for (l in nuovi) { n <- n + 1L; dbExecute(con, "INSERT INTO luoghi (fascicolo_id, ordine, luogo) VALUES (?, ?, ?)", list(d$id[k], n, l)) }
  }
  dbExecute(con, "UPDATE fascicoli SET luogo_reato = NULL WHERE TRIM(COALESCE(luogo_reato, '')) <> ''")
  if (exists("registra", mode = "function")) registra("INFO", "migrazione", sprintf("luogo del reato tra i luoghi citati: %d schede", nrow(d)))
  invisible(nrow(d))
}

# Un valore del soggettario che il tesauro ha messo sotto una classe (17/11/2026: Amministrazione e Tribunali locali sotto Giurisdizione, D33) si riporta alla classe e
# il termine resta in `reato_termini`. Vale per ogni termine del profilo, non solo per quei due. Idempotente; nessun dato si perde (se la scheda ha già la classe, il
# termine si limita a uscire dai capi e a restare registrato).
.migra_termini_in_classi <- function(con) {
  if (!exists("TERMINI_REATO") || !nrow(TERMINI_REATO)) return(invisible(0L))
  n <- 0L; toccate <- character(0)
  for (i in seq_len(nrow(TERMINI_REATO))) {
    t <- TERMINI_REATO$termine[i]; cl <- TERMINI_REATO$classe[i]
    # Se il fondo ha ammesso espressamente quel valore (decisione sua, `forma` vuota) vince la decisione del fondo: la scheda non si tocca.
    schede <- dbGetQuery(con, "SELECT r.fascicolo_id FROM reati r JOIN fascicoli f ON f.id = r.fascicolo_id
                               WHERE r.reato = ? COLLATE NOCASE
                                 AND NOT EXISTS (SELECT 1 FROM vocabolario_fondo v WHERE v.fondo_id = f.fondo_id AND v.campo = 'reato'
                                                 AND v.forma IS NULL AND lower(v.valore) = lower(r.reato))", list(t))$fascicolo_id
    if (!length(schede)) next
    for (f in schede) {
      ha <- nrow(dbGetQuery(con, "SELECT 1 FROM reati WHERE fascicolo_id = ? AND reato = ?", list(f, cl))) > 0
      if (ha) dbExecute(con, "DELETE FROM reati WHERE fascicolo_id = ? AND reato = ? COLLATE NOCASE", list(f, t))
      else    dbExecute(con, "UPDATE reati SET reato = ? WHERE fascicolo_id = ? AND reato = ? COLLATE NOCASE", list(cl, f, t))
      dbExecute(con, "INSERT OR IGNORE INTO reato_termini (fascicolo_id, reato, termine) VALUES (?, ?, ?)", list(f, cl, t))
    }
    for (tab in c("persona_reati", "reato_luoghi")) {
      dbExecute(con, sprintf("UPDATE OR IGNORE %s SET reato = ? WHERE reato = ? COLLATE NOCASE", tab), list(cl, t))
      dbExecute(con, sprintf("DELETE FROM %s WHERE reato = ? COLLATE NOCASE", tab), list(t))
    }
    n <- n + length(schede); toccate <- union(toccate, schede)
    if (exists("registra", mode = "function")) registra("INFO", "migrazione", sprintf("«%s» è un termine sotto «%s»: %d schede", t, cl, length(schede)))
  }
  # Il regesto composto (non scritto a mano) segue i capi: lo si ricompone subito, così che il primo salvataggio non lo cambi di nascosto. Quello scritto a mano non si tocca.
  if (length(toccate) && exists("componi_regesto_scheda", mode = "function") && exists("leggi_fascicolo", mode = "function"))
    for (id in toccate) tryCatch({
      f <- leggi_fascicolo(con, id)
      if (!isTRUE(f$regesto_manuale == 1L)) { r <- componi_regesto_scheda(f); if (!is.na(r) && !identical(r, f$regesto)) dbExecute(con, "UPDATE fascicoli SET regesto = ? WHERE id = ?", list(r, id)) }
    }, error = function(e) NULL)
  invisible(n)
}

.applica_migrazioni <- function(con) {
  .rifai_schede_persona(con)
  .rifai_piani_busta(con)
  for (m in .migrazioni) {
    tabella <- m[1]; colonna <- m[2]; tipo <- m[3]
    presenti <- dbGetQuery(con, sprintf("PRAGMA table_info(%s)", tabella))$name
    if (!(colonna %in% presenti)) {
      dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", tabella, colonna, tipo))
      if (exists("registra", mode = "function")) registra("INFO", "migrazione", sprintf("aggiunta la colonna %s.%s", tabella, colonna))
    }
  }
  .migra_luogo_reato(con)     # 26/10/2026: «Luogo del reato» → luoghi citati
  .migra_apostrofi(con)       # 20/10/2026: «Sant’Uffizio» → «Sant'Uffizio» (D13)
  .migra_a_fondi(con)         # 19/10/2026: il fondo è un id (R/fondi.R); prima dell'indice, che lo usa
  .migra_termini_in_classi(con)   # 17/11/2026: i termini del tesauro non stanno fra le classi (D33); dopo i fondi, perché guarda le decisioni del fondo
  # Unico per segnatura, suffisso compreso (16 e 16-bis sono diversi). Si crea DOPO le migrazioni (su un database vecchio la colonna del suffisso non c'è) e si toglie l'indice di prima.
  dbExecute(con, "DROP INDEX IF EXISTS idx_segnatura")
  dbExecute(con, "DROP INDEX IF EXISTS idx_segnatura_busta_suffisso")    # 19/10/2026: era sul NOME del fondo
  dbExecute(con, "DROP INDEX IF EXISTS idx_segnatura_suffisso")       # prima del 8/10: senza il suffisso della busta
  dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_segnatura_fondo_id
                    ON fascicoli(fondo_id, busta, busta_suffisso, fascicolo, fascicolo_suffisso)")
  .indice_senza_numeri(con)
  .trigger_fondo_id(con)
}

# In un indice UNIQUE i NULL sono tutti diversi: «b. 5» senza fascicolo poteva ripetersi all'infinito. Per quelle righe un secondo indice conta il NULL come valore.
# Se ci sono già doppioni l'indice non nasce: lo dicono il registro e Qualità («Segnature doppie senza numero»); si riprova a ogni apertura. Ritorna TRUE se l'indice c'è.
.indice_senza_numeri <- function(con) {
  tryCatch({
    dbExecute(con, "CREATE UNIQUE INDEX IF NOT EXISTS idx_segnatura_senza_numeri
                      ON fascicoli(fondo_id, COALESCE(busta, -1), busta_suffisso, COALESCE(fascicolo, -1), fascicolo_suffisso)
                      WHERE busta IS NULL OR fascicolo IS NULL")
    TRUE
  }, error = function(e) {
    if (exists("registra", mode = "function"))
      registra("AVVISO", "indice delle segnature senza numero non creato", conditionMessage(e))
    FALSE
  })
}

apri_db <- function(percorso = percorso_db()) {
  con <- dbConnect(SQLite(), percorso)
  dbExecute(con, "PRAGMA foreign_keys = ON")
  # Più finestre o istanze sullo stesso file: una scrittura attende fino a 5 secondi invece di cadere con «database is locked». Niente WAL: le copie copiano il solo .sqlite.
  dbExecute(con, "PRAGMA busy_timeout = 5000")
  for (istruzione in DDL) dbExecute(con, istruzione)
  .applica_migrazioni(con)
  con
}

source("R/fondi.R")
