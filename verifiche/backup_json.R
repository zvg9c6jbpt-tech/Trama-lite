# ─────────────────────────────────────────────────────────────
# verifiche/backup_json.R: il backup JSON deve restituire esattamente
# ciò che c'era: ogni fascicolo (campi extra e timestamp compresi) e
# ogni piano di busta, senza passare dal profilo Arianna.
#
# Lavora su COPIE temporanee: il database vero non viene né aperto in
# scrittura né toccato.
#
# Uso, dalla cartella del progetto:
#     Rscript verifiche/backup_json.R
# Atteso: 0 differenze. Se no esce con stato 1 (con source() in RStudio
# si ferma con un errore).
# ─────────────────────────────────────────────────────────────

suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
source("verifiche/aiuti.R")

fallito <- FALSE
segnala <- function(messaggio) {
  cat("NON TORNA:", messaggio, "\n")
  fallito <<- TRUE
}

# --- giro 1: esporta dal database vero (in copia), ripristina in uno vuoto ---

origine_copia <- database_di_prova()
con_origine <- apri_db(origine_copia)
# La sigla della busta («aa.», Bologna) e quella del fascicolo devono fare il giro del backup:
# il 8/10 il collaudo sul database vero ha trovato che si perdevano (110 schede tornavano «b.»).
invisible(dbExecute(con_origine, "UPDATE fascicoli SET sigla_busta = 'aa.', sigla_fascicolo = 'reg.' WHERE id = (SELECT id FROM fascicoli ORDER BY id LIMIT 1)"))

# Le tabelle delle schede persona restano nello schema (compatibilità con le copie di Trama) e il giro le deve conservare uguali.

json <- tempfile(fileext = ".json")
esporta_backup_json(con_origine, json)

con_vuoto <- apri_db(tempfile(fileext = ".sqlite"))
esito <- ripristina_backup_json(con_vuoto, json, modo = "sostituisci")

ids_origine <- dbGetQuery(con_origine, "SELECT id FROM fascicoli ORDER BY id")$id
ids_ripristinati <- dbGetQuery(con_vuoto, "SELECT id FROM fascicoli ORDER BY id")$id

cat(sprintf("fascicoli letti: %d · importati: %d · respinti: %d\n",
            esito$letti, esito$importati, length(esito$respinti)))

if (!identical(ids_origine, ids_ripristinati)) {
  segnala(sprintf("elenco fascicoli diverso: %d nell'origine, %d nel ripristino",
                   length(ids_origine), length(ids_ripristinati)))
}

diversi <- character(0)
for (id in intersect(ids_origine, ids_ripristinati)) {
  a <- leggi_fascicolo(con_origine, id)
  b <- leggi_fascicolo(con_vuoto, id)
  row.names(a$imputati) <- NULL
  row.names(b$imputati) <- NULL
  # Per nome, non per posizione: un ALTER TABLE aggiunge sempre le
  # colonne in fondo, una CREATE TABLE le mette dove sta nel DDL - stesso
  # contenuto, ordine diverso fra un database migrato e uno nuovo.
  if (!isTRUE(all.equal(a[sort(names(a))], b[sort(names(b))]))) diversi <- c(diversi, id)
}
if (length(diversi)) segnala(sprintf("%d fascicoli diversi dopo il ripristino: %s",
                                     length(diversi), paste(head(diversi, 5), collapse = ", ")))

piani_a <- dbGetQuery(con_origine, "SELECT * FROM piani_busta ORDER BY fondo_id, busta")
piani_b <- dbGetQuery(con_vuoto,   "SELECT * FROM piani_busta ORDER BY fondo_id, busta")
if (!isTRUE(all.equal(piani_a, piani_b, check.attributes = FALSE)))
  segnala("piani_busta diversi dopo il ripristino")

schede_a <- dbGetQuery(con_origine, "SELECT id, forma_canonica, note, creato, modificato FROM schede_persona ORDER BY id")
schede_b <- dbGetQuery(con_vuoto,   "SELECT id, forma_canonica, note, creato, modificato FROM schede_persona ORDER BY id")
if (!isTRUE(all.equal(schede_a, schede_b, check.attributes = FALSE)))
  segnala(sprintf("schede_persona diverse dopo il ripristino: %d nell'origine, %d nel ripristino",
                  nrow(schede_a), nrow(schede_b)))

varianti_a <- dbGetQuery(con_origine, "SELECT scheda_id, variante FROM persona_varianti ORDER BY 1, 2")
varianti_b <- dbGetQuery(con_vuoto,   "SELECT scheda_id, variante FROM persona_varianti ORDER BY 1, 2")
if (!isTRUE(all.equal(varianti_a, varianti_b, check.attributes = FALSE)))
  segnala("persona_varianti diverse dopo il ripristino")

assorbiti_a <- dbGetQuery(con_origine, "SELECT scheda_id, id_assorbito FROM persona_assorbiti ORDER BY 1, 2")
assorbiti_b <- dbGetQuery(con_vuoto,   "SELECT scheda_id, id_assorbito FROM persona_assorbiti ORDER BY 1, 2")
if (!isTRUE(all.equal(assorbiti_a, assorbiti_b, check.attributes = FALSE)))
  segnala("persona_assorbiti diversi dopo il ripristino")

cat(sprintf("schede persona: %d · varianti: %d · assorbiti: %d\n",
            nrow(schede_a), nrow(varianti_a), nrow(assorbiti_a)))

cat(if (length(diversi) == 0 && identical(ids_origine, ids_ripristinati)) "giro 1 (sostituisci): ok\n"
    else "giro 1 (sostituisci): NON TORNA\n")


# --- giro 2: "accoda" su id già presenti va tutto ai respinti, senza toccare nulla ---

prima <- dbGetQuery(con_vuoto, "SELECT COUNT(*) AS n FROM fascicoli")$n
esito2 <- ripristina_backup_json(con_vuoto, json, modo = "accoda")
dopo <- dbGetQuery(con_vuoto, "SELECT COUNT(*) AS n FROM fascicoli")$n

if (dopo != prima) {
  segnala(sprintf("accoda con id duplicati ha cambiato il conteggio: %d -> %d", prima, dopo))
} else if (esito2$importati != 0 || length(esito2$respinti) != esito2$letti) {
  segnala("accoda con id duplicati non ha respinto tutto come atteso")
} else {
  cat("giro 2 (accoda su duplicati, tutti respinti, nulla cambia): ok\n")
}

dbDisconnect(con_origine)
dbDisconnect(con_vuoto)

# --- la data di creazione del fondo fa il giro del backup (23/10, dalla verifica nel browser) ---
for (modo_ in c("sostituisci", "accoda")) {
  o_ <- apri_db(tempfile(fileext = ".sqlite")); invisible(salva_fascicolo(o_, { f_ <- fascicolo_vuoto(FALSE); f_$archivio <- "Archivio Data"; f_$fondo <- "Fondo Data"; f_$busta <- 1L; f_$fascicolo <- 1L; f_$titolo <- "Prova"; f_ }))
  dbExecute(o_, "UPDATE fondi SET creato = '2020-01-02T03:04:05'")
  pj_ <- tempfile(fileext = ".json"); esporta_backup_json(o_, pj_)
  d_ <- apri_db(tempfile(fileext = ".sqlite"))
  if (identical(modo_, "sostituisci")) { invisible(salva_fascicolo(d_, { f_ <- fascicolo_vuoto(FALSE); f_$archivio <- "Altro"; f_$fondo <- "Altro"; f_$busta <- 1L; f_$fascicolo <- 1L; f_ })) }
  ripristina_backup_json(d_, pj_, modo = modo_)
  cr_ <- dbGetQuery(d_, "SELECT creato FROM fondi WHERE denominazione = 'Fondo Data'")$creato
  if (!identical(cr_, "2020-01-02T03:04:05")) segnala(sprintf("la data di creazione del fondo non fa il giro del backup (%s): %s", modo_, paste(cr_, collapse = ",")))
  dbDisconnect(o_); dbDisconnect(d_)
}

# --- giro 3 (23/10/2026): il giro intero dell'utente, backup JSON → ripristino → export del foglio a 28 colonne ---
# Per ogni fondo il foglio esportato dopo il ripristino (accoda e sostituisci) è uguale, cella per cella, a quello di prima.
local({
  leggi_ <- function(p) as.data.frame(readxl::read_excel(p, col_types = "text"), check.names = FALSE)
  o_ <- apri_db(database_di_prova()); on.exit(dbDisconnect(o_), add = TRUE)
  fondi_ <- elenco_fondi(o_); js_ <- tempfile(fileext = ".json"); esporta_backup_json(o_, js_)
  prima_ <- lapply(fondi_$id, function(i) { x <- tempfile(fileext = ".xlsx"); esporta_arianna(o_, x, fondo = i); leggi_(x) })
  for (modo_ in c("accoda", "sostituisci")) {
    d_ <- apri_db(tempfile(fileext = ".sqlite"))
    if (modo_ == "sostituisci") ripristina_backup_json(d_, js_, "accoda")
    invisible(ripristina_backup_json(d_, js_, modo_, cartella_sicurezza = tempfile()))
    for (k_ in seq_along(fondi_$id)) {
      if (!fondi_$id[k_] %in% elenco_fondi(d_)$id) { segnala(sprintf("giro intero (%s): il fondo %d non c'è dopo il ripristino", modo_, k_)); next }
      x <- tempfile(fileext = ".xlsx"); esporta_arianna(d_, x, fondo = fondi_$id[k_]); dopo_ <- leggi_(x); a_ <- prima_[[k_]]
      if (!identical(dim(a_), dim(dopo_)) || any(is.na(a_) != is.na(dopo_) | (!is.na(a_) & !is.na(dopo_) & a_ != dopo_)))
        segnala(sprintf("giro intero (%s): il foglio del fondo %d esportato dopo il ripristino non è uguale a quello di prima", modo_, k_))
    }
    dbDisconnect(d_)
  }
})

cat(if (!fallito) "ESITO: ok\n" else "ESITO: NON TORNA\n")
if (fallito) {
  if (interactive()) stop("il backup JSON NON TORNA") else quit(status = 1)
}
