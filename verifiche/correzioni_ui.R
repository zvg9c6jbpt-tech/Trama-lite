# Regressioni UI del 28/09: solo database in memoria o copie temporanee.
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages(for (f in list.files("R", pattern="[.]R$", full.names=TRUE)) source(f))
# Scegliere per gruppi nella finestra delle correzioni (22/10/2026): i tipi, i filtri, i menu con i conti
stopifnot(identical(.tipo_correzione("senza cognome certo: non va fra le persone citate (Modena, Lodovico da) \u00b7 decidi tu: non è spuntata"), c("senza cognome certo", "decidi tu")),
          identical(.tipo_correzione("nella lingua del paese (Koln \u2192 K\u00f6ln (Colonia))"), "nella lingua del paese"), identical(.tipo_correzione(""), character(0)))
con <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- 1L
f$persone <- c("Rossi, Mario", "Rosso, Mario")
f$reati <- c("Eresia", "Magia")
f$imputati_esaminati <- 1L
stopifnot(!length(persone_imputate(f)), !length(.imputati_scheda(f)))
stopifnot(!length(assi_coppie("persona_reati", list(persona=f$persone, imputati_esaminati=TRUE))$righe))
salva_fascicolo(con, f)
g <- leggi_fascicolo(con, f$id)
stopifnot(g$imputati_esaminati == 1L, !length(persone_imputate(g)))
h <- raccogli_modulo(valori_da_fascicolo(g)); h$id <- g$id
salva_fascicolo(con, h)
stopifnot(leggi_fascicolo(con, f$id)$imputati_esaminati == 1L)
j <- tempfile(fileext=".json"); esporta_backup_json(con, j)
b <- apri_db(":memory:"); invisible(ripristina_backup_json(b, j))
stopifnot(identical(leggi_fascicolo(con, f$id), leggi_fascicolo(b, f$id)))
# Vecchio backup senza flag: le persone citate sono le persone della scheda (nessuna distinzione di ruolo dal 19/10 sera).
x <- jsonlite::fromJSON(j, simplifyVector=FALSE); x$fascicoli[[1]]$imputati_esaminati <- NULL
writeLines(jsonlite::toJSON(x, auto_unbox=TRUE, null="null", na="null"), j)
invisible(ripristina_backup_json(b, j, "sostituisci"))
stopifnot(identical(persone_imputate(leggi_fascicolo(b, f$id)), f$persone))
# Precedente: suffissi, cambio busta, numerazione modificata, valori vuoti.
c <- apri_db(":memory:")
for (suff in c("", "bis", "ter", "quater", "x")) {
  z <- fascicolo_vuoto(); z$id <- paste0("n16", suff); z$busta <- 1L; z$fascicolo <- 16L
  z$fascicolo_suffisso <- suff; z$estremo_remoto <- if (suff == "bis") 1571L else if (suff == "") 1570L else NA_integer_; salva_fascicolo(c, z)
}
fondo <- z$fondo
stopifnot(is.null(fascicolo_precedente(c, fondo, 1, 16)),
          fascicolo_precedente(c, fondo, 1, 16, suffisso="bis")$id == "n16",
          fascicolo_precedente(c, fondo, 1, 16, suffisso="ter")$id == "n16bis",
          fascicolo_precedente(c, fondo, 1, 16, suffisso="quater")$id == "n16ter",
          fascicolo_precedente(c, fondo, 1, 17)$id == "n16x",
          fascicolo_precedente(c, fondo, 2, 1)$id == "n16x",
          fascicolo_precedente(c, fondo, 1, 16, escludi="n16ter", suffisso="quater")$id == "n16bis",
          is.null(fascicolo_precedente(c, fondo, NULL, 1)))
stopifnot(identical(anno_precedente(c, fondo, 1, 16, suffisso="ter"), list(anno=1571L, da="b.1 f.16bis")),
          anno_precedente(c, fondo, 1, 17)$anno == 1571L)
dbDisconnect(c)
dbDisconnect(con); dbDisconnect(b)
cat("Correzioni UI: dati, attribuzioni e compatibilità JSON verificati.\n")
