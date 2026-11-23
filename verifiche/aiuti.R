# ─────────────────────────────────────────────────────────────
# verifiche/aiuti.R: cio' che i collaudi hanno in comune (8/10)
#
# database_di_prova(): il database su cui lavora un collaudo, SEMPRE una copia
# temporanea scrivibile, mai il database vero e mai un file dentro la cartella del
# programma (il programma deve poter stare in una cartella di sola lettura):
#   - di norma una copia del database vero dell'utente (cartella dati predefinita,
#     senza guardare TRAMA_HOME: serve a isolare le SCRITTURE, non i dati);
#   - se non c'e' (computer pulito, integrazione continua) o se si chiede
#     TRAMA_DB_COLLAUDI=esempio: il fondo di esempio inventato (dati/esempio/);
#   - TRAMA_DB_COLLAUDI=<percorso>: un altro database da copiare.
# ─────────────────────────────────────────────────────────────
source("R/percorsi.R")
Sys.setenv(TRAMA_COLLAUDO = "1")   # i collaudi non scrivono nel registro vero (R/registro.R)
database_di_prova <- function() {
  scelta <- Sys.getenv("TRAMA_DB_COLLAUDI", "")
  # Il database vero: trama.sqlite (dal 13/10/2026) o, se l'app non è ancora partita col nome nuovo, regestum.sqlite.
  vero <- if (nzchar(scelta) && scelta != "esempio") scelta else {
    nuovo <- file.path(cartella_dati_predefinita(), "trama.sqlite")
    if (file.exists(nuovo)) nuovo else file.path(cartella_dati_predefinita(), "regestum.sqlite")
  }
  copia <- tempfile(fileext = ".sqlite")
  if (!identical(scelta, "esempio") && file.exists(vero)) {
    a <- DBI::dbConnect(RSQLite::SQLite(), vero, flags = RSQLite::SQLITE_RO)
    b <- DBI::dbConnect(RSQLite::SQLite(), copia)
    RSQLite::sqliteCopyDatabase(a, b); DBI::dbDisconnect(a); DBI::dbDisconnect(b)
    cat("[collaudo su una copia del database vero]\n")
  } else {
    suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
    con <- apri_db(copia); invisible(carica_esempio(con)); DBI::dbDisconnect(con)
    cat("[collaudo sul fondo di esempio]\n")
  }
  Sys.chmod(copia, "0600")
  copia
}
