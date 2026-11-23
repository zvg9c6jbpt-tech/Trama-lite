# ─────────────────────────────────────────────────────────────
# registro.R: il registro di Trama
# Un file di testo nella CARTELLA DATI (mai in quella del programma, che può essere di sola lettura): <cartella_dati>/log/trama.log (+ trama.log.1 … .3, la rotazione).
# Una riga per evento: «2026-10-13 10:42:05 [ERRORE] salvataggio: <dettaglio breve>». Serve a capire dopo che cosa è andato storto
# (salvataggio, copia di sicurezza, import, ripristino, cancellazione, migrazione dello schema).
# Regole:
#  - **Mai nomi di persone, titoli o regesti**: solo eventi, numeri, nomi di fondo e di file. Il dettaglio è tagliato a 200 caratteri.
#  - **Non rompe mai niente**: ogni errore di scrittura è inghiottito.
#  - Non scrive durante i collaudi (`TRAMA_DB`, `TRAMA_DEFAULT_DI_PROVA`, `TRAMA_COLLAUDO`), salvo `TRAMA_REGISTRO=1`.
#  - Rotazione: oltre 512 KB il file diventa `.1`, il `.1` diventa `.2`, ecc.; se ne tengono tre.
# ─────────────────────────────────────────────────────────────
REGISTRO_MAX_BYTE <- 512 * 1024
REGISTRO_COPIE    <- 3L

registro_percorso <- function() file.path(cartella_dati(), "log", "trama.log")

# Spento nei collaudi: con un database di prova (`TRAMA_DB`), i valori di prova della scheda vuota (`TRAMA_DEFAULT_DI_PROVA`) o `TRAMA_COLLAUDO` (lo mettono tutte.R e aiuti.R).
.registro_attivo <- function() nzchar(Sys.getenv("TRAMA_REGISTRO", "")) ||
  !any(nzchar(Sys.getenv(c("TRAMA_DB", "TRAMA_DEFAULT_DI_PROVA", "TRAMA_COLLAUDO"), "")))

.registro_ruota <- function(p, max_byte = REGISTRO_MAX_BYTE, copie = REGISTRO_COPIE) {
  if (!file.exists(p) || is.na(file.size(p)) || file.size(p) <= max_byte) return(invisible(FALSE))
  for (i in rev(seq_len(copie))) {
    da <- if (i == 1L) p else paste0(p, ".", i - 1L)
    a  <- paste0(p, ".", i)
    if (file.exists(da)) file.copy(da, a, overwrite = TRUE, copy.mode = FALSE)
  }
  cat("", file = p)   # il corrente riparte vuoto
  invisible(TRUE)
}

# registra("ERRORE", "salvataggio", "UNIQUE constraint failed…"). Livelli: INFO, AVVISO, ERRORE.
registra <- function(livello = "INFO", evento, dettaglio = "", percorso = registro_percorso()) {
  if (!.registro_attivo()) return(invisible(FALSE))
  tryCatch({
    d <- gsub("[\r\n\t]+", " ", paste(as.character(dettaglio), collapse = " "))
    if (nchar(d) > 200) d <- paste0(substr(d, 1, 200), "…")
    dir.create(dirname(percorso), recursive = TRUE, showWarnings = FALSE)
    .registro_ruota(percorso)
    riga <- sprintf("%s [%s] %s%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), toupper(livello), evento,
                    if (nzchar(d)) paste0(": ", d) else "")
    suppressWarnings(cat(riga, "\n", sep = "", file = percorso, append = TRUE))
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}
