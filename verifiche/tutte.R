# Tutti i collaudi con un comando solo:
#
#     Rscript verifiche/tutte.R            # tutti
#     Rscript verifiche/tutte.R funzioni roundtrip   # solo quelli nominati
#
# Ogni script gira in un processo suo (come se lo lanciassi a mano), così uno che
# cade non ferma gli altri. Un collaudo ha tre esiti (23/10/2026):
#   riuscito  esce con stato 0 e non dichiara salti;
#   fallito   esce con stato diverso da 0;
#   saltato   esce con stato 0 ma stampa una riga che inizia con `SALTATO: ` e il motivo (non ha controllato niente, o non tutto):
#             nel riepilogo non conta fra i riusciti, e il motivo si legge accanto.
# Alla fine una riga per script e `ESITO: ok` oppure `ESITO: NON TORNA` con stato 1. Con `TRAMA_VERIFICA_COMPLETA=1` un collaudo
# saltato rende la verifica `ESITO: INCOMPLETO` con stato 2: serve per dire «controllato tutto», che senza i tab veri non è vero.
# L'output completo di chi fallisce viene stampato; degli altri solo l'ultima riga.
# Gli script lavorano già su copie temporanee: qui non si tocca nessun database.

# La prova della finestra di revisione in un browser vero (revisione_browser) si attiva da sola se shinytest2 e chromote ci sono (03/11/2026); altrimenti resta saltata.
if (!nzchar(Sys.getenv("TRAMA_BROWSER")) && requireNamespace("shinytest2", quietly = TRUE) && requireNamespace("chromote", quietly = TRUE)) Sys.setenv(TRAMA_BROWSER = "1")
Sys.setenv(TRAMA_COLLAUDO = "1")   # i collaudi non scrivono nel registro vero (R/registro.R)
cartella <- Sys.getenv("TRAMA_COLLAUDI_CARTELLA", "verifiche")   # un'altra cartella serve solo a provare questo riepilogo (verifiche/riepilogo_suite.R)
completa <- identical(Sys.getenv("TRAMA_VERIFICA_COMPLETA"), "1")
if (!dir.exists(cartella)) stop("Lancia dalla cartella del progetto: Rscript verifiche/tutte.R")

# I collaudi lavorano sul fondo di esempio completo (24 fascicoli, dati/esempio/esempio.xlsx). Il repository pubblico ne porta uno minimo
# (6 fascicoli): meglio dirlo subito che far cadere una dozzina di collaudi con «file non trovato». Il dataset completo si scarica a parte (README).
if (!nzchar(Sys.getenv("TRAMA_ESEMPIO_MINIMO_OK"))) {
  n_esempio <- tryCatch(nrow(openxlsx::read.xlsx(file.path("dati", "esempio", "esempio.xlsx"))), error = function(e) 0L)
  if (n_esempio < 24L) {
    cat(sprintf("ESITO: INCOMPLETO. Il fondo di esempio in dati/esempio/ ha %d fascicoli, i collaudi ne vogliono 24.\n", n_esempio),
        "Scarica il dataset di prova completo (vedi README) e mettilo in dati/esempio/, oppure rigeneralo: Rscript dati/esempio/genera_esempio.R\n", sep = "")
    quit(status = 2)
  }
}
tutti <- setdiff(sub("\\.R$", "", list.files(cartella, pattern = "\\.R$")), c("tutte", "aiuti", "indice_codice"))   # aiuti.R e indice_codice.R non sono collaudi
# Prima i più rapidi e i più generali: se cadono loro, il resto dice poco.
ordine <- c("funzioni", "roundtrip", "salvataggio_neutro", "backup_json", "export_convenzioni", "layout_xlsx")
tutti <- c(intersect(ordine, tutti), setdiff(tutti, ordine))
# Collaudi che si rifiutano di girare fuori da una copia dell'intera app (scrivono
# nel database accanto ad app.R): qui si saltano e lo si dice. Si lanciano a mano,
# sulla copia, come dice la loro prima riga.
protetti <- "ripristino_interfaccia"
saltati <- intersect(protetti, tutti); tutti <- setdiff(tutti, protetti)

scelti <- commandArgs(trailingOnly = TRUE)
if (length(scelti)) {
  scelti <- sub("\\.R$", "", basename(scelti))
  ignoti <- setdiff(scelti, c(tutti, protetti))
  if (length(ignoti)) stop("Collaudi che non esistono: ", paste(ignoti, collapse = ", "))
  tutti <- setdiff(scelti, protetti); saltati <- intersect(scelti, protetti)
}

# Il database vero non deve cambiare: i collaudi lavorano su copie (`database_di_prova()`), e qui lo si prova con
# l'impronta del file prima e dopo (12/10). Se cambia, qualcuno ha scritto: un collaudo o un'altra sessione di Trama.
impronta_db <- function() { p <- local({ source("R/percorsi.R", local = TRUE); percorso_db() }); if (file.exists(p)) unname(tools::md5sum(p)) else NA_character_ }
impronta_prima <- impronta_db()
# L'impronta del file basta solo se il database non è in modalità WAL (un `-wal` accanto terrebbe scritture fuori dal
# file): si dice quale modalità è, e si controllano integrità e chiavi esterne (12/10, su richiesta di un secondo parere).
stato_db <- function() {
  p <- local({ source("R/percorsi.R", local = TRUE); percorso_db() })
  if (!file.exists(p)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), p, flags = RSQLite::SQLITE_RO); on.exit(DBI::dbDisconnect(con))
  list(journal = DBI::dbGetQuery(con, "PRAGMA journal_mode")[[1]], wal = file.exists(paste0(p, "-wal")),
       integrita = paste(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]], collapse = "; "),
       chiavi_esterne = nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")))
}
stato_prima <- stato_db()

rscript <- file.path(R.home("bin"), "Rscript")
esiti <- data.frame(collaudo = tutti, riuscito = NA, saltato = FALSE, motivo = "", secondi = NA_real_, ultima = "", stringsAsFactors = FALSE)
# Collaudi in parallelo (03/11/2026): TRAMA_COLLAUDI_PARALLELI=N (default 4; 1 = uno alla volta, con l'output man mano). Ogni collaudo ha
# la sua cartella dati temporanea (TRAMA_HOME), così nessuno scrive dove scrive un altro e nessuno tocca la cartella dati vera.
n_par <- suppressWarnings(as.integer(Sys.getenv("TRAMA_COLLAUDI_PARALLELI", "4"))); if (is.na(n_par) || n_par < 1L) n_par <- 1L
if (.Platform$OS.type == "windows") n_par <- 1L      # mclapply non fa fork su Windows
lancia <- function(i) {
  t0 <- Sys.time()
  casa <- tempfile("trama-collaudo-"); dir.create(casa)
  on.exit(unlink(casa, recursive = TRUE), add = TRUE)
  out <- suppressWarnings(system2(rscript, shQuote(file.path(cartella, paste0(tutti[i], ".R"))), stdout = TRUE, stderr = TRUE, env = paste0("TRAMA_HOME=", shQuote(casa))))
  stato <- attr(out, "status"); if (is.null(stato)) stato <- 0L
  list(out = out, stato = as.integer(stato), secondi = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
if (n_par > 1L && length(tutti) > 1L) {
  cat(sprintf("%d collaudi, %d alla volta … ", length(tutti), n_par)); flush.console()
  # I più lenti prima (dall'ultima suite non si sa: i tre noti), così la coda finale è corta.
  lenti <- intersect(c("tab_reali", "corpus_normalizzazione", "normalizzazione_prova"), tutti)
  ordine_lancio <- c(match(lenti, tutti), setdiff(seq_along(tutti), match(lenti, tutti)))
  ris <- vector("list", length(tutti)); ris[ordine_lancio] <- parallel::mclapply(ordine_lancio, lancia, mc.cores = n_par, mc.preschedule = FALSE)
  cat("fatto\n")
} else ris <- NULL
for (i in seq_along(tutti)) {
  if (is.null(ris)) { cat(sprintf("[%d/%d] %s … ", i, length(tutti), tutti[i])); flush.console(); r <- lancia(i) } else r <- ris[[i]]
  if (inherits(r, "try-error") || is.null(r)) r <- list(out = "il collaudo non è partito", stato = 1L, secondi = NA_real_)
  out <- r$out; stato <- r$stato
  esiti$secondi[i] <- r$secondi
  esiti$riuscito[i] <- identical(as.integer(stato), 0L)
  righe <- out[nzchar(trimws(out))]
  esiti$ultima[i] <- if (length(righe)) trimws(righe[length(righe)]) else ""
  salti <- sub("^SALTATO: *", "", grep("^SALTATO: ", out, value = TRUE))
  if (esiti$riuscito[i] && length(salti)) { esiti$saltato[i] <- TRUE; esiti$motivo[i] <- paste(salti, collapse = " · ") }
  if (!is.null(ris)) cat(sprintf("[%d/%d] %s … ", i, length(tutti), tutti[i]))
  cat(if (!esiti$riuscito[i]) "NON TORNA" else if (esiti$saltato[i]) "saltato" else "ok", sprintf("(%.0f s)\n", esiti$secondi[i]))
  if (!esiti$riuscito[i]) cat(paste0("    ", out), sep = "\n")
}

cat("\n")
for (i in seq_len(nrow(esiti))) {
  if (esiti$saltato[i]) cat(sprintf("%-10s %-24s %4.0f s  %s\n", "saltato", esiti$collaudo[i], esiti$secondi[i], esiti$motivo[i]))
  else cat(sprintf("%-10s %-24s %4.0f s  %s\n", if (esiti$riuscito[i]) "ok" else "NON TORNA", esiti$collaudo[i], esiti$secondi[i],
                   substr(esiti$ultima[i], 1, 70)))
}
for (x in saltati) cat(sprintf("%-10s %-24s        solo su una copia dell'app, a mano\n", "saltato", x))
falliti <- esiti$collaudo[!esiti$riuscito]
non_eseguiti <- c(esiti$collaudo[esiti$saltato], saltati)
impronta_dopo <- impronta_db()
stato_dopo <- stato_db()
if (!is.null(stato_dopo)) {
  buono <- identical(stato_dopo$integrita, "ok") && stato_dopo$chiavi_esterne == 0L
  cat(sprintf("%-10s  il database vero: journal «%s»%s · integrity_check: %s · chiavi esterne non valide: %d\n", if (buono) "ok" else "NON TORNA",
              stato_dopo$journal, if (stato_dopo$wal) " (c'è un file -wal: l'impronta del solo file non basta)" else "", stato_dopo$integrita, stato_dopo$chiavi_esterne))
  if (!buono) falliti <- c(falliti, "integrita_database_vero")
}
if (!is.na(impronta_prima) && !identical(impronta_prima, impronta_dopo)) {
  cat("NON TORNA   il database vero è cambiato durante la suite (impronta diversa): un collaudo ha scritto sul database vero, o un'altra sessione di Trama lo stava usando\n")
  falliti <- c(falliti, "database_vero_invariato")
} else if (!is.na(impronta_prima)) cat("ok          il database vero è invariato (stessa impronta prima e dopo)\n")
cat(sprintf("\n%d collaudi: %d riusciti, %d falliti, %d saltati · %.0f s\n", nrow(esiti) + length(saltati), sum(esiti$riuscito & !esiti$saltato),
            length(falliti), length(non_eseguiti), sum(esiti$secondi)))
if (length(falliti)) {
  cat("ESITO: NON TORNA (", paste(falliti, collapse = ", "), ")\n", sep = "")
  quit(status = 1)
}
if (length(non_eseguiti)) {
  if (completa) { cat("ESITO: INCOMPLETO (non eseguiti: ", paste(non_eseguiti, collapse = ", "), ")\n", sep = ""); quit(status = 2) }
  cat("ESITO: ok - ma ", length(non_eseguiti), " non eseguiti: ", paste(non_eseguiti, collapse = ", "), "\n", sep = "")
} else cat("ESITO: ok\n")
