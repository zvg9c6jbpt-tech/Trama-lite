# ─────────────────────────────────────────────────────────────
# verifiche/alias_luoghi.R: gli alias dei luoghi del profilo (D34)
# «Casale» nel tab di Torino è Casale Monferrato: la concordanza lo scrive, lo registra con la ragione e non chiede nulla (prima restava «Casale»,
# una delle frazioni con quel nome). In un altro archivio «Casale» resta com'è. Un luogo scritto bene non si tocca.
#     Rscript verifiche/alias_luoghi.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
mod <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
riga <- function(archivio, seg, luoghi) { r <- setNames(rep(NA_character_, length(mod)), mod)
  r["Segnatura"] <- paste0(archivio, ", Fondo di Prova, ", seg); r["Titolo"] <- "Processo per magia"; r["Regesto o descrizione sintetica"] <- "Atti per magia."
  r["Luoghi citati o indicizzati"] <- luoghi; r["Estremo remoto"] <- "1600"; r["Tipologia documentaria"] <- "Atti"; r["Soggettario"] <- "Magia"; r["Lingua"] <- "italiano"
  as.data.frame(as.list(r), check.names = FALSE) }
d <- rbind(riga("Archivio di Stato di Torino", "b. 1, fasc. 1", "Casale"),
           riga("Archivio di Stato di Torino", "b. 1, fasc. 2", "Torino|Casale"),
           riga("Archivio di Stato di Torino", "b. 1, fasc. 3", "Casale Monferrato"),
           riga("Archivio di Stato di Modena", "b. 1, fasc. 1", "Modena|Casale"))
f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, f)
cr <- concordanza_righe(leggi_arianna(f)); L <- cr$dati[["Luoghi citati o indicizzati"]]
v("Torino: «Casale» → Casale Monferrato", identical(L[1], "Casale Monferrato"))
v("Torino: dentro un elenco, gli altri luoghi restano", identical(L[2], "Torino|Casale Monferrato"))
v("Torino: un luogo già scritto bene non si tocca", identical(L[3], "Casale Monferrato"))
v("Modena: «Casale» resta com'era", identical(L[4], "Modena|Casale"))
reg <- cr$registro
v("registrato con la ragione (2 righe di Torino, nessuna di Modena)", sum(grepl("alias del luogo nel profilo", reg$perche)) == 2L && all(reg$riga[grepl("alias del luogo", reg$perche)] %in% c(2L, 3L)))
# idempotente: rielaborare l'uscita non cambia nulla
a2 <- leggi_arianna(f); a2$dati <- cr$dati; cr2 <- concordanza_righe(a2)
v("idempotente", identical(cr2$dati[["Luoghi citati o indicizzati"]], L))
if (length(falliti)) { cat("\nFALLITI:", length(falliti), "\n"); quit(status = 1) }
cat("ESITO: ok\n")
