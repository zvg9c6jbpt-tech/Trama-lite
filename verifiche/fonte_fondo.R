# ──────────────────────────────────────────────
# verifiche/fonte_fondo.R: da dove vengono i dati di un fondo (24/10/2026, Stefano)
# Una riga di testo libero per fondo, facoltativa: si scrive in «Nuovo fondo», in «Importa nuovo» e nella casella di Import / Export;
# viaggia con l'export (proprietà del file) e col backup JSON. Nessuna cella del modello cambia.
#     Rscript verifiche/fonte_fondo.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
FONTE <- "Inventario dell'Archivio di Prova, trascritto dal gruppo di lavoro, con accordo UNIMORE"
proprieta_di <- function(xlsx) { d <- tempfile(); utils::unzip(xlsx, "docProps/core.xml", exdir = d); paste(readLines(file.path(d, "docProps/core.xml"), warn = FALSE), collapse = "") }

con <- apri_db(":memory:")
id <- crea_archivio(con, "Archivio di Prova", "Fondo con fonte", fonte = paste0("  ", FONTE, "  \n"))
id2 <- crea_archivio(con, "Archivio di Prova", "Fondo senza fonte")
v("«Nuovo fondo» con la fonte: si ricorda, ripulita da spazi e a capo", identical(fonte_del_fondo(con, id), FONTE))
v("senza fonte: vuota, e il fondo si crea lo stesso", identical(fonte_del_fondo(con, id2), "") && nrow(elenco_fondi(con)) == 2L)
imposta_fonte_fondo(con, id2, "Fotografie dell'inventario cartaceo")
v("si imposta su un fondo già esistente, senza toccare l'altro", identical(fonte_del_fondo(con, id2), "Fotografie dell'inventario cartaceo") && identical(fonte_del_fondo(con, id), FONTE))
imposta_fonte_fondo(con, id2, "")
v("svuotata la casella, la fonte si toglie", identical(fonte_del_fondo(con, id2), ""))
troppo <- try(imposta_fonte_fondo(con, id2, strrep("a", FONTE_MAX + 1L)), silent = TRUE)
v("un testo troppo lungo è rifiutato in italiano e non si salva", inherits(troppo, "try-error") && grepl("troppo lunga", as.character(troppo)) && identical(fonte_del_fondo(con, id2), ""))
tr2 <- try(crea_archivio(con, "Altro", "Lungo", fonte = strrep("a", FONTE_MAX + 1L)), silent = TRUE)
v("…e «Nuovo fondo» con un testo troppo lungo non lascia un fondo a metà", inherits(tr2, "try-error") && nrow(elenco_fondi(con)) == 2L)

# l'export: la fonte è nelle proprietà del file, nessuna cella cambia
a <- leggi_arianna(FOGLIO_ESEMPIO); a$dati <- a$dati[1:2, , drop = FALSE]
e <- importa_nuovo_archivio(con, a, "Archivio Nuovo", "Dataset con fonte", NULL, fonte = FONTE)
fid <- elenco_fondi(con)$id[elenco_fondi(con)$fondo == "Dataset con fonte"]
v("«Importa nuovo» porta la fonte nel fondo creato", e$importati == 2L && identical(fonte_del_fondo(con, fid), FONTE))
x <- tempfile(fileext = ".xlsx"); esporta_arianna(con, x, fondo = fid)
xml <- proprieta_di(x)
v("l'export dice la fonte nelle proprietà del foglio (Oggetto)", grepl("Fonte dei dati: Inventario dell&apos;Archivio di Prova|Fonte dei dati: Inventario dell'Archivio di Prova", xml) && grepl("UNIMORE", xml))
importa_nuovo_archivio(con, a, "Archivio Nuovo", "Dataset senza fonte", NULL)
senza <- elenco_fondi(con)$id[elenco_fondi(con)$fondo == "Dataset senza fonte"]
v("senza fonte il foglio non dice «Fonte dei dati»", { y <- tempfile(fileext = ".xlsx"); esporta_arianna(con, y, fondo = senza); !grepl("Fonte dei dati", proprieta_di(y)) })

# il backup JSON la porta e il ripristino la rimette
bk <- tempfile(fileext = ".json"); esporta_backup_json(con, bk)
v("il backup JSON porta la fonte del fondo", grepl("UNIMORE", paste(readLines(bk, warn = FALSE), collapse = "")))
con2 <- apri_db(":memory:"); ripristina_backup_json(con2, bk, "accoda")
fid2 <- elenco_fondi(con2)$id[elenco_fondi(con2)$fondo == "Dataset con fonte"]
v("il ripristino in un database vuoto rimette la fonte", length(fid2) == 1L && identical(fonte_del_fondo(con2, fid2), FONTE))
# un fondo già presente con la sua fonte la conserva; senza, la prende dal backup
imposta_fonte_fondo(con2, fid2, "Fonte scritta qui")
ripristina_backup_json(con2, bk, "accoda")
v("un fondo che ha già la sua fonte non la perde nel ripristino", identical(fonte_del_fondo(con2, fid2), "Fonte scritta qui"))
# un database di prima, senza la colonna: la migrazione la aggiunge
vecchio <- tempfile(fileext = ".sqlite"); cv <- apri_db(vecchio); dbExecute(cv, "ALTER TABLE fondi DROP COLUMN fonte"); dbDisconnect(cv)
cv <- apri_db(vecchio); v("un database senza la colonna «fonte» la riceve all'apertura", "fonte" %in% dbGetQuery(cv, "PRAGMA table_info(fondi)")$name)

# Dall'app vera (testServer su database e cartella dati temporanei): la casella di Import / Export e «Nuovo fondo».
copione <- tempfile(fileext = ".R")
writeLines(c(
  'db <- tempfile(fileext = ".sqlite"); Sys.setenv(TRAMA_DB = db, TRAMA_HOME = tempfile("h")); suppressMessages(library(shiny))',
  'fonte <- function(nome) { c2 <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(c2)); x <- DBI::dbGetQuery(c2, "SELECT fonte FROM fondi WHERE denominazione = ?", list(nome))$fonte; if (length(x) && !is.na(x)) x else "<vuota>" }',
  'shiny::testServer(".", {',
  '  session$setInputs(home_esempio = 1); session$flushReact(); nome <- DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), db), "SELECT denominazione FROM fondi")$denominazione[1]',
  '  session$setInputs(fonte_fondo = "  Inventario di prova,   trascritto  "); session$flushReact(); cat("RIS dopo:", fonte(nome), "\\n")',
  '  session$setInputs(fonte_fondo = strrep("x", 600)); session$flushReact(); cat("RIS lunga:", fonte(nome), "\\n")',
  '  session$setInputs(fonte_fondo = ""); session$flushReact(); cat("RIS vuota:", fonte(nome), "\\n")',
  '  session$setInputs(home_nuovo_archivio = 1); session$setInputs(arch_archivio = "Archivio Z", arch_fondo = "Fondo Z", arch_serie = "", arch_fonte = "Dal registro cartaceo"); session$setInputs(arch_crea = 1); session$flushReact()',
  '  cat("RIS nuovo:", fonte("Fondo Z"), "\\n")', '})'), copione)
ris <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(copione), stdout = TRUE, stderr = FALSE)); ris <- trimws(sub("^RIS ", "", grep("^RIS ", ris, value = TRUE)))
v("app: la casella salva la fonte del fondo scelto, ripulita", identical(ris[1], "dopo: Inventario di prova, trascritto"))
v("app: un testo troppo lungo non si salva (resta quello di prima)", identical(ris[2], "lunga: Inventario di prova, trascritto"))
v("app: svuotare la casella toglie la fonte", identical(ris[3], "vuota: <vuota>"))
v("app: «Nuovo fondo» salva la fonte scritta nella finestra", identical(ris[4], "nuovo: Dal registro cartaceo"))
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
