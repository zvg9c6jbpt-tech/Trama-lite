# Il fondo di esempio (dati/esempio/, 5/10): il foglio si importa per intero in
# un database vuoto, non entra mai in uno che ha già fascicoli, e non contiene
# nulla dei fondi veri. Lavora su database in memoria.
#
#     Rscript verifiche/esempio.R

suppressMessages({
  library(shiny); library(bslib); library(DBI)
  for (f in c("percorsi", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo", "qualita", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "attribuzione", "home"))
    source(file.path("R", paste0(f, ".R")))
})
falliti <- character(0)
controlla <- function(nome, ok) { cat(if (isTRUE(ok)) "ok        " else "NON TORNA ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

controlla("il foglio di esempio esiste", file.exists(FOGLIO_ESEMPIO))
con <- apri_db(":memory:")
e <- carica_esempio(con)
controlla("24 righe lette, 24 importate, nessuna respinta", identical(c(e$letti, e$importati, nrow(e$respinti)), c(24L, 24L, 0L)))
d <- dbGetQuery(con, "SELECT fondo, archivio, COUNT(*) AS n, COUNT(DISTINCT busta) AS buste FROM fascicoli GROUP BY fondo, archivio")
controlla("un fondo solo, tre buste", nrow(d) == 1 && d$n == 24 && d$buste == 3 && d$fondo == "Tribunale di esempio")
controlla("titoli e regesti composti dall'app", dbGetQuery(con, "SELECT COUNT(*) AS n FROM fascicoli WHERE titolo LIKE 'Processo contro %'")$n >= 8)
controlla("ci sono schede incomplete, per le code di lavoro",
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM fascicoli f WHERE NOT EXISTS (SELECT 1 FROM reati r WHERE r.fascicolo_id = f.id)
                                                                 OR NOT EXISTS (SELECT 1 FROM luoghi l WHERE l.fascicolo_id = f.id)")$n >= 2)
secondo <- tryCatch(carica_esempio(con), error = function(e) conditionMessage(e))
controlla("in un database che ha già fascicoli non si carica", is.character(secondo) && grepl("database vuoto", secondo) &&
          dbGetQuery(con, "SELECT COUNT(*) AS n FROM fascicoli")$n == 24)
# Nessun nome dei fondi veri: il foglio deve potersi pubblicare.
testo <- paste(unlist(openxlsx::read.xlsx(FOGLIO_ESEMPIO, sep.names = " ")), collapse = " ")
controlla("nessun riferimento agli archivi veri", !grepl("Napoli|Capua|Modena, Inquisizione|Sant'Uff|Diocesano", testo))
dbDisconnect(con)

cat("\n")
if (length(falliti)) { cat("ESITO: NON TORNA\n"); if (!interactive()) quit(status = 1) else stop("verifiche fallite") }
cat("ESITO: ok\n")
