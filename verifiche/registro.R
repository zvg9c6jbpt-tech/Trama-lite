# ──────────────────────────────────────────────
# verifiche/registro.R: il registro di Trama (13/10)
# Scrive nella cartella dati (mai nel programma), una riga per evento, ruota oltre la soglia,
# taglia il dettaglio, non rompe mai e non scrive nei collaudi con un database di prova.
#     Rscript verifiche/registro.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("R/percorsi.R"); source("R/registro.R")
falliti <- character(); v <- function(nome, ok) { cat(if (ok) "ok  " else "NO  ", nome, "\n"); if (!ok) falliti <<- c(falliti, nome) }
casa <- tempfile("trama_registro"); dir.create(casa)
Sys.setenv(TRAMA_HOME = casa); Sys.unsetenv(c("TRAMA_DB", "TRAMA_REGISTRO", "TRAMA_DEFAULT_DI_PROVA", "TRAMA_COLLAUDO"))
p <- registro_percorso()
registra("INFO", "prova", "uno"); registra("ERRORE", "guasto", "due\nrighe\te tab")
v("il registro sta nella cartella dati, in log/", identical(normalizePath(dirname(p)), normalizePath(file.path(casa, "log"))))
r <- readLines(p)
v("una riga per evento", length(r) == 2)
v("formato data [LIVELLO] evento: dettaglio", grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2} \\[INFO\\] prova: uno$", r[1]))
v("a capo e tabulazioni nel dettaglio diventano spazi", grepl("\\[ERRORE\\] guasto: due righe e tab$", r[2]))
registra("INFO", "lungo", strrep("x", 500))
v("il dettaglio è tagliato a 200 caratteri", nchar(sub("^.*lungo: ", "", tail(readLines(p), 1))) <= 201)
# rotazione
cat(strrep("a", REGISTRO_MAX_BYTE + 10), "\n", file = p, append = TRUE)
registra("INFO", "dopo la rotazione")
v("oltre la soglia il file ruota (.1) e il corrente riparte", file.exists(paste0(p, ".1")) && length(readLines(p)) == 1)
for (i in 1:5) { cat(strrep("a", REGISTRO_MAX_BYTE + 10), "\n", file = p, append = TRUE); registra("INFO", "giro") }
v("se ne tengono al più tre copie", file.exists(paste0(p, ".3")) && !file.exists(paste0(p, ".4")))
# non rompe mai
v("una cartella non scrivibile non fa cadere nessuno e dice FALSE", isFALSE(registra("INFO", "x", percorso = "/proc/inesistente/trama.log")))
v("un percorso impossibile non lancia errori", isFALSE(tryCatch(registra("INFO", "x", percorso = file.path(p, "dentro")), error = function(e) "ERRORE")))
# collaudi: niente scritture con un database di prova
Sys.setenv(TRAMA_DB = "/tmp/prova.sqlite"); n <- length(readLines(p)); registra("INFO", "non deve comparire")
v("con TRAMA_DB (collaudi) non scrive", length(readLines(p)) == n)
Sys.unsetenv("TRAMA_DB"); Sys.setenv(TRAMA_COLLAUDO = "1"); n <- length(readLines(p)); registra("INFO", "non deve comparire")
v("con TRAMA_COLLAUDO non scrive", length(readLines(p)) == n)
Sys.setenv(TRAMA_REGISTRO = "1"); registra("INFO", "forzato")
v("TRAMA_REGISTRO=1 lo riaccende", length(readLines(p)) == n + 1)
cat(if (length(falliti)) "ESITO: NON TORNA\n" else "ESITO: ok\n"); if (length(falliti)) quit(status = 1)
