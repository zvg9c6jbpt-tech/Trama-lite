# ──────────────────────────────────────────────
# verifiche/avanzamento.R: avanzamento per busta, schede incomplete e «Continua» (22/10/2026, R/avanzamento.R)
#   1. per ogni busta: schede, attese dal piano, numeri che mancano, schede incomplete, la prima da riaprire;
#   2. le schede incomplete dicono quale dei quattro campi fondamentali manca; la datazione per secolo non è una lacuna;
#   3. «Continua»: la scheda che segue l'ultima toccata, e il piano concluso.
#     Rscript verifiche/avanzamento.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1")
suppressMessages({ library(DBI); library(RSQLite); library(uuid); library(jsonlite); library(readxl); library(openxlsx) })
source("verifiche/aiuti.R")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

con <- apri_db(tempfile(fileext = ".sqlite"))
nuova <- function(busta, num, completa = TRUE, luogo = TRUE) {
  f <- fascicolo_vuoto(FALSE); f$archivio <- "Archivio di Prova"; f$fondo <- "Fondo Alfa"; f$busta <- busta; f$fascicolo <- num
  f$titolo <- "Processo contro Mario Rossi"; f$estremo_remoto <- 1600L; f$persone <- "Rossi, Mario"
  if (completa || luogo) f$luoghi <- "Modena"
  if (completa) f$reati <- "Eresia"
  salva_fascicolo(con, f)
}
invisible(nuova(1L, 1L)); invisible(nuova(1L, 2L, completa = FALSE, luogo = FALSE)); invisible(nuova(1L, 3L, completa = FALSE)); invisible(nuova(2L, 1L))
fid <- elenco_fondi(con)$id[1]
salva_piano(con, fid, 1L, 1L, 4L)

# 1. avanzamento
av <- avanzamento_buste(con, fid)
v("due buste", nrow(av) == 2L && identical(av$busta, c(1L, 2L)))
v("busta 1: 3 schede su 4 attese, manca il 4", av$schede[1] == 3L && av$attese[1] == 4L && av$mancano[1] == 1L && av$numeri[1] == "4")
v("busta 1: due incomplete, la prima è la scheda 2", av$incomplete[1] == 2L && identical(av$prima_incompleta[1], elenco_fascicoli(con, fid)$id[elenco_fascicoli(con, fid)$fascicolo == 2L]))
v("busta 2 senza piano: attese vuote, nessuna incompleta", is.na(av$attese[2]) && is.na(av$mancano[2]) && av$incomplete[2] == 0L && is.na(av$prima_incompleta[2]))
v("un fondo senza schede non ha buste", nrow(avanzamento_buste(con, "inesistente")) == 0L)
v("etichetta della busta", identical(etichetta_busta(12L, ""), "b. 12") && identical(etichetta_busta(232L, "-bis"), "b. 232-bis"))

# 2. incomplete
inc <- schede_incomplete(con, fid)
v("due schede incomplete", nrow(inc) == 2L)
v("manca luogo e reato; manca solo il reato", identical(inc$cosa_manca, c("luogo; reato", "reato")))
v("la segnatura è quella della scheda", grepl("1", inc$segnatura[1]) && grepl("2", inc$segnatura[1]) && grepl("3", inc$segnatura[2]))
v("completezza_core e campi_core dicono la stessa cosa", identical(completezza_core(elenco_fascicoli(con, fid)), as.integer(rowSums(campi_core(elenco_fascicoli(con, fid))))))

# 2b. datazione per secolo: presente, non assente (22/10/2026). Un fondo a parte, per non cambiare i conti delle prove seguenti.
con2 <- apri_db(tempfile(fileext = ".sqlite"))
sec <- function(num, data_testo) {
  f <- fascicolo_vuoto(FALSE); f$archivio <- "Archivio di Prova"; f$fondo <- "Fondo Secoli"; f$busta <- 1L; f$fascicolo <- num
  f$titolo <- "Processo"; f$persone <- "Rossi, Mario"; f$luoghi <- "Modena"; f$reati <- "Eresia"
  if (!is.null(data_testo)) f$extra <- list("Estremo remoto non risolto" = data_testo)
  salva_fascicolo(con2, f)
}
invisible(sec(1L, "sec. XV")); invisible(sec(2L, "prima metà del sec. XV")); invisible(sec(3L, NULL)); invisible(sec(4L, "data illeggibile"))
fid2 <- elenco_fondi(con2)$id[1]; inc2 <- schede_incomplete(con2, fid2)
v("«sec. XV» e «prima metà del sec. XV» non sono lacune; senza data o con una data non risolta (non un secolo) l'anno manca", nrow(inc2) == 2L && all(inc2$cosa_manca == "anno"))
v("le incomplete sono la scheda 3 (senza data) e la 4 (data non risolta)", identical(sort(sub(".*fasc\\.? ?", "", inc2$segnatura)), c("3", "4")))
v("le schede datate solo per secolo si contano a parte", schede_solo_secolo(con2, fid2) == 2L)
v("nessun anno inventato: nel campo numerico restano vuote", all(is.na(elenco_fascicoli(con2, fid2)$anno[1:2])))

# «Continua»: la scheda che segue l'ultima toccata
tocca <- function(busta, num) {      # la scheda diventa l'ultima modificata (i secondi non bastano a ordinare due salvataggi di fila)
  dbExecute(con, "UPDATE fascicoli SET modificato = '2000-01-01 00:00:00'")
  dbExecute(con, "UPDATE fascicoli SET modificato = '2030-01-01 00:00:00' WHERE fondo_id = ? AND busta = ? AND fascicolo = ?", list(fid, busta, num))
}
tocca(2L, 1L); pr <- prossimo_da_schedare(con, fid)
v("ultima nella busta 2 (senza piano): continua dal numero che manca nella busta 1 (il 4)", identical(c(pr$busta, pr$fascicolo), c(1L, 4L)) && identical(pr$suffisso_b, ""))
tocca(1L, 3L); pr <- prossimo_da_schedare(con, fid)
v("ultima la scheda 3 della busta 1: continua col 4 della stessa busta", identical(c(pr$busta, pr$fascicolo), c(1L, 4L)))
invisible(nuova(1L, 4L)); tocca(1L, 4L); pr <- prossimo_da_schedare(con, fid)
v("piano concluso: «Continua» non propone un numero oltre il piano", is.null(pr) && piano_concluso(con, fid))
pf <- prossimo_fuori_piano(con, fid)
v("…ma «Aggiungi fuori piano» offre il numero che segue l'ultimo nella busta (il 5)", identical(c(pf$busta, pf$fascicolo), c(1L, 5L)))
v("un fondo senza piano non è «concluso»", !piano_concluso(con2, fid2))
# Busta 1 col piano finito, busta 2 senza piano e l'ultima scheda toccata è lì: non è «piano completato», si continua la busta 2
salva_piano(con2, fid2, 1L, 1L, 4L)
for (n_ in 1:5) { f_ <- fascicolo_vuoto(FALSE); f_$archivio <- "Archivio di Prova"; f_$fondo <- "Fondo Secoli"; f_$busta <- 2L; f_$fascicolo <- n_; f_$titolo <- "Processo"; salva_fascicolo(con2, f_) }
dbExecute(con2, "UPDATE fascicoli SET modificato = '2000-01-01 00:00:00'")
dbExecute(con2, "UPDATE fascicoli SET modificato = '2030-01-01 00:00:00' WHERE fondo_id = ? AND busta = 2 AND fascicolo = 5", list(fid2))
pr2 <- prossimo_da_schedare(con2, fid2)
v("ultima scheda in una busta senza piano: «Continua» propone b. 2, fasc. 6", !is.null(pr2) && identical(c(pr2$busta, pr2$fascicolo), c(2L, 6L)))
v("…e il piano non è «concluso» (la Home non dice «Piano completato»)", !piano_concluso(con2, fid2))
dbExecute(con2, "UPDATE fascicoli SET modificato = '2030-01-01 00:00:00' WHERE fondo_id = ? AND busta = 1 AND fascicolo = 4", list(fid2))
v("ultima scheda nella busta col piano finito: il piano è concluso e «Continua» tace", piano_concluso(con2, fid2) && is.null(prossimo_da_schedare(con2, fid2)))

v("fondo inesistente: niente da continuare", is.null(prossimo_da_schedare(con, "inesistente")))
dbExecute(con, "DELETE FROM piani_busta"); tocca(2L, 1L); pr <- prossimo_da_schedare(con, fid)
v("senza piani: la busta dell'ultima scheda col numero che segue", identical(c(pr$busta, pr$fascicolo), c(2L, 2L)))
salva_piano(con, fid, 1L, 1L, 4L)   # il piano torna come prima per le prove dopo

if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
