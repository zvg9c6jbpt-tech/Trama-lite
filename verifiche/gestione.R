# Regressioni su database in memoria e CSV temporanei; nessun dato reale scritto.
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
n <- 0L
verifica <- function(condizione) { stopifnot(condizione); n <<- n + 1L }
fallisce <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
con <- apri_db(":memory:")
crea <- function(numero, fondo = "A") {
  f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- numero; f$fondo <- fondo
  f$persone <- "Rossi, Antonio"; f$reati <- "Eresia"
  salva_fascicolo(con, f); f$id
}
ids <- vapply(1:3, crea, "")
prima <- leggi_fascicolo(con, ids[1]); f <- prima
f$note <- "Non deve restare"; f$imputati <- data.frame(esteso = "X")
verifica(fallisce(salva_fascicolo(con, f)))
verifica(identical(prima, leggi_fascicolo(con, ids[1])))

# Un errore dopo DELETE deve ripristinare i fascicoli precedenti.
a <- leggi_arianna(FOGLIO_ESEMPIO)
conversione <- fascicolo_da_riga
fascicolo_da_riga <- function(...) stop("errore simulato dopo la cancellazione")
verifica(fallisce(importa_arianna(con, a, "sostituisci")))
fascicolo_da_riga <- conversione
verifica(setequal(elenco_fascicoli(con)$id, ids))
# «Sostituisci» con un foglio di sole righe respinte non cancella niente (19/10): prima il database passava a 0 schede.
{ n_prima <- nrow(dbGetQuery(con, "SELECT 1 FROM fascicoli"))
  foglio_senza_fondo <- leggi_arianna("dati/esempio/prova-normalizzazione.xlsx")      # segnature senza il fondo: tutte respinte
  r <- importa_arianna(con, foglio_senza_fondo, "sostituisci")
  verifica(isTRUE(r$sostituzione_annullata) && r$importati == 0L && nrow(r$respinti) > 0L)
  verifica(nrow(dbGetQuery(con, "SELECT 1 FROM fascicoli")) == n_prima)
  verifica(setequal(elenco_fascicoli(con)$id, ids)) }

ids_b <- vapply(1:2, function(i) crea(i, "B"), "")

# Le frecce e il trascinamento non devono alterare segnature e contenuto.
schede_prima <- lapply(ids, function(id) leggi_fascicolo(con, id))
salva_ordine_busta(con, "A", 1L, rev(ids))
verifica(identical(elenco_busta(con, "A", 1L)$id, rev(ids)))
verifica(identical(schede_prima, lapply(ids, function(id) leggi_fascicolo(con, id))))
verifica(fallisce(salva_ordine_busta(con, "A", 1L, ids[1:2])))
p <- piano_collocazioni(elenco_busta(con, "A", 1L), 1L, 1L)
applica_collocazioni(con, p)
verifica(identical(elenco_busta(con, "A", 1L)$id, rev(ids)))
verifica(identical(leggi_fascicolo(con, ids[3])$reati, "Eresia"))
verifica(fallisce(applica_collocazioni(con, p))) # anteprima ormai obsoleta
d <- elenco_busta(con, "A", 1L)
verifica(fallisce(applica_collocazioni(con, piano_collocazioni(d[1, ], 1L, 2L))))
verifica(identical(d, elenco_busta(con, "A", 1L)))
verifica(fallisce(piano_collocazioni(d, 1.5, 1L)))

# Profili multipli, campi aggiuntivi e collisioni di intestazioni.
PROFILI$Altro <- rbind(.col("Inventario interno", sezione = "Contesto"), .col("A-B"), .col("A B"))
pr <- profilo_completo("Generale", "Campo futuro")
verifica(!anyDuplicated(vapply(seq_len(nrow(pr)), function(i) id_input(pr[i, ]), "")))
f <- leggi_fascicolo(con, ids[1])
f$extra <- list("Consistenza (unità di misura)" = "carte", "Titolo originale" = "Titolo antico",
                "Inventario interno" = "ABC", "Campo futuro" = c("uno", "due"))
salva_fascicolo(con, f)
g <- raccogli_modulo(valori_da_fascicolo(f, pr), pr); g$id <- f$id
salva_fascicolo(con, g)
verifica(identical(leggi_fascicolo(con, f$id)$extra[sort(names(f$extra))], f$extra[sort(names(f$extra))]))
PROFILI$Altro <- NULL

# Backup completo delle coorti e dell'ordine; vecchia versione ancora leggibile.
salva_coorte(con, "Eresia", "reato:eresia")
salva_ordine_busta(con, "A", 1L, ids)
j <- tempfile(fileext = ".json"); esporta_backup_json(con, j)
c2 <- apri_db(":memory:")
e <- ripristina_backup_json(c2, j)
verifica(e$importati == 5L)
verifica(identical(elenco_coorti(con)[, c("nome", "query")], elenco_coorti(c2)[, c("nome", "query")]))
verifica(identical(elenco_busta(con, "A", 1L)$id, elenco_busta(c2, "A", 1L)$id))
salva_coorte(c2, "Eresia", "reato:magia")
ripristina_backup_json(c2, j, "accoda")
verifica(elenco_coorti(c2)$query == "reato:magia")
vecchio <- fromJSON(j, simplifyVector = FALSE); vecchio$versione <- 1L
vecchio$coorti <- NULL; vecchio$ordine_fascicoli <- NULL
j1 <- tempfile(); writeLines(toJSON(vecchio, auto_unbox = TRUE, null = "null"), j1)
verifica(length(leggi_backup_json(j1)$fascicoli) == 5L)
c3 <- apri_db(":memory:")
invisible(dbExecute(c3, "CREATE TRIGGER errore_prova BEFORE INSERT ON reati BEGIN SELECT RAISE(ABORT, 'errore simulato'); END"))
verifica(fallisce(ripristina_backup_json(c3, j)))
verifica(nrow(elenco_fascicoli(c3)) == 0L)

invisible(lapply(list(con, c2, c3), dbDisconnect))
cat(sprintf("Gestione: %d verifiche passate.\n", n))
