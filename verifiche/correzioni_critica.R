# ──────────────────────────────────────────────
# verifiche/correzioni_critica.R: le correzioni della critica del 22/10/2026
# Suffisso della busta nelle azioni in blocco, indice delle segnature senza numero, fondo attivo come id, nome
# del fondo normalizzato, blocco della lista dei luoghi, zip slip, rotazione delle copie gravi, impronta del
# database alla riapertura, «Sostituisci» che conserva i dati solo-database, ordine di lingue e produttori.
#     Rscript verifiche/correzioni_critica.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1")
suppressMessages({ library(DBI); library(RSQLite); library(uuid); library(jsonlite) })
source("verifiche/aiuti.R")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
scheda <- function(con, busta, fascicolo, bsuf = "", archivio = "Archivio", fondo = "Fondo") {
  f <- fascicolo_vuoto(FALSE); f$archivio <- archivio; f$fondo <- fondo; f$busta <- busta; f$fascicolo <- fascicolo
  f$busta_suffisso <- bsuf; f$titolo <- "T"; f$titolo_manuale <- 1L; f$regesto <- "R"; f$regesto_manuale <- 1L
  salva_fascicolo(con, f)
}

# 1. suffisso della busta: «232-bis» non è «232»
con <- apri_db(":memory:")
scheda(con, 232L, 3L, "-bis")
fid <- elenco_fondi(con)$id[1]
g <- genera_fascicoli_vuoti(con, fid, 232L, 1L, 4L)
n232 <- dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE busta = 232 AND busta_suffisso = ''")$n
v("Genera fascicoli vuoti: la b. 232 ottiene 4 schede, anche se esiste la 232-bis fasc. 3", n232 == 4L)
e <- applica_note_rapide(con, fid, 233L, "fasc. 1: processo contro Mario Rossi", prova = TRUE)
v("Note rapide su una busta che non c'è: nessuna scheda toccata", is.data.frame(e) || is.list(e))
dbDisconnect(con)

# 2. segnature senza numero: non si ripetono
con <- apri_db(":memory:")
scheda(con, 5L, NA_integer_)
r <- try(scheda(con, 5L, NA_integer_), silent = TRUE)
v("due schede «b. 5» senza fascicolo: la seconda è respinta", inherits(r, "try-error") && grepl("UNIQUE", as.character(r)))
v("il rifiuto si spiega a chi scheda", grepl("già occupata", .spiega_rifiuto(conditionMessage(attr(r, "condition")))))
scheda(con, 5L, 1L)
v("busta con fascicolo diverso: entra", dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli")$n == 2L)
dbDisconnect(con)

# 3. nome del fondo con apostrofo tipografico e NBSP: un fondo solo, con le sue schede
con <- apri_db(":memory:")
id <- crea_archivio(con, "Archivio X", "Sant’Uffizio")
v("crea_archivio scrive la grafia pulita", identical(dbGetQuery(con, "SELECT denominazione d FROM fondi WHERE id = ?", list(id))$d, "Sant'Uffizio"))
v("lo stesso nome con apostrofo dritto è lo stesso fondo", !is.na(fondo_id_da(con, "Archivio X", "Sant'Uffizio")) && identical(fondo_id_da(con, "Archivio X", "Sant'Uffizio"), id))
dbDisconnect(con)

# 4. dopo l'eliminazione l'elenco dei fondi dà id (e non denominazioni)
con <- apri_db(":memory:")
scheda(con, 1L, 1L, fondo = "Sant'Ufficio", archivio = "Bologna"); scheda(con, 1L, 1L, fondo = "Sant'Ufficio", archivio = "Napoli")
ef <- elenco_fondi(con)
v("elenco_fondi$id sono UUID distinti anche con la stessa denominazione", length(unique(ef$id)) == 2L && all(nchar(ef$id) == 36L))
dbDisconnect(con)

# 5. blocco della lista dei luoghi rimasto da una sessione interrotta
cartella <- tempfile("luoghi-"); dir.create(cartella); lista <- file.path(cartella, "luoghi.csv")
file.copy("dati/luoghi.csv", lista)
dir.create(paste0(lista, ".lock")); Sys.setFileTime(paste0(lista, ".lock"), Sys.time() - 3600)
entrati <- aggiungi_luoghi_alla_lista("Luogo di prova inventato", percorso = lista)
v("un blocco vecchio non ferma più «Aggiungi alla lista»", identical(entrati, "Luogo di prova inventato"))
dir.create(paste0(lista, ".lock"))
v("un blocco recente (altra modifica in corso) ferma ancora", length(aggiungi_luoghi_alla_lista("Altro luogo inventato", percorso = lista)) == 0L)
unlink(cartella, recursive = TRUE)

# 5b. una regione («Sassonia») non è un alias di un capoluogo: se si sceglie «Aggiungi alla lista» entra con le coordinate del capoluogo (02/11/2026)
l_base <- leggi_lista_luoghi("dati/luoghi.csv")
v("Sassonia, Galizia e Hampshire non sono più alias nella base; i capoluoghi sono luoghi a sé", !any(c("sassonia", "galizia", "hampshire") %in% l_base$chiave) && all(c("dresden", "santiagodicompostela", "winchester") %in% l_base$chiave))
v("la Bulgaria è lo Stato (Q219), non Sofia", identical(l_base$nome[l_base$chiave == "bulgaria"], "Bulgaria") && identical(l_base$wikidata[l_base$chiave == "bulgaria"], "Q219"))
cartella <- tempfile("luoghi-"); dir.create(cartella); lista <- file.path(cartella, "luoghi.csv"); file.copy("dati/luoghi.csv", lista)
aggiungi_luoghi_alla_lista(c("Sassonia", "Hampshire"), percorso = lista); l_reg <- leggi_lista_luoghi(lista)
v("«Sassonia» entra con paese, identificativo e coordinate di Dresda", identical(unlist(l_reg[l_reg$nome == "Sassonia", c("paese", "wikidata", "coordinate")], use.names = FALSE), unlist(l_base[l_base$chiave == "dresden", c("paese", "wikidata", "coordinate")], use.names = FALSE)))
reg_t <- regioni_luoghi(); v("ogni regione ha il suo capoluogo nella lista (per nome)", all(reg_t$chiave_capoluogo %in% chiave_grafia(l_base$nome)) && !anyDuplicated(reg_t$chiave) && !any(reg_t$chiave %in% l_base$chiave))
v("«Hampshire» entra con quelli di Winchester", identical(l_reg$wikidata[l_reg$nome == "Hampshire"], "Q172157"))
unlink(cartella, recursive = TRUE)

# 5c. «Bernaudo» (forma arcaica di Bernalda) e «Fiorano» sono alias italiani: una proposta, mai una correzione automatica (03/11/2026)
a_arcaico <- leggi_arianna("dati/esempio/esempio.xlsx"); col_l <- a_arcaico$colonna[["Luoghi citati o indicizzati"]]; a_arcaico$dati[[col_l]][1] <- "Bernaudo|Fiorano"
t_dec <- vapply(proposte_import(a_arcaico, NULL, NULL, luoghi_ignoti = FALSE)$decisioni, function(d) d$testo, "")
v("«Bernaudo» propone Bernalda e «Fiorano» Fiorano Modenese, senza cambiare la cella", any(grepl("«Bernaudo»: forse «Bernalda»", t_dec, fixed = TRUE)) && any(grepl("«Fiorano»: forse «Fiorano Modenese»", t_dec, fixed = TRUE)))

# 5d. una copia personale vecchia perde gli alias delle regioni e corregge la Bulgaria, ma tiene un alias che l'utente ha riscritto (03/11/2026)
home_prima <- Sys.getenv("TRAMA_HOME"); home_r <- tempfile("home-"); Sys.setenv(TRAMA_HOME = home_r)
dir_r <- tempfile("base-"); dir.create(dir_r); base_r <- file.path(dir_r, "luoghi.csv")
file.copy("dati/luoghi.csv", base_r); file.copy("dati/luoghi_correzioni.csv", file.path(dir_r, "luoghi_correzioni.csv"))
dir.create(cartella_luoghi(), recursive = TRUE)
vecchia <- l_base[!l_base$chiave %in% c("santiagodicompostela", "winchester", "bernaudo"), ]
vecchia[vecchia$chiave == "bulgaria", c("nome", "paese", "wikidata", "coordinate")] <- list("Sofia", "Bulgaria", "Q472", "42.697886, 23.321726")
vecchia <- rbind(vecchia, data.frame(chiave = c("sassonia", "galizia", "hampshire"), nome = c("Dresda", "Santiago di Compostela", "Mio Hampshire"), paese = c("Germania", "Spagna", "Regno Unito"),
                                       wikidata = c("Q1731", "Q14314", "Q172157"), coordinate = c("51.049329, 13.738144", "42.883333, -8.533333", "51.063333, -1.308611"), stringsAsFactors = FALSE))
.scrivi_lista_luoghi(vecchia, file.path(cartella_luoghi(), "luoghi.csv")); writeLines("vecchia", file.path(cartella_luoghi(), "base.md5"))
l_r <- leggi_lista_luoghi(prepara_cartella_dati(base_r)$lista)
v("copia personale vecchia: Sassonia e Galizia tolte, i capoluoghi arrivano", !any(c("sassonia", "galizia") %in% l_r$chiave) && all(c("santiagodicompostela", "winchester", "bernaudo") %in% l_r$chiave))
v("copia personale vecchia: un «Hampshire» riscritto dall'utente resta", identical(l_r$nome[l_r$chiave == "hampshire"], "Mio Hampshire"))
v("copia personale vecchia: la Bulgaria diventa lo Stato (Q219)", identical(l_r$nome[l_r$chiave == "bulgaria"], "Bulgaria") && identical(l_r$wikidata[l_r$chiave == "bulgaria"], "Q219"))
unlink(dir_r, recursive = TRUE); Sys.setenv(TRAMA_HOME = home_prima)

# 6. zip slip: dal ripristino completo si estraggono solo i file noti
lavoro <- tempfile("zip-"); dir.create(lavoro)
dentro <- file.path(lavoro, "dentro"); dir.create(dentro)
db0 <- file.path(dentro, "trama.sqlite"); c0 <- apri_db(db0); scheda(c0, 1L, 1L); dbDisconnect(c0)
zipfile <- file.path(lavoro, "copia.zip")
dir.create(file.path(lavoro, "x", "sotto"), recursive = TRUE)
file.copy(db0, file.path(lavoro, "x", "trama.sqlite")); writeLines("fuori", file.path(lavoro, "x", "sotto", "evil.txt"))
old <- setwd(file.path(lavoro, "x"))
zip::zip(zipfile, c("trama.sqlite", "sotto/evil.txt"), mode = "cherry-pick")
setwd(old)
dove <- file.path(lavoro, "dove"); dir.create(dove)
esito <- ripristina_backup_completo(zipfile, dove = dove)
v("il ripristino completo riporta il database", esito$fascicoli == 1L && file.exists(file.path(dove, "trama.sqlite")))
v("il ripristino non estrae altri membri dello zip", !file.exists(file.path(dove, "sotto", "evil.txt")))
unlink(lavoro, recursive = TRUE)

# 7. storia: un blob che non è un dato non si ripristina
con <- apri_db(":memory:")
scheda(con, 1L, 1L); id1 <- dbGetQuery(con, "SELECT id FROM fascicoli")$id
dbExecute(con, "INSERT INTO storia_fascicoli (fascicolo_id, quando, campi, prima) VALUES (?, 'ora', 'Titolo', ?)",
          list(id1, jsonlite::base64_enc(serialize(function() 1, NULL))))
r <- try(annulla_ultima_modifica(con, id1), silent = TRUE)
v("annulla: una funzione serializzata non è una scheda e si rifiuta", inherits(r, "try-error"))
v(".solo_dati accetta una scheda vera", .solo_dati(leggi_fascicolo(con, id1)))
dbDisconnect(con)

# 8. copie gravi: oltre le ultime 10 per motivo, solo le più vecchie di 90 giorni
rot <- tempfile("rot-"); dir.create(rot)
for (i in 1:14) { f <- file.path(rot, sprintf("trama-prima-sostituzione-%02d.sqlite", i)); writeLines("x", f)
                  Sys.setFileTime(f, Sys.time() - (if (i <= 6) 200 else 5) * 86400 - i * 60) }
ruota_copie_prima(rot)
resto <- list.files(rot)
v("copie gravi: le vecchie in eccesso se ne vanno, le recenti restano", length(resto) == 10L || all(sprintf("trama-prima-sostituzione-%02d.sqlite", 7:14) %in% resto))
v("copie gravi: mai meno delle ultime 10", length(resto) >= 10L)
unlink(rot, recursive = TRUE)

# 9. riaprire il database non lo cambia
db <- tempfile(fileext = ".sqlite"); c1 <- apri_db(db); scheda(c1, 1L, 1L); dbDisconnect(c1)
h1 <- unname(tools::md5sum(db)); c2 <- apri_db(db); dbDisconnect(c2)
v("riaprire il database non cambia il file (impronta uguale)", identical(h1, unname(tools::md5sum(db))))
unlink(db)

# 10. «Sostituisci» con un foglio esportato non perde i dati solo-database
con <- apri_db(":memory:")
f <- fascicolo_vuoto(FALSE); f$archivio <- "A"; f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- 1L
f$persone <- "Rossi, Mario"; f$reati <- "Eresia"; f$luoghi <- "Napoli"
f$imputati <- data.frame(esteso = "Mario Rossi", indicizzato = "Rossi, Mario", stringsAsFactors = FALSE)
f$tortura <- "sì"; f$tortura_esaminata <- 1L; f$sentenza <- "no"; f$sentenza_esaminata <- 1L
f$persona_reati <- data.frame(persona = "Rossi, Mario", reato = "Eresia", stringsAsFactors = FALSE)
salva_fascicolo(con, f)
fid <- elenco_fondi(con)$id[1]; prima <- leggi_fascicolo(con, dbGetQuery(con, "SELECT id FROM fascicoli")$id)
xl <- tempfile(fileext = ".xlsx"); esporta_arianna(con, xl, fondo = fid)
e <- importa_arianna(con, leggi_arianna(xl), modo = "sostituisci", fondo = fid)
dopo <- leggi_fascicolo(con, dbGetQuery(con, "SELECT id FROM fascicoli")$id)
v("Sostituisci: la scheda torna", e$importati == 1L && is.null(e$sostituzione_annullata))
v("Sostituisci: tortura e sentenza esaminate restano", identical(dopo$tortura, "sì") && dopo$tortura_esaminata == 1L && identical(dopo$sentenza, "no") && dopo$sentenza_esaminata == 1L)
v("Sostituisci: i luoghi citati restano", identical(as.character(dopo$luoghi), "Napoli"))
v("Sostituisci: le coppie persona-reato restano", NROW(prima$persona_reati) == 1L && NROW(dopo$persona_reati) == 1L)
v("Sostituisci: titolo e regesto non cambiano natura", dopo$titolo_manuale == prima$titolo_manuale && dopo$regesto_manuale == prima$regesto_manuale && identical(dopo$regesto, prima$regesto))
dbDisconnect(con)

# 11. l'ordine delle lingue e dei produttori è quello scritto
con <- apri_db(":memory:")
f <- fascicolo_vuoto(FALSE); f$archivio <- "A"; f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- 1L
f$lingue <- c("Latino", "Italiano"); f$produttori <- c("Inquisizione di Napoli", "Curia di Roma")
salva_fascicolo(con, f); g <- leggi_fascicolo(con, dbGetQuery(con, "SELECT id FROM fascicoli")$id)
v("lingue: Latino prima di Italiano, come scritto", identical(g$lingue, c("Latino", "Italiano")))
v("produttori: nell'ordine scritto", identical(g$produttori, c("Inquisizione di Napoli", "Curia di Roma")))
tutte <- leggi_fascicoli(con, dbGetQuery(con, "SELECT id FROM fascicoli")$id)
v("la lettura in blocco rispetta l'ordine", identical(tutte[[1]]$lingue, c("Latino", "Italiano")))
dbDisconnect(con)

# L2 + L3 (LACUNE.md): doppioni senza numero già nel database o in un backup
doppi <- apri_db(":memory:")
dbExecute(doppi, "DROP INDEX idx_segnatura_senza_numeri")
scheda(doppi, 5L, NA_integer_); scheda(doppi, 5L, NA_integer_); scheda(doppi, 6L, 1L)
v("L2: con i doppioni l'indice non si può creare", identical(.indice_senza_numeri(doppi), FALSE))
qd <- qualita(doppi); riga <- qd[qd$controllo == "Segnature doppie senza numero", ]
v("L2: Qualità conta le due schede con segnatura doppia", nrow(riga) == 1L && riga$casi == 2L)
v("L2: il clic sulla riga porta alle due schede", length(qualita_fascicoli(doppi, "Segnature doppie senza numero")) == 2L)
id6 <- dbGetQuery(doppi, "SELECT id FROM fascicoli WHERE busta = 6")$id
v("L2: una segnatura normale (b. 6, fasc. 1) non è segnalata", !(id6 %in% qualita_fascicoli(doppi, "Segnature doppie senza numero")))
pj <- tempfile(fileext = ".json"); esporta_backup_json(doppi, pj)
nuovo <- apri_db(":memory:")
e <- ripristina_backup_json(nuovo, pj, modo = "sostituisci")
v("L3: «Sostituisci» riporta tutte le schede, doppioni compresi", e$importati == 3L && dbGetQuery(nuovo, "SELECT COUNT(*) n FROM fascicoli")$n == 3L)
v("L3: e lo dice nei dettagli", any(grepl("senza numero", e$dettagli$riferimento)))
qn <- qualita(nuovo); v("L3: Qualità li elenca anche dopo il ripristino", qn$casi[qn$controllo == "Segnature doppie senza numero"] == 2L)
acc <- apri_db(":memory:"); scheda(acc, 5L, NA_integer_)
e2 <- ripristina_backup_json(acc, pj, modo = "accoda")
v("L3: «Accoda» respinge il doppione con il motivo", length(e2$respinti) >= 1L && any(grepl("Segnatura già occupata", e2$dettagli$motivo)))
dbExecute(doppi, "DELETE FROM fascicoli WHERE id = (SELECT id FROM fascicoli WHERE busta = 5 LIMIT 1)")
v("L2: tolto il doppione l'indice si crea", isTRUE(.indice_senza_numeri(doppi)))
qd <- qualita(doppi); v("L2: e il controllo torna a zero", qd$casi[qd$controllo == "Segnature doppie senza numero"] == 0L)
lapply(list(doppi, nuovo, acc), dbDisconnect)

# L1 (LACUNE.md): «Rimuovi duplicati» non confonde fondi diversi e non cancella senza una scelta per gruppo
dp <- apri_db(":memory:")
sc <- function(fondo, busta, fasc, titolo = "T") { f <- fascicolo_vuoto(FALSE); f$archivio <- "Arch"; f$fondo <- fondo; f$busta <- busta; f$fascicolo <- fasc
  f$titolo <- titolo; f$titolo_manuale <- 1L; salva_fascicolo(dp, f) }
sc("Serie 1", 1L, 1L); sc("Serie 2", 1L, 1L); sc("Serie I", 2L, 1L); sc("Serie II", 2L, 1L); sc("Serie 10", 3L, 1L); sc("Serie 20", 3L, 1L)
v("L1: «Serie 1» e «Serie 2» (e I/II, 10/20) non sono lo stesso fondo", nrow(duplicati_fascicoli(dp)) == 0L)
a1 <- sc("Sant'Ufficio", 5L, 1L, "Processo contro Rossi"); a2 <- sc("Sant'Uffizio", 5L, 1L, "Altro titolo")
dd <- duplicati_fascicoli(dp)
v("L1: la stessa grafia scritta in due modi è ancora un duplicato", nrow(dd) == 2L && setequal(dd$id, c(a1, a2)))
v("L1: titoli diversi sono segnalati", all(dd$diversi))
v("L1: senza dire quali, non si elimina niente (l'argomento è obbligatorio)", inherits(try(rimuovi_duplicati(dp), silent = TRUE), "try-error"))
v("L1: eliminare tutte le copie di un gruppo è rifiutato", inherits(try(rimuovi_duplicati(dp, c(a1, a2)), silent = TRUE), "try-error") &&
  nrow(dbGetQuery(dp, "SELECT 1 FROM fascicoli")) == 8L)
v("L1: una scheda che non è un duplicato è rifiutata", inherits(try(rimuovi_duplicati(dp, dbGetQuery(dp, "SELECT id FROM fascicoli WHERE fondo = 'Serie 1'")$id), silent = TRUE), "try-error"))
g <- dd$gruppo[1]
v("L1: un gruppo non spuntato non si tocca", length(scelta_duplicati(dd, integer(0))) == 0L)
v("L1: si elimina l'altra copia, a scelta di chi guarda", identical(scelta_duplicati(dd, g, stats::setNames(list(a2), as.character(g))), a1))
v("L1: senza scelta resta la proposta", identical(scelta_duplicati(dd, g), dd$id[!dd$tieni]))
n <- rimuovi_duplicati(dp, scelta_duplicati(dd, g, stats::setNames(list(a2), as.character(g))))
v("L1: eliminata una sola scheda, resta quella scelta", n == 1L && nrow(dbGetQuery(dp, "SELECT 1 FROM fascicoli WHERE id = ?", list(a2))) == 1L &&
  nrow(dbGetQuery(dp, "SELECT 1 FROM fascicoli")) == 7L)
dbDisconnect(dp)

# L7 (LACUNE.md): «Sostituisci» dice quali schede perderebbero dati perché la loro segnatura non è più nel foglio
o7 <- apri_db(":memory:")
for (n in 1:3) { f <- fascicolo_vuoto(FALSE); f$archivio <- "A"; f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- n; f$titolo <- "T"; f$titolo_manuale <- 1L
  if (n == 2L) { f$tortura <- "sì"; f$tortura_esaminata <- 1L }; salva_fascicolo(o7, f) }
fid7 <- elenco_fondi(o7)$id[1]; xl7 <- tempfile(fileext = ".xlsx"); esporta_arianna(o7, xl7, fondo = fid7)
a7 <- leggi_arianna(xl7)
v("L7: un foglio esportato dallo stesso fondo non ha schede senza corrispondenza", nrow(schede_senza_corrispondenza(o7, a7, fid7)) == 0L)
col <- a7$colonna[["Segnatura"]]; segn <- as.character(a7$dati[[col]]); i2 <- grep("fasc\\.? ?2", segn)[1]
a7$dati[[col]][i2] <- sub("fasc\\.? ?2", "fasc. 20", segn[i2])
o <- schede_senza_corrispondenza(o7, a7, fid7)
v("L7: corretta una segnatura, la scheda con dati solo-database è elencata", nrow(o) == 1L && grepl("fasc. 2", o$segnatura, fixed = TRUE) && grepl("tortura", o$perde))
v("L7: le schede senza dati da perdere (fasc. 1 e 3) non sono elencate", nrow(o) == 1L)
dbDisconnect(o7)

# L4 (LACUNE.md): i duplicati esatti in luoghi, soggettario, lingue e produttori non si fondono in silenzio
o4 <- apri_db(":memory:")
f <- fascicolo_vuoto(FALSE); f$archivio <- "A"; f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- 1L; f$titolo <- "T"; f$titolo_manuale <- 1L
f$luoghi <- c("Napoli", "Roma"); f$reati <- "Eresia"; f$lingue <- "Latino"; f$produttori <- "Curia di Roma"; salva_fascicolo(o4, f)
fid4 <- elenco_fondi(o4)$id[1]; xl4 <- tempfile(fileext = ".xlsx"); esporta_arianna(o4, xl4, fondo = fid4)
a4 <- leggi_arianna(xl4)
testo_dec <- function(a) vapply(proposte_import(a, o4, fid4)$decisioni, function(d) d$testo, "")
v("L4: un foglio pulito non ha decisioni sui duplicati", !any(grepl("duplicato esatto", testo_dec(a4))))
cl <- a4$colonna[["Luoghi citati o indicizzati"]]; cr <- a4$colonna[["Soggettario"]]; cg <- a4$colonna[["Lingua"]]; cp <- a4$colonna[["Soggetto produttore"]]
a4$dati[[cl]][1] <- "Napoli|Roma|Napoli"; a4$dati[[cr]][1] <- "Eresia|Magia|Eresia"; a4$dati[[cg]][1] <- "latino; Latino"; a4$dati[[cp]][1] <- "Curia di Roma|Curia di Roma"
t4 <- testo_dec(a4)
v("L4: doppione fra i luoghi", any(grepl("duplicato esatto in «Luoghi citati o indicizzati»: «Napoli»", t4, fixed = TRUE)))
v("L4: doppione nel soggettario", any(grepl("«Soggettario»: «Eresia»", t4, fixed = TRUE)))
v("L4: doppione fra le lingue (maiuscole comprese)", any(grepl("«Lingua»", t4, fixed = TRUE)))
v("L4: doppione fra i produttori", any(grepl("«Soggetto produttore»: «Curia di Roma»", t4, fixed = TRUE)))
dbDisconnect(o4)

# 26/10/2026: «Luogo del reato» non c'è più; chi lo porta (database vecchio, backup, foglio) lo vede passare fra i luoghi citati.
o9 <- apri_db(":memory:"); f9 <- fascicolo_vuoto(FALSE); f9$archivio <- "A"; f9$fondo <- "F"; f9$busta <- 1L; f9$fascicolo <- 1L; f9$luoghi <- "Carpi"
salva_fascicolo(o9, f9); id9 <- dbGetQuery(o9, "SELECT id FROM fascicoli")$id
dbExecute(o9, "UPDATE fascicoli SET luogo_reato = 'Modena; Napoli' WHERE id = ?", list(id9))
v("luogo del reato: la migrazione lo porta fra i luoghi citati", .migra_luogo_reato(o9) == 1L &&
  identical(dbGetQuery(o9, "SELECT luogo FROM luoghi ORDER BY ordine")$luogo, c("Carpi", "Modena", "Napoli")))
v("luogo del reato: la colonna resta vuota e la migrazione è idempotente", is.na(dbGetQuery(o9, "SELECT luogo_reato FROM fascicoli")$luogo_reato) && .migra_luogo_reato(o9) == 0L)
g9 <- leggi_fascicolo(o9, id9); g9$luogo_reato <- "Bologna"; salva_fascicolo(o9, g9)
v("luogo del reato: salvando una scheda che lo porta, entra fra i citati", "Bologna" %in% leggi_fascicolo(o9, id9)$luoghi && is.na(dbGetQuery(o9, "SELECT luogo_reato FROM fascicoli")$luogo_reato))
dbDisconnect(o9)

# L8 (LACUNE.md): la finestra di revisione non si blocca con migliaia di celle lunghe
set.seed(7); pw <- paste0("p", sample(1:300, 400, TRUE))
lungo <- function() paste(sample(pw, 140), collapse = " ")
celle8 <- data.frame(riga = 1:400, segnatura = "b. 1", colonna = "Regesto o descrizione sintetica", prima = vapply(1:400, function(i) lungo(), ""), perche = "prova", stringsAsFactors = FALSE)
celle8$dopo <- sub("p1 ", "P1 ", celle8$prima)
t8 <- system.time(h8 <- paste(as.character(vista_revisione(list(celle = celle8, decisioni = list(), totale = nrow(celle8)))), collapse = ""))[["elapsed"]]
v(sprintf("L8: 400 celle lunghe si preparano in tempi accettabili (%.1f s)", t8), t8 < 15)
v("L8: ogni cella ha la sua voce nell'elenco, anche oltre il budget", lengths(regmatches(h8, gregexpr('class="rev-voce"', h8, fixed = TRUE))) == 400L)
v("L8: le celle evidenziate ci sono (<ins>/<del>)", grepl("<ins", h8, fixed = TRUE) || grepl("<del", h8, fixed = TRUE))

# L6 (LACUNE.md): controlli statici sui lanciatori Windows (Windows: da qui si fanno solo controlli statici, vedi distribuzione/windows)
grezzo <- function(f) readBin(f, "raw", file.size(f))
for (f in "avvia.bat") {
  b <- grezzo(f); lf_soli <- sum(b == as.raw(10)) - sum(b[-length(b)] == as.raw(13) & b[-1] == as.raw(10))
  v(sprintf("L6: %s ha righe CRLF (cmd e VBScript leggono male i soli LF)", basename(f)), lf_soli == 0L && any(b == as.raw(13)))
  v(sprintf("L6: %s è solo ASCII (la tabella dei caratteri di cmd cambia da PC a PC)", basename(f)), all(b < as.raw(128)))
}
bat <- paste(readLines("avvia.bat", warn = FALSE), collapse = "\n")
v("L6: avvia.bat spegne l'autoloader di renv prima di lanciare Rscript", regexpr("RENV_CONFIG_AUTOLOADER_ENABLED", bat) > 0 && regexpr("RENV_CONFIG_AUTOLOADER_ENABLED", bat) < regexpr('"%RS%" -e', bat, fixed = TRUE))
v("L6: avvia.bat controlla i pacchetti prima di runApp", regexpr("source('R/richiesti.R'); controlla_pacchetti()", bat, fixed = TRUE) > 0 && regexpr("controlla_pacchetti()", bat, fixed = TRUE) < regexpr("shiny::runApp", bat, fixed = TRUE))
v("L6: avvia.bat usa la cartella libreria solo se R è della versione per cui è costruita", grepl('libreria\\R-versione.txt', bat, fixed = TRUE) && grepl('set "R_LIBS=%~dp0libreria"', bat, fixed = TRUE) && regexpr("RENV_CONFIG_AUTOLOADER_ENABLED", bat) < regexpr("R-versione.txt", bat, fixed = TRUE))
v("L6: il costruttore dello zip per Windows e il suo LEGGIMI ci sono", file.exists("distribuzione/windows/costruisci.sh") && file.exists("distribuzione/windows/LEGGIMI.txt") && all(readBin("distribuzione/windows/LEGGIMI.txt", "raw", file.size("distribuzione/windows/LEGGIMI.txt")) < as.raw(128)))
cmd <- paste(readLines("avvia.command", warn = FALSE), collapse = "\n")
v("L6: avvia.command spegne l'autoloader di renv se la libreria non è usabile", grepl("RENV_CONFIG_AUTOLOADER_ENABLED=FALSE", cmd, fixed = TRUE) && regexpr("RENV_CONFIG", cmd) < regexpr("controlla_pacchetti", cmd))
v(".gitattributes tiene CRLF ai file di Windows", all(grepl("crlf", readLines(".gitattributes")[grepl("bat", readLines(".gitattributes"))])))

cat(if (!length(falliti)) "ESITO: ok\n" else sprintf("ESITO: %d NON TORNA: %s\n", length(falliti), paste(falliti, collapse = "; ")))
quit(status = if (length(falliti)) 1L else 0L)
