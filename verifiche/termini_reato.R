# ─────────────────────────────────────────────────────────────
# verifiche/termini_reato.R: la tabella dei termini del tesauro (reato_termini, D33), passo 3a
# Il database sa conservare il termine specifico sotto la classe: scrittura, lettura, copia di sicurezza, cancellazione.
# Qui si prova il contenitore (l'import lo riempie dal passo 3b: lo provano profilo_dominio.R e i tab veri).
#
#     Rscript verifiche/termini_reato.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1", TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({
  library(shiny)
  for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f)
})
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else { .esiti$falliti <- c(.esiti$falliti, nome); cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto) }
}
coppie <- function(...) { a <- list(...); data.frame(reato = vapply(a, `[`, "", 1), termine = vapply(a, `[`, "", 2), stringsAsFactors = FALSE) }
termini_di <- function(con, id) { d <- leggi_fascicolo(con, id)$reato_termini; paste(d$reato, d$termine, sep = " > ") }
nuova <- function(con, num, reati, termini = NULL) {
  f <- fascicolo_vuoto(); f$fascicolo <- num; f$busta <- 1L; f$tipologia_documentaria <- "Processi"; f$persone <- "Rossi, Mario"; f$reati <- reati
  f$estremo_remoto <- 1600L; f$lingue <- "Italiano"; f$luoghi <- "Capua"; if (!is.null(termini)) f$reato_termini <- termini
  salva_fascicolo(con, f)
}

con <- apri_db(tempfile(fileext = ".sqlite"))
atteso("la tabella esiste", "reato_termini" %in% dbListTables(con), TRUE)

# scrittura: solo coppie che la scheda ha (classe presente) e che il profilo conosce (termine sotto quella classe)
id1 <- nuova(con, 1L, c("Crimini sessuali", "Magia"),
             coppie(c("Crimini sessuali", "Bigamia"), c("Magia", "Bigamia"), c("Eresia", "Apostasia"), c("Magia", "Astrologia"), c("Crimini sessuali", "Inventato")))
atteso("si tengono solo le coppie valide", termini_di(con, id1), c("Crimini sessuali > Bigamia", "Magia > Astrologia"))

# chi non porta i termini (altri chiamanti) non li cancella
f <- leggi_fascicolo(con, id1); f$reato_termini <- NULL; f$note <- "una nota"; salva_fascicolo(con, f)
atteso("salvare senza i termini non li cancella", termini_di(con, id1), c("Crimini sessuali > Bigamia", "Magia > Astrologia"))

# la maschera li porta: tolta la classe, il termine sotto va via con lei
f <- leggi_fascicolo(con, id1); f$reati <- "Magia"; salva_fascicolo(con, f)
atteso("tolta la classe, va via il suo termine", termini_di(con, id1), "Magia > Astrologia")

# la maschera toglie la classe senza portare i termini: il termine sotto la classe tolta non resta orfano, gli altri restano
f <- leggi_fascicolo(con, id1); f$reati <- c("Magia", "Crimini sessuali"); f$reato_termini <- coppie(c("Magia", "Astrologia"), c("Crimini sessuali", "Bigamia")); salva_fascicolo(con, f)
f <- leggi_fascicolo(con, id1); f$reato_termini <- NULL; f$reati <- "Crimini sessuali"; salva_fascicolo(con, f)
atteso("tolta la classe senza portare i termini: via il suo termine, resta l'altro", termini_di(con, id1), "Crimini sessuali > Bigamia")
f <- leggi_fascicolo(con, id1); f$reato_termini <- NULL; f$reati <- c("Magia", "Crimini sessuali"); salva_fascicolo(con, f)
f <- leggi_fascicolo(con, id1); f$reati <- "Magia"; f$reato_termini <- coppie(c("Magia", "Astrologia")); salva_fascicolo(con, f)

# un termine si toglie portando l'elenco senza di lui; l'elenco vuoto toglie tutto
f <- leggi_fascicolo(con, id1); f$reato_termini <- f$reato_termini[0, ]; salva_fascicolo(con, f)
atteso("elenco vuoto: nessun termine", termini_di(con, id1), character(0))

# lettura di molte schede insieme = lettura di una per una
id2 <- nuova(con, 2L, c("Eresia", "Crimini sessuali"), coppie(c("Eresia", "Apostasia"), c("Crimini sessuali", "Bigamia")))
id3 <- nuova(con, 3L, "Censura")
tutte <- leggi_fascicoli(con, c(id1, id2, id3))
atteso("leggi_fascicoli: stessi termini della lettura singola", lapply(tutte, function(x) x$reato_termini),
       lapply(c(id1, id2, id3), function(i) leggi_fascicolo(con, i)$reato_termini))
atteso("una scheda senza termini ha un elenco vuoto con le due colonne", names(tutte[[3]]$reato_termini), c("reato", "termine"))

# copia di sicurezza: esce, rientra in un database nuovo, si ritrova
p <- tempfile(fileext = ".json"); esporta_backup_json(con, p)
atteso("il backup ha i termini", nrow(jsonlite::fromJSON(p)$reato_termini), 2L)
nuovo <- apri_db(tempfile(fileext = ".sqlite")); invisible(ripristina_backup_json(nuovo, p, modo = "accoda"))
atteso("il backup ripristinato ha gli stessi termini", dbGetQuery(nuovo, "SELECT reato, termine FROM reato_termini ORDER BY reato, termine"),
       dbGetQuery(con, "SELECT reato, termine FROM reato_termini ORDER BY reato, termine"))
# un backup di prima del tesauro (senza la sezione) si legge ancora
vecchio <- jsonlite::fromJSON(p, simplifyVector = FALSE); vecchio$reato_termini <- NULL
pv <- tempfile(fileext = ".json"); writeLines(jsonlite::toJSON(vecchio, auto_unbox = TRUE, null = "null"), pv)
senza <- apri_db(tempfile(fileext = ".sqlite")); r <- ripristina_backup_json(senza, pv, modo = "accoda")
atteso("un backup vecchio si ripristina, senza termini", c(r$importati, dbGetQuery(senza, "SELECT COUNT(*) n FROM reato_termini")$n), c(3L, 0L))

# cancellare la scheda porta via i suoi termini
dbExecute(con, "DELETE FROM fascicoli WHERE id = ?", list(id2))
atteso("cancellata la scheda, via i termini", dbGetQuery(con, "SELECT COUNT(*) n FROM reato_termini WHERE fascicolo_id = ?", list(id2))$n, 0L)

# migrazione: un database con «Amministrazione» fra i capi la riporta a Giurisdizione e ne conserva il termine; una seconda apertura non cambia nulla
vec <- tempfile(fileext = ".sqlite"); cv <- apri_db(vec)
ia <- nuova(cv, 1L, c("Amministrazione", "Magia")); ib <- nuova(cv, 2L, c("Giurisdizione", "Tribunali locali"))
pa <- leggi_fascicolo(cv, ia); dbExecute(cv, "INSERT INTO persona_reati (fascicolo_id, persona, reato) VALUES (?, 'Rossi, Mario', 'Amministrazione')", list(ia))
atteso("prima della migrazione i valori sono fra i capi", dbGetQuery(cv, "SELECT COUNT(*) n FROM reati WHERE reato IN ('Amministrazione','Tribunali locali')")$n, 2L)
dbDisconnect(cv); cv <- apri_db(vec)
atteso("i capi: classe al posto del termine, ordine intatto", list(leggi_fascicolo(cv, ia)$reati, leggi_fascicolo(cv, ib)$reati), list(c("Giurisdizione", "Magia"), "Giurisdizione"))
atteso("i termini restano registrati", c(termini_di(cv, ia), termini_di(cv, ib)), c("Giurisdizione > Amministrazione", "Giurisdizione > Tribunali locali"))
atteso("anche la coppia persona-capo segue la classe", dbGetQuery(cv, "SELECT reato FROM persona_reati WHERE fascicolo_id = ?", list(ia))$reato, "Giurisdizione")
dbDisconnect(cv); cv <- apri_db(vec)
atteso("seconda apertura: nulla cambia", c(nrow(dbGetQuery(cv, "SELECT * FROM reato_termini")), nrow(dbGetQuery(cv, "SELECT * FROM reati"))), c(2L, 3L))

# una decisione del fondo che ammette il valore vince: la migrazione non lo tocca
vf <- tempfile(fileext = ".sqlite"); cf <- apri_db(vf)
ic <- nuova(cf, 1L, "Magia"); fid <- dbGetQuery(cf, "SELECT fondo_id FROM fascicoli WHERE id = ?", list(ic))$fondo_id
dbExecute(cf, "INSERT INTO reati (fascicolo_id, reato, ordine) VALUES (?, 'Amministrazione', 2)", list(ic))
registra_valore(cf, fid, "reato", "Amministrazione")
dbDisconnect(cf); cf <- apri_db(vf)
atteso("il fondo ha ammesso «Amministrazione»: la migrazione la lascia", dbGetQuery(cf, "SELECT reato FROM reati WHERE fascicolo_id = ? ORDER BY ordine", list(ic))$reato, c("Magia", "Amministrazione"))
atteso("e non registra nessun termine", nrow(dbGetQuery(cf, "SELECT * FROM reato_termini")), 0L)

# ── interfaccia (passo 3d): maschera, elenco, ricerca, Qualità ──
ci <- apri_db(tempfile(fileext = ".sqlite"))
j1 <- nuova(ci, 1L, c("Crimini sessuali", "Magia"), coppie(c("Crimini sessuali", "Bigamia"), c("Magia", "Astrologia")))
j2 <- nuova(ci, 2L, "Crimini sessuali", coppie(c("Crimini sessuali", "Bigamia")))
j3 <- nuova(ci, 3L, "Eresia")
# dal fascicolo ai valori della maschera, e dalla maschera di nuovo al fascicolo: le stesse coppie
p <- profilo_completo()
atteso("il profilo della maschera ha il campo dei termini", "reato_termini" %in% p$campo, TRUE)
v <- valori_da_fascicolo(leggi_fascicolo(ci, j1), p)
atteso("la maschera riceve i termini della scheda", sort(v$reato_termini), c("Astrologia", "Bigamia"))
inp <- v; inp$reato_termini <- "Bigamia"
atteso("la maschera restituisce le coppie (classe, termine)", raccogli_modulo(inp, p)$reato_termini,
       data.frame(reato = "Crimini sessuali", termine = "Bigamia", stringsAsFactors = FALSE))
inp$reato_termini <- NULL
atteso("nessuna casella spuntata: elenco vuoto, non «invariato»", nrow(raccogli_modulo(inp, p)$reato_termini), 0L)
inp$reato_termini <- c("Bigamia", "Inventato")
atteso("un termine che il profilo non conosce non passa", raccogli_modulo(inp, p)$reato_termini$termine, "Bigamia")
# salvare dalla maschera con una sola casella toglie l'altro termine
f <- leggi_fascicolo(ci, j1); f$reato_termini <- raccogli_modulo(list(reato_termini = "Astrologia"), p)$reato_termini; salva_fascicolo(ci, f)
atteso("salvato dalla maschera", termini_di(ci, j1), "Magia > Astrologia")
f <- leggi_fascicolo(ci, j1); f$reato_termini <- coppie(c("Crimini sessuali", "Bigamia"), c("Magia", "Astrologia")); salva_fascicolo(ci, f)
# l'elenco porta i termini, la ricerca li trova (classe e termine), senza confondere le schede
d <- elenco_fascicoli(ci)
atteso("l'elenco ha la colonna dei termini", d$termini[match(c(j1, j2, j3), d$id)], c("Bigamia | Astrologia", "Bigamia", NA_character_))
trovate <- function(q) sort(filtra_avanzato(d, q)$id)
atteso("reato:bigamia trova le schede col termine", trovate("reato:bigamia"), sort(c(j1, j2)))
atteso("reato:astrologia trova una scheda sola", trovate("reato:astrologia"), j1)
atteso("reato:eresia trova ancora la classe", trovate("reato:eresia"), j3)
atteso("reato:crimini trova ancora le classi", trovate("reato:crimini"), sort(c(j1, j2)))
atteso("il testo libero cerca anche nei termini", trovate("astrologia"), j1)
# Qualità: un termine senza la sua classe è una anomalia, e si vede
q0 <- qualita(ci); atteso("Qualità: nessun termine orfano", q0$casi[q0$controllo == "Termini senza la loro classe"], 0L)
dbExecute(ci, "INSERT INTO reato_termini (fascicolo_id, reato, termine) VALUES (?, 'Eresia', 'Apostasia')", list(j1))
q1 <- qualita(ci); atteso("Qualità: un termine orfano si conta", q1$casi[q1$controllo == "Termini senza la loro classe"], 1L)
atteso("Qualità: e porta alla scheda", qualita_fascicoli(ci, "Termini senza la loro classe"), j1)
f <- leggi_fascicolo(ci, j1); salva_fascicolo(ci, f)
atteso("salvando la scheda l'orfano sparisce", qualita(ci)$casi[qualita(ci)$controllo == "Termini senza la loro classe"], 0L)

cat(sprintf("\nverifiche: %d passate, %d fallite\n", .esiti$ok, length(.esiti$falliti)))
if (length(.esiti$falliti)) { cat("ESITO: NON TORNA\n"); quit(status = 1) }
cat("ESITO: ok - la tabella dei termini regge\n")
