# ─────────────────────────────────────────────────────────────
# verifiche/pulisci.R: «Pulisci un foglio» (7/10, pulisci_foglio in R/arianna_pulisci.R)
# Un foglio sporco esce nel modello con le regole dell'import e le decisioni già registrate; il database vero
# non si tocca; quattro fogli di conti (Correzioni, Registro modifiche, Da decidere, Respinte); uno zip se i fondi sono più d'uno; licenza e autore
# viaggiano nelle proprietà del file; le righe senza segnatura (note in fondo al foglio) non diventano fascicoli.
#
#     Rscript verifiche/pulisci.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny)
  for (f in c("percorsi", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo", "analisi", "qualita",
              "attribuzione", "ricerca", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "revisione_import", "colonne", "lista_luoghi", "luoghi", "fusioni", "luoghi_citati", "elenco",
              "backup_electron", "backup"))
    source(file.path("R", paste0(f, ".R")))
})
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else {
    .esiti$falliti <- c(.esiti$falliti, nome)
    cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto)
  }
}

# Il «database vero»: due fondi, una decisione registrata per Alfa, l'attribuzione.
da <- apri_db(tempfile(fileext = ".sqlite"))
nuova <- function(db, fondo, busta, num, persone, reati) {
  f <- fascicolo_vuoto(); f$fondo <- fondo; f$archivio <- paste("Archivio di", fondo); f$busta <- busta; f$fascicolo <- num
  f$tipologia_documentaria <- "Processi"; f$persone <- persone; f$reati <- reati; f$estremo_remoto <- 1600L
  f$lingue <- c("Italiano", "Latino"); f$luoghi <- "Capua"; salva_fascicolo(db, f)
}
invisible(nuova(da, "Alfa", 1L, 1L, "Rossi, Mario", "Magia")); invisible(nuova(da, "Beta", 1L, 1L, "Verdi, Paolo", "Eresia"))
registra_valore(da, "Alfa", "reato", "Pirateria", "Giurisdizione")
scrivi_impostazione(da, "attribuzione_schedatore", "Stefano Colavecchia")
prima <- dbGetQuery(da, "SELECT COUNT(*) n FROM fascicoli")$n

# Un foglio sporco: lo si fa dal modello, poi si sporca.
base <- do.call(rbind, lapply(c("Alfa", "Beta"), function(f) tabella_arianna(da, fondo = f)))
col <- function(d, nome) which(.chiave(names(d)) == .chiave(nome))
scrivi <- function(d, file) { openxlsx::write.xlsx(d, file); file }
sporco <- base[c(1, 1, 1), ]                                    # tre righe dello stesso fondo; la terza ripete la segnatura
sporco[[col(sporco, "Segnatura")]] <- unname(mapply(function(x, r) sub("fasc\\. 1$", r, x), sporco[[col(sporco, "Segnatura")]], c("fasc. 11", "fasc. 12", "fasc. 11")))
sporco[[col(sporco, "Soggettario")]] <- c("Crimini Sessuali|Pippo", "Pirateria", "Magia")
sporco[[col(sporco, "Lingua")]] <- c("italiano|latino", "Italiano; Latino", "italiano")
sporco[[col(sporco, "Persone citate o indicizzate")]] <- c("De Lama, Anna", "Rossi, Mario", "Rossi, Mario")
f1 <- scrivi(sporco, tempfile(fileext = ".xlsx"))
a <- leggi_arianna(f1)
p <- pulisci_foglio(a, da = da, nome = "sporco.xlsx")
atteso("un fondo: un file xlsx col nome del foglio", p$nome, "sporco-pulito.xlsx")
atteso("righe: tre lette, due pulite, una respinta (segnatura ripetuta)", c(p$letti, p$importati, nrow(p$respinti)), c(3L, 2L, 1L))
atteso("il database vero non cambia", dbGetQuery(da, "SELECT COUNT(*) n FROM fascicoli")$n, prima)
pulito <- openxlsx::read.xlsx(p$percorso, 1)
atteso("pulito: nel modello a 28 colonne", ncol(pulito), 28L)
atteso("pulito: le maiuscole si riscrivono, il valore nuovo resta", pulito[[col(pulito, "Soggettario")]][1], "Crimini sessuali|Pippo")
atteso("pulito: la variante registrata per il fondo si applica", pulito[[col(pulito, "Soggettario")]][2], "Giurisdizione")
atteso("pulito: lingue minuscole con «; »", pulito[[col(pulito, "Lingua")]], c("italiano; latino", "italiano; latino"))
atteso("pulito: la particella maiuscola torna minuscola e va in fondo", pulito[[col(pulito, "Persone citate o indicizzate")]][1], "Lama, Anna de")
atteso("pulito: la tipologia del foglio è «Atti»", unique(pulito[[col(pulito, "Tipologia documentaria")]]), "Atti")
fogli <- names(openxlsx::loadWorkbook(p$percorso))
atteso("quattro fogli di conti accanto al modello", fogli[-1], c("Correzioni", "Registro modifiche", "Da decidere", "Respinte"))
atteso("Correzioni dice la regola delle maiuscole", any(grepl("Crimini Sessuali", openxlsx::read.xlsx(p$percorso, "Correzioni")[[1]], fixed = TRUE)), TRUE)
atteso("Respinte dice il motivo", nzchar(openxlsx::read.xlsx(p$percorso, "Respinte")$Motivo[1]), TRUE)
nucleo <- tempfile(); utils::unzip(p$percorso, files = "docProps/core.xml", exdir = nucleo)
atteso("l'autore viaggia nelle proprietà del file", grepl("Stefano Colavecchia", paste(readLines(file.path(nucleo, "docProps", "core.xml"), warn = FALSE), collapse = ""), fixed = TRUE), TRUE)
atteso("il foglio pulito si reimporta identico", { b <- apri_db(":memory:"); invisible(importa_arianna(b, leggi_arianna(p$percorso)))
  d <- tabella_arianna(b, fondo = "Alfa"); m1 <- as.matrix(d); m2 <- as.matrix(pulito); dimnames(m1) <- dimnames(m2) <- NULL
  m1[is.na(m1)] <- ""; m2[is.na(m2)] <- ""   # una cella vuota è «» nel database e NA letta dal foglio
  uguali <- identical(dim(m1), dim(m2)) && all(m1 == m2)
  uguali }, TRUE)

# Due fondi nello stesso foglio: uno zip con un file per fondo.
f2 <- scrivi(base, tempfile(fileext = ".xlsx"))
p2 <- pulisci_foglio(leggi_arianna(f2), da = da, nome = "due.xlsx")
atteso("due fondi: uno zip", c(p2$nome, length(p2$fondi)), c("due-pulito.zip", 2L))
atteso("due fondi: un file per fondo nello zip", sort(zip::zip_list(p2$percorso)$filename), c("due-pulito-Alfa.xlsx", "due-pulito-Beta.xlsx"))

# Senza database di appoggio (`da` assente) funziona lo stesso, solo senza le decisioni registrate.
p3 <- pulisci_foglio(a, nome = "sporco.xlsx")
atteso("senza decisioni registrate la variante resta com'era", openxlsx::read.xlsx(p3$percorso, 1)[[col(pulito, "Soggettario")]][2], "Pirateria")

# Un foglio senza righe: errore chiaro.
vuoto <- scrivi(base[0, ], tempfile(fileext = ".xlsx"))
atteso("foglio vuoto: errore", inherits(try(pulisci_foglio(leggi_arianna(vuoto), da = da), silent = TRUE), "try-error"), TRUE)

# Righe senza segnatura (Archiginnasio di Bologna, 7/10): le note in fondo al foglio non diventano fascicoli, in nessuna modalità.
note <- base[c(1, 1), ]; note[] <- NA
note[[col(note, "Note libere")]] <- c("La metadatazione effettuata segue l'inventario di Mancini.", "Altra nota in fondo al foglio.")
fn <- scrivi(rbind(base[1, ], note), tempfile(fileext = ".xlsx")); an <- leggi_arianna(fn)
atteso("senza segnatura: due righe", length(righe_senza_segnatura(an)), 2L)
atteso("senza segnatura: l'anteprima lo dice", any(grepl("senza segnatura", correzioni_import(an)$regola)), TRUE)
atteso("senza segnatura: e non le chiama «segnature ripetute»", any(grepl("ripetute", correzioni_import(an)$regola)), FALSE)
b1 <- apri_db(":memory:"); r1 <- importa_arianna(b1, an)
atteso("senza segnatura, accoda: una entra e due sono respinte con il loro motivo", c(r1$importati, nrow(r1$respinti), as.integer(identical(unique(r1$respinti$motivo), .MOTIVO_SENZA_SEGNATURA))), c(1L, 2L, 1L))
b2 <- apri_db(":memory:"); r2 <- importa_nuovo_archivio(b2, an, "Archivio nuovo", "Fondo nuovo")
atteso("senza segnatura, nuovo fondo: una scheda sola, nessuna senza numeri",
       c(dbGetQuery(b2, "SELECT COUNT(*) n FROM fascicoli")$n, dbGetQuery(b2, "SELECT COUNT(*) n FROM fascicoli WHERE busta IS NULL AND fascicolo IS NULL")$n), c(1L, 0L))
atteso("senza segnatura, nuovo fondo: le due righe stanno fra le respinte", c(r2$importati, nrow(r2$respinti)), c(1L, 2L))
p4 <- pulisci_foglio(an, nome = "note.xlsx")
atteso("senza segnatura, pulisci: una riga pulita, due respinte", c(p4$importati, nrow(p4$respinti)), c(1L, 2L))
solo_note <- scrivi(note, tempfile(fileext = ".xlsx"))
atteso("tutte senza segnatura: errore chiaro", inherits(try(importa_nuovo_archivio(apri_db(":memory:"), leggi_arianna(solo_note), "A", "F"), silent = TRUE), "try-error"), TRUE)
# L'archivio accanto al fondo (7/10): il foglio dice il suo archivio, e il fondo ha il suo.
atteso("archivi nel foglio: archivio, fondo e righe", archivi_nel_foglio(leggi_arianna(f2))[, c("archivio", "fondo", "righe")],
       data.frame(archivio = c("Archivio di Alfa", "Archivio di Beta"), fondo = c("Alfa", "Beta"), righe = c(1L, 1L), stringsAsFactors = FALSE)[order(c(-1, -1)), ])
atteso("archivio del fondo", c(archivio_del_fondo(da, "Alfa"), archivio_del_fondo(da, "Beta")), c("Archivio di Alfa", "Archivio di Beta"))
atteso("archivio del fondo: senza fondo o fondo che non c'è, niente", c(is.null(archivio_del_fondo(da, "")), is.null(archivio_del_fondo(da, "Gamma"))), c(TRUE, TRUE))
atteso("i campi di archivio e fondo non chiedono nomi di persona al browser",
       grepl('autocomplete="off"', as.character(senza_suggerimenti(textInput("x", "Fondo"))), fixed = TRUE), TRUE)

# --- dati puliti e refusi corretti, solo le 28 colonne (Stefano, 8/10) -----------------------------
modello <- names(openxlsx::read.xlsx("dati/modello_export.xlsx", 1, sep.names = " "))
atteso("pulito: esattamente le colonne del modello, nello stesso ordine", names(openxlsx::read.xlsx(p$percorso, 1, sep.names = " ")), modello)
extra <- base[c(1, 1, 1), ]; extra[[col(extra, "Segnatura")]] <- c("Archivio di Alfa, Alfa, b. 5, fasc. 21", "Archivio di Alfa, Alfa, b. 5, fasc. 22", "Archivio di Alfa, Alfa, b. 5, fasc. 23")
extra[[col(extra, "Soggettario")]] <- c("Magia.", "Pippo", "Eresia")
extra[["Colonna di troppo"]] <- c("a", "b", "c"); extra[["Trascrizione"]] <- c("x", "y", "z")      # due colonne che il modello non ha
extra[["Eventi"]] <- NULL; extra[["Numerazione"]] <- NULL                                          # e due che il modello ha e il foglio no
p2 <- pulisci_foglio(leggi_arianna(scrivi(extra, tempfile(fileext = ".xlsx"))), da = da, nome = "refusi.xlsx")
pul2 <- openxlsx::read.xlsx(p2$percorso, 1, sep.names = " ")
atteso("le colonne di troppo non escono e quelle assenti si aggiungono: sempre le 28", names(pul2), modello)
atteso("le colonne assenti sono vuote nel foglio pulito", all(is.na(pul2[["Eventi"]])) && all(is.na(pul2[["Numerazione"]])), TRUE)
atteso("un refuso a un passo dal vocabolario si corregge («Magia.» → «Magia»)", pul2[[col(pul2, "Soggettario")]][1], "Magia")
atteso("un valore che non assomiglia a niente resta com'è", pul2[[col(pul2, "Soggettario")]][2], "Pippo")
corr2 <- openxlsx::read.xlsx(p2$percorso, "Correzioni")
atteso("Correzioni dice il refuso corretto, con la colonna e le schede", unlist(corr2[grepl("refuso corretto", corr2[[1]], fixed = TRUE), ]), c(Regola = "refuso corretto: «Magia.» → «Magia»", Colonna = "Soggettario (reato)", Celle = "1"))
atteso("Correzioni dice quali colonne ha aggiunto", sort(corr2[[2]][grepl("assente nel foglio", corr2[[1]], fixed = TRUE)]), c("Eventi", "Numerazione"))
dec2 <- openxlsx::read.xlsx(p2$percorso, "Da decidere")
# «Reati della riga» (02/11) confronta regesto e soggettario: qui il regesto è quello del foglio base, che dice un altro reato; non c'entra col refuso.
atteso("Da decidere elenca «Pippo», non il refuso già corretto", dec2$Valore[dec2$Colonna != "Reati della riga"], "Pippo")
atteso("il risultato dice quanti valori restano da decidere", p2$da_decidere$Valore[p2$da_decidere$Colonna != "Reati della riga"], "Pippo")
atteso("il database vero non cambia nemmeno col refuso", dbGetQuery(da, "SELECT COUNT(*) n FROM fascicoli")$n, prima)

# --- il regesto strutturato col punto finale dell'export non crea un reato «Magia.» ---
atteso("analizza_regesto: l'ultima riga col punto finale", analizza_regesto("Nome del luogo di consumazione del reato: Modena\nTipo di reato: Magia.")$reati, "Magia")
atteso("analizza_regesto: «no.» finale", analizza_regesto("Tipo di reato: Eresia\nApplicazione della tortura: no\nSentenza o decreto formale di spedizione: no.")$sentenza, "no")

# --- le equivalenze fuori vocabolario (22/10): Bigamia, Apostasia, Ebrei entrano nella loro classe, e si vede ---
eq <- base[c(1, 1, 1, 1), ]
eq[[col(eq, "Segnatura")]] <- unname(mapply(function(x, r) sub("fasc\\. 1$", r, x), eq[[col(eq, "Segnatura")]], c("fasc. 21", "fasc. 22", "fasc. 23", "fasc. 24")))
eq[[col(eq, "Soggettario")]] <- c("Bigamia", "apostasia", "Ebrei", "Magia|Bigamia")
pe <- pulisci_foglio(leggi_arianna(scrivi(eq, tempfile(fileext = ".xlsx"))), da = da, nome = "eq.xlsx")
pq <- openxlsx::read.xlsx(pe$percorso, 1)
atteso("equivalenze: Bigamia, apostasia, Ebrei, Magia|Bigamia", pq[[col(pq, "Soggettario")]], c("Crimini sessuali", "Eresia", "Ebraismo", "Magia|Crimini sessuali"))
atteso("equivalenze: la regola è dichiarata nelle Correzioni", any(grepl("«Bigamia» diventa «Crimini sessuali» (equivalenza della Guida al Tab)", openxlsx::read.xlsx(pe$percorso, "Correzioni")[[1]], fixed = TRUE)), TRUE)
atteso("equivalenze: una decisione registrata del fondo vince", {
  registra_valore(da, "Alfa", "reato", "Bigamia", "Clero"); r <- regole_fondo(da, "Alfa"); r$forma[r$campo == "reato" & r$valore == "Bigamia"] }, "Clero")

cat(sprintf("\nverifiche: %d passate, %d fallite\n", .esiti$ok, length(.esiti$falliti)))
if (length(.esiti$falliti)) { cat("ESITO: NON TORNA\n"); quit(status = 1) }
cat("ESITO: ok\n")
