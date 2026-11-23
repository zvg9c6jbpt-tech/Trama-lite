# ─────────────────────────────────────────────────────────────
# verifiche/profilo_dominio.R: il profilo del materiale (profili/<nome>/profilo.json)
# 1. Il profilo si legge e dà gli stessi valori che prima stavano scritti nel codice (impronta qui sotto).
# 2. Un profilo mancante, incompleto o rotto ferma l'avvio e dice che cosa manca: niente valori di ripiego.
# 3. Le funzioni che lo usano fanno ciò che facevano (produttore, città, sovrani, «S.»).
#
#     Rscript verifiche/profilo_dominio.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1", TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({
  library(shiny)
  for (f in c("percorsi", "profilo_dominio", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo",
              "analisi", "qualita", "attribuzione", "ricerca", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "lista_luoghi", "luoghi"))
    source(file.path("R", paste0(f, ".R")))
})
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else { .esiti$falliti <- c(.esiti$falliti, nome); cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto) }
}
ferma <- function(codice, ...) tryCatch({ codice; NA_character_ }, error = function(e) conditionMessage(e))

# ── 1. i valori del profilo «inquisizione» sono quelli di prima ──
atteso("anni ammessi", ANNI_AMMESSI, c(1200L, 1899L))
atteso("soggettario: 19 classi, Eresia per prima", c(length(VOCABOLARI$reato), VOCABOLARI$reato[1]), c("19", "Eresia"))
atteso("supporti", VOCABOLARI$supporto, c("cartaceo", "papiraceo", "pergamenaceo", "membranaceo"))
atteso("lingue", VOCABOLARI$lingua, c("Italiano", "Latino", "Francese", "Slavo"))
atteso("definizione", VOCABOLARI$definizione, "fascicolo")
atteso("tipologie", VOCABOLARI$tipologia_documentaria, c("Atti", "Processi", "Atti per diversi", "Informazioni", "Denunce", "Atti difensivi"))
atteso("produttori", VOCABOLARI$soggetto[1], "Inquisizione di Modena")
atteso("equivalenze scritte nel profilo (D20)", EQUIVALENZE_ESPLICITE, list(reato = c(Bigamia = "Crimini sessuali", Apostasia = "Eresia", Ebrei = "Ebraismo")))
atteso("equivalenze: le tre di D20 e in più i termini del tesauro (anche le varianti), senza doppioni", c(length(EQUIVALENZE$reato), anyDuplicated(tolower(names(EQUIVALENZE$reato)))), c(26L, 0L))
atteso("equivalenze: «Astrologia» va in Magia, «Questua non autorizzata» in Giurisdizione", unname(EQUIVALENZE$reato[c("Astrologia", "Questua non autorizzata")]), c("Magia", "Giurisdizione"))
atteso("tesauro: 22 termini (20 sigle di Trenti + Amministrazione e Tribunali locali), tutti sotto una classe del vocabolario", c(nrow(TERMINI_REATO), as.integer(all(TERMINI_REATO$classe %in% VOCABOLARI$reato))), c(22L, 1L))
cl <- function(sigla) TERMINI_REATO$classe[which(TERMINI_REATO$sigla == sigla)]
atteso("tesauro: le decisioni del 17/11", c(cl("AP"), cl("AS"), cl("IP"), cl("OS")), c("Eresia", "Magia", "Bestemmia e miscredenza", "Bestemmia e miscredenza"))
atteso("tesauro: sigle di Trenti → classi", vapply(c("BG", "MS", "MST", "BE", "SA", "CF", "EC", "GR", "QT", "HB", "LP", "ER", "PE", "(AB)", "SM", "CP"), cl, ""),
       c(BG = "Crimini sessuali", MS = "Magia", MST = "Magia", BE = "Bestemmia e miscredenza", SA = "Bestemmia e miscredenza", CF = "Sacramenti", EC = "Sacramenti",
         GR = "Giurisdizione", QT = "Giurisdizione", HB = "Ebraismo", LP = "Censura", ER = "Eresia", PE = "Eresia", `(AB)` = "Eresia", SM = "Santità", CP = "Bestemmia e miscredenza"))
# D20 e il tesauro non si contraddicono: ogni equivalenza che nomina un termine del tesauro lo manda nella stessa classe
for (t in intersect(names(EQUIVALENZE_ESPLICITE$reato), TERMINI_REATO$termine))
  atteso(paste0("tesauro e D20 coincidono: ", t), unname(EQUIVALENZE_ESPLICITE$reato[t]), TERMINI_REATO$classe[TERMINI_REATO$termine == t])
atteso("tesauro: un secondo profilo senza termini", length(leggi_profilo_dominio("epistolario", "verifiche/profili_prova")$tesauro$reato$termini), 0L)
atteso("normalizzazioni", NORMALIZZAZIONI, c("Eresia*" = "Eresia"))
atteso("tipi del titolo", TIPI_TITOLO[["Processo"]], "contro")
atteso("connettori fissi", names(CONNETTORE_FISSO), c("Atti", "Denuncia", "Denunce"))
atteso("plurale", PLURALE_TIPO, c(Denuncia = "Denunce"))
atteso("tipologia → parola", unname(PAROLA_TIPOLOGIA[c("Processi", "Denunce")]), c("Processo", "Denuncia"))
atteso("«Atti per diversi»", TIPOLOGIA_PER_DIVERSI, "Atti per diversi")
atteso("correzioni di segnatura", vapply(CORREZIONI_SEGNATURA, function(c) c$fascicolo, 1L), c(11L, 11L))

# ── 2. un profilo guasto ferma l'avvio ──
tmp <- tempfile("profili"); dir.create(file.path(tmp, "prova"), recursive = TRUE)
ok_json <- readLines("profili/inquisizione/profilo.json", encoding = "UTF-8", warn = FALSE)
atteso("profilo che non esiste", grepl("non trovato", ferma(leggi_profilo_dominio("nonesiste", tmp))), TRUE)
atteso("nome non valido", grepl("non valido", ferma(leggi_profilo_dominio("../x", tmp))), TRUE)
writeLines("{ non json", file.path(tmp, "prova", "profilo.json"))
atteso("JSON rotto", grepl("JSON valido", ferma(leggi_profilo_dominio("prova", tmp))), TRUE)
mutilato <- jsonlite::fromJSON(paste(ok_json, collapse = "\n"), simplifyVector = FALSE); mutilato$sovrani <- NULL
writeLines(jsonlite::toJSON(mutilato, auto_unbox = TRUE), file.path(tmp, "prova", "profilo.json"))
atteso("sezione mancante", grepl("«sovrani».*«titoli»", ferma(leggi_profilo_dominio("prova", tmp))), TRUE)
anni_rotti <- jsonlite::fromJSON(paste(ok_json, collapse = "\n"), simplifyVector = FALSE); anni_rotti$anni_ammessi <- list(1900, 1800)
writeLines(jsonlite::toJSON(anni_rotti, auto_unbox = TRUE), file.path(tmp, "prova", "profilo.json"))
atteso("anni invertiti", grepl("anni_ammessi", ferma(leggi_profilo_dominio("prova", tmp))), TRUE)
# il tesauro dei reati: ogni guasto ferma l'avvio e dice quale
guasta_tesauro <- function(cambia) {
  g <- jsonlite::fromJSON(paste(ok_json, collapse = "\n"), simplifyVector = FALSE); g <- cambia(g)
  writeLines(jsonlite::toJSON(g, auto_unbox = TRUE), file.path(tmp, "prova", "profilo.json")); ferma(leggi_profilo_dominio("prova", tmp)) }
atteso("tesauro: sezione mancante", grepl("«tesauro».*«reato»", guasta_tesauro(function(g) { g$tesauro <- NULL; g })), TRUE)
atteso("tesauro: classe che non esiste", grepl("classe non nel vocabolario: «Inventata»", guasta_tesauro(function(g) { g$tesauro$reato$termini[[1]]$classe <- "Inventata"; g })), TRUE)
atteso("tesauro: identificativo ripetuto", grepl("identificativo ripetuto", guasta_tesauro(function(g) { g$tesauro$reato$termini[[2]]$id <- g$tesauro$reato$termini[[1]]$id; g })), TRUE)
atteso("tesauro: termine ripetuto (anche in minuscolo)", grepl("ripetuti", guasta_tesauro(function(g) { g$tesauro$reato$termini[[2]]$termine <- tolower(g$tesauro$reato$termini[[1]]$termine); g })), TRUE)
atteso("tesauro: un termine ha il nome di una classe", grepl("nome di una classe", guasta_tesauro(function(g) { g$tesauro$reato$termini[[1]]$termine <- "Magia"; g })), TRUE)
atteso("tesauro: manca l'origine", grepl("non ha «origine»", guasta_tesauro(function(g) { g$tesauro$reato$termini[[1]]$origine <- NULL; g })), TRUE)
writeLines(ok_json, file.path(tmp, "prova", "profilo.json"))
atteso("profilo intatto", leggi_profilo_dominio("prova", tmp)$profilo$nome, "inquisizione")

# ── 3. le funzioni che lo usano ──
atteso("produttore: forma lunga", .produttore_canonico("Tribunale dell'Inquisizione di Modena"), "Inquisizione di Modena")
atteso("produttore: forma lunga con apostrofo tipografico", .produttore_canonico("Tribunale dell’Inquisizione di Modena"), "Inquisizione di Modena")
atteso("produttore: maiuscola della città", .produttore_canonico("inquisizione di benevento"), "Inquisizione di Benevento")
atteso("produttore: Roma è Sant'Uffizio", .produttore_canonico(c("Inquisizione di Roma", "inquisizione di roma")), "Sant'Uffizio")
atteso("produttore: un altro ente resta", .produttore_canonico("Archivio di Stato"), "Archivio di Stato")
atteso("città dal produttore", citta_da_produttore(c("Inquisizione di Capua", "Sant'Uffizio", "Santo Uffizio d'Aversa")), c("Capua", "Aversa", "Roma"))
atteso("sovrano: papa", .formula_sovrano("Paolo (Paolo; papa)"), TRUE)
atteso("sovrano: non un titolo", .formula_sovrano("Paolo (Paolo; frate)"), FALSE)
atteso("«S.»: Uffizio, Sede, Padre", sciogli_santi(c("S. Uffizio", "S. Sede", "S. Padre")), c("Sant'Uffizio", "Santa Sede", "Santo Padre"))
atteso("«S.»: Sacra Congregazione non si scioglie", sciogli_santi("S. Congregazione"), "S. Congregazione")
atteso("«S.»: un santo", sciogli_santi("S. Severo"), "San Severo")
atteso("«S.t» davanti a Officio (profilo)", sciogli_santi("S.t Officio"), "Sant'Officio")
atteso("«S.t» davanti a un nome resta", sciogli_santi("S.t Severo"), "S.t Severo")
atteso("scheda di prova dal profilo", unlist(PROFILO_DOMINIO$prova[c("archivio", "fondo", "serie")], use.names = FALSE),
       c("Archivio di Stato di Modena", "Tribunale dell'Inquisizione di Modena", "Processi"))
# le anagrafiche degli archivi
atteso("anagrafiche: 6 archivi, tutti con Wikidata", c(length(PROFILO_DOMINIO$anagrafiche$archivi), as.integer(all(vapply(PROFILO_DOMINIO$anagrafiche$archivi, function(x) grepl("^Q[0-9]+$", x$wikidata), TRUE)))), c(6L, 1L))
atteso("anagrafiche: ricerca per nome o variante, senza badare a maiuscole", c(anagrafica_archivio("ARCHIVIO di  Stato di Modena")$isil, anagrafica_archivio("Archivio di Stato di Modena")$wikidata), c("IT-MO0172", "Q3621657"))
atteso("anagrafiche: archivio ignoto o vuoto", c(is.null(anagrafica_archivio("Archivio di Marte")), is.null(anagrafica_archivio("")), is.null(anagrafica_archivio(NA))), c(TRUE, TRUE, TRUE))
atteso("anagrafiche: un archivio senza ISIL verificato resta senza", anagrafica_archivio("Archivio di Stato di Torino")$isil, "")
atteso("anagrafiche: un secondo profilo senza archivi", length(leggi_profilo_dominio("epistolario", "verifiche/profili_prova")$anagrafiche$archivi), 0L)
atteso("anagrafiche: sezione mancante", grepl("«anagrafiche».*«archivi»", guasta_tesauro(function(g) { g$anagrafiche <- NULL; g })), TRUE)
atteso("anagrafiche: Wikidata malformato", grepl("Wikidata malformato: «Q12x»", guasta_tesauro(function(g) { g$anagrafiche$archivi[[1]]$wikidata <- "Q12x"; g })), TRUE)
atteso("anagrafiche: ISIL malformato", grepl("ISIL malformato", guasta_tesauro(function(g) { g$anagrafiche$archivi[[1]]$isil <- "MO0172"; g })), TRUE)
atteso("anagrafiche: identificativo ripetuto", grepl("identificativo ripetuto", guasta_tesauro(function(g) { g$anagrafiche$archivi[[2]]$wikidata <- g$anagrafiche$archivi[[1]]$wikidata; g })), TRUE)
atteso("anagrafiche: variante ripetuta", grepl("nome o variante ripetuti", guasta_tesauro(function(g) { g$anagrafiche$archivi[[2]]$varianti <- list("archivio di stato di modena"); g })), TRUE)
atteso("anagrafiche: archivio senza nome", grepl("non ha «nome»", guasta_tesauro(function(g) { g$anagrafiche$archivi[[1]]$nome <- ""; g })), TRUE)
# i codici ISO 639 delle lingue
atteso("lingue: ogni lingua del vocabolario ha il suo codice", codice_lingua(c("Italiano", "Latino", "Francese", "Slavo")), c("ita", "lat", "fra", "sla"))
atteso("lingue: con la norma", codice_lingua(c("Latino", "Slavo"), norma = TRUE), c("lat ISO 639-3", "sla ISO 639-5"))
atteso("lingue: una lingua che non c'è non ha codice", codice_lingua(c("Tedesco", NA)), c(NA_character_, NA_character_))
atteso("lingue: «Slavo» è un gruppo e il profilo lo dice", grepl("gruppo delle lingue slave", PROFILO_DOMINIO$tesauro$lingua$codici$Slavo$nota), TRUE)
atteso("lingue: un secondo profilo senza codici", is.null(leggi_profilo_dominio("epistolario", "verifiche/profili_prova")$tesauro$lingua), TRUE)
atteso("lingue: lingua senza codice", grepl("lingua senza codice: «Latino»", guasta_tesauro(function(g) { g$tesauro$lingua$codici$Latino <- NULL; g })), TRUE)
atteso("lingue: codice per una lingua fuori dal vocabolario", grepl("non è nel vocabolario: «Tedesco»", guasta_tesauro(function(g) { g$tesauro$lingua$codici$Tedesco <- list(codice = "deu", norma = "ISO 639-3"); g })), TRUE)
atteso("lingue: codice di forma sbagliata", grepl("non di tre lettere", guasta_tesauro(function(g) { g$tesauro$lingua$codici$Latino$codice <- "la"; g })), TRUE)
atteso("lingue: codice dato a due lingue", grepl("dato a due lingue", guasta_tesauro(function(g) { g$tesauro$lingua$codici$Latino$codice <- "ita"; g })), TRUE)
atteso("lingue: norma sconosciuta", grepl("norma mancante o sconosciuta", guasta_tesauro(function(g) { g$tesauro$lingua$codici$Latino$norma <- "ISO 639-9"; g })), TRUE)
atteso("lingue: un gruppo senza nota", grepl("serve una nota", guasta_tesauro(function(g) { g$tesauro$lingua$codici$Slavo$nota <- ""; g })), TRUE)
# gli alias dei luoghi
atteso("alias: «Casale» vale solo nell'Archivio di Stato di Torino", c(alias_luogo("Casale", "Archivio di Stato di Torino, Inquisizione, b. 1")$luogo, is.null(alias_luogo("Casale", "Archivio di Stato di Modena, b. 1")), is.null(alias_luogo("Casale", ""))), c("Casale Monferrato", "TRUE", "TRUE"))
atteso("alias: maiuscole e spazi non contano, altro nome no", c(alias_luogo("  casale ", "Archivio di Stato di Torino, b. 1")$luogo, is.null(alias_luogo("Caselle", "Archivio di Stato di Torino, b. 1"))), c("Casale Monferrato", "TRUE"))
atteso("alias: ogni luogo di arrivo è nella lista dei luoghi, una volta sola", vapply(PROFILO_DOMINIO$anagrafiche$luoghi_alias, function(x) sum(leggi_lista_luoghi("dati/luoghi.csv")$nome == x$luogo), 1L), rep(1L, length(PROFILO_DOMINIO$anagrafiche$luoghi_alias)))
atteso("alias: un secondo profilo senza alias", length(leggi_profilo_dominio("epistolario", "verifiche/profili_prova")$anagrafiche$luoghi_alias), 0L)
atteso("alias: sezione mancante", grepl("«anagrafiche».*«luoghi_alias»", guasta_tesauro(function(g) { g$anagrafiche$luoghi_alias <- NULL; g })), TRUE)
atteso("alias: senza ragione", grepl("non ha «perche»", guasta_tesauro(function(g) { g$anagrafiche$luoghi_alias[[1]]$perche <- ""; g })), TRUE)
atteso("alias: archivio fuori anagrafica", grepl("archivio non nell'anagrafica", guasta_tesauro(function(g) { g$anagrafiche$luoghi_alias[[1]]$archivio <- "Archivio di Marte"; g })), TRUE)
atteso("alias: ripetuto", grepl("alias ripetuto", guasta_tesauro(function(g) { g$anagrafiche$luoghi_alias[[2]] <- g$anagrafiche$luoghi_alias[[1]]; g })), TRUE)
# gli enti produttori noti
atteso("enti: 7 voci; ente e variante si riconoscono, il resto no", c(length(PROFILO_DOMINIO$anagrafiche$enti), anagrafica_ente("senato di  nizza")$nome, anagrafica_ente("Conseil del Commis")$variante, anagrafica_ente("Conseil des Commis")$variante, is.null(anagrafica_ente("Senato di Marte"))), c("7", "Senato di Nizza", "TRUE", "FALSE", "TRUE"))
atteso("enti: nessuna Wikidata inventata (nessun ente ne ha una non verificata)", all(vapply(PROFILO_DOMINIO$anagrafiche$enti, function(x) is.null(x$wikidata) || !nzchar(x$wikidata), TRUE)), TRUE)
atteso("enti: frase → produttore proposto, nome → niente", c(produttore_descrizione("Decreti e atti del S.Uffizio sul magnetismo animale e sui liberi muratori"), is.null(produttore_descrizione("Senato di Nizza")), is.null(produttore_descrizione("Inquisizione di Torino")), is.na(produttore_descrizione("Atti vari della curia vescovile di Modena e Reggio"))), c("Sant'Uffizio", "TRUE", "TRUE", "TRUE"))
atteso("enti: un secondo profilo senza enti", length(leggi_profilo_dominio("epistolario", "verifiche/profili_prova")$anagrafiche$enti), 0L)
atteso("enti: sezione mancante", grepl("«anagrafiche».*«enti»", guasta_tesauro(function(g) { g$anagrafiche$enti <- NULL; g })), TRUE)
atteso("enti: senza ragione", grepl("ente non ha «perche»", guasta_tesauro(function(g) { g$anagrafiche$enti[[1]]$perche <- ""; g })), TRUE)
atteso("enti: Wikidata malformato", grepl("Wikidata malformato", guasta_tesauro(function(g) { g$anagrafiche$enti[[1]]$wikidata <- "nizza"; g })), TRUE)
atteso("enti: variante ripetuta", grepl("nome o variante ripetuti", guasta_tesauro(function(g) { g$anagrafiche$enti[[2]]$varianti <- list("senato di nizza"); g })), TRUE)
atteso("traduzioni inglesi dal profilo", VOCABOLARI_EN$reato[["Crimini sessuali"]], "Sexual offences")

if (length(.esiti$falliti)) { cat("\nFALLITI:", length(.esiti$falliti), "\n"); quit(status = 1) }
cat("ESITO: ok -", .esiti$ok, "prove del profilo\n")
