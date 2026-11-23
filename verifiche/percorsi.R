# ─────────────────────────────────────────────────────────────
# verifiche/percorsi.R: dati separati dal programma (R/percorsi.R)
#
# Lavora su cartelle temporanee (TRAMA_HOME e una finta cartella
# dell'app): non tocca né il database vero né ~/Documents/Trama Lite.
# Atteso ESITO: ok; altrimenti stampa quale NON TORNA ed esce con 1.
#     Rscript verifiche/percorsi.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
ko <- 0L
verifica <- function(cond, msg) if (!isTRUE(cond)) { cat("NON TORNA:", msg, "\n"); ko <<- ko + 1L }
Sys.unsetenv("TRAMA_DB")
base <- normalizePath(LISTA_LUOGHI_BASE)

# 1. cartella predefinita e variabile
Sys.unsetenv("TRAMA_HOME")
verifica(grepl("Trama Lite$", cartella_dati()), "default non finisce in «Trama Lite»")
verifica(!grepl("Trama$", cartella_dati()), "i dati di Lite non devono stare nella cartella di Trama")
verifica(!startsWith(cartella_dati(), normalizePath(".")), "default dentro la cartella dell'app")
home <- tempfile("home-"); Sys.setenv(TRAMA_HOME = home)
verifica(identical(cartella_dati(), normalizePath(home, winslash = "/", mustWork = FALSE)), "TRAMA_HOME ignorata")

# 2. primo avvio: cartelle e lista dei luoghi seminata dalla base, senza toccare la base
md5_base <- unname(tools::md5sum(base))
r <- prepara_cartella_dati(base)
verifica(dir.exists(cartella_backup()), "manca backup/")
verifica(file.exists(r$lista) && unname(tools::md5sum(r$lista)) == md5_base, "copia personale diversa dalla base")
verifica(identical(percorso_lista_luoghi(), r$lista), "percorso_lista_luoghi non punta alla copia personale")
verifica(identical(colnames(leggi_lista_luoghi(r$lista)), c("chiave", "nome", "paese", "wikidata", "coordinate")), "la lista ha chiave, nome, paese e, accanto, identificativo Wikidata e coordinate (nessun servizio esterno)")

# 2b. un programma di sola lettura (app installata) non rende di sola lettura la copia personale
home_ro <- tempfile("home-ro-"); Sys.setenv(TRAMA_HOME = home_ro)
base_ro <- tempfile(fileext = ".csv"); file.copy(base, base_ro); Sys.chmod(base_ro, "0444")
r_ro <- prepara_cartella_dati(base_ro)
verifica(file.access(r_ro$lista, 2) == 0L, "lista personale non scrivibile: ha ereditato la sola lettura della base")
verifica(is.null(try(.scrivi_lista_luoghi(leggi_lista_luoghi(r_ro$lista), r_ro$lista), silent = TRUE)), "non si riesce a riscrivere la lista personale")
Sys.setenv(TRAMA_HOME = home); Sys.chmod(base_ro, "0644")

# 3. la copia personale non si sovrascrive al riavvio
g <- leggi_lista_luoghi(r$lista); g$paese[1] <- "modificato a mano"
.scrivi_lista_luoghi(g, r$lista)
prepara_cartella_dati(base)
verifica(leggi_lista_luoghi(r$lista)$paese[1] == "modificato a mano", "riavvio sovrascrive il personale")

# 4. base nuova: si aggiungono solo le chiavi nuove, le mie restano
b2 <- tempfile(fileext = ".csv"); gb <- leggi_lista_luoghi(base)
nuova <- gb[1, ]; nuova$chiave <- "luogo inventato"; nuova$nome <- "Luogo inventato"
.scrivi_lista_luoghi(rbind(gb, nuova), b2)
r2 <- prepara_cartella_dati(b2); gp <- leggi_lista_luoghi(r$lista)
verifica(r2$aggiunte == 1L && nrow(gp) == nrow(gb) + 1L, "voce nuova non aggiunta")
verifica(gp$paese[1] == "modificato a mano", "aggiornamento base tocca le voci mie")
verifica(prepara_cartella_dati(b2)$aggiunte == 0L, "aggiornamento ripetuto aggiunge ancora")
verifica(length(list.files(file.path(cartella_luoghi(), "backup-luoghi"))) == 1L, "manca la copia prima dell'aggiornamento")

# 4b. lista di prima (tre colonne) e base con identificativo, coordinate e correzioni (02/11/2026)
home_v <- Sys.getenv("TRAMA_HOME"); home2 <- tempfile("home-"); Sys.setenv(TRAMA_HOME = home2)
dir_b <- tempfile("base-"); dir.create(dir_b); base3 <- file.path(dir_b, "luoghi.csv")
.scrivi_lista_luoghi(data.frame(chiave = c("bargecchiamassarosa", "massarosa", "mia", "sbagliata"), nome = c("Bargecchia", "Massarosa", "Mia", "Cosa"), paese = c("Italia", "Italia", "", ""),
                                wikidata = c("Q1", "Q2", "", ""), coordinate = c("43.1, 10.1", "43.2, 10.2", "", ""), stringsAsFactors = FALSE), base3)
writeLines(c('"chiave";"nome_prima";"nome"', '"bargecchiamassarosa";"Massarosa";"Bargecchia"', '"sbagliata";"Cosa";""'), file.path(dir_b, "luoghi_correzioni.csv"))
dir.create(cartella_luoghi(), recursive = TRUE)
utils::write.table(data.frame(chiave = c("bargecchiamassarosa", "mia", "sbagliata"), nome = c("Massarosa", "Mia", "Cosa"), paese = c("Italia", "Mio paese", "")),
                   file.path(cartella_luoghi(), "luoghi.csv"), sep = ";", row.names = FALSE, qmethod = "double", fileEncoding = "UTF-8")
writeLines("vecchia", file.path(cartella_luoghi(), "base.md5"))
r4 <- prepara_cartella_dati(base3); l4 <- leggi_lista_luoghi(r4$lista)
verifica(l4$nome[l4$chiave == "bargecchiamassarosa"] == "Bargecchia", "luoghi_correzioni: il nome sbagliato di prima non è stato corretto")
verifica(!"sbagliata" %in% l4$chiave, "luoghi_correzioni: la voce con nome nuovo vuoto non è stata tolta")
verifica(l4$paese[l4$chiave == "mia"] == "Mio paese" && l4$nome[l4$chiave == "mia"] == "Mia", "luoghi_correzioni: toccata una voce mia")
verifica(l4$wikidata[l4$chiave == "bargecchiamassarosa"] == "Q1" && l4$coordinate[l4$chiave == "bargecchiamassarosa"] == "43.1, 10.1", "la voce di prima non ha preso identificativo e coordinate dalla base")
verifica("massarosa" %in% l4$chiave && l4$wikidata[l4$chiave == "massarosa"] == "Q2", "la voce nuova della base non è entrata con i suoi dati")
verifica(identical(names(l4), LISTA_COLONNE), "la lista deve avere sempre le cinque colonne")
dentro4 <- aggiungi_luoghi_alla_lista("Nuovissimo", r4$lista); l5 <- leggi_lista_luoghi(r4$lista)
verifica(identical(dentro4, "Nuovissimo") && l5$wikidata[l5$chiave == "massarosa"] == "Q2", "aggiungere un luogo cancella identificativo e coordinate delle altre voci")
Sys.setenv(TRAMA_HOME = home_v)

# 4c. identificativo sbagliato nella copia personale (02/11/2026): si cambia solo se è ancora quello di prima; un nome mio con la stessa chiave non prende i dati di un altro luogo
home3 <- tempfile("home-"); Sys.setenv(TRAMA_HOME = home3)
dir_c <- tempfile("base-"); dir.create(dir_c); base4 <- file.path(dir_c, "luoghi.csv")
.scrivi_lista_luoghi(data.frame(chiave = c("gussago", "toano", "torricella", "bernaudo"), nome = c("Gussago", "Toano", "Torricella Peligna", "Bernalda"), paese = "Italia",
                                wikidata = c("Q104653", "Q111292", "Q5", "Q52279"), coordinate = c("45.6, 10.15", "44.38, 10.56", "42.0, 14.0", "40.4, 16.6"), stringsAsFactors = FALSE), base4)
writeLines(c('"chiave";"nome_prima";"nome";"wikidata_prima";"wikidata";"coordinate"', '"bernaudo";"Bernalda";"";"";"";""',
             '"gussago";"";"";"Q3873424";"Q104653";"45.6, 10.15"', '"toano";"";"";"Q3749241";"Q111292";"44.38, 10.56"'), file.path(dir_c, "luoghi_correzioni.csv"))
dir.create(cartella_luoghi(), recursive = TRUE)
.scrivi_lista_luoghi(data.frame(chiave = c("gussago", "toano", "torricella", "bernaudo"), nome = c("Gussago", "Toano", "Torricella", "Bernalda"), paese = "Italia",
                                wikidata = c("Q3873424", "Q777", "", "Q52279"), coordinate = c("45.7, 10.3", "1, 1", "", "40.4, 16.6"), stringsAsFactors = FALSE), file.path(cartella_luoghi(), "luoghi.csv"))
writeLines("vecchia", file.path(cartella_luoghi(), "base.md5"))
l6 <- leggi_lista_luoghi(prepara_cartella_dati(base4)$lista)
verifica(l6$wikidata[l6$chiave == "gussago"] == "Q104653" && l6$coordinate[l6$chiave == "gussago"] == "45.6, 10.15", "luoghi_correzioni: l'identificativo sbagliato di prima non è stato sostituito")
verifica(l6$wikidata[l6$chiave == "toano"] == "Q777" && l6$coordinate[l6$chiave == "toano"] == "1, 1", "luoghi_correzioni: un identificativo diverso da quello sbagliato (scelto da chi usa la copia) è stato toccato")
verifica(l6$wikidata[l6$chiave == "torricella"] == "" && l6$nome[l6$chiave == "torricella"] == "Torricella", "una voce mia con la stessa chiave ma un altro nome ha preso i dati di un luogo della base")
verifica(!"bernaudo" %in% l6$chiave, "luoghi_correzioni: «Bernaudo» non è stato tolto")
Sys.setenv(TRAMA_HOME = home_v)

# 5. aggiungi_luoghi_alla_lista: entrano solo i nuovi, con una copia prima; nessun doppione
dentro <- aggiungi_luoghi_alla_lista(c("Xyzzyville", "Napoli", "xyzzyville"))
verifica(identical(dentro, "Xyzzyville"), sprintf("aggiungi_luoghi_alla_lista: attesa solo la voce nuova, entrato: %s", paste(dentro, collapse = ", ")))
verifica(nrow(leggi_lista_luoghi()) == nrow(gp) + 1L, "aggiungi_luoghi_alla_lista: la lista deve crescere di uno")
verifica(length(aggiungi_luoghi_alla_lista("Xyzzyville")) == 0L, "aggiungi_luoghi_alla_lista: la stessa voce non rientra")

# 6. percorso_db: nella cartella dei dati, con un nome solo
verifica(basename(percorso_db()) == "trama.sqlite", "percorso_db non si chiama trama.sqlite")
verifica(identical(percorso_db(), file.path(cartella_dati(), "trama.sqlite")), "percorso_db non punta ai dati")
c1 <- apri_db(percorso_db()); f <- fascicolo_vuoto(); f$fondo <- "Prova"; f$busta <- 1L; f$fascicolo <- 1L; invisible(salva_fascicolo(c1, f))
b <- backup_automatico(c1)
verifica(startsWith(normalizePath(b$percorso), normalizePath(cartella_backup())), "backup automatico fuori dalla cartella dati")
verifica(!is.null(ultimo_backup_automatico()), "ultimo backup non trovato")
dbDisconnect(c1)

# 7. TRAMA_DB vince su tutto
Sys.setenv(TRAMA_DB = "/tmp/x.sqlite"); verifica(percorso_db() == "/tmp/x.sqlite", "TRAMA_DB non vince")
# 8. «Apri la cartella» (5/10): un comando solo, sulla cartella giusta, e mai a vuoto
chiamate <- list()
finto <- function(cmd, args, ...) { chiamate[[length(chiamate) + 1L]] <<- c(cmd, args); 0L }
cart <- tempfile("dati-"); dir.create(cart)
apri_cartella(cart, lancia = finto)
verifica(length(chiamate) == 1L, "apri_cartella deve lanciare un comando solo")
verifica(grepl(basename(cart), chiamate[[1]][2], fixed = TRUE), "apri_cartella non punta alla cartella")
verifica(chiamate[[1]][1] %in% c("open", "explorer", "xdg-open"), "apri_cartella: comando inatteso")
e <- tryCatch(apri_cartella(file.path(cart, "non-esiste"), lancia = finto), error = function(e) conditionMessage(e))
verifica(is.character(e) && grepl("non esiste", e) && length(chiamate) == 1L, "una cartella che non c'è deve dare errore senza lanciare nulla")
# 8. copie di sicurezza: nomi, ordine, copia completa
d <- tempfile("dati-rin-"); dir.create(d)
cx <- apri_db(file.path(d, "trama.sqlite"))
f <- fascicolo_vuoto(); f$fondo <- "Prova"; f$busta <- 1L; f$fascicolo <- 1L; f$titolo <- "Titolo"; invisible(salva_fascicolo(cx, f)); dbDisconnect(cx)
# copie di sicurezza: i due prefissi si riconoscono, si ordinano per data e si ruotano insieme
cb <- tempfile("copie-"); dir.create(cb)
for (n in c("trama-2026-10-13.sqlite", "regestum-2026-10-12.sqlite", "regestum-2026-10-14.sqlite", "regestum-2026-10-01-0930.sqlite", "trama-prima-manuale-20261013-101010-x.sqlite", "regestum-prima-valori-20261001-1.sqlite"))
  file.create(file.path(cb, n))
verifica(basename(ultimo_backup_automatico(cb)) == "regestum-2026-10-14.sqlite", "ultimo backup: non ordina per data fra i due prefissi")
ec <- elenco_copie(cb)
verifica(nrow(ec) == 6L && sum(ec$tipo == "automatica del giorno") == 3L && sum(ec$tipo == "periodica") == 1L && sum(ec$tipo == "manuale") == 1L, "elenco copie: non riconosce i due prefissi")
# una copia completa ha trama.sqlite
cn <- apri_db(file.path(d, "trama.sqlite")); zn <- tempfile(fileext = ".zip")
invisible(backup_completo(cn, zn, cart_luoghi = file.path(d, "luoghi"))); dbDisconnect(cn)
ln <- tryCatch(leggi_backup_completo(zn), error = function(e) conditionMessage(e))
verifica(is.list(ln) && identical(ln$membro, "trama.sqlite"), "copia completa nuova: manca trama.sqlite")
cat(if (ko == 0L) "ESITO: ok\n" else sprintf("ESITO: %d NON TORNA\n", ko))
if (ko) quit(status = 1)
