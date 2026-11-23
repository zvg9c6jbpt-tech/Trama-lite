# ──────────────────────────────────────────────
# verifiche/normalizzazione_prova.R: il Tab di prova di Stefano (13/10)
# `dati/esempio/prova-normalizzazione.xlsx` (dati inventati) è un Tab sul modello con errori e refusi voluti, esportato
# da Numbers: il primo foglio è un «Riepilogo di esportazione», la tabella è in «Foglio1». Si verifica che Trama:
#   - trovi la tabella nel foglio giusto e lo dica;
#   - legga «;» fra le voci (persone, luoghi, reati, soggetti citati) senza rompere i nomi;
#   - corregga i refusi sicuri (reati, lingue) e ne proponga altri;
#   - dica con parole chiare perché una segnatura con un nome solo non entra, e la faccia entrare se si indicano archivio e fondo;
#   - esca nel modello a 28 colonne, senza righe respinte, e il giro foglio pulito → import → export sia identico.
#     Rscript verifiche/normalizzazione_prova.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
F <- "dati/esempio/prova-normalizzazione.xlsx"
a <- leggi_arianna(F)
v("la tabella è letta dal foglio «Foglio1», non dal Riepilogo", identical(a$foglio, "Foglio1") && identical(a$primo_foglio, "Riepilogo di esportazione"))
v("nove righe nel modello", nrow(a$dati) == 9L && identical(a$profilo, "Modello"))
cs <- correzioni_import(a)
v("l'anteprima dice da quale foglio legge", any(grepl("tabella letta dal foglio «Foglio1»", cs$regola)))
v("l'anteprima conta i «;» fra le voci", sum(cs$celle[grepl("«;» al posto di «|»", cs$regola)]) >= 5L)
# senza archivio e fondo: respinte, e il motivo si capisce
con <- apri_db(tempfile(fileext = ".sqlite")); e <- importa_arianna(con, a, modo = "accoda")
v("segnatura con un nome solo: nessuna riga entra", e$importati == 0L && nrow(e$respinti) == 9L)
v("il motivo è in italiano e dice che cosa fare", all(grepl("non dice il fondo", e$respinti$motivo)))
dbDisconnect(con)
# con archivio e fondo indicati: pulisce
p <- pulisci_foglio(a, nome = "prova.xlsx", archivio = "Archivio di Prova", fondo = "Tribunale dell’Inquisizione di Prova")
v("nove pulite, nessuna respinta", p$importati == 9L && nrow(p$respinti) == 0L)
pul <- as.data.frame(readxl::read_excel(p$percorso, sheet = 1, col_types = "text"), check.names = FALSE)
mod <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
v("il foglio pulito ha le 28 colonne del modello, nello stesso ordine", identical(names(pul), mod) && length(mod) == 28L)
v("le tre persone di «Maria Due;Nicola tre» sono tre", length(strsplit(pul[["Persone citate o indicizzate"]][1], "|", fixed = TRUE)[[1]]) == 3L)
v("persone nel formato «Cognome, Nome»", grepl("^Bianchi, Antonio$", pul[["Persone citate o indicizzate"]][2]))
v("reato con refuso sicuro corretto (miscredenxe → miscredenza)", grepl("^Bestemmia e miscredenza\\|Eresia$", pul[["Soggettario"]][3]))
v("«Magia; Eresie» → «Magia|Eresia»", identical(pul[["Soggettario"]][5], "Magia|Eresia"))
v("«Maggia» → «Magia»", identical(pul[["Soggettario"]][6], "Magia"))
v("lingue: «Italiano;l atino» → «italiano; latino»", identical(pul[["Lingua"]][8], "italiano; latino"))
v("segnatura completa di archivio e fondo", grepl("^Archivio di Prova, Tribunale dell'Inquisizione di Prova, b\\. 1, fasc\\. 1$", pul[["Segnatura"]][1]))
v("regesto e note col punto finale", grepl("\\.$", pul[["Regesto o descrizione sintetica"]][1]) && identical(pul[["Note libere"]][1], "Non consultabile."))
v("la scheda «Correzioni» c'è e dice il foglio", "Correzioni" %in% readxl::excel_sheets(p$percorso) && any(grepl("Foglio1", p$correzioni$regola)))
# I nomi della riga NON si correggono per voti (Stefano, 20/10, D11): «Giovanni Violo» fra «Giovanni Viola» di titolo e soggetti resta com'è e va in «Da decidere»; solo le maiuscole dello stesso nome si sistemano da sole
pe <- pul[["Persone citate o indicizzate"]]; ti <- pul[["Titolo"]]; so <- pul[["Soggetti citati o indicizzati"]]; lu <- pul[["Luoghi citati o indicizzati"]]
v("«Giovanni Violo» resta «Violo, Giovanni»: la riga dice Viola, ma si decide, non si vota (D11)", identical(pe[6], "Violo, Giovanni") && identical(so[6], "Giovanni Viola") && any(grepl("Giovanni Violo» \\(Persone\\) ≠ «Giovanni Viola", p$da_decidere$Valore)))
v("«Mario Rosso» resta «Rosso, Mario» (titolo, regesto e soggetti dicono Rossi): decisione con la forma più scritta come suggerimento", identical(pe[3], "Rosso, Mario") && any(grepl("Mario Rosso» \\(Persone\\) ≠ «Mario Rossi» \\(Soggetti citati, Titolo, Regesto\\) · la più scritta: «Mario Rossi»", p$da_decidere$Valore)))
v("«Palo Cavallo» resta com'è (decisione), «Claudia PEcora» → «Pecora» (solo maiuscole, da sola)", identical(pe[9], "Cavallo, Palo|Pecora, Claudia") && identical(so[9], "Paolo Cavallo|Claudia Pecora"))
v("«Maria Verde» nei soggetti resta «Maria Verde» (decisione, mai per voti)", identical(so[5], "Maria Verde"))
v("maiuscole: «PAolo Uno», «UNo», «MAria Gialla» a posto", grepl("^Uno, Paolo", pe[4]) && grepl("^Paolo Uno\\|", so[4]) && identical(ti[7], "Processo contro Maria Gialla"))
v("il connettore «cicra» → «circa» nel titolo", identical(ti[6], "Atti circa Giovanni Viola"))
v("virgola senza spazio nel regesto: «Due,Nicola» → «Due, Nicola»", grepl("Maria Due, Nicola Tre per eresia", pul[["Regesto o descrizione sintetica"]][1], fixed = TRUE))
v("luoghi: «MantovaM» → «Mantova», «Modena!Forlì» → «Modena|Forlì», «TRapani» → «Trapani»", identical(lu[5], "Mantova|Modena|Forlì") && identical(lu[6], "Bergamo|Trapani"))
v("«Maria Tre» ≠ «Maria Tra»: pareggio, va in «Da decidere» e non si corregge", any(grepl("Maria Tre» \\(Persone\\) ≠ «Maria Tra", p$da_decidere$Valore)) && grepl("Tre, Maria", pe[4]) && grepl("Maria Tra", so[4]))
v("ogni cella toccata è una riga di «Correzioni» con prima e dopo", sum(grepl("^riga \\d+: «.+» → «.+»", p$correzioni$regola)) >= 15L)
cz <- concordanza_righe(a2 <- leggi_arianna(p$percorso))
v("sul foglio già pulito non c'è più niente da concordare", nrow(cz$registro) == 0L)
# giro: il foglio pulito, reimportato e riesportato, non cambia più
v("il foglio pulito si rilegge dal primo foglio", is.na(a2$foglio) && nrow(a2$dati) == 9L)
con2 <- apri_db(tempfile(fileext = ".sqlite")); e2 <- importa_arianna(con2, a2, modo = "accoda")
fondo2 <- dbGetQuery(con2, "SELECT DISTINCT fondo FROM fascicoli")$fondo
t2 <- tabella_arianna(con2, fondo = fondo2)
v("nove importate dal foglio pulito", e2$importati == 9L && nrow(e2$respinti) == 0L)
v("giro identico: riesportato = pulito (tutte le celle)", isTRUE(all.equal(unname(as.matrix(t2)), unname(as.matrix(pul)), check.attributes = FALSE)) ||
  identical(ifelse(is.na(as.matrix(t2)), "", as.matrix(t2)), ifelse(is.na(as.matrix(pul)), "", as.matrix(pul)), attrib.as.set = TRUE) ||
  all(ifelse(is.na(as.matrix(t2)), "", as.matrix(t2)) == ifelse(is.na(as.matrix(pul)), "", as.matrix(pul))))
dbDisconnect(con2)
# Prudenza sui dati veri (13/10): regesti strutturati, titoli latini, nomi con qualifiche, luoghi veri a una lettera da un altro
# non si toccano. (La prima versione cambiava 7.500 celle di Modena.)
finto <- list(colonna = c("Titolo" = "T", "Regesto o descrizione sintetica" = "R", "Persone citate o indicizzate" = "P",
                          "Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
  dati = data.frame(S = c("x, b. 1, fasc. 1", "x, b. 1, fasc. 2", "x, b. 1, fasc. 3", "x, b. 1, fasc. 4"),
                    T = c("Processus contra infrascriptos blasphemos", "Montecuccoli, Cesare, conte, feudatario", "Atti circa Mario Rossi", "Di Conte, Bartolomeo"),
                    R = c("Luogo di consumazione del reato: Fiumalbo\nTipo di reato: Eresia\nApplicazione della tortura: sì", NA, "Atti circa Mario Rossi.", NA),
                    P = c("Giordani, capitano", "Montecuccoli, Cesare", "Rossi, Mario", "Chizza (Cizza), Giulio"),
                    L = c("Bazzano|Trezzo|Vallata", "Vagli|Vagli Sotto", "Solignano Vecchio (Solignano Nuovo|Castelvetro di Modena)", "Avignon"),
                    stringsAsFactors = FALSE))
prud <- concordanza_righe(finto)
v("dati veri: nessuna cella toccata in regesti strutturati, titoli latini, qualifiche, luoghi veri e parentesi", nrow(prud$registro) == 0L && identical(prud$dati, finto$dati))
# un luogo con la prova del refuso (maiuscola dentro) si corregge, uno vero con doppia no
v("luogo con maiuscola fuori posto corretto, luogo vero con doppia no",
  identical(concordanza_righe(list(colonna = c("Luoghi citati o indicizzati" = "L"), dati = data.frame(L = c("MantovaM", "Bazzano"), stringsAsFactors = FALSE)))$dati$L, c("Mantova", "Bazzano")))
# Le regole di separatori e spazi (Stefano, 13/10): «|» mai con spazi; «;» mai prima, sempre dopo; lingue minuscole «italiano; latino»
v("«|»: nessuno spazio prima o dopo", identical(.pulisci_spazi("Roma | Milano|  Foggia |Lodi"), "Roma|Milano|Foggia|Lodi"))
v("«;»: mai prima, sempre dopo", identical(.pulisci_spazi("italiano ;latino;  greco"), "italiano; latino; greco"))
v("la virgola come il «;»", identical(.pulisci_spazi("A ,B,  C"), "A, B, C"))
v("cifre: «1,5» non si tocca", identical(.pulisci_spazi("misura 1,5 cm"), "misura 1,5 cm"))
tutte <- unlist(lapply(setdiff(names(pul), "Regesto o descrizione sintetica"), function(n) pul[[n]]))
tutte <- tutte[!is.na(tutte)]
v("nel foglio pulito nessun «|» ha spazi attorno", !any(grepl(" \\||\\| ", tutte)))
v("nel foglio pulito nessun «;» ha uno spazio prima o manca dopo", !any(grepl(" ;|;[^ ]", tutte)))
lin <- pul[["Lingua"]][!is.na(pul[["Lingua"]])]
v("colonna Lingua: tutta minuscola, «x; y», mai «Italiano; Latino»", all(grepl("^[a-zà-ù]+(; [a-zà-ù]+)*$", lin)) && !any(grepl("[A-Z]", lin)))
v("persone, luoghi e reati: il «;» non resta in nessuna cella", !any(grepl(";", c(pul[["Persone citate o indicizzate"]], pul[["Luoghi citati o indicizzati"]], pul[["Soggettario"]], pul[["Soggetti citati o indicizzati"]]))))
# un foglio con spazi attorno al «|» e «;» storti in ogni colonna: tutto a posto in uscita
sporco <- list(colonna = c("Titolo" = "T", "Persone citate o indicizzate" = "P", "Luoghi citati o indicizzati" = "L", "Lingua" = "G", "Segnatura" = "S", "Soggettario" = "R"),
  dati = data.frame(S = "x, b. 1, fasc. 1", T = "Atti circa Mario Rossi", P = "Rossi, Mario | Bianchi, Anna", L = "Roma |Milano ; Foggia",
                    G = "Italiano ;Latino|greco", R = "Magia | Eresia", stringsAsFactors = FALSE))
cs <- concordanza_righe(sporco)$dati
v("«|» e «;» storti in tutte le colonne si sistemano", identical(cs$P, "Rossi, Mario|Bianchi, Anna") && identical(cs$R, "Magia|Eresia") && identical(cs$G, "italiano; latino; greco"))      # in Lingua «|» è sempre un errore (22/10: anche con una lingua fuori vocabolario)
# La finestra «Errori e discordanze» (Stefano, 13/10) e i luoghi proposti
source("R/errori_import.R")
el <- elenco_errori_import(pulito = p)
v("l'elenco: nessuna respinta, un caso da decidere (Maria Tre/Tra), molte discordanze corrette", nrow(el$respinte) == 0L && nrow(el$da_decidere) >= 1L && nrow(el$discordanze) >= 15L && el$totale > 0)
con3 <- apri_db(tempfile(fileext = ".sqlite")); e3 <- importa_arianna(con3, leggi_arianna(F), modo = "accoda"); dbDisconnect(con3)
el3 <- elenco_errori_import(esito = e3)
v("l'elenco di un import respinto: le 9 righe col motivo", nrow(el3$respinte) == 9L && all(grepl("fondo", el3$respinte$Motivo)))
xl <- tempfile(fileext = ".xlsx"); scrivi_errori_xlsx(el, xl)
v("l'elenco si scarica in xlsx con tre fogli", identical(readxl::excel_sheets(xl), c("Respinte", "Da decidere", "Discordanze")))
v("un import senza problemi non ha niente da mostrare", elenco_errori_import(esito = list(respinti = data.frame(riga = integer(0), segnatura = character(0), motivo = character(0))))$totale == 0L)
prop <- concordanza_righe(list(colonna = c("Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
                               dati = data.frame(S = "x, b. 1, fasc. 1", L = "Malalberg|Roma", stringsAsFactors = FALSE)))
v("«Malalberg» (un luogo italiano) non si cambia da solo: si propone «Malalbergo» in «Da decidere»", identical(prop$dati$L, "Malalberg|Roma") && any(grepl("«Malalberg».*«Malalbergo»", prop$da_decidere$testo)))
prop2 <- concordanza_righe(list(colonna = c("Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
                                dati = data.frame(S = "x, b. 1, fasc. 1", L = "Avignon|Roma", stringsAsFactors = FALSE)))
v("«Avignon» è la forma locale: non si propone più la forma italiana «Avignone»", identical(prop2$dati$L, "Avignon|Roma") && !any(grepl("Avignone", prop2$da_decidere$testo)))
# Controllare PRIMA di importare (Stefano, 13/10: «se importiamo tutti questi errori l'app diventa inutile»)
source("R/revisione_import.R")
pr <- proposte_import(leggi_arianna(F))
v("la revisione trova le correzioni e le sei cose da decidere (cinque nomi discordi, anno 15557)", nrow(pr$celle) >= 20L && length(pr$decisioni) == 6L)
v("i cinque nomi discordi sono tutte decisioni, nessuna corretta per voti", sum(vapply(pr$decisioni, function(d) is.null(d$tipo), TRUE)) == 5L && !any(grepl("concorda con le altre colonne", pr$celle$perche)))
v("ogni correzione ha riga, colonna, prima, dopo e il perché", all(nzchar(pr$celle$perche)) && all(pr$celle$prima != pr$celle$dopo))
v("«Eresie» e «miscredenxe» sono correzioni del vocabolario (reati), non cose da decidere",
  any(grepl("Eresia", pr$celle$dopo[pr$celle$colonna == "Soggettario"])) && !any(grepl("vocabolario", vapply(pr$decisioni, function(d) d$testo, ""))))
d_anno <- Filter(function(d) identical(d$tipo, "anno"), pr$decisioni)[[1]]
v("l'anno «15557» propone 1557 e 1555, e si può scrivere a mano", identical(sort(d_anno$opzioni), c("1555", "1557")) && isTRUE(d_anno$libero))
d_nome <- Filter(function(d) is.null(d$tipo) && grepl("Maria Tre", d$testo, fixed = TRUE), pr$decisioni)[[1]]
v("il pareggio «Maria Tre»/«Maria Tra» ha le due opzioni e le due celle da toccare", identical(sort(d_nome$opzioni), c("Maria Tra", "Maria Tre")) && nrow(d_nome$bersagli) == 2L)
giuste <- c("Mario Rosso" = "Mario Rossi", "Maria Tre" = "Maria Tre", "Maria Verdi" = "Maria Verdi", "Giovanni Violo" = "Giovanni Viola", "Palo Cavallo" = "Paolo Cavallo")
dn <- Filter(function(d) is.null(d$tipo), pr$decisioni)
sc_ <- c(stats::setNames(lapply(dn, function(d) unname(giuste[vapply(names(giuste), function(k) startsWith(sub("^riga [0-9]+ \\([^)]*\\): «", "", d$testo), k), TRUE)][1])), vapply(dn, function(d) as.character(d$id), "")), stats::setNames(list("1557"), as.character(d_anno$id)))
a_tutto <- applica_proposte(leggi_arianna(F), pr, scelte = sc_)
v("applicate tutte le correzioni e le sei scelte: niente refusi nel foglio",
  identical(a_tutto$dati[["Estremo recente"]][5], "1557") && grepl("^Bestemmia e miscredenza\\|Eresia$", a_tutto$dati$Soggettario[3]) &&
  !any(grepl("TRapani|MantovaM|PAolo|Violo|Rosso|cicra|;|Maggia|Eresie", unlist(a_tutto$dati[c("Titolo", "Persone citate o indicizzate", "Luoghi citati o indicizzati", "Soggettario", "Soggetti citati o indicizzati")]))))
a_niente <- applica_proposte(leggi_arianna(F), pr, ok = integer(0))
v("senza spunte e senza scelte il foglio resta com'è", identical(a_niente$dati, leggi_arianna(F)$dati))
una <- applica_proposte(leggi_arianna(F), pr, ok = pr$celle$id[1])
v("una sola spunta tocca una sola cella", sum(una$dati != leggi_arianna(F)$dati, na.rm = TRUE) == 1L)
v("evidenzia_diff: tolto barrato, messo evidenziato", identical(evidenzia_diff("Atti cicra X", "Atti circa X")$dopo, "Atti <ins>circa</ins> X") && grepl("<del>cicra</del>", evidenzia_diff("Atti cicra X", "Atti circa X")$prima, fixed = TRUE))
# il cognome minuscolo è un errore eclatante («Nicola tre»), «Giordani, capitano» e «padre minimo» no
v("«Nicola tre» → «Nicola Tre»; «Giordani, capitano», «padre minimo», «ignoto il reato contestato» non si toccano",
  identical(.nome_proprio("Nicola tre"), "Nicola Tre") && identical(.nome_proprio("capitano Giordani"), "capitano Giordani") &&
  identical(.nome_proprio("padre minimo"), "padre minimo") && identical(.nome_proprio("ignoto il reato contestato"), "ignoto il reato contestato"))
# 13/10 (revisione): abbinamento ambiguo, diff di testi lunghi, conteggio esatto delle celle cambiate
amb <- .abbina_nomi(c("Paolo Rossi"), c("Paolo Rosso", "Paola Rossi"))
v("un nome con due candidati vicini non si abbina per ordine: è ambiguo", nrow(amb) == 0L && length(attr(amb, "ambigui")) == 1L)
chiaro <- .abbina_nomi(c("Paolo Rossi", "Maria Verdi"), c("Paolo Rosso", "Maria Verdi"))
v("un abbinamento senza concorrenti resta (uno a uno)", nrow(chiaro) == 2L && length(attr(chiaro, "ambigui")) == 0L)
lungo <- paste(rep("parola", 400), collapse = " "); t0 <- Sys.time(); dl <- evidenzia_diff(lungo, paste(lungo, "fine"))
v("un testo molto lungo si mostra intero, senza calcolare il confronto (veloce, senza tag)", as.numeric(Sys.time() - t0, units = "secs") < 1 && !grepl("<ins>", dl$dopo))
a3 <- leggi_arianna(F); pr3 <- proposte_import(a3, NULL, NULL)
tutte <- applica_proposte(a3, pr3, ok = pr3$celle$id)
v("il conteggio dice le celle davvero cambiate (non più delle spunte)", attr(tutte, "celle_cambiate") <= nrow(pr3$celle) && attr(tutte, "celle_cambiate") >= nrow(pr3$celle) - 3L)
v("nessuna spunta = zero celle cambiate", identical(attr(applica_proposte(a3, pr3, ok = integer(0)), "celle_cambiate"), 0L))
# «Pulisci e scarica» dopo la finestra: ciò che non si spunta NON torna da solo nel foglio pulito
pp <- pulisci_foglio(leggi_arianna(F), archivio = "Archivio di Prova", fondo = "Fondo di Prova", revisionato = TRUE)
zz <- readxl::read_excel(pp$percorso, sheet = 1)
v("revisionato, senza correzioni scelte: i refusi restano com'erano (decide chi guarda)", any(grepl("Violo", unlist(zz))) && any(grepl("cicra", unlist(zz))))
pq <- pulisci_foglio(leggi_arianna(F), archivio = "Archivio di Prova", fondo = "Fondo di Prova")
v("non revisionato (uso diretto): corregge da solo il connettore, ma i nomi discordi restano (D11)", !any(grepl("cicra", unlist(readxl::read_excel(pq$percorso, sheet = 1)))) && any(grepl("Violo", unlist(readxl::read_excel(pq$percorso, sheet = 1)))))
# 13/10: l'estremo recente non viene prima del remoto («1514 e 1512» è un errore)
ad <- leggi_arianna(F); ad$dati[["Estremo remoto"]][2] <- "1514"; ad$dati[["Estremo recente"]][2] <- "1512"
pd <- proposte_import(ad, NULL, NULL)
di <- Filter(function(d) identical(d$tipo, "ordine_anni"), pd$decisioni)
v("1514 e 1512: una decisione sull'ordine delle date, con le tre scelte", length(di) == 1L && di[[1]]$riga == 3L && length(di[[1]]$opzioni) == 3L && grepl("1512", di[[1]]$testo))
sc <- function(x) { a2 <- applica_proposte(ad, pd, ok = integer(0), scelte = stats::setNames(list(x), as.character(di[[1]]$id))); c(a2$dati[["Estremo remoto"]][2], a2$dati[["Estremo recente"]][2]) }
v("scambia: 1512 e 1514", identical(unname(sc("scambia")), c("1512", "1514")))
v("tieni solo il remoto: 1514 e nessun recente", identical(unname(sc("solo_remoto")), c("1514", NA)))
v("tieni solo l'altro: 1512 come remoto, nessun recente", identical(unname(sc("solo_recente")), c("1512", NA)))
v("senza scelta (lascia com'è) le date restano com'erano", identical(unname(applica_proposte(ad, pd, ok = integer(0))$dati[["Estremo remoto"]][2]), "1514"))
ad$dati[["Estremo recente"]][2] <- "1514"
v("date uguali o in ordine: nessuna decisione", !any(vapply(proposte_import(ad, NULL, NULL)$decisioni, function(d) identical(d$tipo, "ordine_anni"), TRUE)))
# 13/10 sera (Stefano): «d'Auria» resta «d'Auria», anche quando la cella corregge altro
mini <- function(per, tit, reg, sog = NA_character_) list(
  colonna = c("Persone citate o indicizzate" = "P", "Titolo" = "T", "Regesto o descrizione sintetica" = "R", "Soggetti citati o indicizzati" = "S", "Segnatura" = "G"),
  dati = data.frame(G = "x, b. 1, fasc. 1", P = per, T = tit, R = reg, S = sog, stringsAsFactors = FALSE))
dA <- concordanza_righe(mini("d'Auria, Tommaso", "Atti circa Tommaso D'Auria", "Atti circa Tommaso D'Auria."))
v("«d'Auria» nelle persone e «D'Auria» nel titolo: nessuna cella cambia", nrow(dA$registro) == 0L && identical(dA$dati$P, "d'Auria, Tommaso") && identical(dA$dati$T, "Atti circa Tommaso D'Auria"))
dB <- concordanza_righe(mini("d'Auria, Tomaso", "Atti circa Tommaso D'Auria", "Atti circa Tommaso D'Auria."))
v("un refuso nel nome («Tomaso») NON si corregge per voti: resta com'è e va in «Da decidere», e la particella non cambia", identical(dB$dati$P, "d'Auria, Tomaso") && identical(dB$dati$T, "Atti circa Tommaso D'Auria") && length(dB$decisioni) == 1L)
dL <- concordanza_righe(mini("de'Lucis, Carla", "Atti circa Carla de' Lucis", "Atti circa Carla de'Lucis."))
v("«de'Lucis» e «de' Lucis» (spazio dopo l'apostrofo): è la stessa grafia, nessuna cella cambia (Sant'Officio, riga 65)", nrow(dL$registro) == 0L && identical(dL$dati$P, "de'Lucis, Carla") && identical(dL$dati$T, "Atti circa Carla de' Lucis"))
v(".solo_particella_apostrofo riconosce lo spazio dopo l'apostrofo ma non un nome diverso", .solo_particella_apostrofo("Carla de'Lucis", "Carla de' Lucis") && !.solo_particella_apostrofo("Carla de'Lucis", "Carla de' Luci"))
dC <- concordanza_righe(mini("Rossi, Mario", "Atti circa Mario Rossi", "Atti circa Mario Rossi.", "Mario Rosso"))
v("i «Soggetti» possono essere fonte di nomi: se contengono un nome diverso (Rosso/Rossi) si decide, non si corregge", identical(dC$dati$S, "Mario Rosso") && length(dC$decisioni) == 1L)
dD <- concordanza_righe(mini("Rossi, Mario", "Atti circa Mario Rossi", "Atti circa Mario Rossi.", NA_character_))
v("i «Soggetti» vuoti (come nei fogli veri) non cambiano niente", nrow(dD$registro) == 0L)
# 13/10 sera: gli anni stanno fra 1200 e 1899
ay <- leggi_arianna(F); ay$dati[["Estremo remoto"]][2] <- "1901"; ay$dati[["Estremo recente"]][3] <- "1150"
py <- proposte_import(ay, NULL, NULL)
dy <- Filter(function(d) identical(d$tipo, "anno"), py$decisioni)
v("anni fuori da 1200–1899 («1901», «1150»): decisioni che dicono l'intervallo", sum(grepl("fuori dall'intervallo 1200", vapply(dy, function(d) d$testo, ""))) == 2L)
ay$dati[["Estremo remoto"]][2] <- "1200"; ay$dati[["Estremo recente"]][3] <- "1899"
v("i limiti 1200 e 1899 sono ammessi", !any(grepl("fuori dall'intervallo", vapply(Filter(function(d) identical(d$tipo, "anno"), proposte_import(ay, NULL, NULL)$decisioni), function(d) d$testo, ""))))
# 14/10 (risposte di Stefano al memo): sequenza, persone senza cognome, reati nel regesto
SEGN <- "Segnatura"; REG <- "Regesto o descrizione sintetica"; PER <- "Persone citate o indicizzate"
aq <- leggi_arianna(F); aq$dati[[SEGN]][6] <- sub("f\\.5$", "f.2", aq$dati[[SEGN]][6])
pq <- proposte_import(aq, NULL, NULL)
av <- Filter(function(d) identical(d$tipo, "avviso") && grepl("fuori sequenza", d$testo), pq$decisioni)
v("la segnatura fuori sequenza (f.2 dopo f.4) è una difformità segnalata, senza scelte", length(av) == 1L && grepl("fasc. 2 viene dopo", av[[1]]$testo) && length(av[[1]]$opzioni) == 0L)
v("un foglio in ordine non ha avvisi di sequenza", !any(vapply(proposte_import(leggi_arianna(F), NULL, NULL)$decisioni, function(d) grepl("fuori sequenza", d$testo), TRUE)))
ap <- leggi_arianna(F); ap$dati[[PER]][2] <- "Due, Maria|Isernia da, Nicola"; ap$dati[[PER]][3] <- "da Isernia, Nicola|Rossi, Mario"
pp <- proposte_import(ap, NULL, NULL); cp <- pp$celle[pp$celle$colonna == PER & pp$celle$riga %in% c(3L, 4L), ]
v("«Nicola da Isernia» non va fra le persone citate: si propone di toglierlo (con la ragione)", nrow(cp) == 2L && all(grepl("senza cognome certo", cp$perche)) && !any(grepl("Isernia", cp$dopo)))
ar <- leggi_arianna(F); ar$dati[[REG]][2] <- "Processo per eresiaa contro Antonio Bianchi."; ar$dati[[REG]][3] <- "Atti per diversi per bestemmia e miscredenza; ebraismo e magia."
ar$dati[[REG]][4] <- "Denunce per furto contro ignoti."
pr_ <- proposte_import(ar, NULL, NULL)
cr_ <- pr_$celle[pr_$celle$colonna == REG & pr_$celle$riga == 3L, ]
v("«per eresiaa» (refuso) → «per eresia» nel regesto, con la ragione", nrow(cr_) == 1L && grepl("per eresia contro", cr_$dopo) && grepl("reato", cr_$perche))
v("le classi giuste («bestemmia e miscredenza; ebraismo e magia») non si toccano", !any(pr_$celle$colonna == REG & pr_$celle$riga == 4L))
v("«per furto» non è una classe: avviso, e il testo non cambia", any(vapply(pr_$decisioni, function(d) identical(d$tipo, "avviso") && grepl("furto", d$testo), TRUE)) && !any(pr_$celle$colonna == REG & pr_$celle$riga == 5L))
# 14/10: Regesto e Soggettario devono dire gli stessi reati; se collidono decide Stefano (revisione)
SOG <- "Soggettario"
ac <- leggi_arianna(F); ac$dati[[REG]][2] <- "Processo per eresia contro Antonio Bianchi."; ac$dati[[SOG]][2] <- "Giurisdizione"
pc <- proposte_import(ac, NULL, NULL); dc <- Filter(function(d) identical(d$tipo, "reati") && d$riga == 3L, pc$decisioni)
v("reati diversi fra Regesto («eresia») e Soggettario («Giurisdizione»): una decisione con le due scelte", length(dc) == 1L && length(dc[[1]]$opzioni) == 2L && grepl("Eresia", dc[[1]]$testo) && grepl("Giurisdizione", dc[[1]]$testo))
sc_ <- function(x) { a2 <- applica_proposte(ac, pc, ok = integer(0), scelte = stats::setNames(list(x), as.character(dc[[1]]$id))); c(a2$dati[[REG]][2], a2$dati[[SOG]][2]) }
v("16/10: con un Soggettario valido la scelta di partenza è «regesto» (vince il Soggettario) e la scheda lo mostra preselezionato", identical(dc[[1]]$predefinita, "regesto") && grepl("checked=\"checked\" value=\"regesto\"|value=\"regesto\" checked", as.character(scheda_decisione(dc[[1]]))) && grepl("Proposta: vale il Soggettario", dc[[1]]$nota) && grepl("Proposta: vale il Soggettario", as.character(scheda_decisione(dc[[1]]))))
v("scelta «soggettario»: il Soggettario prende i reati del Regesto, il Regesto resta", identical(unname(sc_("soggettario")), c("Processo per eresia contro Antonio Bianchi.", "Eresia")))
v("scelta «regesto»: il Regesto prende i reati del Soggettario, il Soggettario resta", identical(unname(sc_("regesto")), c("Processo per giurisdizione contro Antonio Bianchi.", "Giurisdizione")))
ac$dati[[SOG]][2] <- "Eresia"
v("Regesto e Soggettario che dicono lo stesso: nessuna decisione", !any(vapply(proposte_import(ac, NULL, NULL)$decisioni, function(d) identical(d$tipo, "reati"), TRUE)))
ac$dati[[SOG]][2] <- NA_character_
dv <- Filter(function(d) identical(d$tipo, "reati") && d$riga == 3L, proposte_import(ac, NULL, NULL)$decisioni)
v("Soggettario vuoto e Regesto con un reato: si propone di scriverlo nel Soggettario (unica scelta)", length(dv) == 1L && length(dv[[1]]$opzioni) == 1L)
ac$dati[[REG]][2] <- "Processo per eresia e magia contro Antonio Bianchi."; ac$dati[[SOG]][2] <- "Magia|Eresia"
v("stessi reati in ordine diverso: nessuna decisione", !any(vapply(proposte_import(ac, NULL, NULL)$decisioni, function(d) identical(d$tipo, "reati"), TRUE)))
# 14/10: le tipologie della guida al Tab (Atti, Lettere, Decreti; più d'una con «; ») fanno il giro import → export
tip <- function(val) { a <- leggi_arianna(F); a$dati[["Tipologia documentaria"]] <- val
  cn <- apri_db(tempfile(fileext = ".sqlite")); importa_nuovo_archivio(cn, a, "Archivio di Prova", "Fondo T", ""); unique(tabella_arianna(cn, fondo = "Fondo T")[["Tipologia documentaria"]]) }
v("tipologia «Lettere; Decreti» esce com'è", identical(tip("Lettere; Decreti"), "Lettere; Decreti"))
v("tipologia «lettere» (minuscolo) esce «Lettere»", identical(tip("lettere"), "Lettere"))
v("tipologia «Atti» e un valore di un altro inventario escono «Atti»", identical(tip("Atti"), "Atti") && identical(tip("Unità archivistiche"), "Atti"))
# 14/10: i luoghi stranieri nella lingua del paese (guida al Tab; Stefano: «correggi nella lingua del paese d'origine»)
al <- leggi_arianna(F); LU <- "Luoghi citati o indicizzati"
al$dati[[LU]][2] <- "Londra|Siviglia|Roma|Costantinopoli (Istanbul)"; al$dati[[LU]][3] <- "Zante|Liegi (Leodien)|Firenze"
pl <- proposte_import(al, NULL, NULL); cl <- pl$celle[pl$celle$colonna == LU & pl$celle$riga %in% c(3L, 4L), ]
# 20/10 (D7, D8): forma standard «Locale (Italiano)», senza toponimo antico; la sola forma italiana è una PROPOSTA spenta, la locale nuda è automatica
v("luoghi stranieri: «Londra|Siviglia|Roma|Costantinopoli (Istanbul)» → «London (Londra)|Sevilla (Siviglia)|Roma|Istanbul (Costantinopoli)»", any(cl$dopo == "London (Londra)|Sevilla (Siviglia)|Roma|Istanbul (Costantinopoli)"))
v("luoghi stranieri: «Zante|Liegi (Leodien)|Firenze» → «Zakynthos (Zante)|Liège (Liegi)|Firenze» (l'Italia non si tocca)", any(cl$dopo == "Zakynthos (Zante)|Liège (Liegi)|Firenze"))
v("luoghi stranieri: la sola forma italiana è una proposta spenta e lo dice", all(!cl$consigliata) && all(grepl("forma italiana: proposta", cl$perche)))
v("luogo_forma_locale: la locale nuda è automatica («London» → «London (Londra)»), la già standard non cambia", identical(forma_locale_luogo("London"), "London (Londra)") && isTRUE(luogo_forma_locale("London")$sicura) && all(is.na(forma_locale_luogo(c("London (Londra)", "Firenze", "Roma", "Istanbul (Costantinopoli)", "Embrun", "Bistrița")))))
# 22/10: «S.» sciolto sempre (San, Santo, Santa, Sant'), come correzione sicura e spuntata (il 14/10 era una proposta con la prova della lista)
as_ <- leggi_arianna(F); as_$dati[[LU]][2] <- "S. Possidonio|S.Elmo|S. Zzzz|Napoli"
ps <- proposte_import(as_, NULL, NULL); cs <- ps$celle[ps$celle$colonna == LU & ps$celle$riga == 3L, ]
v("«S.»: «S. Possidonio|S.Elmo|S. Zzzz|Napoli» → «San Possidonio|Sant'Elmo|San Zzzz|Napoli»", any(cs$dopo == "San Possidonio|Sant'Elmo|San Zzzz|Napoli"))
v("«S.»: è una correzione sicura, spuntata di partenza, e dice «sciolto»", nrow(cs) > 0 && all(cs$consigliata) && all(grepl("sciolto", cs$perche)))
v("sciogli_santi: ciò che è già esteso non cambia", identical(sciogli_santi(c("San Possidonio", "Napoli", "Sant'Elmo")), c("San Possidonio", "Napoli", "Sant'Elmo")))
# 16/10: luogo assente dalla lista dei luoghi = caso dubbio come i nomi (non blocca, non è preselezionato, «aggiungi» lo mette nella lista personale)
al <- leggi_arianna(F); LUO <- unname(al$colonna["Luoghi citati o indicizzati"]); al$dati[[LUO]][1] <- "Napoli|Zzxqvillaggio"
pl <- proposte_import(al, NULL, NULL); dl <- Filter(function(d) identical(d$tipo, "luoghi_ignoti"), pl$decisioni)
v("luoghi ignoti: UNA scheda sola, con «Zzxqvillaggio» e nessuna scelta di partenza", length(dl) == 1L && "Zzxqvillaggio" %in% dl[[1]]$luoghi && is.null(dl[[1]]$predefinita))
v("luoghi ignoti: un luogo noto (Napoli) non è fra quelli da decidere", !"Napoli" %in% dl[[1]]$luoghi)
v("luoghi ignoti: senza spunte non cambia il foglio e non aggiunge niente", length(attr(applica_proposte(al, pl, ok = integer(0)), "luoghi_da_aggiungere")) == 0L && identical(applica_proposte(al, pl, ok = integer(0))$dati[[LUO]][1], "Napoli|Zzxqvillaggio"))
a_l <- applica_proposte(al, pl, ok = integer(0), scelte = stats::setNames(list("Zzxqvillaggio"), as.character(dl[[1]]$id)))
v("luoghi ignoti: la spunta lascia il testo com'è e segna il luogo da aggiungere", identical(attr(a_l, "luoghi_da_aggiungere"), "Zzxqvillaggio") && identical(a_l$dati[[LUO]][1], "Napoli|Zzxqvillaggio"))
al$dati[[LUO]][2] <- "Napoli|Castelfantasma"; pl2 <- proposte_import(al, NULL, NULL); d2 <- Filter(function(d) identical(d$tipo, "luoghi_ignoti"), pl2$decisioni)[[1]]
a_2 <- applica_proposte(al, pl2, ok = integer(0), scelte = stats::setNames(list("Zzxqvillaggio\001Castelfantasma"), as.character(d2$id)))
v("luoghi ignoti: più spunte insieme le segna tutte", setequal(attr(a_2, "luoghi_da_aggiungere"), c("Zzxqvillaggio", "Castelfantasma")))
v("luoghi ignoti: non si offrono per le schede già salvate né per «Pulisci» (luoghi_ignoti = FALSE)", !any(vapply(proposte_import(al, NULL, NULL, luoghi_ignoti = FALSE)$decisioni, function(d) identical(d$tipo, "luoghi_ignoti"), TRUE)))
al$dati[[LUO]][3] <- "Napoli|Zzx\r\nQuux"
v("luoghi ignoti: un nome con ritorno a capo non si propone (si spezzerebbe)", !any(grepl("Quux", unlist(lapply(proposte_import(al, NULL, NULL)$decisioni, function(d) d$luoghi)))))
v("scheda dei luoghi ignoti: caselle da spuntare, nessuna spuntata", { h <- as.character(scheda_decisione(d2)); grepl("checkbox", h) && !grepl("checked=\"checked\"", h) })
v("reati preselezionati: la scheda ha il segno «Preselezionata» e la finestra lo conta", grepl("Preselezionata", as.character(scheda_decisione(dc[[1]]))) && grepl("scelte preselezionate da verificare", as.character(modale_revisione(pc))))
v("decisioni in ordine di riga", !is.unsorted(vapply(pc$decisioni, function(d) d$riga, 1L)))
dS <- concordanza_righe(list(colonna = c("Persone citate o indicizzate" = "P", "Titolo" = "T", "Regesto o descrizione sintetica" = "R", "Soggetti citati o indicizzati" = "S", "Segnatura" = "G"),
  dati = data.frame(G = "Archivio X, Fondo Y, b. 3, fasc. 4", P = "Rossi, Maria", T = "Atti circa Maria Rosi", R = "Atti circa Maria Rossa", S = NA_character_, stringsAsFactors = FALSE)))
v("la segnatura di una decisione sui nomi tiene la busta («b. 3, fasc. 4»)", length(dS$decisioni) > 0L && all(vapply(dS$decisioni, function(d) identical(d$segnatura, "b. 3, fasc. 4"), TRUE)))
v("la segnatura di una decisione non ripete «riga N»", !any(vapply(proposte_import(a, NULL, NULL)$decisioni, function(d) grepl("^riga ", d$segnatura), TRUE)))
tg <- tempfile(fileext = ".csv"); file.copy(LISTA_LUOGHI_BASE, tg)
fg <- aggiungi_luoghi_alla_lista("Zzxqvillaggio", percorso = tg); gg <- leggi_lista_luoghi(tg); rg_ <- gg[gg$nome == "Zzxqvillaggio", ]
v("luogo ignoto: la voce entra nella lista dei luoghi", identical(fg, "Zzxqvillaggio") && nrow(rg_) == 1L && identical(colnames(gg), LISTA_COLONNE))
unlink(c(tg, file.path(dirname(tg), "backup-luoghi")), recursive = TRUE)
# 16/10 (Stefano): i due punti NON sono un separatore: «Eresia:Ebraismo» è un errore, in tutte le colonne a più valori
v("«:» fra i valori: «Eresia:Ebraismo» → «Eresia|Ebraismo», «Bologna: Modena» → «Bologna|Modena», lingue e tipologie con «; »",
  identical(.due_punti_a_separatore("Eresia:Ebraismo"), "Eresia|Ebraismo") && identical(.due_punti_a_separatore("Bologna: Modena"), "Bologna|Modena") &&
  identical(.due_punti_a_separatore("italiano:latino", "; "), "italiano; latino"))
v("«:» dentro le parentesi o in un indirizzo non si tocca", identical(.due_punti_a_separatore("Roma (nota: antica)"), "Roma (nota: antica)") && identical(.due_punti_a_separatore("https://x.it/a:b"), "https://x.it/a:b"))
ac2 <- leggi_arianna(F)
for (nm in c("Soggettario", "Luoghi citati o indicizzati", "Persone citate o indicizzate", "Soggetti citati o indicizzati", "Lingua")) ac2$dati[[unname(ac2$colonna[nm])]][3] <- c(Soggettario = "Eresia:Ebraismo", `Luoghi citati o indicizzati` = "Bologna:Modena",
  `Persone citate o indicizzate` = "Rossi, Mario:Bianchi, Luigi", `Soggetti citati o indicizzati` = "Rossi, Mario:Bianchi, Luigi", Lingua = "italiano:latino")[[nm]]
pc2 <- proposte_import(ac2, NULL, NULL); cc2 <- pc2$celle[pc2$celle$riga == 4L & grepl("separatore", pc2$celle$perche), ]
v("«:» fra i valori: ogni colonna a più valori ha la sua correzione, con la ragione, spuntata", nrow(cc2) == 5L && all(cc2$consigliata) && all(grepl("«:» non è un separatore", cc2$perche)))
v("«Eresia:Ebraismo» diventa «Eresia|Ebraismo» e non resta come reato fuori vocabolario",
  identical(cc2$dopo[cc2$colonna == "Soggettario"], "Eresia|Ebraismo") && !any(vapply(pc2$decisioni, function(d) identical(d$tipo, "vocabolario") && grepl("Eresia:Ebraismo", d$testo), TRUE)))
v("«:» fra i valori: l'anteprima lo conta come errore", any(grepl("«:» non è un separatore", correzioni_import(ac2)$regola)))
# 16/10 (Stefano): gli unici separatori sono «|» (senza spazi) e «; »; la virgola solo nel testo semplice, mai come separatore
noti_ <- chiave_grafia(c("Bologna", "Modena", "Forlì"))
v("luoghi: «,» «/» «+» «&» «e» « - » fra luoghi noti diventano «|»; un luogo solo e il «|» giusto restano",
  all(c(.separatori_luoghi("Bologna, Modena", noti_), .separatori_luoghi("Bologna/Modena", noti_), .separatori_luoghi("Bologna + Modena", noti_),
        .separatori_luoghi("Bologna & Modena", noti_), .separatori_luoghi("Bologna e Modena", noti_), .separatori_luoghi("Bologna - Modena", noti_)) == "Bologna|Modena") &&
  identical(.separatori_luoghi("Bologna", noti_), "Bologna") && identical(.separatori_luoghi("Bologna|Modena", noti_), "Bologna|Modena") &&
  identical(.separatori_luoghi("Bologna e Zzxq", noti_), "Bologna e Zzxq"))
v("luoghi: la virgola o il «/» dentro le parentesi non si toccano", identical(.separatori_luoghi("Solignano (Vecchio, Nuovo/Antico)", noti_), "Solignano (Vecchio, Nuovo/Antico)"))
rr <- vocabolario("reato")
v("reati: «Eresia, Magia», «Eresia e Magia», «Eresia/Magia», «Eresia + Magia» → «Eresia|Magia»",
  all(vapply(c("Eresia, Magia", "Eresia e Magia", "Eresia/Magia", "Eresia + Magia", "Eresia & Magia", "Eresia : Magia"), function(x) identical(.separatori_con_classi(x, rr, "|"), "Eresia|Magia"), TRUE)))
v("reati: la classe «Bestemmia e miscredenza» non si spezza; «|» e un reato solo restano; un testo che non è un elenco di classi resta",
  identical(.separatori_con_classi("Bestemmia e miscredenza", rr, "|"), "Bestemmia e miscredenza") && identical(.separatori_con_classi("Bestemmia e miscredenza, Magia", rr, "|"), "Bestemmia e miscredenza|Magia") &&
  identical(.separatori_con_classi("Eresia|Magia", rr, "|"), "Eresia|Magia") && identical(.separatori_con_classi("Eresia", rr, "|"), "Eresia") && identical(.separatori_con_classi("Eresia, Zzxq", rr, "|"), "Eresia, Zzxq"))
v("lingue e tipologie: «italiano, latino», «italiano/latino», «italiano e latino» → «italiano; latino»",
  all(vapply(c("italiano, latino", "italiano/latino", "italiano e latino"), function(x) identical(.separatori_con_classi(x, vocabolario("lingua"), "; "), "italiano; latino"), TRUE)))
pp <- .separatori_persone("Rossi, Mario e Bianchi, Luigi")
v("persone: «/», «+», «&», «e» fra «Cognome, Nome» diventano «|»; il nome con la sua virgola resta",
  identical(pp$v, "Rossi, Mario|Bianchi, Luigi") && identical(.separatori_persone("Rossi, Mario/Bianchi, Luigi")$v, "Rossi, Mario|Bianchi, Luigi") &&
  identical(.separatori_persone("Rossi, Mario")$v, "Rossi, Mario") && identical(.separatori_persone("Rossi, Mario|Bianchi, Luigi")$v, "Rossi, Mario|Bianchi, Luigi"))
v("persone: più di una virgola in una persona non si corregge da sola, si segnala", isTRUE(.separatori_persone("Rossi, Mario, Bianchi, Luigi")$avviso) && identical(.separatori_persone("Rossi, Mario, Bianchi, Luigi")$v, "Rossi, Mario, Bianchi, Luigi") && !.separatori_persone("Rossi, Mario")$avviso)
ac3 <- leggi_arianna(F)
imp3 <- function(nome, valore, riga = 3L) { a_ <- ac3; a_$dati[[unname(a_$colonna[nome])]][riga - 1L] <- valore; proposte_import(a_, NULL, NULL) }
p3 <- imp3("Soggettario", "Eresia, Magia"); c3 <- p3$celle[p3$celle$riga == 3L & p3$celle$colonna == unname(ac3$colonna["Soggettario"]), ]
v("«Eresia, Magia» nel Soggettario: correzione spuntata «Eresia|Magia» con la ragione, e nessuna decisione «non è nel vocabolario»",
  nrow(c3) == 1L && identical(c3$dopo, "Eresia|Magia") && isTRUE(c3$consigliata) && grepl("separatore non ammesso", c3$perche) &&
  !any(vapply(p3$decisioni, function(d) identical(d$tipo, "vocabolario") && grepl("Eresia, Magia", d$testo), TRUE)))
v("separatori: l'anteprima conta come errore anche la virgola, il «/» e la «e» fra i valori", { a_ <- ac3; a_$dati[[unname(a_$colonna["Soggettario"])]][2] <- "Eresia, Magia"; any(grepl("separatore non ammesso", correzioni_import(a_)$regola)) })
v("«|» in Lingua e Tipologia è un errore: «italiano|latino» → «italiano; latino», «Atti|Processi» → «Atti; Processi»; nel Soggettario resta giusto",
  identical(.separatori_con_classi("italiano|latino", vocabolario("lingua"), "; ", ";"), "italiano; latino") && identical(.separatori_con_classi("Atti|Processi", vocabolario("tipologia_documentaria"), "; ", ";"), "Atti; Processi") &&
  identical(.separatori_con_classi("Eresia|Magia", vocabolario("reato"), "|"), "Eresia|Magia"))
p5 <- imp3("Lingua", "italiano|latino"); c5 <- p5$celle[p5$celle$riga == 3L & p5$celle$colonna == unname(ac3$colonna["Lingua"]), ]
v("«italiano|latino» in Lingua: correzione spuntata «italiano; latino» con la ragione", nrow(c5) == 1L && identical(c5$dopo, "italiano; latino") && isTRUE(c5$consigliata) && grepl("separatore non ammesso", c5$perche))
p6 <- imp3("Segnatura", "Archivio X, Fondo Y, b. 1|fasc. 2"); a6 <- Filter(function(d) identical(d$tipo, "avviso") && grepl("contiene «\\|»", d$testo), p6$decisioni)
v("«|» in Segnatura (e in ogni campo di testo): è un errore segnalato, e la cella non si cambia da sola",
  length(a6) >= 1L && !any(p6$celle$riga == 3L & p6$celle$colonna == unname(ac3$colonna["Segnatura"])))
# 16/10 (Stefano): il Luogo di produzione è di norma uno solo ma può avere più valori: il separatore standard è «|»
p7 <- imp3("Luogo di produzione (data topica)", "Bologna, Modena"); c7 <- p7$celle[p7$celle$riga == 3L & p7$celle$colonna == unname(ac3$colonna["Luogo di produzione (data topica)"]), ]
v("Luogo di produzione: più valori si separano con «|» (la virgola diventa «|»); un valore solo resta com'è", nrow(c7) == 1L && identical(c7$dopo, "Bologna|Modena") &&
  nrow({ p8 <- imp3("Luogo di produzione (data topica)", "Bologna"); p8$celle[p8$celle$riga == 3L & p8$celle$colonna == unname(ac3$colonna["Luogo di produzione (data topica)"]), ] }) == 0L)
for (nm in c("Note libere", "Titolo", "Segnatura")) {
  p4 <- imp3(nm, "Atti: Rossi, Mario/Bianchi + Verdi & figli, e altri"); c4 <- p4$celle[p4$celle$riga == 3L & p4$celle$colonna == unname(ac3$colonna[nm]) & grepl("separatore non ammesso", p4$celle$perche), ]
  v(sprintf("la virgola e gli altri segni nel testo semplice (%s) non si toccano", nm), nrow(c4) == 0L)
}
# Google Sheets (Drive) scrive i numeri interi con «.0» (anno 1555 -> «1555.0», 20/10): negli anni e nella quantità l'intero col «.0» è l'intero
mod <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
gs <- as.data.frame(matrix(NA_character_, 4, length(mod), dimnames = list(NULL, mod)), check.names = FALSE)
gs[["Segnatura"]] <- sprintf("Archivio di Prova, Fondo di Prova, b. 1, fasc. %d", 1:4)
gs[["Estremo remoto"]] <- c("1555.0", "1555.00", "1600.9", "1556"); gs[["Estremo recente"]] <- c("1557.0", NA, NA, NA)
gs[["consistenza.quantità"]] <- c("3.0", NA, NA, NA)
fgs <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(gs, fgs); ag <- leggi_arianna(fgs)
v("anni e quantità col «.0» (Google Sheets) tornano interi: 1555.0 → 1555, 1555.00 → 1555, 3.0 → 3",
  identical(ag$dati[["Estremo remoto"]][1:2], c("1555", "1555")) && identical(ag$dati[["Estremo recente"]][1], "1557") && identical(ag$dati[["consistenza.quantità"]][1], "3"))
v("«1600.9» non è un anno e resta com'è (si decide a mano, mai in silenzio)", identical(ag$dati[["Estremo remoto"]][3], "1600.9"))
# 20/10 (ProvaTrama-2): «\\» è un separatore sbagliato (non un luogo ignoto); un nome senza spazio si ripropone dalla colonna Persone;
# «Processo per contro X» dice che manca il reato (X è la persona, mai una classe del vocabolario dei reati)
v("«Torino\\Aosta» → «Torino|Aosta» (luoghi) e «Rossi, Mario\\Bianchi, Anna» → due persone", identical(.separatori_luoghi("Torino\\Aosta", c("torino", "aosta")), "Torino|Aosta") &&
  identical(.separatori_persone("Rossi, Mario\\Bianchi, Anna")$v, "Rossi, Mario|Bianchi, Anna"))
v("«Domenicodi Cineda» → «Domenico di Cineda» se la colonna Persone dice «Cineda, Domenico di»; il resto non si tocca",
  identical(.nomi_staccati_male("Processo contro Domenicodi Cineda", "Cineda, Domenico di"), "Processo contro Domenico di Cineda") &&
  identical(.nomi_staccati_male("contro Adamo Rossi", "Rossi, Ada Mo"), "contro Adamo Rossi") &&
  identical(.nomi_staccati_male("contro Domenico di Cineda", "Cineda, Domenico di"), "contro Domenico di Cineda"))
gp <- gs[1, , drop = FALSE]; gp[["Titolo"]] <- "Processo per eresia a Marco Antonio Giani"; gp[["Regesto o descrizione sintetica"]] <- "Processo per contro Marco Antonio Giani"
gp[["Persone citate o indicizzate"]] <- "Giani, Marco Antonio"; gp[["Estremo remoto"]] <- "1555"; gp[["Estremo recente"]] <- NA
fgp <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(gp, fgp)
pgp <- proposte_import(leggi_arianna(fgp), apri_db(tempfile(fileext = ".sqlite")))
tgp <- vapply(Filter(function(d) identical(as.character(d$tipo)[1], "avviso"), pgp$decisioni), function(d) d$testo, "")
v("«Processo per contro X» nel regesto: avviso «manca il reato», mai «X non è una classe del vocabolario dei reati»", any(grepl("manca il reato dopo «per»: «contro Marco Antonio Giani» è la persona", tgp)) && !any(grepl("non è una classe", tgp)))
v("Titolo = testo libero (D16): «Processo per eresia a X» non dà nessun avviso sul vocabolario dei reati", !any(grepl("^Titolo:", tgp)))
# «ed» è congiunzione davanti a un reato che comincia per «e» («Ebraismo ed eresia»): due reati, non una classe sconosciuta
cl_ <- tolower(unique(vocabolario("reato")))
r1_ <- .reati_nel_testo("Processo per Ebraismo ed eresia contro Paola Rossa.", cl_)
v("«Ebraismo ed eresia» sono due reati del vocabolario (nessuna parte sconosciuta)", nrow(r1_) == 2L && !anyNA(r1_$classe))
v("«ed» davanti a un reato che non comincia per «e» non separa (resta una parte sola, sconosciuta)", { r2_ <- .reati_nel_testo("Processo per magia ed bestemmia contro X", cl_); nrow(r2_) == 1L && anyNA(r2_$classe) })
gq <- gp; gq[["Titolo"]] <- "Atti per diversi"; gq[["Regesto o descrizione sintetica"]] <- "Atti per eresia per diversi"
fgq <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(gq, fgq)
pgq <- proposte_import(leggi_arianna(fgq), apri_db(tempfile(fileext = ".sqlite")))
tgq <- vapply(Filter(function(d) identical(as.character(d$tipo)[1], "avviso"), pgq$decisioni), function(d) d$testo, "")
v("«Atti per eresia per diversi»: l'avviso dice che i «per» sono due, non che «eresia per diversi» è una classe", any(grepl("ha due «per»", tgq)) && !any(grepl("non è una classe", tgq)))
# 20/10 (D4): il duplicato esatto fra le persone è una decisione con scelta: lasciando com'è si fondono; scrivendo una forma, il primo la prende
gd <- as.data.frame(as.list(stats::setNames(rep(NA_character_, length(names(al$dati))), names(al$dati))), check.names = FALSE)
al_d <- al; al_d$dati <- gd; al_d$dati[["Segnatura"]] <- "b. 1, fasc. 1"; al_d$dati[["Persone citate o indicizzate"]] <- "Rossi, Paolo|Bianchi, Maria|Rossi, Paolo"
pd <- proposte_import(al_d); dd <- Filter(function(d) grepl("^duplicato esatto", d$testo), pd$decisioni)
v("duplicato esatto: una decisione libera, «Lascia com'è» non cambia la cella", length(dd) == 1L && isTRUE(dd[[1]]$libero) && identical(applica_proposte(al_d, pd, ok = integer(0), scelte = list())$dati[["Persone citate o indicizzate"]], "Rossi, Paolo|Bianchi, Maria|Rossi, Paolo"))
v("duplicato esatto: scrivendo una forma, il primo la prende e il secondo resta", identical(applica_proposte(al_d, pd, ok = integer(0), scelte = stats::setNames(list("Rossi, Paolo (padre)"), as.character(dd[[1]]$id)))$dati[["Persone citate o indicizzate"]], "Rossi, Paolo (padre)|Bianchi, Maria|Rossi, Paolo"))
# 22/10 (D16): check di base sui refusi nel testo libero, dal lessico del corpus; solo proposta, mai spuntata
lt_ <- c("Causae expeditae in causarum Mutinae", "Catalogus causarum expeditarum", "Processus causarum pendentium", "Index causarum et reorum", "Catalogus causaraum expeditarum")
les_ <- .lessico_corpus(lt_)
s1_ <- .sospetti_refusi("Catalogus causaraum expeditarum", les_)
v("refuso sospetto: «causaraum» (1 volta) → «causarum» (4 volte)", nrow(s1_) == 1L && identical(s1_$proposta, "causarum"))
v("refuso sospetto: la parola giusta, i nomi e l'inizio di frase non si segnalano", nrow(.sospetti_refusi("Causarum Mutinae", les_)) == 0L)
v("refuso sospetto: la differenza sull'ultima lettera è una desinenza, non un refuso («causarus»)", nrow(.sospetti_refusi("catalogus causarus", .lessico_corpus(c(lt_, "Catalogus causarus")))) == 0L)
v("refuso sospetto: con un corpus piccolo (nessuna parola a 3 occorrenze) si tace", nrow(.sospetti_refusi("causaraum expeditarum", .lessico_corpus(c("causarum expeditarum", "causaraum expeditarum")))) == 0L)
v("refuso sospetto: due candidate frequenti alla pari, si tace", {
  l2_ <- .lessico_corpus(c(rep("tamenque", 3), rep("tamenqui", 3), "tamenqua")); nrow(.sospetti_refusi("tamenqua", l2_)) == 0L })
v("refuso sospetto: si riscrive solo la parola intera", identical(.applica_refusi("causaraum e causaraumx", s1_), "causarum e causaraumx"))
gl <- gs[rep(1L, length(lt_)), , drop = FALSE]
gl[["Segnatura"]] <- sprintf("Archivio di Prova, Fondo di Prova, b. 1, fasc. %d", seq_along(lt_))
gl[["Titolo"]] <- lt_; gl[["Regesto o descrizione sintetica"]] <- NA_character_; gl[["Persone citate o indicizzate"]] <- NA_character_
gl[["Estremo remoto"]] <- "1555"; gl[["Estremo recente"]] <- NA
fgl <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(gl, fgl); agl <- leggi_arianna(fgl)
pgl <- proposte_import(agl)
cl_ <- pgl$celle[grepl("refuso sospetto", pgl$celle$perche), , drop = FALSE]
v("refuso sospetto: una proposta sulla riga giusta, NON spuntata di partenza", nrow(cl_) == 1L && cl_$colonna == "Titolo" && !cl_$consigliata && cl_$dopo == "Catalogus causarum expeditarum")
v("refuso sospetto: «Lascia com'è» non cambia la cella", identical(applica_proposte(agl, pgl, ok = integer(0), scelte = list())$dati[["Titolo"]], lt_))
v("refuso sospetto: accettandola la cella è corretta e al secondo passaggio non resta niente da proporre", {
  ag2_ <- applica_proposte(agl, pgl, ok = cl_$id, scelte = list()); nrow(proposte_import(ag2_)$celle) == 0L && ag2_$dati[["Titolo"]][5] == "Catalogus causarum expeditarum" })
# 02/11/2026, dal foglio di prova da 100 righe: date illeggibili, luoghi sconosciuti, produttore con maiuscola diversa, punteggiatura finale.
fp_ <- list(
  colonna = c("Segnatura" = "S", "Estremo remoto" = "ER", "Estremo recente" = "EC", "Luoghi citati o indicizzati" = "L", "Soggetto produttore" = "P", "Note libere" = "N",
              "Regesto o descrizione sintetica" = "R"),
  dati = data.frame(S = sprintf("x, b. 1, fasc. %d", 1:8),
                    ER = c("1561?", "1056", "1570", "n.d.", "sec. XVI", "1 5 6 3", "1561", "1561"),
                    EC = c(NA, NA, "1565", NA, NA, NA, NA, NA),
                    L = c("Bologna", "Bologna", "Bologna", "Xyzburg|Bologna", "Bologna", "Bologna", "Bologna", "Bologna"),
                    P = c(rep("Tribunale di prova", 6), "Tribunale di Prova", "Tribunale di prova"),
                    N = c(NA, NA, NA, NA, NA, NA, "Carte rovinate  ,", NA),
                    R = c(NA, NA, NA, NA, NA, NA, NA, "Atti circa Mario Rossi.."), stringsAsFactors = FALSE))
ad_ <- avvisi_date(fp_)$testo
v("date: «1561?» e «1 5 6 3» segnalate come non leggibili", sum(grepl("non è un anno leggibile", ad_)) == 2L)
v("date: anno fuori intervallo segnalato", sum(grepl("fuori dall'intervallo", ad_)) == 1L)
v("date: recente prima del remoto segnalato", sum(grepl("precedente all'estremo remoto", ad_)) == 1L)
v("date: «n.d.» e «sec. XVI» non segnalate", !any(grepl("^riga (5|6) ", ad_)) && length(ad_) == 4L)
pr_ <- concordanza_righe(fp_, avvisi_luoghi = TRUE); t_ <- pr_$da_decidere$testo
v("luogo sconosciuto e raro: segnalato, cella invariata", sum(grepl("«Xyzburg» non è nella lista", t_)) == 1L && identical(pr_$dati$L[4], "Xyzburg|Bologna"))
v("luogo sconosciuto: senza `avvisi_luoghi` (finestra di revisione) non si segnala", !any(grepl("Xyzburg", concordanza_righe(fp_)$da_decidere$testo)))
v("produttore con maiuscola diversa: da decidere", sum(grepl("soggetto produttore «Tribunale di Prova»", t_)) == 1L)
v("punteggiatura finale: virgola tolta, punto doppio ridotto", identical(pr_$dati$N[7], "Carte rovinate") && identical(pr_$dati$R[8], "Atti circa Mario Rossi."))
fr_ <- list(colonna = c("Segnatura" = "S", "Regesto o descrizione sintetica" = "R", "Soggettario" = "G"),
            dati = data.frame(S = sprintf("x, b. 1, fasc. %d", 1:6),
                              R = c("Processo per eresia contro Mario Rossi.", "Processo per magia contro Mario Rossi.", "Atti per magia; eresia contro Mario Rossi.",
                                    "Processo per eresia contro Mario Rossi.", "Processo per eresia contro Mario Rossi.", "Atti circa Mario Rossi."),
                              G = c("Eresia", "Eresia", "Eresia|Magia", NA, "Furto", "Eresia"), stringsAsFactors = FALSE))
ar_ <- avvisi_reati(fr_)
v("reati: regesto e soggettario diversi segnalati, anche col soggettario vuoto", identical(ar_$riga, c(3L, 5L)) && grepl("«magia»", ar_$testo[1]) && grepl("niente", ar_$testo[2]))
lg_ <- data.frame(chiave = c("otranto", "ottaviano", "sant'antonio abate"), nome = c("Otranto", "Ottaviano", "Sant'Antonio Abate"), paese = "Italia", wikidata = "", coordinate = "", stringsAsFactors = FALSE)
lg_$chiave <- chiave_grafia(lg_$nome)
sc_ <- list(colonna = c("Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
            dati = data.frame(S = sprintf("x, b. 1, fasc. %d", 1:4), L = c("Otarnto", "Ottoviano", "Satn'Antonio Abate", "Xyzburg"), stringsAsFactors = FALSE))
dc_ <- concordanza_righe(sc_, lista_luoghi = lg_)$decisioni
v("luoghi: lettere scambiate sono una proposta, mai una correzione sola; una lettera sbagliata non si propone (troppo rumore)",
  length(dc_) == 2L && all(grepl("forse", vapply(dc_, `[[`, "", "testo"))) && grepl("«Otranto»", dc_[[1]]$testo) && grepl("«Sant'Antonio Abate»", dc_[[2]]$testo))
v("luoghi: la correzione con scambio o sostituzione non cambia la cella", identical(concordanza_righe(sc_, lista_luoghi = lg_)$dati$L, sc_$dati$L))
# 02/11: la lista dei luoghi attraversa la normalizzazione senza essere toccata, ed è pulita
gl_ <- leggi_lista_luoghi(file.path("dati", "luoghi.csv"))
rl_ <- concordanza_righe(list(colonna = c("Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
  dati = data.frame(S = sprintf("x, b. 1, fasc. %d", seq_len(nrow(gl_))), L = gl_$nome, stringsAsFactors = FALSE)), lista_luoghi = gl_)
v("lista dei luoghi: ogni nome attraversa la normalizzazione identico; gli unici dubbi sono gli avvisi sugli omonimi",
  identical(rl_$dati$L, gl_$nome) && all(vapply(rl_$decisioni, function(d) d$tipo == "omonimo" && grepl("esiste in più luoghi", d$testo), TRUE)))
v("lista dei luoghi: chiavi uniche, apostrofo dritto, nessun separatore nei nomi, nessun «/»",
  !anyDuplicated(gl_$chiave) && !any(grepl("[’‘`´|;/]", gl_$nome)))
v("lista dei luoghi: wikidata «Q123» e coordinate «lat, lon»; le italiane stanno in Italia",
  all(!nzchar(gl_$wikidata) | grepl("^Q[0-9]+$", gl_$wikidata)) && {
    xy_ <- do.call(rbind, strsplit(gl_$coordinate[nzchar(gl_$coordinate) & gl_$paese == "Italia"], ", "))
    all(grepl("^-?[0-9.]+, -?[0-9.]+$", gl_$coordinate[nzchar(gl_$coordinate)])) && all(as.numeric(xy_[, 1]) > 35 & as.numeric(xy_[, 1]) < 48 & as.numeric(xy_[, 2]) > 6 & as.numeric(xy_[, 2]) < 19) })
# 02/11: omonimi - la cella non cambia; in «Da decidere» le possibilità, la prima proposta è la più vicina agli altri luoghi della riga
om_ <- data.frame(chiave = c("castroprovbergamo", "castroprovlecce", "lecce", "bergamo", "londra", "london", "casaleuno", "casaledue"),
                  nome = c("Castro", "Castro", "Lecce", "Bergamo", "Londra", "Londra", "Casale", "Casale"), paese = "Italia",
                  wikidata = c("Q100192", "Q52129", "Q1", "Q2", "Q84", "Q84", "Q5", "Q6"),
                  coordinate = c("45.802990, 10.067060", "40.007022, 18.425733", "40.351, 18.172", "45.698, 9.677", "51.507, -0.127", "51.507, -0.127", "40.1, 15.1", "45.3, 8.4"), stringsAsFactors = FALSE)
om_$chiave[1:2] <- c("castroprovbergamo", "castroprovlecce")
ao_ <- list(colonna = c("Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
            dati = data.frame(S = sprintf("x, b. 1, fasc. %d", 1:5), L = c("Lecce|Castro", "Bergamo|Castro", "Castro", "Londra", "Casale"), stringsAsFactors = FALSE))
ro_ <- concordanza_righe(ao_, lista_luoghi = om_)
to_ <- vapply(ro_$decisioni, function(d) d$testo, "")
v("omonimi: la cella non cambia e il caso è in «Da decidere»", identical(ro_$dati$L, ao_$dati$L) && length(to_) == 3L && all(vapply(ro_$decisioni, function(d) d$tipo, "") == "omonimo"))
v("omonimi: la prima proposta è il più vicino agli altri luoghi della riga (Lecce → Castro di Lecce, Bergamo → Castro di Bergamo)",
  grepl("prima proposta è «Castro \\(comune in provincia di Lecce\\)»", to_[1]) && grepl("prima proposta è «Castro \\(comune in provincia di Bergamo\\)»", to_[2]))
v("omonimi: senza altri luoghi nella riga elenca le possibilità e non propone", grepl("non ha altri luoghi", to_[3]) && !grepl("prima proposta", to_[3]))
v("omonimi: lo stesso luogo in due grafie («Londra»/«London», stesso Wikidata) e più frazioni senza un comune non sono omonimi", !any(grepl("Londra|Casale", to_)))
# 02/11: omonimo scelto nella finestra - la cella non cambia, la scelta va nella traccia e nel foglio «Luoghi scelti»
ao2_ <- leggi_arianna(F); ao2_$dati[["Luoghi citati o indicizzati"]][2] <- "Lecce|Castro"
po_ <- proposte_import(ao2_, NULL, NULL)
do_ <- Filter(function(d) identical(d$tipo, "omonimo"), po_$decisioni)
v("omonimi: nella finestra c'è una decisione con le possibilità, nessuna preselezionata", length(do_) == 1L && length(do_[[1]]$opzioni) == 2L && is.null(do_[[1]]$predefinita))
sc_o <- setNames(list(do_[[1]]$opzioni[1]), as.character(do_[[1]]$id))
ap_o <- applica_proposte(ao2_, po_, ok = integer(0), scelte = sc_o)
v("omonimi: scelto il primo, la cella resta «Lecce|Castro» e la scelta è nella traccia con identificativo",
  identical(ap_o$dati[["Luoghi citati o indicizzati"]][2], "Lecce|Castro") && nrow(attr(ap_o, "omonimi_scelti")) == 1L && grepl("^Q", attr(ap_o, "omonimi_scelti")$Wikidata) && grepl("Lecce", attr(ap_o, "omonimi_scelti")$Scelta))
v("omonimi: senza scelta nessuna traccia", is.null(attr(applica_proposte(ao2_, po_, ok = integer(0), scelte = list()), "omonimi_scelti")))
pl_ <- pulisci_foglio(ap_o, archivio = "Archivio di Prova", fondo = "Fondo di Prova", revisionato = TRUE)
v("omonimi: il file pulito ha il foglio «Luoghi scelti»", "Luoghi scelti" %in% readxl::excel_sheets(pl_$percorso) && nrow(readxl::read_excel(pl_$percorso, sheet = "Luoghi scelti")) == 1L)
# 02/11: una grafia senza accenti o in maiuscolo che corrisponde a più nomi diversi («Paterno» / «Paternò») non si riscrive: si propone
cz_ <- function(v) concordanza_righe(list(colonna = c("Luoghi citati o indicizzati" = "L", "Segnatura" = "S"),
  dati = data.frame(S = sprintf("x, b. 1, fasc. %d", seq_along(v)), L = v, stringsAsFactors = FALSE)), lista_luoghi = gl_)
rp_ <- cz_(c("PATERNÒ", "Paterno'", "Catania|PATERNO", "Paterno", "Paternò"))
v("chiave in collisione: «PATERNÒ», «Paterno'», «Catania|PATERNO» non cambiano da soli e sono una proposta fra le due forme",
  identical(rp_$dati$L, c("PATERNÒ", "Paterno'", "Catania|PATERNO", "Paterno", "Paternò")) && length(rp_$decisioni) == 3L &&
  all(vapply(rp_$decisioni, function(d) setequal(d$opzioni, c("Paterno", "Paternò")), TRUE)))
# 02/11: una voce «di prima» (chiave semplice, senza identificativo) accanto alla frazione con identificativo non è un comune: niente falso omonimo
v("omonimi: «Limidi» (frazione di Soliera) non è un omonimo; nessuna etichetta mostra pezzi di chiave",
  length(cz_(c("Modena|Limidi", "Limidi", "Vitriola", "Sillico"))$decisioni) == 0L)
nn_ <- names(table(gl_$nome))[table(gl_$nome) > 1]
eti_ <- unlist(lapply(Filter(function(d) d$tipo == "omonimo", cz_(nn_)$decisioni), function(d) sub("^.*\\((.*)\\)$", "\\1", d$opzioni)))
v("omonimi: ogni possibilità è «comune», «frazione», «comune in provincia di X» o «frazione di X»; nessun pezzo di chiave",
  length(eti_) > 100L && all(grepl("^(comune|frazione|comune in provincia di .+|frazione di .+|comune in un'altra provincia)$", eti_)))
# 02/11: la lista non ha due nomi diversi con lo stesso identificativo, e un identificativo non diventa un altro luogo fra una versione e l'altra
v("lista dei luoghi: uno stesso identificativo Wikidata non vale per due nomi che non sono lo stesso luogo scritto in due modi (solo Stati e città estere hanno alias)",
  { w_ <- gl_[nzchar(gl_$wikidata) & gl_$paese == "Italia", ]; all(tapply(chiave_grafia(w_$nome), w_$wikidata, function(k) length(unique(k))) == 1L) })
# 02/11: l'omonimo scelto in finestra sta in «Luoghi scelti» e non anche in «Da decidere»; il file pulito si apre anche con un lettore rigoroso
dd_ <- readxl::read_excel(pl_$percorso, sheet = "Da decidere", col_types = "text")
v("omonimi: dopo la scelta il caso non è più in «Da decidere» del file pulito", !any(grepl("Castro", dd_$Valore)))
v("omonimi: senza scelta il caso resta in «Da decidere»", { pn_ <- pulisci_foglio(ao2_, archivio = "Archivio di Prova", fondo = "Fondo di Prova"); any(grepl("Castro", readxl::read_excel(pn_$percorso, sheet = "Da decidere", col_types = "text")$Valore)) })
v("omonimi: dopo la scelta l'esito dell'import non lo conta fra i dubbi", nrow(.senza_omonimi_scelti(data.frame(riga = do_[[1]]$riga, testo = paste0("riga: ", do_[[1]]$testo), stringsAsFactors = FALSE), ap_o)) == 0L)
relazioni_ok <- function(xlsx) {
  d_ <- tempfile(); dir.create(d_); utils::unzip(xlsx, exdir = d_)
  rels_ <- list.files(file.path(d_, "xl/worksheets/_rels"), pattern = "[.]rels$", full.names = TRUE)
  all(vapply(rels_, function(r) { t_ <- regmatches(paste(readLines(r, warn = FALSE), collapse = ""), gregexpr('Target="[^"]+"', paste(readLines(r, warn = FALSE), collapse = "")))[[1]]
    all(file.exists(normalizePath(file.path(d_, "xl/worksheets", sub('^Target="(.*)"$', "\\1", t_)), mustWork = FALSE))) }, TRUE))
}
v("file pulito: nessun foglio ha relazioni verso parti che nel file non ci sono (lo rifiuterebbero openpyxl e pandas)", relazioni_ok(pl_$percorso))
v("file del registro degli errori: lo stesso", { pe_ <- tempfile(fileext = ".xlsx"); scrivi_errori_xlsx(elenco_errori_import(pulito = pl_), pe_); relazioni_ok(pe_) })
# 02/11: la chiave di una grafia diversa da quella della lista: per un luogo italiano si propone il nome della lista, per uno straniero la forma scritta resta
ra_ <- cz_(c("Racalbuto", "Berlin", "Sassonia", "Castel San Pietro"))
v("chiavi alternative: «Racalbuto» e «Castel San Pietro» (Italia) sono proposte col nome della lista; «Berlin» e «Sassonia» (estero) restano come scritti, senza avviso",
  identical(ra_$dati$L, c("Racalbuto", "Berlin", "Sassonia", "Castel San Pietro")) && length(ra_$decisioni) == 2L &&
  grepl("«Racalmuto»", ra_$decisioni[[1]]$testo) && grepl("«Castel San Pietro Terme»", ra_$decisioni[[2]]$testo))
cat(if (length(falliti)) sprintf("ESITO: NON TORNA (%s)\n", paste(falliti, collapse = "; ")) else "ESITO: ok\n"); if (length(falliti)) quit(status = 1)
