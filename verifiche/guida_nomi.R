# ──────────────────────────────────────────────
# verifiche/guida_nomi.R: i nomi come li vuole la Guida al Tab (22/10/2026)
#   1. papi, re, imperatori e le consorti di re e imperatori hanno la formula «Nome (forma italiana; titolo)», senza virgola: non si
#      indicizzano in «Cognome, Nome» (prima «Marie-Antoinette d'Autriche (…)» usciva «d'Autriche (…), Marie-Antoinette»);
#   2. «S.» si scioglie sempre in San, Santo, Santa o Sant' (decisione di Stefano, 22/10).
#     Rscript verifiche/guida_nomi.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1")
suppressMessages({ library(DBI); library(RSQLite); library(uuid); library(jsonlite); library(readxl); library(openxlsx) })
source("verifiche/aiuti.R")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

# 1. la formula dei sovrani
formule <- c("Marie-Antoinette d'Autriche (Maria Antonietta d'Asburgo-Lorena; regina di Francia)", "Catherine de Médicis (Caterina de' Medici; regina di Francia)",
             "Eleonora Gonzaga (imperatrice)", "Louis XIV (Luigi XIV; re di Francia)", "Pius VII (Pio VII; Chiaramonti, Barnaba; papa)", "Karl V (Carlo V; imperatore)",
             "Napoléon I (Napoleone I; imperatore dei Francesi)", "Carlos III (Carlo III; re di Spagna)")
for (x in formule) {
  v(sprintf("formula del sovrano riconosciuta: %s", substr(x, 1, 40)), .formula_sovrano(x))
  v(sprintf("l'export non la riscrive: %s", substr(x, 1, 40)), identical(.nome_persona_export(.nome_persona_import(x)), x))
}
v("«Rossi, Mario (conte di Cavour)» non è un sovrano", !.formula_sovrano("Rossi, Mario (conte di Cavour)"))
v("«Mario Rossi (1550-1620)» non è un sovrano", !.formula_sovrano("Mario Rossi (1550-1620)"))
v("un nome senza virgola l'export dal database non lo gira (lo decide l'anteprima, 23/10)", identical(.nome_persona_export("Mario Rossi"), "Mario Rossi"))
v("la forma generale con titolo resta com'è", identical(.nome_persona_export("Gonzaga, Ferrante I (conte di Guastalla)"), "Gonzaga, Ferrante I (conte di Guastalla)"))
v("la particella dopo il nome con titolo, come prima", identical(.nome_persona_export("de Lama, Anna"), "Lama, Anna de"))

# dal foglio al foglio: la formula attraversa «Pulisci e scarica» senza cambiare
d <- as.data.frame(read_excel("dati/modello_export.xlsx", col_types = "text"), check.names = FALSE)
m <- d[0, ]; m[seq_along(formule), ] <- NA
m[["Segnatura"]] <- sprintf("Archivio di Prova, Fondo Prova, b. %d, fasc. 1", seq_along(formule)); m[["Titolo"]] <- "Atti"
m[["Persone citate o indicizzate"]] <- formule
f1 <- tempfile(fileext = ".xlsx"); write.xlsx(m, f1)
p <- pulisci_foglio(leggi_arianna(f1), nome = "sovrani.xlsx")
pul <- as.data.frame(read_excel(p$percorso, sheet = 1, col_types = "text"), check.names = FALSE)
v("«Pulisci e scarica» lascia le formule dei sovrani come sono", identical(pul[["Persone citate o indicizzate"]], formule))

# 2. «S.» sciolto sempre
casi <- c("S. Severo" = "San Severo", "S. Maria Maggiore" = "Santa Maria Maggiore", "S. Stefano" = "Santo Stefano", "S. Anna" = "Sant'Anna", "S. Angelo" = "Sant'Angelo",
          "S.Elmo" = "Sant'Elmo", "S.ta Maria" = "Santa Maria", "S.to Spirito" = "Santo Spirito", "S. Giorgio" = "San Giorgio", "S. Luca" = "San Luca",
          "S. Nicola" = "San Nicola", "S. Croce" = "Santa Croce", "S. Sebastiano" = "San Sebastiano", "S.ti Cosma e Damiano" = "Santi Cosma e Damiano",
          "S. Uffizio" = "Sant'Uffizio", "S. Officio di Napoli" = "Sant'Officio di Napoli", "S. Sede" = "Santa Sede", "S. Padre" = "Santo Padre",
          "Borromeo, S. Carlo" = "Borromeo, San Carlo", "chiesa di S. Francesco e S. Chiara" = "chiesa di San Francesco e Santa Chiara")
for (k in names(casi)) v(sprintf("«%s» → «%s»", k, casi[[k]]), identical(sciogli_santi(k), casi[[k]]))
for (k in c("S. Congregazione", "S. Consulta", "SS. Trinità", "Ss. Apostoli", "Rossi, Mario S.", "San Marco", "Sant'Angelo", "Modena"))
  v(sprintf("«%s» non cambia", k), identical(sciogli_santi(k), k))
v("un valore vuoto o NA resta com'è", identical(sciogli_santi(c(NA, "")), c(NA, "")))
v("è idempotente", identical(sciogli_santi(sciogli_santi(names(casi))), unname(casi)))

# dal foglio: luoghi, persone, soggetti, titolo e regesto; non le note né la segnatura
m <- d[0, ]; m[1, ] <- NA
m[["Segnatura"]] <- "Archivio di Prova, Fondo Prova, b. 1, fasc. 1"
m[["Titolo"]] <- "Processo contro Carlo Rossi a S. Severo"; m[["Regesto o descrizione sintetica"]] <- "Processo per eresia contro Carlo Rossi, a S. Severo."
m[["Persone citate o indicizzate"]] <- "Rossi, Carlo|S. Carlo Borromeo"; m[["Soggetti citati o indicizzati"]] <- "Carlo Rossi|S. Francesco"
m[["Luoghi citati o indicizzati"]] <- "S. Severo|S. Maria Maggiore"; m[["Luogo di produzione (data topica)"]] <- "S. Stefano"
m[["Note libere"]] <- "Vedi S. Severo."
f2 <- tempfile(fileext = ".xlsx"); write.xlsx(m, f2)
cz <- concordanza_righe(leggi_arianna(f2))$dati
v("luoghi citati e luogo di produzione sciolti", identical(cz[["Luoghi citati o indicizzati"]], "San Severo|Santa Maria Maggiore") && identical(cz[["Luogo di produzione (data topica)"]], "Santo Stefano"))
v("soggetti sciolti; nelle persone «S.» resta (si propone)", identical(cz[["Persone citate o indicizzate"]], "Rossi, Carlo|S. Carlo Borromeo") && identical(cz[["Soggetti citati o indicizzati"]], "Carlo Rossi|San Francesco"))
v("titolo e regesto sciolti", grepl("a San Severo$", cz[["Titolo"]]) && grepl("a San Severo\\.$", cz[["Regesto o descrizione sintetica"]]))
v("l'anteprima conta le celle con «S.»", sum(correzioni_import(leggi_arianna(f2))$celle[grepl("«S.» sciolto", correzioni_import(leggi_arianna(f2))$regola)]) >= 6L)
v("le note restano com'erano", identical(cz[["Note libere"]], "Vedi S. Severo."))
v("la segnatura non cambia", identical(cz[["Segnatura"]], "Archivio di Prova, Fondo Prova, b. 1, fasc. 1"))

# 3. Dai tab veri (22/10): sigle, «Sua…» e il dubbio «Nome S. Cognome»
for (k in c("lettera a S.M", "di S. A", "cancelliere S.U", "S.A.R. il duca", "S.M. Cattolica", "del S. Santità", "a S. Eccellenza", "S. Maestà"))
  v(sprintf("sigla o «Sua…»: «%s» non cambia", k), identical(sciogli_santi(k), k) && identical(sciogli_santi(k, guardia = FALSE), k))
v("«S. Pontefice» non cambia; «S.t Officio» → «Sant'Officio»; «S.t Office» (francese) non cambia", identical(sciogli_santi("al S. Pontefice"), "al S. Pontefice") && identical(sciogli_santi("del S.t Officio di"), "del Sant'Officio di") && identical(sciogli_santi("au S.t Office"), "au S.t Office"))
v("«del S.to Officio» → «del Sant'Officio»", identical(sciogli_santi("del S.to Officio"), "del Sant'Officio"))
v("«del S. Ufficio» → «del Sant'Ufficio»", identical(sciogli_santi("del S. Ufficio"), "del Sant'Ufficio"))
v("dopo un nome maiuscolo resta («Mario S. Rossi»), nelle colonne di persone e testo", identical(sciogli_santi("Mario S. Rossi"), "Mario S. Rossi"))
v("…e lo scioglie solo se glielo si chiede (la proposta)", identical(sciogli_santi("Mario S. Rossi", dubbi = TRUE), "Mario San Rossi"))
v("nelle colonne di luoghi si scioglie sempre", identical(sciogli_santi("Borgo S. Donnino", guardia = FALSE), "Borgo San Donnino") && identical(sciogli_santi("Mario S. Rossi", guardia = FALSE), "Mario San Rossi"))
v("dopo un nome di luogo o di ente si scioglie anche nelle altre colonne", identical(sciogli_santi("Convento S. Maria"), "Convento Santa Maria") && identical(sciogli_santi("Borgo S. Donnino"), "Borgo San Donnino"))
v("dopo una virgola o una preposizione si scioglie", identical(sciogli_santi("Rossi, S. Carlo"), "Rossi, San Carlo") && identical(sciogli_santi("a S. Severo"), "a San Severo"))
m <- d[0, ]; m[1, ] <- NA
m[["Segnatura"]] <- "Archivio di Prova, Fondo Prova, b. 1, fasc. 1"; m[["Titolo"]] <- "Lettera a S.M da Carlo S. Rossi"
m[["Regesto o descrizione sintetica"]] <- "Il S.U scrive a S. Santità."; m[["Luoghi citati o indicizzati"]] <- "S. Severo"
f3 <- tempfile(fileext = ".xlsx"); write.xlsx(m, f3)
a3 <- leggi_arianna(f3); cz3 <- concordanza_righe(a3)$dati
v("pipeline: sigle e «Sua Santità» restano, «Carlo S. Rossi» resta, il luogo si scioglie", identical(cz3[["Titolo"]], "Lettera a S.M da Carlo S. Rossi") && identical(cz3[["Regesto o descrizione sintetica"]], "Il S.U scrive a S. Santità.") && identical(cz3[["Luoghi citati o indicizzati"]], "San Severo"))
pr3 <- proposte_import(a3, NULL, NULL, luoghi_ignoti = FALSE); pp3 <- pr3$celle[grepl("da sciogliere", pr3$celle$perche), , drop = FALSE]
v("«Carlo S. Rossi» è una proposta, non spuntata di partenza, con la forma sciolta", nrow(pp3) == 1L && !pp3$consigliata && grepl("Carlo San Rossi", pp3$dopo, fixed = TRUE) && grepl("S.M", pp3$dopo, fixed = TRUE))

# 4. Dizionario dei santi (dati/santi.csv, 22/10): il genere viene dal nome, non dalla desinenza; nelle persone un nome ignoto non si indovina
casi4 <- c("S. Scolastica" = "Santa Scolastica", "S. Beatrice" = "Santa Beatrice", "S. Agata" = "Sant'Agata", "S. Giuseppe" = "San Giuseppe", "S. Spirito" = "Santo Spirito", "S. Bonaventura" = "San Bonaventura", "S. Caterina" = "Santa Caterina")
for (k in names(casi4)) v(sprintf("dizionario: «%s» → «%s»", k, casi4[[k]]), identical(sciogli_santi(k), casi4[[k]]))
v("un nome che non è nel dizionario si indovina ancora nei luoghi (ipotesi)", identical(sciogli_santi("S. Rossi", guardia = FALSE), "San Rossi") && santo_noto("Scolastica") && !santo_noto("Rossi"))
v("nelle persone un nome ignoto resta com'è con ignoti = «lascia»", identical(sciogli_santi("S. Rossi", ignoti = "lascia"), "S. Rossi") && identical(sciogli_santi("S. Carlo Borromeo", ignoti = "lascia"), "San Carlo Borromeo") && identical(sciogli_santi("S. Uffizio", ignoti = "lascia"), "Sant'Uffizio"))
m <- d[0, ]; m[1, ] <- NA
m[["Segnatura"]] <- "Archivio di Prova, Fondo Prova, b. 1, fasc. 1"; m[["Persone citate o indicizzate"]] <- "S. Scolastica|S. Rossi|Bianchi, Luca"; m[["Soggetti citati o indicizzati"]] <- "S. Beatrice"
f4 <- tempfile(fileext = ".xlsx"); write.xlsx(m, f4)
a4 <- leggi_arianna(f4); cz4 <- concordanza_righe(a4)$dati
v("persone: nessun «S.» si scioglie da solo, nemmeno la santa del dizionario", identical(cz4[["Persone citate o indicizzate"]], "S. Scolastica|S. Rossi|Bianchi, Luca") && identical(cz4[["Soggetti citati o indicizzati"]], "Santa Beatrice"))
pr4 <- proposte_import(a4, NULL, NULL, luoghi_ignoti = FALSE); pp4 <- pr4$celle[grepl("da sciogliere", pr4$celle$perche), , drop = FALSE]
v("«S. Scolastica» e «S. Rossi» sono proposte spente, con la forma sciolta, mai applicate da sole", nrow(pp4) == 1L && !any(pp4$consigliata) && grepl("Santa Scolastica", pp4$dopo, fixed = TRUE) && grepl("San Rossi", pp4$dopo, fixed = TRUE))
p4 <- pulisci_foglio(a4, nome = "s4.xlsx"); o4 <- as.data.frame(readxl::read_excel(p4$percorso, sheet = 1, col_types = "text"), check.names = FALSE)
v("«Pulisci» non scrive «San Rossi» se la proposta non è accettata", !any(grepl("San Rossi", unlist(o4), fixed = TRUE)))
v("persone: dopo il cognome «S. Giovanni» può essere un'iniziale: resta e si propone (anche se il santo è noto)", identical(sciogli_santi("Rossi, S. Giovanni", ignoti = "persone"), "Rossi, S. Giovanni") && identical(sciogli_santi("S. Giovanni", ignoti = "persone"), "S. Giovanni") && identical(sciogli_santi("Rossi, S. Giovanni", dubbi = TRUE), "Rossi, San Giovanni"))

# San Zeno, San Zaccaria, San Zenone: l'uso corrente (Verona, Venezia), non «Santo» davanti a Z (23/10, secondo parere)
casiz <- c("S. Zeno" = "San Zeno", "S. Zaccaria" = "San Zaccaria", "S. Zenone" = "San Zenone", "S. Stefano" = "Santo Stefano", "S. Spirito" = "Santo Spirito")
for (k in names(casiz)) v(sprintf("luoghi: «%s» → «%s»", k, casiz[[k]]), identical(sciogli_santi(k, guardia = FALSE), casiz[[k]]))
m7 <- d[0, ]; m7[1, ] <- NA; m7[["Segnatura"]] <- "Archivio di Prova, Fondo Prova, b. 1, fasc. 1"; m7[["Luoghi citati o indicizzati"]] <- "S. Zeno|S. Zaccaria"
f7 <- tempfile(fileext = ".xlsx"); write.xlsx(m7, f7); p7 <- pulisci_foglio(leggi_arianna(f7), nome = "z.xlsx")
v("file esportato: «San Zeno|San Zaccaria»", identical(as.data.frame(readxl::read_excel(p7$percorso, sheet = 1, col_types = "text"), check.names = FALSE)[["Luoghi citati o indicizzati"]], "San Zeno|San Zaccaria"))

# Il dizionario dei santi: se il file non si trova non si memorizza il vuoto (23/10): ritrovato il file, torna a funzionare, e il guasto si dice
home0 <- Sys.getenv("TRAMA_HOME"); Sys.setenv(TRAMA_HOME = tempfile("home-santi-")); dir.create(Sys.getenv("TRAMA_HOME"))      # il registro dell'errore non va nella cartella vera
dir0 <- getwd(); .SANTI$d <- NULL
setwd(tempdir()); vuoto_ <- length(dizionario_santi()); nota_ <- tryCatch(santo_noto("Scolastica"), error = function(e) NA)
setwd(dir0); if (nzchar(home0)) Sys.setenv(TRAMA_HOME = home0) else Sys.unsetenv("TRAMA_HOME")
v("senza il file il dizionario è vuoto ma non resta vuoto per sempre", vuoto_ == 0L && identical(nota_, FALSE) && length(dizionario_santi()) > 200L && santo_noto("Scolastica"))
v("il dizionario ha il genere di ogni nome (M o F) e nessun nome doppio", all(dizionario_santi() %in% c("M", "F")) && !anyDuplicated(names(dizionario_santi())))
v("«Persone»: nessun «S.» si scioglie da solo, anche con un santo del dizionario (S. Giovanni, S. Scolastica, S. Carlo Borromeo)",
  all(vapply(c("S. Giovanni", "S. Scolastica", "S. Carlo Borromeo"), function(x) identical(sciogli_santi(x, ignoti = "persone"), x), TRUE)))

if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
