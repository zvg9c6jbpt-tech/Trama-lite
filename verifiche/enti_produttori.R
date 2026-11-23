# ─────────────────────────────────────────────────────────────
# verifiche/enti_produttori.R: gli enti produttori dell'anagrafica del profilo (D34)
# Un ente noto («Senato di Nizza») non è un'anomalia e non si tocca; una sua variante («Conseil del Commis») si riscrive nel nome, e si registra; una frase al posto
# del nome («Decreti e atti del S.Uffizio sul magnetismo animale…») è una decisione: produttore «Sant'Uffizio» e la frase in «Note libere», senza perdere niente.
#     Rscript verifiche/enti_produttori.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
mod <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
FRASE <- "Decreti e atti del S.Uffizio sul magnetismo animale e sui liberi muratori"
riga <- function(seg, prod, note = NA_character_) { r <- setNames(rep(NA_character_, length(mod)), mod)
  r["Segnatura"] <- paste0("Archivio di Stato di Torino, Materie ecclesiastiche, ", seg); r["Titolo"] <- "Atti"; r["Regesto o descrizione sintetica"] <- "Atti per magia."
  r["Soggetto produttore"] <- prod; r["Note libere"] <- note; r["Estremo remoto"] <- "1600"; r["Tipologia documentaria"] <- "Atti"; r["Soggettario"] <- "Magia"; r["Lingua"] <- "italiano"
  as.data.frame(as.list(r), check.names = FALSE) }
d <- rbind(riga("m. 1", "Senato di Nizza"), riga("m. 2", "Conseil del Commis"), riga("m. 3", FRASE), riga("m. 4", FRASE, "Una nota già scritta."),
           riga("m. 5", "Inquisizione di Torino"), riga("m. 6", "Atti vari della curia vescovile di Modena e Reggio"))
f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, f)
a <- leggi_arianna(f); P <- "Soggetto produttore"

cr <- concordanza_righe(a); Pr <- cr$dati[[P]]
v("un ente noto resta com'è", identical(Pr[1], "Senato di Nizza"))
v("la variante si riscrive nel nome dell'ente", identical(Pr[2], "Conseil des Commis"))
v("la riscrittura è registrata", any(cr$registro$riga == 3L & grepl("variante di un ente dell'anagrafica", cr$registro$perche)))
v("la regola «Inquisizione di <città>» non cambia", identical(Pr[5], "Inquisizione di Torino"))
v("la frase non si tocca in concordanza", identical(Pr[3], FRASE))

pr <- proposte_import(a, NULL, NULL)
dd <- Filter(function(x) identical(x$tipo, "produttore_descrizione"), pr$decisioni)
v("due frasi con «S.Uffizio» → due decisioni, proposto «Sant'Uffizio»", length(dd) == 2L && all(vapply(dd, function(x) x$proposto, "") == "Sant'Uffizio"))
av <- Filter(function(x) identical(x$tipo, "avviso") && grepl("è una frase", x$testo), pr$decisioni)
v("una frase che non dice chi è: solo un avviso, nessun cambio", length(av) == 1L && av[[1]]$riga == 7L)
v("l'ente noto e la variante non sono frasi", !any(vapply(pr$decisioni, function(x) x$riga %in% c(2L, 3L) && grepl("frase", x$testo), TRUE)))
v("proposta di partenza: «sposta», il lavoro resta allo schedatore", all(vapply(dd, function(x) x$predefinita, "") == "sposta"))
sc <- stats::setNames(rep(list("sposta"), length(dd)), vapply(dd, function(x) as.character(x$id), ""))
a2 <- applica_proposte(a, pr, ok = integer(0), scelte = sc)
v("scelto «sposta»: produttore «Sant'Uffizio»", identical(a2$dati[[P]][3], "Sant'Uffizio") && identical(a2$dati[[P]][4], "Sant'Uffizio"))
v("la frase è in Note libere", identical(a2$dati[["Note libere"]][3], paste("Produttore indicato nel foglio:", FRASE)))
v("una nota già scritta resta, la frase si aggiunge", identical(a2$dati[["Note libere"]][4], paste("Una nota già scritta.", paste("Produttore indicato nel foglio:", FRASE), sep = " · ")))
v("nient'altro cambia (altre righe uguali)", identical(a2$dati[[P]][c(1, 5, 6)], a$dati[[P]][c(1, 5, 6)]) && all(is.na(a2$dati[["Note libere"]][c(1, 2, 5, 6)])))
sc2 <- stats::setNames(rep(list("lascia"), length(dd)), vapply(dd, function(x) as.character(x$id), ""))
a3 <- applica_proposte(a, pr, ok = integer(0), scelte = sc2)
v("«Lascia com'è»: nessuna cella cambia", identical(a3$dati[[P]], a$dati[[P]]) && identical(a3$dati[["Note libere"]], a$dati[["Note libere"]]))
v("la decisione presa entra nel registro come dichiarata", {
  pv <- attr(a2, "proposte_viste"); sum(grepl("decisione presa nella finestra \\(produttore_descrizione\\)", pv$perche)) >= 2L })
if (length(falliti)) { cat("\nFALLITI:", length(falliti), "\n"); quit(status = 1) }
cat("ESITO: ok\n")
