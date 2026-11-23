# ─────────────────────────────────────────────────────────────
# verifiche/nomi_e_suffissi.R: tre lacune trovate con la prova «asp 2» (30/10/2026)
#   1. il suffisso numerale della segnatura ha una forma sola: «7 bis», «7ter», «4-ter.», «20-Bis» → «-bis», «-ter»; gli altri suffissi non si toccano;
#   2. un refuso nel nome del titolo o del regesto («Tomaso Bassi», «Tommaso Basi») contro le Persone della riga va in «Da decidere» (prima il titolo
#      di soli nomi cadeva e il regesto «Atti per <reato> circa <nome>», quello che Trama compone, non si leggeva);
#   3. «Gallo Matteo» con titolo «Matteo Gallo» si indicizza «Gallo, Matteo» (l'ordine lo dà il testo); senza testo resta l'ultima parola come cognome.
#     Rscript verifiche/nomi_e_suffissi.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

# 1. suffissi
suf <- function(x) { r <- .scomponi_segnatura(x); c(r$busta_suffisso, r$fascicolo_suffisso) }
v("«fasc. 7 bis» → «-bis»", identical(suf("A, B, b. 2, fasc. 7 bis"), c("", "-bis")))
v("«fasc. 7ter» → «-ter»", identical(suf("A, B, b. 2, fasc. 7ter"), c("", "-ter")))
v("«fasc. 4-ter.» → «-ter»", identical(suf("A, B, b. 3, fasc. 4-ter."), c("", "-ter")))
v("«fasc.20-Bis» → «-bis»", identical(suf("A, B, b. 3, fasc.20-Bis"), c("", "-bis")))
v("«b. 232 bis» → «-bis» sulla busta", identical(suf("A, B, b. 232 bis, fasc. 1"), c("-bis", "")))
v("idempotente: «-bis» resta «-bis»", identical(suf("A, B, b. 3, fasc. 4-bis"), c("", "-bis")))
v("altri suffissi intatti: «-a» e «-2»", identical(suf("A, B, b. 3, fasc. 2-a"), c("", "-a")) && identical(suf("A, B, b. 3, fasc. 1-2"), c("", "-2")))
v("l'ordine dei suffissi non cambia (bis < ter)", all(.rango_suffisso(c("-bis", "-ter")) == c(2L, 3L)))

# 2. e 3. nomi, su un foglio costruito dal modello
mod <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
riga <- function(seg, tit, reg, per) { r <- setNames(rep(NA_character_, length(mod)), mod)
  r["Segnatura"] <- paste0("Archivio di Prova, Fondo di Prova, ", seg); r["Titolo"] <- tit; r["Regesto o descrizione sintetica"] <- reg; r["Persone citate o indicizzate"] <- per
  r["Estremo remoto"] <- "1600"; r["Tipologia documentaria"] <- "Atti"; r["Soggettario"] <- "Magia"; r["Lingua"] <- "italiano"; as.data.frame(as.list(r), check.names = FALSE) }
d <- rbind(riga("b. 1, fasc. 1", "Tomaso Bassi", "Processo per magia contro Tomaso Bassi.", "Bassi, Tommaso"),
           riga("b. 1, fasc. 2", "Tommaso Bassi", "Atti per magia circa Tommaso Basi.", "Bassi, Tommaso"),
           riga("b. 1, fasc. 3", "Matteo Gallo", "Processo per magia contro Matteo Gallo.", "Gallo Matteo"),
           riga("b. 1, fasc. 4", "Lettera di Paolo a Pietro", "Atti per magia.", "Rossi Mario"),
           riga("b. 1, fasc. 5", "Anna Verdi", "Processo per magia contro Anna Verdi.", "Verdi, Anna"))
f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, f)
a <- leggi_arianna(f); cr <- concordanza_righe(a)
dec <- paste(cr$da_decidere$testo, collapse = "\n")
v("«Tomaso Bassi» (titolo) contro «Bassi, Tommaso» va in «Da decidere»", grepl("riga 2.*Tomaso Bassi", dec))
v("«Tommaso Basi» nel regesto «Atti per … circa …» va in «Da decidere»", grepl("riga 3.*Tommaso Basi", dec))
v("il titolo che non somiglia a nessuno («Lettera di Paolo a Pietro») non genera decisioni", !grepl("riga 5", dec))
v("la riga a posto non genera decisioni", !grepl("riga 6", dec))
v("i refusi non si correggono da soli", identical(cr$dati[[a$colonna[["Titolo"]]]][1], "Tomaso Bassi"))
pers <- cr$dati[[a$colonna[["Persone citate o indicizzate"]]]]
v("«Gallo Matteo» con titolo «Matteo Gallo» → «Gallo, Matteo»", identical(pers[3], "Gallo, Matteo"))
v("senza testo che lo dica resta l'ultima parola come cognome: «Rossi Mario» → «Mario, Rossi»", identical(pers[4], "Mario, Rossi"))
v("una persona già giusta non cambia", identical(pers[5], "Verdi, Anna"))
# 4. produttore e regesto in minuscolo, luogo di conservazione con un refuso, due varianti della stessa persona (stesso giorno)
r2 <- function(seg, lc, prod, reg, per) { x <- riga(seg, "Anna Verdi", reg, per); x[["Luogo di conservazione"]] <- lc; x[["Soggetto produttore"]] <- prod; x }
base_lc <- "Archivio di Stato di Benevento"
d2 <- do.call(rbind, c(lapply(1:5, function(k) r2(sprintf("b. 2, fasc. %d", k), base_lc, "Inquisizione di Benevento", "Processo per magia contro Anna Verdi.", "Verdi, Anna")),
  list(r2("b. 2, fasc. 6", "Archivio di Stato di Benvento", "inquisizione di benevento", "processo per magia contro Anna Verdi.", "Verdi, Anna|Rossi, Giovan Battista|Rossi, Gian Battista"))))
f2 <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d2, f2)
a2 <- leggi_arianna(f2); c2 <- concordanza_righe(a2); dd <- paste(c2$da_decidere$testo, collapse = "\n")
v("produttore «inquisizione di benevento» → «Inquisizione di Benevento»", identical(c2$dati[["Soggetto produttore"]][6], "Inquisizione di Benevento"))
v("regesto «processo per…» → iniziale maiuscola", identical(c2$dati[["Regesto o descrizione sintetica"]][6], "Processo per magia contro Anna Verdi."))
v("luogo di conservazione con un refuso: «Da decidere», non si corregge da solo", grepl("Benvento.*forse.*Benevento", dd) && identical(c2$dati[["Luogo di conservazione"]][6], "Archivio di Stato di Benvento"))
v("due varianti della stessa persona nella cella: segnalate", grepl("Giovan Battista.*Gian Battista.*stessa persona", dd))
v("una riga a posto non genera decisioni sul luogo di conservazione", !grepl("riga 2.*conservazione", dd))
# 5. la maiuscola del nome si corregge da sola, anche se il cognome discorda («marco Cerretoli» contro «Marco Cerretani»)
d3 <- rbind(riga("b. 3, fasc. 1", "Marco Cerretani", "Denuncia contro Marco Cerretani.", "Cerretoli, marco"))
f3 <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d3, f3)
a3 <- leggi_arianna(f3); c3 <- concordanza_righe(a3)
v("«Cerretoli, marco» → «Cerretoli, Marco» da solo (la maiuscola non è una decisione)", identical(c3$dati[["Persone citate o indicizzate"]][1], "Cerretoli, Marco"))
v("la decisione sul cognome resta, con i nomi già in maiuscola", any(grepl("«Marco Cerretoli».*«Marco Cerretani»", c3$da_decidere$testo)))
if (length(falliti)) { cat("\nNON TORNA:", paste(falliti, collapse = "; "), "\n"); cat("ESITO: NON TORNA\n"); quit(status = 1) }
cat("\nESITO: ok\n")
