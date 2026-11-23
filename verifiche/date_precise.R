# ──────────────────────────────────────────────
# verifiche/date_precise.R: giorno e mese facoltativi negli estremi, «!» come separatore sbagliato, soggettario della Guida al Tab (22/10/2026)
#   1. «aaaa-mm-gg» e «mese aaaa» (Guida al Tab) entrano come anno + data intera, escono com'erano, si ripuliscono («1625-1-9», «Marzo 1630»);
#      una data impossibile (30 febbraio) resta una decisione; l'ordine remoto/recente guarda anche mese e giorno solo se li hanno entrambe.
#   2. Se l'anno del campo cambia, esce l'anno (la data intera non si inventa).
#   3. «!» fra voci è un errore in reati, lingue, luoghi e persone; il soggettario ammette le 19 classi (D33).
#     Rscript verifiche/date_precise.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1")
suppressMessages({ library(DBI); library(RSQLite); library(uuid); library(jsonlite); library(readxl); library(openxlsx) })
source("verifiche/aiuti.R")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

# 1. riconoscimento
v("1625-01-09 è una data intera", identical(.data_precisa("1625-01-09")$canonica, "1625-01-09"))
v("«1625-1-9» si scrive 1625-01-09", identical(.data_precisa("1625-1-9")$canonica, "1625-01-09"))
v("«Marzo 1644» si scrive «marzo 1644»", identical(.data_precisa("Marzo 1644")$canonica, "marzo 1644"))
v("«1644-03» si scrive «marzo 1644»", identical(.data_precisa("1644-03")$canonica, "marzo 1644"))
v("30 febbraio non è una data", is.null(.data_precisa("1625-02-30")))
v("29 febbraio: bisestile sì (1600), no (1900)", !is.null(.data_precisa("1600-02-29")) && is.null(.data_precisa("1900-02-29")))
v("l'anno nudo e il secolo non sono date intere", is.null(.data_precisa("1644")) && is.null(.data_precisa("sec. XV")))
v("«marzo 1625» non contraddice «1625-03-05»", !.data_dopo(.data_precisa("marzo 1625"), .data_precisa("1625-03-05")))
v("1625-03-05 viene dopo 1625-02-01", .data_dopo(.data_precisa("1625-03-05"), .data_precisa("1625-02-01")))

# 2. dal foglio al database e ritorno
sorgente <- apri_db(":memory:")
mk <- function(n, remoto, recente) {
  f <- fascicolo_vuoto(FALSE); f$archivio <- "Archivio"; f$fondo <- "Fondo"; f$busta <- 1L; f$fascicolo <- n
  f$estremo_remoto <- remoto; f$estremo_recente <- recente; f$titolo <- "Processo contro Mario Rossi"; f$titolo_manuale <- 1L
  f$regesto <- "Processo per eresia contro Mario Rossi."; f$regesto_manuale <- 1L; f$persone <- "Rossi, Mario"; f$reati <- "Eresia"
  salva_fascicolo(sorgente, f)
}
mk(1L, 1625L, 1630L); mk(2L, 1625L, 1625L); mk(3L, 1625L, 1630L)
fid <- elenco_fondi(sorgente)$id[1]
base <- tempfile(fileext = ".xlsx"); esporta_arianna(sorgente, base, fondo = fid)
d <- as.data.frame(read_excel(base, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
d[["Estremo remoto"]]  <- c("1625-1-9", "marzo 1625", "1625-02-30")
d[["Estremo recente"]] <- c("Marzo 1630", "1625-03-05", "1630")
sporco <- tempfile(fileext = ".xlsx"); write.xlsx(d, sporco)

a <- leggi_arianna(sporco); dest <- apri_db(":memory:")
pr <- proposte_import(a, dest, NULL)
cella <- function(col, riga) { k <- which(pr$celle$colonna == col & pr$celle$riga == riga); if (length(k)) pr$celle$dopo[k[1]] else NA_character_ }
v("«1625-1-9» è corretto in «1625-01-09»", identical(cella("Estremo remoto", 2L), "1625-01-09"))
v("«Marzo 1630» è corretto in «marzo 1630»", identical(cella("Estremo recente", 2L), "marzo 1630"))
tipi <- vapply(pr$decisioni, function(x) paste(x$tipo %||% "", x$riga), "")
v("«30 febbraio» resta una decisione sull'anno", "anno 4" %in% tipi)
v("le date intere valide non chiedono nulla", !any(c("anno 2", "anno 3") %in% tipi))
v("«marzo 1625» e «1625-03-05» (stessa data parziale) non fanno scattare l'ordine", !("ordine_anni 3" %in% tipi))

# accettando le correzioni sicure, solo le righe 1-2 sono importabili con la data intera
a2 <- applica_proposte(a, pr, pr$celle$id, list("4" = NULL))
a2$dati <- a2$dati[1:2, , drop = FALSE]
e <- importa_arianna(dest, a2, modo = "accoda")
v("le due righe entrano", e$importati == 2L)
righe <- dbGetQuery(dest, "SELECT fascicolo, estremo_remoto, estremo_recente FROM fascicoli ORDER BY fascicolo")
v("l'anno entra nei campi numerici", identical(righe$estremo_remoto, c(1625L, 1625L)) && identical(righe$estremo_recente, c(1630L, 1625L)))
fid2 <- elenco_fondi(dest)$id[1]
fuori <- tempfile(fileext = ".xlsx"); esporta_arianna(dest, fuori, fondo = fid2)
x <- as.data.frame(read_excel(fuori, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
v("l'export riscrive le date intere come stanno nella guida",
  identical(x[["Estremo remoto"]], c("1625-01-09", "marzo 1625")) && identical(x[["Estremo recente"]], c("marzo 1630", "1625-03-05")))

# idempotenza: il foglio esportato attraversa l'import con 0 celle cambiate
pr2 <- proposte_import(leggi_arianna(fuori), apri_db(":memory:"), NULL)
v("il foglio esportato non chiede correzioni sulle date", !any(grepl("Estremo", pr2$celle$colonna)) && !any(vapply(pr2$decisioni, function(x) isTRUE(x$tipo %in% c("anno", "ordine_anni")), TRUE)))

# se l'anno cambia, esce l'anno
dbExecute(dest, "UPDATE fascicoli SET estremo_remoto = 1626 WHERE fascicolo = 1")
cambiato <- tempfile(fileext = ".xlsx"); esporta_arianna(dest, cambiato, fondo = fid2)
y <- as.data.frame(read_excel(cambiato, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
v("anno cambiato a mano: nel foglio esce l'anno, non la data vecchia", identical(y[["Estremo remoto"]][1], "1626"))

# l'ordine con mese e giorno
d2 <- d; d2[["Estremo remoto"]] <- c("1625-03-05", "1625-03-05", "1625-03-05"); d2[["Estremo recente"]] <- c("1625-02-01", "marzo 1625", "1625-03-05")
ord <- tempfile(fileext = ".xlsx"); write.xlsx(d2, ord)
pr3 <- proposte_import(leggi_arianna(ord), apri_db(":memory:"), NULL)
tipi3 <- vapply(pr3$decisioni, function(x) paste(x$tipo %||% "", x$riga), "")
v("recente 1625-02-01 prima di remoto 1625-03-05: è un errore", "ordine_anni 2" %in% tipi3)
v("recente «marzo 1625» dopo remoto 1625-03-05: non lo è", !("ordine_anni 3" %in% tipi3) && !("ordine_anni 4" %in% tipi3))

# 3. «!» e vocabolario
v("«Giurisdizione!Eresia» → «Giurisdizione|Eresia»", identical(.separatori_con_classi("Giurisdizione!Eresia", vocabolario("reato"), "|"), "Giurisdizione|Eresia"))
v("«Italiano!Latino» → «Italiano; Latino»", identical(.separatori_con_classi("Italiano!Latino", vocabolario("lingua"), "; ", ";"), "Italiano; Latino"))
v("«Modena!Bologna» → «Modena|Bologna»", identical(.separatori_luoghi("Modena!Bologna", c("modena", "bologna")), "Modena|Bologna"))
v("«Rossi, Mario!Bianchi, Luca» → con «|»", identical(.separatori_persone("Rossi, Mario!Bianchi, Luca")$v, "Rossi, Mario|Bianchi, Luca"))
guida <- c("Giurisdizione", "Istruzioni della Congregazione", "Ebraismo", "Islam", "Cristiani e riti orientali", "Protestanti",
           "Bestemmia e miscredenza", "Eresia", "Censura", "Clero", "Crimini sessuali", "Magia", "Dispute dottrinali", "Liturgia", "Sacramenti", "Santità", "Miracoli", "Scienze", "Inosservanza religiosa")
v("il soggettario ha le 19 classi (le 21 della Guida al Tab senza Amministrazione e Tribunali locali, che sono termini sotto Giurisdizione)", setequal(guida, vocabolario("reato")))
v("«Amministrazione» e «Tribunali locali» sono termini sotto Giurisdizione", all(TERMINI_REATO$classe[TERMINI_REATO$termine %in% c("Amministrazione", "Tribunali locali")] == "Giurisdizione") && all(c("Amministrazione", "Tribunali locali") %in% TERMINI_REATO$termine))
v("ogni voce ha il suo inglese", all(guida %in% names(VOCABOLARI_EN$reato)))

# 4. la datazione per secolo della Guida al Tab (22/10, decisione 6): si riconosce, non si converte in anno, esce com'era scritta, niente da decidere
for (x in c("sec. XV", "prima metà del sec. XV", "Seconda metà del sec. XVII", "primo quarto del sec. XIII", "ultimo quarto del sec. XIX", "fine sec. XV", "inizio  sec. XVI"))
  v(sprintf("«%s» è una datazione per secolo", x), .datazione_secolo(x))
for (x in c("sec. XX", "sec. XV-XVI", "circa 1600", "1600", "XV secolo", "metà del sec. XV", "terzo quarto sec. XV", NA, ""))
  v(sprintf("«%s» non lo è", x), !.datazione_secolo(x))
sec <- d; sec[["Estremo remoto"]] <- c("sec. XV", "seconda metà del sec. XVII", "circa 1600"); sec[["Estremo recente"]] <- c(NA, "fine sec. XVII", NA)
fsec <- tempfile(fileext = ".xlsx"); write.xlsx(sec, fsec)
asec <- leggi_arianna(fsec)
prs <- proposte_import(asec, apri_db(":memory:"), NULL)
tipis <- vapply(prs$decisioni, function(x) paste(x$tipo %||% "", x$riga), "")
v("i secoli non chiedono di decidere", !any(c("anno 2", "anno 3") %in% tipis))
v("«circa 1600» resta una decisione", "anno 4" %in% tipis)
v("l'anteprima conta le datazioni per secolo", sum(correzioni_import(asec)$celle[grepl("datazione per secolo", correzioni_import(asec)$regola)]) == 3L)
des <- apri_db(":memory:"); asec$dati <- asec$dati[1:3, , drop = FALSE]
es <- importa_arianna(des, asec, modo = "accoda")
v("i secoli entrano senza anno", es$importati == 3L && all(is.na(dbGetQuery(des, "SELECT estremo_remoto FROM fascicoli ORDER BY fascicolo")$estremo_remoto)))
v("il solo «circa 1600» è fra le date non inserite", identical(es$date_non_inserite$valore, "circa 1600"))
fsec2 <- tempfile(fileext = ".xlsx"); esporta_arianna(des, fsec2, fondo = elenco_fondi(des)$id[1])
xs <- as.data.frame(read_excel(fsec2, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
v("nel foglio i secoli escono com'erano scritti", identical(xs[["Estremo remoto"]], c("sec. XV", "seconda metà del sec. XVII", "circa 1600")) && identical(xs[["Estremo recente"]][2], "fine sec. XVII"))

if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
