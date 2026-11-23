# ─────────────────────────────────────────────────────────────
# costruisci_dizionario.R: dal foglio tradotto al dizionario dell'app (1/10)
# Legge traduzione/testi_interfaccia.xlsx (Interfaccia + Campi, colonna
# «english») e scrive www/trama-en.json, che usano il traduttore nel
# browser (www/traduci.js) e, per i grafici, R/i18n.R (traduci_testo).
#   esatti  : testo italiano intero -> inglese
#   modelli : testi con %d / %s -> espressione regolare + modello inglese
# Il vocabolario controllato resta in R/i18n.R (VOCABOLARI_EN) e si
# aggiunge agli esatti. Si rilancia a ogni modifica del foglio:
#   Rscript traduzione/costruisci_dizionario.R
# ─────────────────────────────────────────────────────────────
suppressMessages({ library(openxlsx); library(jsonlite) })
suppressMessages(for (f in c("R/compose.R", "R/profili.R", "R/i18n.R")) source(f))

fogli <- do.call(rbind, lapply(c("Interfaccia", "Campi"), function(s) read.xlsx("traduzione/testi_interfaccia.xlsx", s)[, c("italiano", "english")]))
fogli <- fogli[!is.na(fogli$english) & nzchar(fogli$english) & fogli$italiano != fogli$english, ]
voc <- do.call(rbind, lapply(VOCABOLARI_EN, function(v) data.frame(italiano = names(v), english = unname(v), stringsAsFactors = FALSE)))
voc <- voc[voc$italiano != voc$english & !voc$italiano %in% fogli$italiano, ]
tutti <- rbind(fogli, voc); tutti <- tutti[!duplicated(tutti$italiano), ]

segnaposto <- "%[0-9.]*[sd]"
# «fascicol%s spuntat%s»: un %s attaccato a una lettera è una desinenza
# del plurale italiano, non un dato. Nel modello diventa «qualche lettera»
# senza cattura; l'inglese di queste righe si scrive «singolare || plurale»
# e si sceglie dal primo numero (1 = singolare).
# È una desinenza solo se l'inglese ha meno segnaposto dell'italiano:
# in «fascicoli datati%s» il %s è un dato vero (una nota in coda).
desinenza <- "(?<=[a-zà-ù])(?<!%s)(?<!%d)%s"
conta <- function(x) lengths(regmatches(x, gregexpr(segnaposto, x)))
ha_desinenze <- function(it, en) conta(it) != conta(strsplit(paste0(en, " "), " || ", fixed = TRUE)[[1]][1])
senza_desinenze <- function(x) gsub(desinenza, "", x, perl = TRUE)
con_segn <- grepl(segnaposto, tutti$italiano)
ordine <- function(x) vapply(regmatches(x, gregexpr(segnaposto, x)), function(m) paste(sub("%[0-9.]*", "", m), collapse = ""), "")
bordi <- function(it, en) paste0(sub("^(\\s*).*$", "\\1", it), trimws(en), sub("^.*?(\\s*)$", "\\1", it, perl = TRUE))
forme <- function(it, en) { f <- strsplit(en, " || ", fixed = TRUE)[[1]]; if (length(f) == 1) f <- c(f, f); vapply(f[1:2], function(x) bordi(it, x), "") }
diversi <- con_segn & vapply(seq_len(nrow(tutti)), function(i)
  any(ordine(forme(tutti$italiano[i], tutti$english[i])) !=
      ordine(if (ha_desinenze(tutti$italiano[i], tutti$english[i])) senza_desinenze(tutti$italiano[i]) else tutti$italiano[i])), TRUE)
if (any(diversi)) { cat("ATTENZIONE: segnaposto diversi fra italiano e inglese (restano in italiano):\n"); print(tutti[diversi, ]) }

# %d -> un numero, %s -> qualunque testo (il più corto che fa tornare il
# resto), desinenza -> fino a tre lettere, %% -> il segno di percento.
regex_di <- function(x, des = TRUE) {
  if (des) x <- gsub(desinenza, "\u0001", x, perl = TRUE)
  pezzi <- strsplit(paste0(x, "\u0002"), segnaposto, perl = TRUE)[[1]]; pezzi[length(pezzi)] <- sub("\u0002$", "", pezzi[length(pezzi)])
  segni <- regmatches(x, gregexpr(segnaposto, x))[[1]]
  esc <- gsub("([\\\\^$.|?*+()\\[\\]{}])", "\\\\\\1", pezzi, perl = TRUE)
  esc <- gsub("%%", "%", gsub("\u0001", "[a-z\u00e0-\u00f9]{0,3}", esc, fixed = TRUE), fixed = TRUE)
  gruppi <- ifelse(grepl("d$", segni), "(\\d[\\d.,]*)", "([\\s\\S]*?)")
  out <- esc[1]; for (i in seq_along(segni)) out <- paste0(out, gruppi[i], if (i + 1 <= length(esc)) esc[i + 1] else "")
  paste0("^", out, "$")
}
modello_en <- function(x) { x <- gsub("%%", "%", x, fixed = TRUE); i <- 0
  while (grepl(segnaposto, x)) { i <- i + 1; x <- sub(segnaposto, paste0("$", i), x) }; x }
m <- tutti[con_segn & !diversi, ]
modelli <- lapply(seq_len(nrow(m)), function(i) { f <- forme(m$italiano[i], m$english[i])
  list(re = regex_di(m$italiano[i], ha_desinenze(m$italiano[i], m$english[i])), en = modello_en(f[[2]]), en1 = if (f[[1]] != f[[2]]) modello_en(f[[1]])) })
modelli <- lapply(modelli, function(x) x[!vapply(x, is.null, TRUE)])
# I modelli più specifici (più testo fisso) prima: «%d fascicoli datati · %d–%d» prima di «%d fascicoli datati%s».
modelli <- modelli[order(-nchar(gsub("\\([^)]*\\)|\\[[^]]*\\](\\{0,3\\})?", "", vapply(modelli, function(x) x$re, ""))))]

# Il riassunto dell'import (riassunto_import() in R/arianna.R) si compone a pezzi: «24 letti · 0 importati · 20 già presenti · 4 respinti».
# Nel foglio non può stare come riga (la scansione del codice non lo vede intero), quindi i tre modelli con la coda sono qui, nel codice
# (26/10/2026: prima erano scritti a mano nel JSON e una ricostruzione li ha cancellati).
n <- "(\\d[\\d.,]*)"
modelli <- c(list(
  list(re = paste0("^", n, " letti \u00b7 ", n, " importati \u00b7 ", n, " gi\u00e0 presenti \u00b7 ", n, " respinti([\\s\\S]*?)$"),
       en = "$1 read \u00b7 $2 imported \u00b7 $3 already present \u00b7 $4 rejected$5"),
  list(re = paste0("^", n, " letti \u00b7 ", n, " importati \u00b7 ", n, " gi\u00e0 presenti( \u00b7 [\\s\\S]*?)$"),
       en = "$1 read \u00b7 $2 imported \u00b7 $3 already present$4"),
  list(re = paste0("^", n, " letti \u00b7 ", n, " importati \u00b7 ", n, " respinti([\\s\\S]*?)$"),
       en = "$1 read \u00b7 $2 imported \u00b7 $3 rejected$4")), modelli)

# I titoli composti: «Tipo connettore Nomi» e «Tipo per diversi», dal foglio
# Titoli. Valgono anche nelle celle delle tabelle (sono modelli interi);
# «e» fra i nomi diventa «and», i nomi restano.
tt <- tryCatch(read.xlsx("traduzione/testi_interfaccia.xlsx", "Titoli"), error = function(e) NULL)
titoli_esatti <- NULL
if (!is.null(tt)) {
  rx <- function(x) gsub("([\\\\^$.|?*+()\\[\\]{}])", "\\\\\\1", x, perl = TRUE)
  tipi <- tt[tt$parte == "tipo", ]; conn <- tt[tt$parte == "connettore", ]
  coda <- tt[tt$parte == "coda", ]; fra <- tt$english[tt$parte == "fra i nomi"][1]
  mt <- unlist(lapply(seq_len(nrow(tipi)), function(i) lapply(seq_len(nrow(conn)), function(j)
    list(re = paste0("^", rx(tipi$italiano[i]), " ", rx(conn$italiano[j]), " ([\\s\\S]+)$"),
         en = paste0(tipi$english[i], " ", conn$english[j], " $1"), e = paste0(" ", fra, " ")))), recursive = FALSE)
  # i tipi più lunghi prima: «Atti del processo contro» prima di «Atti …»
  mt <- mt[order(-nchar(vapply(mt, function(x) x$re, "")))]
  modelli <- c(mt, modelli)
  titoli_esatti <- do.call(rbind, lapply(seq_len(nrow(coda)), function(k) data.frame(
    italiano = paste(tipi$italiano, coda$italiano[k]), english = paste(tipi$english, coda$english[k]), stringsAsFactors = FALSE)))
  # Senza reato il regesto composto ha la forma del titolo («Informazioni circa
  # ignoti»): il modello dei titoli lascerebbe «ignoti» com'è, quindi le due
  # code fisse si scrivono per intero, tipo per tipo.
  rgs <- tt[tt$parte == "regesto" & tt$italiano %in% c("ignoti", "diversi soggetti"), ]
  if (nrow(rgs)) {
    tp <- rbind(tipi[, c("italiano", "english")], tt[tt$parte == "regesto" & tt$italiano == "Processi", c("italiano", "english")])
    g <- expand.grid(t = seq_len(nrow(tp)), c = seq_len(nrow(conn)), r = seq_len(nrow(rgs)))
    titoli_esatti <- rbind(titoli_esatti, data.frame(stringsAsFactors = FALSE,
      italiano = paste(tp$italiano[g$t], conn$italiano[g$c], rgs$italiano[g$r]),
      english  = paste(tp$english[g$t], conn$english[g$c], rgs$english[g$r])))
  }
  tutti <- rbind(tutti, titoli_esatti[!titoli_esatti$italiano %in% tutti$italiano, ]); con_segn <- c(con_segn, rep(FALSE, nrow(tutti) - length(con_segn)))
}

e <- tutti[!con_segn, ]
diz <- list(esatti = as.list(stats::setNames(e$english, e$italiano)), modelli = modelli)
# Il regesto composto narrativo (compose.R: «Tipo per reato; reato contro|circa
# Nomi»): i pezzi per tradurlo a schermo, parola per parola nota. I reati sono
# quelli del foglio Vocabolario, in minuscolo come li scrive il regesto.
if (!is.null(tt) && any(tt$parte == "regesto")) {
  voc <- read.xlsx("traduzione/testi_interfaccia.xlsx", "Vocabolario"); voc <- voc[voc$campo == "reato", ]
  rg <- tt[tt$parte == "regesto", ]; pezzo <- function(x) rg$english[rg$italiano == x][1]
  tipi_r <- rbind(tt[tt$parte == "tipo", c("italiano", "english")], rg[rg$italiano == "Processi", c("italiano", "english")])
  diz$regesto <- list(
    tipi = as.list(stats::setNames(tipi_r$english, tipi_r$italiano)),
    # (i nomi propri restano maiuscoli anche in mezzo alla frase)
    reati = as.list(stats::setNames(ifelse(voc$english %in% c("Judaism", "Islam"), voc$english, tolower(voc$english)), tolower(voc$italiano))),
    connettori = as.list(stats::setNames(tt$english[tt$parte == "connettore"], tt$italiano[tt$parte == "connettore"])),
    per = pezzo("per"), ignoti = pezzo("ignoti"), diversi = pezzo("diversi soggetti"),
    e = tt$english[tt$parte == "fra i nomi"][1],
    per_diversi = paste(tt$english[tt$parte == "tipo" & tt$italiano == "Atti"], tt$english[tt$parte == "coda"][1]))
}
writeLines(toJSON(diz, auto_unbox = TRUE, pretty = FALSE), "www/trama-en.json", useBytes = TRUE)
cat(sprintf("www/trama-en.json: %d testi esatti, %d modelli con segnaposto\n", nrow(e), length(modelli)))
