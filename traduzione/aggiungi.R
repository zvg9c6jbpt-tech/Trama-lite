# aggiungi.R: scrive nel foglio le traduzioni automatiche per i testi
# nati dopo quella di Stefano, segnandole «da rivedere». Uso interno:
#   Rscript traduzione/aggiungi.R file.csv   (due colonne: italiano;english, separatore TAB)
suppressMessages(library(openxlsx)); f <- "traduzione/testi_interfaccia.xlsx"
t <- read.delim(commandArgs(TRUE)[1], header = FALSE, quote = "", stringsAsFactors = FALSE, encoding = "UTF-8", strip.white = FALSE)
for (foglio in c("Interfaccia", "Campi")) {
  d <- read.xlsx(f, foglio); vuote <- which(is.na(d$english) | !nzchar(d$english))
  i <- vuote[d$italiano[vuote] %in% t$V1]; if (!length(i)) next
  d$english[i] <- t$V2[match(d$italiano[i], t$V1)]
  d$tipo[i] <- paste0(d$tipo[i], " · traduzione automatica, da rivedere")
  wb <- loadWorkbook(f); writeData(wb, foglio, d[, c("italiano", "tipo", "dove", "english")], startRow = 2, colNames = FALSE); saveWorkbook(wb, f, overwrite = TRUE)
  cat(foglio, ": aggiunte", length(i), "\n")
}
d <- read.xlsx(f, "Interfaccia"); r <- d$italiano[is.na(d$english) | !nzchar(d$english)]
cat("ancora senza inglese:", length(r), "\n"); if (length(r)) cat(sprintf("[%s]", r), sep = "\n")
