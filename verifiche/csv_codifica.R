# ─────────────────────────────────────────────────────────────
# verifiche/csv_codifica.R: un CSV si legge per intero o si rifiuta (19/11/2026)
#
# Copre: UTF-8 (con \n, con \r\n, con BOM) e Windows-1252 (il «CSV» di Excel su Windows) danno tutte le righe, accenti compresi,
# anche con l'accento a riga 400 (prima R si fermava al byte non valido e restituiva 400 righe su 480 senza dire niente);
# una virgoletta aperta e mai chiusa è un rifiuto con il motivo, non un import a metà; un foglio con colonne sue (come un estratto grezzo)
# dice che ha colonne sue, non che manca una riga di intestazione; un foglio con più fondi in «Accoda» crea i fondi e il foglio lo dichiara
# (archivi_nel_foglio li elenca).
#
#     Rscript verifiche/csv_codifica.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1", TRAMA_COLLAUDO = "1")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
suppressMessages(library(readxl))
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else { .esiti$falliti <- c(.esiti$falliti, nome); cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto) }
}

x <- as.data.frame(read_excel("dati/esempio/esempio.xlsx", col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
grande <- x[rep(seq_len(nrow(x)), length.out = 480), ]; rownames(grande) <- NULL
grande[[1]][400] <- paste0(grande[[1]][400], " perché città")
lavoro <- tempfile("csv-"); dir.create(lavoro)
scrivi <- function(df, nome, enc = "UTF-8", eol = "\n", bom = FALSE) {
  p <- file.path(lavoro, nome); tmp <- tempfile()
  write.table(df, tmp, sep = ";", dec = ",", row.names = FALSE, na = "", fileEncoding = enc, eol = eol, qmethod = "double")
  b <- readBin(tmp, "raw", file.size(tmp)); if (bom) b <- c(as.raw(c(0xef, 0xbb, 0xbf)), b)
  writeBin(b, p); p
}
for (caso in list(list("utf8", "UTF-8", "\n", FALSE), list("utf8_crlf", "UTF-8", "\r\n", FALSE),
                  list("utf8_bom", "UTF-8", "\n", TRUE), list("windows1252", "CP1252", "\r\n", FALSE))) {
  r <- leggi_tabella(scrivi(grande, paste0(caso[[1]], ".csv"), caso[[2]], caso[[3]], caso[[4]]))
  atteso(paste(caso[[1]], "righe"), nrow(r), 480L)
  atteso(paste(caso[[1]], "accento a riga 400"), grepl("perché città", r[[1]][400], fixed = TRUE), TRUE)
}
atteso("codifica dichiarata", attr(leggi_tabella(file.path(lavoro, "windows1252.csv")), "codifica"), "Windows-1252")

mi <- file.path(lavoro, "misto.csv"); lw <- readBin(file.path(lavoro, "windows1252.csv"), "raw", 1e7)
writeBin(c(lw, charToRaw("x;niccolò città\n")), mi)
e <- tryCatch(leggi_tabella(mi), error = function(e) conditionMessage(e))
atteso("codifiche mescolate: rifiuto", is.character(e) && grepl("mescola", e), TRUE)

p <- file.path(lavoro, "aperta.csv"); l <- readLines(file.path(lavoro, "utf8.csv"), encoding = "UTF-8"); l[100] <- sub(";", ';"', l[100]); writeLines(l, p, useBytes = TRUE)
e <- tryCatch(leggi_tabella(p), error = function(e) conditionMessage(e))
atteso("virgoletta aperta: rifiuto", is.character(e) && grepl("per intero", e), TRUE)

grezzo <- file.path(lavoro, "grezzo.csv")
writeLines(c("segnatura;cartella;citta;data;descrizione;consistenza;stato;pagina", "S.O./001;I;Capua;1552;Atto;ff.10;buono;1"), grezzo)
m <- tryCatch(leggi_arianna(grezzo), error = function(e) conditionMessage(e))
atteso("estratto grezzo: dice che ha colonne sue", is.character(m) && grepl("colonne sue", m, fixed = TRUE), TRUE)
atteso("estratto grezzo: la spiegazione delle colonne sue viene prima del consiglio sulla riga", is.character(m) && regexpr("colonne sue", m, fixed = TRUE) < regexpr("PRIMA riga", m, fixed = TRUE), TRUE)

meta <- seq_len(nrow(x)) > nrow(x) / 2
y <- x; y$Segnatura[meta] <- sub("^([^,]+), [^,]+,", "\\1, Fondo Secondo,", y$Segnatura[meta])
f <- scrivi(y, "due_fondi.csv"); a <- leggi_arianna(f)
atteso("due fondi nel foglio", nrow(archivi_nel_foglio(a)), 2L)
db <- file.path(lavoro, "db"); dir.create(db); con <- apri_db(file.path(db, "t.sqlite")); on.exit(DBI::dbDisconnect(con), add = TRUE)
invisible(importa_arianna(con, a, "accoda"))
atteso("accoda: due fondi creati", DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM fondi")$n, 2L)

# L'export non si consegna se non si rilegge uguale: integro passa, troncato o senza una colonna no.
fid <- DBI::dbGetQuery(con, "SELECT fondo_id FROM fascicoli LIMIT 1")$fondo_id
ex <- file.path(lavoro, "export.xlsx"); esporta_arianna(con, ex, fondo = fid)
ec <- file.path(lavoro, "export.csv");  esporta_csv(con, ec, fondo = fid)
atteso("export xlsx integro: verificato", isTRUE(verifica_export(con, ex, fid)), TRUE)
atteso("export csv integro: verificato", isTRUE(verifica_export(con, ec, fid)), TRUE)
tr <- file.path(lavoro, "troncato.csv"); writeLines(readLines(ec, warn = FALSE, encoding = "UTF-8")[1:5], tr, useBytes = TRUE)
atteso("export csv troncato: rifiutato", inherits(try(verifica_export(con, tr, fid), silent = TRUE), "try-error"), TRUE)
sc <- file.path(lavoro, "senza_colonna.xlsx"); openxlsx::write.xlsx(as.data.frame(readxl::read_excel(ex, col_types = "text", .name_repair = "minimal"))[, -3], sc)
atteso("export senza una colonna: rifiutato", inherits(try(verifica_export(con, sc, fid), silent = TRUE), "try-error"), TRUE)

cat(sprintf("csv_codifica: %d controlli riusciti, %d falliti\n", .esiti$ok, length(.esiti$falliti)))
if (length(.esiti$falliti)) quit(status = 1)
