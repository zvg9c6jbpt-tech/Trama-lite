# ─────────────────────────────────────────────────────────────
# verifiche/layout_xlsx.R: le colonne extra del modello e la veste del foglio
#
# Il modello cambia la veste, mai i valori (prove su database temporaneo):
#  - le colonne extra del MODELLO (consistenza, Manifest IIIF, Eventi…) fanno il
#    giro xlsx e CSV, e il giro della maschera (salvataggio senza toccare);
#  - le colonne dei vecchi formati che il modello non ha (Trascrizione) restano
#    nel database e NON escono nell'export;
#  - l'aspetto (larghezze, riga fissa, intestazione) si legge dal file scritto;
#  - lo stesso foglio si scrive oltre le 5.301 righe del riferimento.
#     Rscript verifiche/layout_xlsx.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
con <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- 1L
f$extra <- list("consistenza.quantità" = "3", "consistenza.unitàMisura" = "carte",
                "Manifest IIIF" = "https://esempio.org/iiif/manifest.json",
                "Identificativo immagine o cartella" = "ASDNa_b1_f1", Eventi = c("Abiura", "Sentenza"),
                Trascrizione = "Prima riga\nSeconda riga")      # di un vecchio formato: non esce
salva_fascicolo(con, f)

x <- tempfile(fileext = ".xlsx"); esporta_arianna(con, x)
a <- leggi_arianna(x)
stopifnot(identical(a$colonne, PROFILO_MODELLO$arianna), a$profilo == MODELLO)
stopifnot(!"Trascrizione" %in% a$colonne)                       # il modello non la porta

atteso <- f$extra[setdiff(names(f$extra), "Trascrizione")]
c2 <- apri_db(":memory:"); importa_arianna(c2, a)
g <- leggi_fascicolo(c2, elenco_fascicoli(c2)$id[1])
stopifnot(identical(g$extra[sort(names(atteso))], atteso[sort(names(atteso))]), is.null(g$extra$Trascrizione))

csv <- tempfile(fileext = ".csv"); esporta_csv(con, csv)
c3 <- apri_db(":memory:"); importa_arianna(c3, leggi_arianna(csv))
stopifnot(identical(leggi_fascicolo(c3, elenco_fascicoli(c3)$id[1])$extra, g$extra))

# La maschera, riaperta e salvata senza toccare, non cambia gli extra del modello.
p <- profilo_completo("Generale", dbGetQuery(c2, "SELECT DISTINCT campo FROM campi_extra")$campo)
h <- raccogli_modulo(valori_da_fascicolo(g, p), p); h$id <- g$id
salva_fascicolo(c2, h)
stopifnot(identical(leggi_fascicolo(c2, g$id)$extra, g$extra))

# Il Trascrizione di un vecchio formato è rimasto nel database della scheda originale.
stopifnot(identical(leggi_fascicolo(con, elenco_fascicoli(con)$id[1])$extra$Trascrizione, "Prima riga\nSeconda riga"))

# La veste si controlla sul file Excel effettivamente scritto.
dir <- tempfile(); dir.create(dir); unzip(x, exdir = dir)
xml <- paste(readLines(file.path(dir, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
lay <- jsonlite::fromJSON("dati/layout_export.json", simplifyVector = FALSE)
stopifnot(grepl('state="frozen"', xml, fixed = TRUE),                   # intestazione fissa
          grepl('ht="15.5"', xml, fixed = TRUE),                         # altezza del modello
          grepl('<col [^>]*width="', xml))   # le larghezze ci sono (adattate al contenuto dal 14/10)
styles <- paste(readLines(file.path(dir, "xl/styles.xml"), warn = FALSE), collapse = "")
stopifnot(grepl("FF00FF00", styles, fixed = TRUE), grepl('style="thin"', styles, fixed = TRUE), grepl("<b/>", styles, fixed = TRUE))

# Stessa impaginazione oltre le 5.301 righe del riferimento.
scrivi_xlsx_modello(as.data.frame(setNames(replicate(28, rep("Prova", 5302), simplify = FALSE), PROFILO_MODELLO$arianna)),
                    tempfile(fileext = ".xlsx"))
# Larghezza e altezza si adattano al contenuto (Stefano, 21/10/2026): una colonna interamente vuota è stretta al massimo, il testo
# lungo va a capo e la riga cresce quanto serve; una riga di testo breve resta alta come nel modello.
col <- PROFILO_MODELLO$arianna
dd <- as.data.frame(setNames(replicate(length(col), rep("", 3), simplify = FALSE), col), check.names = FALSE)
dd[["Regesto o descrizione sintetica"]] <- c(strrep("parola ", 60), "breve", "uno\ndue\ntre")
dd[["Estremo remoto"]] <- c("1514", "1515", "1516")
fx <- tempfile(fileext = ".xlsx"); scrivi_xlsx_modello(dd, fx)
dx <- tempfile(); dir.create(dx); unzip(fx, exdir = dx)
sx <- paste(readLines(file.path(dx, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
colx <- regmatches(sx, gregexpr('<col [^>]*/>', sx))[[1]]
larg <- numeric(length(col))
for (x in colx) larg[as.integer(sub('.*min="([0-9]+)".*', "\\1", x)):as.integer(sub('.*max="([0-9]+)".*', "\\1", x))] <- as.numeric(sub('.*width="([0-9.]+)".*', "\\1", x))
j_reg <- match("Regesto o descrizione sintetica", col); j_anno <- match("Estremo remoto", col); vuote <- setdiff(seq_along(col), c(j_reg, j_anno))
stopifnot(all(larg[vuote] <= 3), larg[j_anno] >= 8, larg[j_reg] > 30)
alt <- function(i) { r <- regmatches(sx, regexpr(sprintf('<row r="%d"[^>]*>', i), sx)); as.numeric(sub('.* ht="([0-9.]+)".*', "\\1", r)) }
stopifnot(alt(1) == 15.5, alt(2) >= 60, alt(3) == lay$altezza_predefinita, alt(4) == 45)
stopifnot(grepl('wrapText="1"', paste(readLines(file.path(dx, "xl/styles.xml"), warn = FALSE), collapse = "")))
cat("ESITO: ok - 28 colonne del modello, extra del modello e vecchi formati, veste nel file XLSX\n")
