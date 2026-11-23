# Ogni colonna del modello si può compilare dalla maschera (Stefano, 5/10: «l'excel
# modello per TUTTI gli archivi dovrebbe essere compilabile in tutti i suoi campi
# attraverso Trama»). Si riempie ogni campo della maschera come farebbe chi scheda
# (valori di input -> raccogli_modulo -> salva_fascicolo, su un database in
# memoria), si esporta, e nessuna delle 28 colonne del foglio deve restare vuota.
# Poi il giro inverso: ciò che si è scritto si ritrova riaprendo la scheda.
#
#     Rscript verifiche/modello_compilabile.R

Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny); library(bslib); library(DBI)
  for (f in c("percorsi", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo", "qualita", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "attribuzione"))
    source(file.path("R", paste0(f, ".R")))
})
falliti <- character(0)
controlla <- function(nome, ok) { cat(if (isTRUE(ok)) "ok        " else "NON TORNA ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

con <- apri_db(":memory:")
p <- profilo_completo("Generale")
v <- valori_da_fascicolo(fascicolo_vuoto(con_default = FALSE), p)

compilati <- list(
  archivio = "Archivio di Prova", fondo = "Fondo di prova", serie = "Processi", busta = 3L, fascicolo = 12L,
  altra_segnatura = "vecchia segn. 45",
  titolo = "Processo contro Mario Rossi", titolo_manuale = 1L, estremo_remoto = 1571L, estremo_recente = 1573L,
  definizione = "fascicolo", supporto = "cartaceo", tipologia_documentaria = "Processi",
  regesto = "Processo per eresia contro Mario Rossi.", regesto_manuale = 1L,
  persona = c("Rossi, Mario", "Bianchi, Anna"), imputati = "Mario Rossi",   # gli imputati in forma estesa, come li scrive la maschera
  luogo_conservazione = "Città di Prova", luogo_produzione = "Città di Prova", luogo = c("Napoli", "Roma"),
  soggetto = "Tribunale di prova", reato = c("Eresia", "Magia"), note = "Nota di prova.", lingua = c("Italiano", "Latino"),
  x_dimensioni_altezza = "310", x_dimensioni_larghezza = "220", x_tipo_di_numerazione = "cartulazione",
  x_numerazione = "1-48", x_epoca_numerazione = "coeva", x_consistenza_quantit = "48", x_consistenza_unit_misura = "carte",
  x_soggetti_citati_o_indicizzati = "Confraternita di prova", x_eventi = "Visita pastorale del 1572",
  x_manifest_iiif = "https://esempio.invalid/iiif/manifest.json", x_identificativo_immagine_o_cartella = "b003_f012",
  x_tipologia_documentaria_nel_foglio = "Registri")   # la colonna «Tipologia documentaria»: «Atti» se non la si cambia qui
mancano <- setdiff(names(compilati), names(v))
controlla(paste("ogni valore di prova ha un campo nella maschera", if (length(mancano)) paste0("(mancano: ", paste(mancano, collapse = ", "), ")") else ""), !length(mancano))
for (k in names(compilati)) v[[k]] <- compilati[[k]]

f <- raccogli_modulo(v, p)
if (.vuoto(f$id)) f$id <- "prova-1"
salva_fascicolo(con, f)
tab <- tabella_arianna(con, fondo = "Fondo di prova")
modello <- names(openxlsx::read.xlsx("dati/modello_export.xlsx", sep.names = " "))
controlla("l'export ha le 28 colonne del modello, nello stesso ordine", identical(trimws(names(tab)), trimws(modello)) && ncol(tab) == 28L)
vuote <- names(tab)[vapply(tab, function(x) is.na(x[1]) || !nzchar(trimws(as.character(x[1]))), logical(1))]
controlla(paste("nessuna colonna resta vuota", if (length(vuote)) paste0("(vuote: ", paste(vuote, collapse = " | "), ")") else ""), !length(vuote))
controlla("«Tipologia documentaria» è quella scritta a mano nel suo campo", identical(as.character(tab[["Tipologia documentaria"]][1]), "Registri"))
cat("\n"); for (k in names(tab)) cat(sprintf("  %-38s %s\n", trimws(k), substr(as.character(tab[[k]][1]), 1, 60)))

# Il giro inverso: riaprendo la scheda, la maschera mostra ciò che si è scritto.
w <- valori_da_fascicolo(leggi_fascicolo(con, f$id), p)
diversi <- names(compilati)[!vapply(names(compilati), function(k)
  identical(sort(as.character(w[[k]])), sort(as.character(compilati[[k]]))), logical(1))]
controlla(paste("riaprendo la scheda ogni campo ha il valore scritto", if (length(diversi)) paste0("(diversi: ", paste(diversi, collapse = ", "), ")") else ""), !length(diversi))

# E per file: xlsx e CSV scritti e riletti hanno le stesse 28 celle.
x <- tempfile(fileext = ".xlsx"); esporta_arianna(con, x, fondo = "Fondo di prova")
letto <- openxlsx::read.xlsx(x, sep.names = " ")
controlla("il file xlsx scritto ha 28 colonne piene", ncol(letto) == 28L && !any(is.na(letto[1, ])))
dbDisconnect(con)

cat("\n")
if (length(falliti)) { cat("ESITO: NON TORNA\n"); if (!interactive()) quit(status = 1) else stop("verifiche fallite") }
cat("ESITO: ok\n")
