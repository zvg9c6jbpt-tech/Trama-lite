# Il fondo di esempio: 24 fascicoli INVENTATI, per provare Trama senza dati
# d'archivio e per pubblicare il codice senza quelli veri. Persone, date e vicende non esistono; i luoghi sono città
# vere, perché la mappa abbia qualcosa da mostrare.
#
# Si costruiscono come li costruirebbe chi scheda (salva_fascicolo: titolo e
# regesto li compone l'app) su un database in memoria, e si esportano nel modello
# a 28 colonne: `dati/esempio/esempio.xlsx` è un foglio come gli altri, e la Home
# lo importa con «Carica il fondo di esempio».
#
#     Rscript dati/esempio/genera_esempio.R
#
# Con TRAMA_ESEMPIO_MINIMO=1 si scrivono solo i primi 6 fascicoli (due buste): è l'esempio che sta nel repository
# pubblico, bastante per provare l'app. I collaudi vogliono i 24 completi (dataset di prova scaricabile a parte).
# TRAMA_ESEMPIO_USCITA cambia il file di uscita.

Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny); library(DBI)
  for (f in c("percorsi", "profilo_dominio", "registro", "schema", "fondi", "compose", "estrazione", "crud", "profili", "i18n", "modulo", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "concordanza", "analisi", "qualita", "attribuzione"))
    source(file.path("R", paste0(f, ".R")))
})
ARCHIVIO <- "Archivio di Esempio"; FONDO <- "Tribunale di esempio"

# busta, fascicolo, tipologia, imputati (estesi), altre persone, reati, luoghi, anno, anno fino a, note
righe <- list(
  list(1,  1, "Processi",     "Lucia Ferrante",                     "Bassi, Tommaso",              "Eresia",                  c("Bologna", "Ferrara"), 1561, 1562, NA),
  list(1,  2, "Processi",     "Tommaso Bassi",                      NULL,                          "Magia",                   "Bologna",               1561, NA,   NA),
  list(1,  3, "Atti",         "Giovanna Moretti",                   "Ferrante, Lucia",             c("Magia", "Bestemmia e miscredenza"), c("Bologna", "Modena"), 1562, NA, NA),
  list(1,  4, "Informazioni", "Pietro Galli",                       NULL,                          "Censura",                 "Venezia",               1562, 1563, "Carte in cattivo stato di conservazione."),
  list(1,  5, "Denunce",      "Agnese Riva",                        "Galli, Pietro",               "Sacramenti",              "Ferrara",               1563, NA,   NA),
  list(1,  6, "Processi",     c("Marco Fontana", "Elena Fontana"),  NULL,                          "Eresia",                  c("Bologna", "Mantova"), 1563, 1565, NA),
  list(1,  7, "Atti",         "don Carlo Benassi",                  NULL,                          "Clero",                   "Parma",                 1564, NA,   NA),
  list(1,  8, "Processi",     "Lucia Ferranti",                     NULL,                          "Eresia",                  "Bologna",               1564, NA,   "Forse la stessa Lucia Ferrante del fascicolo 1: grafia diversa nella fonte."),
  list(2,  9, "Informazioni", "Samuele Levi",                       "Riva, Agnese",                "Ebraismo",                c("Ferrara", "Venezia"), 1565, NA,   NA),
  list(2, 10, "Processi",     "Tommaso Bassi",                      "Moretti, Giovanna",           c("Magia", "Eresia"),      c("Bologna", "Roma"),    1566, 1567, NA),
  list(2, 11, "Atti",         "Caterina Lodi",                      NULL,                          "Santità",                 "Modena",                1566, NA,   NA),
  list(2, 12, "Denunce",      "Antonio Sarti",                      "Lodi, Caterina",              "Bestemmia e miscredenza", "Modena",                1567, NA,   NA),
  list(2, 13, "Atti per diversi", c("Paolo Negri", "Anna Negri", "Luca Vezzani"), NULL,            "Eresia",                  c("Mantova", "Verona"),  1567, 1569, NA),
  list(2, 14, "Processi",     "Hassan da Tunisi",                   NULL,                          "Islam",                   c("Venezia", "Tunisi"),  1568, NA,   NA),
  list(2, 15, "Atti difensivi", "Marco Fontana",                    NULL,                          "Eresia",                  "Bologna",               1568, NA,   NA),
  list(2, 16, "Processi",     "Elisabetta Conti",                   "Sarti, Antonio",              "Crimini sessuali",        "Padova",                1569, NA,   NA),
  list(3, 17, "Informazioni", "Girolamo Ricci",                     NULL,                          "Giurisdizione",           c("Roma", "Bologna"),    1570, 1571, NA),
  list(3, 18, "Processi",     "Tommaso Bassi",                      "Ricci, Girolamo",             "Magia",                   "Firenze",               1571, NA,   "Terzo procedimento a suo carico."),
  list(3, 19, "Atti",         "Margherita Zani",                    NULL,                          "Magia",                   "Ferrara",               1571, NA,   NA),
  list(3, 20, "Denunce",      "Bartolomeo Orsi",                    "Zani, Margherita",            "Censura",                 "Venezia",               1572, NA,   NA),
  # Tre schede incomplete, perché le code di lavoro abbiano qualcosa dentro:
  list(3, 21, "Atti",         "Francesca Pini",                     NULL,                          NULL,                      "Bologna",               1572, NA,   "Il reato non si legge."),
  list(3, 22, "Processi",     "Lorenzo Marchi",                     NULL,                          "Eresia",                  NULL,                    1573, NA,   NA),
  list(3, 23, "Atti",         NULL,                                 NULL,                          "Sacramenti",              "Parma",                 NA,   NA,   "Senza data nella fonte."),
  list(3, 24, "Processi",     "Vittoria Belli",                     c("Marchi, Lorenzo", "Pini, Francesca"), c("Eresia", "Magia"), c("Bologna", "Ginevra"), 1574, 1576, NA))

MINIMO <- identical(Sys.getenv("TRAMA_ESEMPIO_MINIMO"), "1")
if (MINIMO) righe <- righe[1:6]

con <- apri_db(":memory:")
crea_archivio(con, ARCHIVIO, FONDO)
for (r in righe) {
  f <- fascicolo_vuoto()
  f$id <- sprintf("esempio-%02d", r[[2]]); f$archivio <- ARCHIVIO; f$fondo <- FONDO
  f$busta <- as.integer(r[[1]]); f$fascicolo <- as.integer(r[[2]])
  f$soggetto_conservatore <- ARCHIVIO; f$luogo_conservazione <- "Bologna"; f$luogo_produzione <- "Bologna"
  f$produttori <- "Tribunale di esempio"; f$definizione <- "fascicolo"; f$supporto <- "cartaceo"
  f$lingue <- c("Italiano", "Latino"); f$tipologia_documentaria <- r[[3]]
  est <- as.character(unlist(r[[4]]))
  f$imputati <- if (length(est)) data.frame(esteso = est, indicizzato = vapply(est, indicizza_nome, ""), stringsAsFactors = FALSE)
                else data.frame(esteso = character(0), indicizzato = character(0))
  f$persone <- unique(c(f$imputati$indicizzato, as.character(unlist(r[[5]]))))
  f$reati <- as.character(unlist(r[[6]])); f$luoghi <- as.character(unlist(r[[7]]))
  f$estremo_remoto <- if (is.na(r[[8]])) NA_integer_ else as.integer(r[[8]])
  f$estremo_recente <- if (is.na(r[[9]])) NA_integer_ else as.integer(r[[9]])
  f$data_nd <- as.integer(is.na(r[[8]]))
  f$note <- if (is.na(r[[10]])) NA_character_ else r[[10]]
  salva_fascicolo(con, f)
}
uscita <- Sys.getenv("TRAMA_ESEMPIO_USCITA", "dati/esempio/esempio.xlsx")
esporta_arianna(con, uscita, fondo = FONDO)
d <- openxlsx::read.xlsx(uscita, sep.names = " ")
cat(sprintf("%s: %d fascicoli, %d colonne\n", uscita, nrow(d), ncol(d)))
print(d[intersect(c(1, 13, 14, 23), seq_len(nrow(d))), c("Segnatura", "Titolo", "Estremo remoto", "Regesto o descrizione sintetica")], row.names = FALSE, right = FALSE)
dbDisconnect(con)
