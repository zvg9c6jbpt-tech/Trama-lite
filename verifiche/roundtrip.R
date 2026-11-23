# ─────────────────────────────────────────────────────────────
# verifiche/roundtrip.R: il test che conta, su database temporanei
#
# Dal 2/10 esiste UN SOLO modello di uscita e di entrata (Stefano: «per tutti
# i tab l'unico excel di uscita e di entrata è questo»): le 28 colonne di
# dati/modello_export.xlsx. Questo collaudo ne verifica quattro cose:
#
#  1. le intestazioni dell'export coincidono, lettera per lettera, con la
#     riga 1 del modello (PROFILO_MODELLO non può staccarsene);
#  2. un foglio nel VECCHIO formato ASDNa (esportASDNa-vecchio-formato.xlsx) si
#     importa e si converte: sulle colonne che il modello ha in comune, ogni
#     cella torna come nel foglio (a meno delle correzioni dichiarate); le
#     colonne che il modello non ha non escono, ma il dato resta nel database;
#  3. il modello fa il giro senza perdere nulla: xlsx -> database -> xlsx e
#     -> CSV, e il CSV reimportato -> xlsx, tutti IDENTICI (0 celle diverse);
#  4. l'aspetto del foglio è quello del modello (intestazione verde, larghezze).
#
# Non apre MAI regestum.sqlite: lavora su file temporanei.
#
# Uso, dalla cartella del progetto:
#     Rscript verifiche/roundtrip.R
#
# Atteso: ESITO: ok. Se qualcosa non torna, esce con stato 1.
# ─────────────────────────────────────────────────────────────

suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))

ORIGINALE <- "esportASDNa-vecchio-formato.xlsx"   # foglio nel vecchio formato ASDNa
MODELLO_XLSX <- "dati/modello_export.xlsx"
ko <- 0L
verifica <- function(cond, msg) if (!isTRUE(cond)) { cat("NON TORNA:", msg, "\n"); ko <<- ko + 1L }

# Tutto testo, intestazioni intatte, come nell'import; vuoto = "".
tabella <- function(percorso) {
  d <- as.matrix(leggi_tabella(percorso))
  d[is.na(d)] <- ""
  d
}

# ── 1. le intestazioni sono quelle del modello ────────────────────
modello <- names(leggi_tabella(MODELLO_XLSX))
modello <- modello[nzchar(modello)]
verifica(identical(PROFILO_MODELLO$arianna, modello), "le intestazioni di PROFILO_MODELLO non coincidono con la riga 1 di dati/modello_export.xlsx")
verifica(length(modello) == 28, "il modello deve avere 28 colonne")
lay <- jsonlite::fromJSON("dati/layout_export.json", simplifyVector = FALSE)
verifica(identical(names(lay$colonne), modello), "dati/layout_export.json non è del modello: rilancia dati/genera_layout_export.R")

# ── 2. un foglio nel vecchio formato ASDNa si converte ────────────
# Colonne del modello che nel vecchio foglio avevano un altro nome.
ALTRO_NOME <- c("Persone citate o indicizzate"   = "Persone citate",
                "Luoghi citati o indicizzati"    = "Luoghi citati",
                "Luogo di produzione (data topica)" = "Luogo di produzione (data topica ove presente)",
                "Soggetti citati o indicizzati"  = "Altro soggetto (citato o indicizzato)")
# Celle diverse dal foglio d'origine sulle colonne in comune, tutte volute
# (normalizzazione, TODO 26, 18/09): definizione.tipologia 272 ("Fascicolo"
# -> "fascicolo", forma del vocabolario); Note libere 35 (31 blocchi
# "(auto)" tolti, "non consultabile" in una forma sola in testa); Persone
# citate 1 ("Gallo,Matteo"); Regesto 1 (spazio doppio); Soggettario 1
# ("Bestemmia  e miscredenza"); Supporto (materiale) 272 (stessa regola di
# definizione.tipologia). Totale 582, come prima del modello unico.
# Dal 3/10: altre 59 celle persone (de posposto) e 272 lingue in minuscolo: 913.
# Estensione a di/del: altre 8 celle persone, totale 921.
# Punto finale del regesto: altre 278 celle, totale 1199.
# Punto finale delle note: altre 55 celle, totale 1254.
# Dal 5/10 anche «della» e «da» vanno in fondo al cognome: altre 4 celle di persone, totale 1258.
# Le colonne del vecchio foglio che il modello non ha.
SOLO_VECCHIO <- c("Personaggi indicizzati", "Soggetto conservatore", "Trascrizione", "Soggetto conservatore precedente (ente)")
# Le colonne del modello che il vecchio foglio non aveva: devono uscire vuote.
SOLO_MODELLO <- c("consistenza.quantità", "consistenza.unitàMisura", "Manifest IIIF", "Identificativo immagine o cartella")

if (file.exists(ORIGINALE)) {
originale <- tabella(ORIGINALE)
a <- leggi_arianna(ORIGINALE)
verifica(a$profilo == "ASDNa", "il foglio d'origine va riconosciuto come vecchio formato ASDNa")
verifica(nrow(a$fuori_modello) >= 1 && "Soggetto conservatore" %in% a$fuori_modello$colonna,
         "l'anteprima deve dire quali colonne il modello non ha (Soggetto conservatore ha dati)")
con <- apri_db(tempfile(fileext = ".sqlite"))
e <- importa_arianna(con, a, modo = "sostituisci")
verifica(e$letti == 279L && e$importati == 279L && nrow(e$respinti) == 0L,
         sprintf("import del vecchio foglio: %d letti, %d importati, %d respinti", e$letti, e$importati, nrow(e$respinti)))
conv <- tempfile(fileext = ".xlsx"); esporta_arianna(con, conv)
m1 <- tabella(conv)
verifica(identical(colnames(m1), modello), "l'export non ha le intestazioni del modello")
verifica(nrow(m1) == nrow(originale), "righe esportate diverse da quelle importate")

diverse <- 0L; dove <- character(0)
for (col in modello) {
  vecchio <- if (col %in% names(ALTRO_NOME)) unname(ALTRO_NOME[col]) else col
  if (col %in% SOLO_MODELLO) { verifica(all(m1[, col] == ""), paste("colonna nuova del modello non vuota:", col)); next }
  if (col == "Regesto o descrizione sintetica") vecchio <- grep("^Regesto", colnames(originale), value = TRUE)[1]
  if (!vecchio %in% colnames(originale)) { verifica(FALSE, paste("colonna senza corrispondente nel vecchio foglio:", col)); next }
  w <- which(m1[, col] != originale[, vecchio])
  diverse <- diverse + length(w); if (length(w)) dove <- c(dove, sprintf("%s %d", col, length(w)))
}
cat(sprintf("Vecchio ASDNa -> modello: %d celle diverse sulle colonne in comune (%s)\n", diverse, paste(dove, collapse = "; ")))
# Dal 5/10 il foglio d'origine è allineato al database («Gallo,Matteo» ora ha lo spazio): una cella in meno, 1257.
# Riga 118: «D'Amato, Cesare» (Stefano, 5/10: «teniamo D'Amato»), cognome con l'apostrofo che non si gira: 1256.
# 14/10: «Inquisizione di Roma» esce «Sant'Uffizio» (guida al Tab, colonna U; apostrofo dritto dal 20/10, D13): altre 15 celle di Soggetto produttore, 1271.
verifica(diverse == 1271L, sprintf("celle diverse sulle colonne in comune: %d (attese 1271)", diverse))
# «Soggetto conservatore» è stato eliminato (12/10): nel foglio ASDNa vecchio non entra più; l'ente conservatore sta in
# «Luogo di conservazione», che il vecchio foglio ha già.
sc <- dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE luogo_conservazione IS NOT NULL AND luogo_conservazione <> ''")$n
verifica(sc == 279L, sprintf("il Luogo di conservazione (ente) resta in tutte le 279 schede (trovate %d)", sc))
N <- 279L
} else {
  # Il foglio ASDNa e' un dato d'archivio vero: sta fuori dal programma. Senza, la parte 2 si salta
  # (serve solo alla conversione dal vecchio formato) e il giro del modello (parti 3 e 4) si fa
  # sul fondo di esempio inventato.
  cat("SALTATA la parte 2 (conversione dal vecchio formato): manca esportASDNa-vecchio-formato.xlsx. Il giro del modello usa il fondo di esempio.\n")
  con <- apri_db(tempfile(fileext = ".sqlite"))
  e <- importa_arianna(con, leggi_arianna(FOGLIO_ESEMPIO), modo = "sostituisci")
  conv <- tempfile(fileext = ".xlsx"); esporta_arianna(con, conv)
  m1 <- tabella(conv); N <- e$importati
  verifica(identical(colnames(m1), modello), "l'export non ha le intestazioni del modello")
}

# ── 3. il modello fa il giro senza perdere nulla ──────────────────
giro <- function(etichetta, sorgente, formato) {
  a   <- leggi_arianna(sorgente)
  verifica(a$profilo == MODELLO && nrow(a$sinonimi) == 0 && length(a$mancanti) == 0 && nrow(a$fuori_modello) == 0,
           sprintf("%s: il foglio del modello va riconosciuto come tale, senza sinonimi né colonne mancanti", etichetta))
  con <- apri_db(tempfile(fileext = ".sqlite")); on.exit(dbDisconnect(con))
  e   <- importa_arianna(con, a, modo = "sostituisci")
  out <- tempfile(fileext = paste0(".", formato))
  if (formato == "xlsx") esporta_arianna(con, out) else esporta_csv(con, out)
  n <- tabella(out)
  uguale <- identical(dim(n), dim(m1)) && identical(colnames(n), colnames(m1))
  celle <- if (uguale) sum(n != m1) else NA_integer_
  list(esito = data.frame(giro = etichetta, letti = e$letti, importati = e$importati,
                          respinti = nrow(e$respinti), celle_diverse = celle, stringsAsFactors = FALSE),
       uscita = out)
}
g1 <- giro("modello xlsx -> database -> xlsx", conv, "xlsx")
g2 <- giro("modello xlsx -> database -> CSV",  conv, "csv")
g3 <- giro("CSV  -> database -> xlsx",        g2$uscita, "xlsx")
esiti <- rbind(g1$esito, g2$esito, g3$esito)
print(esiti, row.names = FALSE)
verifica(all(esiti$letti == N & esiti$importati == N & esiti$respinti == 0L), sprintf("letti/importati/respinti non sono %d/%d/0", N, N))
verifica(all(esiti$celle_diverse %in% 0L), "il modello non fa il giro identico: celle diverse")

# ── 4. l'aspetto è quello del modello ─────────────────────────────
dir_x <- tempfile(); dir.create(dir_x); utils::unzip(conv, exdir = dir_x)
leggi_x <- function(f) paste(readLines(file.path(dir_x, f), warn = FALSE), collapse = "")
stili <- leggi_x("xl/styles.xml"); foglio <- leggi_x("xl/worksheets/sheet1.xml")
verifica(grepl("FF00FF00", stili, fixed = TRUE), "manca il verde dell'intestazione del modello (00FF00)")
cols_x <- regmatches(foglio, gregexpr('<col [^>]*/>', foglio))[[1]]
larghezze <- unlist(lapply(cols_x, function(x) rep(as.numeric(sub('.*width="([0-9.]+)".*', "\\1", x)),
                           as.integer(sub('.*max="([0-9]+)".*', "\\1", x)) - as.integer(sub('.*min="([0-9]+)".*', "\\1", x)) + 1L)))
# Le larghezze si adattano al contenuto (Stefano, 14/10): ci sono per tutte le 28 colonne, entro minimo e massimo, e quella del
# Regesto (testo lungo) è più larga di quella di una colonna di numeri. Una colonna interamente vuota è stretta al massimo (21/10).
verifica(length(larghezze) == 28 && all(larghezze >= 2 & larghezze <= 71), "le larghezze delle colonne non sono adattate al contenuto")
verifica(larghezze[match("Regesto o descrizione sintetica", modello)] > larghezze[match("Estremo remoto", modello)], "il Regesto dovrebbe essere più largo di un anno")
verifica(grepl('<row r="1"[^>]*ht="15.5"', foglio), "l'intestazione deve essere alta 15,5 come nel modello")
verifica(grepl("<b/>", stili, fixed = TRUE), "l'intestazione deve essere in grassetto")

cat(if (ko == 0L) "\nESITO: ok\n" else sprintf("\nESITO: %d NON TORNA\n", ko))
# Da Rscript esce con stato 1; con source() in RStudio si ferma con un
# errore, senza chiudere la sessione.
if (ko) {
  if (interactive()) stop("il round-trip NON TORNA") else quit(status = 1)
}
