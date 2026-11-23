# ─────────────────────────────────────────────────────────────
# verifiche/colonne.R: i metadati colonna per colonna (7/10, R/colonne.R)
# Pronto per l'export (le colonne standard di Napoli), vocabolario per fondo,
# varianti che si ricordano, correzione di un valore in tutte le schede, import
# che normalizza i valori controllati, backup JSON del vocabolario.
# Database in memoria o temporanei: non tocca regestum.sqlite.
#
#     Rscript verifiche/colonne.R
# ─────────────────────────────────────────────────────────────
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny)
  for (f in c("percorsi", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo", "analisi", "qualita",
              "attribuzione", "ricerca", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "colonne", "lista_luoghi", "luoghi", "fusioni", "luoghi_citati", "elenco",
              "backup_electron", "backup"))
    source(file.path("R", paste0(f, ".R")))
})
.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else {
    .esiti$falliti <- c(.esiti$falliti, nome)
    cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto)
  }
}

# --- le colonne standard sono davvero colonne del modello ----------
atteso("standard: tutte nel modello", all(.chiave(COLONNE_STANDARD) %in% .chiave(PROFILO_MODELLO$arianna)), TRUE)
atteso("standard: sono quattordici", length(COLONNE_STANDARD), 14L)

# --- classificare i valori -----------------------------------------
base <- vocabolario("reato")
cl <- classifica_valori(c("Magia", "Crimini Sessuali", "Contrabando", "Contrabbando", "Pirateria", "Tortura"),
                        base, ammessi = "Contrabbando", alias = c("Pirateria" = "Giurisdizione"))
atteso("stato: nel vocabolario", cl$stato[1], "ok")
atteso("stato: maiuscole diverse", c(cl$stato[2], cl$proposta[2]), c("maiuscole", "Crimini sessuali"))
atteso("stato: refuso vicino a un ammesso", c(cl$stato[3], cl$proposta[3]), c("vicino", "Contrabbando"))
atteso("stato: ammesso per il fondo", cl$stato[4], "ammesso")
atteso("stato: variante registrata", c(cl$stato[5], cl$proposta[5]), c("variante", "Giurisdizione"))
atteso("stato: valore nuovo", c(cl$stato[6], cl$proposta[6]), c("nuovo", NA))

# --- un fondo con i suoi valori --------------------------------------
con <- apri_db(":memory:")
nuova <- function(fondo, busta, num, reati, lingue = "Italiano", ...) {
  f <- fascicolo_vuoto(); f$fondo <- fondo; f$busta <- busta; f$fascicolo <- num
  f$archivio <- "Archivio X"; f$tipologia_documentaria <- "Processi"; f$persone <- "Rossi, Mario"
  f$reati <- reati; f$lingue <- lingue; f$estremo_remoto <- 1600L
  extra <- list(...); for (k in names(extra)) f[[k]] <- extra[[k]]
  salva_fascicolo(con, f)
}
i1 <- nuova("Bo", 1L, 1L, "Magia")
i2 <- nuova("Bo", 1L, 2L, c("Crimini Sessuali", "Magia"))
i3 <- nuova("Bo", 1L, 3L, "Contrabando")
i4 <- nuova("Bo", 1L, 4L, "Contrabbando")
i5 <- nuova("Na", 1L, 1L, "Contrabbando")
v <- valori_colonna(con, "Bo", "reato")
atteso("colonna: i da decidere stanno in cima", head(v$stato, 3), c("nuovo", "nuovo", "maiuscole"))
atteso("colonna: i conteggi sono di schede", v$schede[v$valore == "Magia"], 2L)
atteso("colonna: da decidere nel fondo", valori_da_decidere(con, "Bo")$valori[valori_da_decidere(con, "Bo")$campo == "reato"], 3L)
atteso("qualità: i reati nuovi sono fuori vocabolario",
       qualita(con, "Bo")$casi[qualita(con, "Bo")$controllo == "Reati fuori vocabolario"], 3L)

# ammettere «Contrabbando» per Bo: per Na resta fuori vocabolario
registra_valore(con, "Bo", "reato", "Contrabbando")
atteso("ammesso: il vocabolario del fondo lo include", "Contrabbando" %in% vocabolario_di(con, "reato", "Bo"), TRUE)
atteso("ammesso: gli altri fondi no", "Contrabbando" %in% vocabolario_di(con, "reato", "Na"), FALSE)
atteso("ammesso: Qualità non lo segnala più a Bo",
       qualita(con, "Bo")$casi[qualita(con, "Bo")$controllo == "Reati fuori vocabolario"], 2L)
atteso("ammesso: a Na sì",
       qualita(con, "Na")$casi[qualita(con, "Na")$controllo == "Reati fuori vocabolario"], 1L)
v <- valori_colonna(con, "Bo", "reato")
atteso("refuso: ora è vicino all'ammesso", c(v$stato[v$valore == "Contrabando"], v$proposta[v$valore == "Contrabando"]), c("vicino", "Contrabbando"))

# --- correggere in tutte le schede ------------------------------------
a <- sostituisci_valore(con, "Bo", "reato", "Contrabando", "Contrabbando", prova = TRUE)
atteso("anteprima: una scheda cambia", nrow(a), 1L)
atteso("anteprima: non scrive", leggi_fascicolo(con, i3)$reati, "Contrabando")
invisible(sostituisci_valore(con, "Bo", "reato", "Contrabando", "Contrabbando"))
atteso("correzione: il reato è quello giusto", leggi_fascicolo(con, i3)$reati, "Contrabbando")
atteso("correzione: il regesto composto si ricompone",
       grepl("contrabbando", leggi_fascicolo(con, i3)$regesto) && !grepl("amminisrazione", leggi_fascicolo(con, i3)$regesto), TRUE)
invisible(sostituisci_valore(con, "Bo", "reato", "Crimini Sessuali", "Crimini sessuali"))
atteso("correzione: in una scheda con due reati l'altro resta", sort(leggi_fascicolo(con, i2)$reati), c("Crimini sessuali", "Magia"))
atteso("correzione: l'altro fondo non si tocca", leggi_fascicolo(con, i5)$reati, "Contrabbando")
atteso("correzione: due valori che diventano uno non si ripetono", {
  j <- nuova("Bo", 2L, 1L, c("Magia", "magia"))
  invisible(sostituisci_valore(con, "Bo", "reato", "magia", "Magia")); leggi_fascicolo(con, j)$reati }, "Magia")

# --- una variante registrata si applica all'import --------------------
registra_valore(con, "Bo", "reato", "Pirateria", "Giurisdizione")
f <- fascicolo_vuoto(); f$fondo <- "Bo"; f$reati <- c("Pirateria", "contrabbando", "Magia", "Eresia*")
atteso("import: variante, maiuscole, forma del vocabolario",
       normalizza_valori_fascicolo(con, f)$reati, c("Giurisdizione", "Contrabbando", "Magia", "Eresia*"))
f$fondo <- "Na"
atteso("import: un altro fondo non vede le regole di Bo", normalizza_valori_fascicolo(con, f)$reati,
       c("Pirateria", "contrabbando", "Magia", "Eresia*"))
atteso("regole: si tolgono", {togli_regola(con, "Bo", "reato", "Pirateria"); sum(!(regole_fondo(con, "Bo")$valore %in% .equivalenze_come_regole()$valore))}, 1L)   # le equivalenze di tutti i fondi (22/10) non sono regole del fondo
atteso("regole: lo stesso valore due volte si aggiorna", {
  registra_valore(con, "Bo", "reato", "Pirateria", "Clero"); registra_valore(con, "Bo", "reato", "Pirateria", "Giurisdizione")
  regole_fondo(con, "Bo")$forma[regole_fondo(con, "Bo")$valore == "Pirateria"] }, "Giurisdizione")
atteso("regole: forma uguale al valore rifiutata", inherits(try(registra_valore(con, "Bo", "reato", "X", "X"), silent = TRUE), "try-error"), TRUE)

# --- «non è un valore di questa colonna»: la decisione (elimina) (Stefano, 8/10, «Modena» fra i reati di ASMo) ---
registra_valore(con, "Bo", "reato", "Modena", FORMA_ELIMINA)
f <- fascicolo_vuoto(); f$fondo <- "Bo"; f$reati <- c("Magia", "Modena", "Contrabbando")
atteso("import: un valore da eliminare si toglie, gli altri restano", normalizza_valori_fascicolo(con, f)$reati, c("Magia", "Contrabbando"))
f$reati <- "Modena"
atteso("import: se era l'unico, la scheda resta senza reati", length(normalizza_valori_fascicolo(con, f)$reati), 0L)
nuova("Bo", 7L, 1L, c("Magia", "Modena"))
i7 <- dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo = 'Bo' AND busta = 7")$id
atteso("classifica: «Modena» è una variante, e la forma dice (elimina)", {v <- valori_colonna(con, "Bo", "reato"); c(v$stato[v$valore == "Modena"], v$proposta[v$valore == "Modena"])}, c("variante", "(elimina)"))
atteso("correzione in blocco con (elimina): toglie il valore dalla scheda", {
  invisible(sostituisci_valore(con, "Bo", "reato", "Modena", FORMA_ELIMINA)); sort(leggi_fascicolo(con, i7)$reati) }, "Magia")
atteso("(elimina) non entra mai nel vocabolario ammesso", FORMA_ELIMINA %in% vocabolario_di(con, "reato", "Bo"), FALSE)

# --- le lingue e i campi semplici --------------------------------------
nuova("Bo", 3L, 1L, "Magia", lingue = c("Italiano", "Tedesco", "Slavo"))
atteso("lingua: «Tedesco» è da decidere", valori_colonna(con, "Bo", "lingua")$stato[valori_colonna(con, "Bo", "lingua")$valore == "Tedesco"], "nuovo")
atteso("lingua: «Slavo» e «Francese» sono nel vocabolario (22/10)", valori_colonna(con, "Bo", "lingua")$stato[valori_colonna(con, "Bo", "lingua")$valore == "Slavo"], "ok")
sostituisci_valore(con, "Bo", "tipologia_documentaria", "Processi", "Atti", prova = TRUE) -> t
atteso("tipo del titolo: la correzione in blocco vale anche per i campi semplici", nrow(t) > 0, TRUE)

# --- supporto e lingua della Guida al Tab (22/10) -------------------------
atteso("supporto: i quattro della Guida, «cartaceo» per primo", vocabolario("supporto"), c("cartaceo", "papiraceo", "pergamenaceo", "membranaceo"))
atteso("supporto: «pergamenaceo» non è più da decidere", classifica_valori(c("pergamenaceo", "carta"), vocabolario("supporto"))$stato, c("ok", "nuovo"))
atteso("lingua: «Francese» e «Slavo» nel vocabolario", classifica_valori(c("Francese", "Slavo", "Tedesco"), vocabolario("lingua"))$stato, c("ok", "ok", "nuovo"))

# --- pronto per l'export ---------------------------------------------------
r <- riempimento_colonne(con, "Na")
atteso("riempimento: 28 colonne", nrow(r), 28L)
atteso("riempimento: 14 standard", sum(r$standard), 14L)
atteso("riempimento: la segnatura c'è in tutte", r$mancanti[r$colonna == "Segnatura"], 0L)
atteso("riempimento: le schede senza una colonna si ritrovano",
       length(attr(r, "ids_mancanti")[["Soggettario"]]) + sum(r$compilate[r$colonna == "Soggettario"]), 1L)
s <- pronto_export(r)
atteso("pronto: una scheda con titolo e regesto da comporre ha colonne standard vuote", s$pronto, FALSE)
# Senza cognome la colonna delle persone resta vuota per regola: non toglie il «pronto».
fp <- fascicolo_vuoto(); fp$fondo <- "Pe"; fp$busta <- 1L; fp$fascicolo <- 1L; fp$persone <- character(0)
salva_fascicolo(con, fp)
atteso("per regola: una scheda senza persone non rende incompleta la colonna",
       "Persone citate o indicizzate" %in% pronto_export(riempimento_colonne(con, "Pe"))$colonne_incomplete, FALSE)
atteso("per regola: la colonna è segnata e le schede vuote si contano ancora",
       with(riempimento_colonne(con, "Pe"), c(per_regola[colonna == "Persone citate o indicizzate"], mancanti[colonna == "Persone citate o indicizzate"] == 1L)),
       c(TRUE, TRUE))
accetta_colonna(con, "Na", "Soggettario")

r2 <- riempimento_colonne(con, "Na"); s2 <- pronto_export(r2)
atteso("accettata: una colonna vuota per scelta conta come completa", s2$complete, s$complete + sum(s$colonne_incomplete %in% "Soggettario"))
atteso("accettata: vale solo per il fondo", any(riempimento_colonne(con, "Bo")$accettata), FALSE)
accetta_colonna(con, "Na", "Soggettario", FALSE)
atteso("accettata: si può togliere", "Soggettario" %in% colonne_accettate(con, "Na"), FALSE)

# --- l'anteprima dell'import dice che cosa riscrive e che cosa resta da decidere -----
file <- tempfile(fileext = ".xlsx"); esporta_arianna(con, file, fondo = "Bo")
wb <- openxlsx::loadWorkbook(file); fogli <- openxlsx::read.xlsx(file)
col <- which(.chiave(names(fogli)) == .chiave("Soggettario"))
openxlsx::writeData(wb, 1, "Crimini Sessuali|Pippo|Contrabando", startCol = col, startRow = 2)
openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
k <- correzioni_import(leggi_arianna(file), con)
atteso("anteprima: le maiuscole si riscrivono", any(grepl("«Crimini Sessuali» diventa «Crimini sessuali»", k$regola)), TRUE)
atteso("anteprima: il refuso si propone", any(grepl("«Contrabando» forse un refuso di «Contrabbando»", k$regola)), TRUE)
atteso("anteprima: il valore nuovo si dice", any(grepl("«Pippo» fuori vocabolario", k$regola)), TRUE)
atteso("anteprima: senza database non c'è questo controllo", any(grepl("fuori vocabolario", correzioni_import(leggi_arianna(file))$regola)), FALSE)

# --- l'import vero applica le regole: maiuscole sì, refuso e valori nuovi no -----------
c3 <- apri_db(":memory:"); invisible(importa_arianna(c3, leggi_arianna(file)))
r3 <- dbGetQuery(c3, "SELECT DISTINCT reato FROM reati")$reato
atteso("import: le maiuscole si riscrivono", "Crimini sessuali" %in% r3 && !"Crimini Sessuali" %in% r3, TRUE)
atteso("import: il refuso e il valore nuovo restano com'erano (si decide dopo)", all(c("Pippo", "Contrabando") %in% r3), TRUE)
dbDisconnect(c3)

# --- il vocabolario per fondo fa il giro del backup JSON ------------------
p <- tempfile(fileext = ".json"); esporta_backup_json(con, p)
c2 <- apri_db(":memory:"); invisible(ripristina_backup_json(c2, p, "accoda"))
atteso("backup: il vocabolario del fondo torna", regole_fondo(c2, "Bo")[, c("campo", "valore", "forma")],
       regole_fondo(con, "Bo")[, c("campo", "valore", "forma")])
for (k in list(con, c2)) DBI::dbDisconnect(k)

cat(sprintf("\nverifiche: %d passate, %d fallite\n", .esiti$ok, length(.esiti$falliti)))
if (length(.esiti$falliti)) { cat("ESITO: NON TORNA\n"); quit(status = 1) }
cat("ESITO: ok\n")
