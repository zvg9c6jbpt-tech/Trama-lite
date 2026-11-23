# ─────────────────────────────────────────────────────────────
# interfaccia.R: l'app vera, avviata come con runApp
#
# Nato il 2/10: una funzione di un file copriva una di un altro solo nell'app, perché runApp carica R/ in ordine
# alfabetico; gli altri collaudi caricano i file in un altro ordine e non lo vedevano. Qui l'app parte come con runApp
# (testServer sulla cartella), su una copia temporanea del database: la Home, la pagina «Luoghi citati», l'anteprima
# dell'import, la tabella dei fascicoli a zero righe, gli errori già nel database, «Pulisci e scarica» e le scelte sui
# luoghi ignoti (Aggiungi alla lista, Elimina) dalla finestra di revisione.
#
#     Rscript verifiche/interfaccia.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
source("verifiche/aiuti.R")
copia <- database_di_prova()
Sys.setenv(TRAMA_DB = copia)
Sys.setenv(TRAMA_HOME = tempfile("trama_lite_interfaccia_"))   # copie di sicurezza e log dei collaudi: mai nella cartella dati vera (13/10)
# Ogni file di R/ deve stare anche nell'elenco source(...) di app.R (13/10): Shiny carica da solo la cartella, ma un test con
# testServer non vede ciò che app.R non carica a mano, e l'ordine di caricamento resterebbe quello alfabetico.
local({
  f <- sub("[.]R$", "", list.files("R", "[.]R$")); l <- readLines("app.R")
  s <- unlist(regmatches(l[grepl("source[(]", l)], gregexpr("(?<=R/)[A-Za-z0-9_]+(?=[.]R)", l[grepl("source[(]", l)], perl = TRUE)))
  mancano <- setdiff(f, s)
  if (length(mancano)) { cat("NON TORNA: file di R/ non caricati da app.R:", paste(mancano, collapse = ", "), "\n"); cat("ESITO: NON TORNA\n"); quit(status = 1) }
})
options(trama.errori_veri = TRUE)          # l'app, da sola, al posto dell'errore mostra un riquadro: qui deve fallire
falliti <- character(0); fatti <- 0L
suppressWarnings(shiny::testServer(".", {
  session$flushReact()
  stopifnot(nrow(dati()) > 0)
  # La Home e la pagina dei luoghi non si rompono, in italiano e in inglese, chiaro e scuro.
  for (lingua in c(FALSE, TRUE)) for (scuro in c(FALSE, TRUE)) {
    session$setInputs(inglese = lingua, scuro = scuro); session$flushReact()
    for (o in c("home_porte", "home_fasi", "luoghi_conta", "luoghi_lista")) {
      fatti <<- fatti + 1L
      h <- tryCatch({ x <- output[[o]]; paste(as.character(if (is.list(x)) x$html else x), collapse = "") }, error = function(e) paste("Errore:", conditionMessage(e)))
      if (!nzchar(h) || grepl("^(Errore|Error)", h)) falliti <<- c(falliti, sprintf("%s [%s, %s]: %s", o, if (lingua) "inglese" else "italiano", if (scuro) "scuro" else "chiaro", substr(h, 1, 100)))
    }
  }
  session$setInputs(inglese = FALSE, scuro = FALSE)
  # Un luogo scelto nella pagina «Luoghi citati» apre la sua scheda.
  fatti <<- fatti + 1L
  r <- tryCatch({
    nome <- DBI::dbGetQuery(DBI::dbConnect(RSQLite::SQLite(), copia, flags = RSQLite::SQLITE_RO), "SELECT luogo FROM luoghi GROUP BY luogo ORDER BY COUNT(*) DESC LIMIT 1")$luogo
    session$setInputs(luogo_scelto_anagrafe = nome); session$flushReact()
    h <- paste(as.character(output$luoghi_scheda$html), collapse = "")
    if (!nzchar(h) || grepl("^(Errore|Error)", h)) stop("la scheda del luogo non si disegna: ", substr(h, 1, 100))
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("scheda del luogo:", r))

  stopifnot(nrow(dati()) > 0)
  # La Home: ripresa, due code di lavoro (incompleti, anomalie), tortura e
  # sentenza in una riga sotto e non in una coda (5/10), fasi richiudibili.
  for (o in c("banner_backup", "home_archivi", "home_prossimo", "home_porte", "home_fasi")) {
    fatti <<- fatti + 1L
    h <- tryCatch(paste(as.character(output[[o]]$html), collapse = ""), error = function(e) conditionMessage(e))
    if (!nzchar(h) || grepl("^(Errore|Error)", h)) falliti <<- c(falliti, paste("home:", o, substr(h, 1, 100)))
    # 16/10: il «prossimo passo» è la fascia sottile «Da guardare» (class home-guarda), con la riga su tortura e sentenza dentro
    if (o == "home_prossimo" && !all(vapply(c("home-guarda", "ancora da esaminare sulla fonte"), grepl, logical(1), x = h, fixed = TRUE)))
      falliti <<- c(falliti, "home: manca la fascia «Da guardare» o la riga su tortura e sentenza")
    if (o == "home_prossimo" && grepl("home-prossimo", h, fixed = TRUE))
      falliti <<- c(falliti, "home: il «Prossimo passo» grande è tornato al posto della fascia sottile")
    if (o == "home_prossimo" && (grepl("Verifiche della fonte da fare", h, fixed = TRUE) || grepl("home-coda", h, fixed = TRUE)))
      falliti <<- c(falliti, "home: tortura e sentenza sono tornate fra le code in primo piano")
    # Le due corsie dicono dove si è (7/10): una sola è «Consigliata», e tutte e due hanno i loro numeri.
    # 16/10: nessuna «Consigliata» (l'avviso sta nella fascia «Da guardare»), i tre box ci sono e un SOLO pulsante pieno (btn-primary)
    if (o == "home_porte" && (grepl("Consigliata", h, fixed = TRUE) || !all(vapply(c("Importa e normalizza", "Esporta", "Schedatura"), grepl, logical(1), x = h, fixed = TRUE))))
      falliti <<- c(falliti, "home: le tre azioni mancano o c'è ancora una «Consigliata»")
    if (o == "home_porte" && length(gregexpr("btn-primary", h, fixed = TRUE)[[1]]) != 1L)
      falliti <<- c(falliti, "home: nei tre box ci dev'essere un solo pulsante pieno")
    if (o == "home_archivi" && !all(vapply(c("Fondo attivo", "Nuovo fondo", "Fondo "), grepl, logical(1), x = h, fixed = TRUE)))
      falliti <<- c(falliti, "home: «Fondo attivo» senza titolo, fondo o «Nuovo fondo»")
    if (o == "home_archivi" && !grepl('class="ora"', h, fixed = TRUE) && !grepl("In uso", h, fixed = TRUE))
      falliti <<- c(falliti, "home: non dice su quale fondo si lavora")
    if (o == "home_archivi" && grepl("Lavora su questo fondo", h, fixed = TRUE) && !grepl("visually-hidden", h, fixed = TRUE))
      falliti <<- c(falliti, "home: i pulsanti dei fondi non hanno un nome accessibile (testo nascosto col nome del fondo)")
    if (o == "home_fasi" && !all(vapply(c("Luoghi", "Gestione e sicurezza"), grepl, logical(1), x = h, fixed = TRUE)) || (o == "home_fasi" && grepl("Scambio", h, fixed = TRUE)))
      falliti <<- c(falliti, "home: gli altri strumenti sono sbagliati")
  }

  # 22/10/2026: l'avanzamento per busta si disegna (o sta zitto se tutte le buste sono a posto)
  fatti <<- fatti + 1L
  hb <- tryCatch(paste(as.character(output$home_buste$html), collapse = ""), error = function(e) paste("Errore:", conditionMessage(e)))
  if (grepl("^(Errore|Error)", hb) || (nzchar(hb) && !grepl("A che punto sono le buste", hb, fixed = TRUE))) falliti <<- c(falliti, paste("home: l'avanzamento per busta non si disegna:", substr(hb, 1, 100)))
  fatti <<- fatti + 1L
  ro <- DBI::dbConnect(RSQLite::SQLite(), copia, flags = RSQLite::SQLITE_RO); av <- avanzamento_buste(ro, fondo_attivo()); DBI::dbDisconnect(ro); aperte <- sum((!is.na(av$mancano) & av$mancano > 0L) | av$incomplete > 0L)
  if ((aperte > 0L) != grepl("home-busta", hb, fixed = TRUE)) falliti <<- c(falliti, sprintf("home: %d buste aperte ma l'avanzamento %s", aperte, if (nzchar(hb)) "è disegnato" else "è vuoto"))
  cat(sprintf("home: %d buste, %d aperte, avanzamento %s\n", nrow(av), aperte, if (nzchar(hb)) "disegnato" else "vuoto"))
  fatti <<- fatti + 1L
  hp <- paste(as.character(output$home_porte$html), collapse = "")
  ro2 <- DBI::dbConnect(RSQLite::SQLite(), copia, flags = RSQLite::SQLITE_RO); dove <- prossimo_da_schedare(ro2, fondo_attivo()); DBI::dbDisconnect(ro2)
  ro2b <- DBI::dbConnect(RSQLite::SQLite(), copia, flags = RSQLite::SQLITE_RO); concluso <- piano_concluso(ro2b, fondo_attivo()); DBI::dbDisconnect(ro2b)
  if ((is.null(dove) && concluso) != grepl("home_fuori_piano", hp, fixed = TRUE)) falliti <<- c(falliti, "home: «Aggiungi fuori piano» compare solo a piano concluso")
  if (!is.null(dove) != grepl("home_continua", hp, fixed = TRUE)) falliti <<- c(falliti, "home: il pulsante «Continua» non c'è (o c'è) quando dovrebbe")
  # A piano concluso la Home dice «Piano completato» e offre «Aggiungi fuori piano» (qui il piano si costruisce, perché il controllo sopra può passare senza provare niente)
  fatti <<- fatti + 1L
  r_pc <- tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), copia); on.exit(DBI::dbDisconnect(con), add = TRUE)
    fa <- fondo_attivo(); sv <- DBI::dbGetQuery(con, "SELECT * FROM piani_busta");
    DBI::dbExecute(con, "DELETE FROM piani_busta")
    ele <- elenco_fascicoli(con, fondo = fa); bs <- sort(unique(ele$busta[!is.na(ele$busta)]))
    for (b_ in bs) { num <- ele$fascicolo[ele$busta == b_ & !is.na(ele$fascicolo)]; salva_piano(con, fa, b_, min(num), max(num)) }
    # tutte le buste col piano riempito dai numeri che ci sono: nessuno manca (se un piano ha buchi, il piano non è concluso: lo si toglie)
    av_ <- avanzamento_buste(con, fa); for (b_ in av_$busta[!is.na(av_$mancano) & av_$mancano > 0L]) DBI::dbExecute(con, "DELETE FROM piani_busta WHERE fondo_id = ? AND busta = ?", list(fa, b_))
    ult <- ele$id[which.max(ele$busta)]; DBI::dbExecute(con, "UPDATE fascicoli SET modificato = '2000-01-01 00:00:00'"); 
    DBI::dbExecute(con, "UPDATE fascicoli SET modificato = '2031-01-01 00:00:00' WHERE id = ?", list(ult))
    ricarica(ricarica() + 1); session$flushReact()
    h_ <- paste(as.character(output$home_porte$html), collapse = "")
    st <- list(conc = piano_concluso(con, fa), dove = prossimo_da_schedare(con, fa)); cat(sprintf("home: piano costruito, concluso = %s\n", st$conc))
    if (st$conc && !is.null(st$dove)) stop("piano concluso ma «Continua» propone ancora un numero")
    if (st$conc && !(grepl("Piano completato", h_, fixed = TRUE) && grepl("home_fuori_piano", h_, fixed = TRUE))) stop("a piano concluso la Home non dice «Piano completato» e non offre «Aggiungi fuori piano»")
    if (!st$conc && grepl("Piano completato", h_, fixed = TRUE)) stop("«Piano completato» compare a piano non concluso")
    DBI::dbExecute(con, "DELETE FROM piani_busta"); if (nrow(sv)) DBI::dbAppendTable(con, "piani_busta", sv)
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r_pc)) falliti <<- c(falliti, paste("home: piano concluso:", r_pc))

  # «Tutti i fondi» (fondo_attivo vuoto): la Home non si rompe (15/10: «Cambia fondo» dava «argument is of length zero»).
  fatti <<- fatti + 1L
  session$setInputs(fondo_attivo = ""); session$flushReact()
  h <- tryCatch(paste(as.character(output$home_archivi$html), collapse = ""), error = function(e) paste("Errore:", conditionMessage(e)))
  if (!nzchar(h) || grepl("^(Errore|Error)", h) || !grepl("Fondo attivo", h, fixed = TRUE))
    falliti <<- c(falliti, paste("home: con «tutti i fondi» la Home non si disegna:", substr(h, 1, 120)))

  # L'ordine della Home: dove lavoro → che cosa faccio adesso → come lavoro → dove trovo il resto.
  fatti <<- fatti + 1L
  ui <- paste(as.character(home_ui()), collapse = "")
  pos <- vapply(c("home_archivi", "home_prossimo", "home_buste", "home_porte", "home_fasi"), function(o) regexpr(paste0('id="', o, '"'), ui, fixed = TRUE)[[1]], 1L)
  if (any(pos < 1) || is.unsorted(pos)) falliti <<- c(falliti, "home: i blocchi non stanno nell'ordine dei fondi, prossimo passo, buste, corsie, resto")
  fatti <<- fatti + 1L
  hf <- paste(as.character(output$home_fasi$html), collapse = "")
  if (!grepl("Altre funzioni", hf, fixed = TRUE) || !grepl("home-piede", hf, fixed = TRUE)) falliti <<- c(falliti, "home: manca il titolo o il piede (copia di sicurezza, cartella dati)")

  # Tempo di «Fondo attivo» (12/10): disegno di home_archivi con tutti i fondi (completezza per fondo, una lettura
  # per fondo), misurato a caldo sul database di prova; soglia 1 secondo. Se la supera: barra solo sul fondo in uso.
  fatti <<- fatti + 1L
  tempo <- system.time(h <- paste(as.character(output$home_archivi$html), collapse = ""))[["elapsed"]]
  if (tempo > 1) falliti <<- c(falliti, sprintf("home: «Fondo attivo» impiega %.2f s (soglia 1 s)", tempo))

  # La striscia e la riga del backup dicono quando è stata l'ultima copia.
  fatti <<- fatti + 1L
  u <- tryCatch(paste(as.character(output$ultimo_backup$html), collapse = ""), error = function(e) conditionMessage(e))
  if (!grepl("copia", u, ignore.case = TRUE) || !grepl("<span>", u, fixed = TRUE)) falliti <<- c(falliti, paste("ultimo_backup:", substr(u, 1, 100)))

  # L'anteprima dell'import dice se il foglio è nel modello o in un vecchio formato (2/10).
  anteprima <- function(percorso) {
    session$setInputs(file_import = data.frame(name = basename(percorso), size = 1, type = "", datapath = percorso))
    session$flushReact()
    tryCatch(paste(as.character(output$anteprima_import$html), collapse = ""), error = function(e) conditionMessage(e))
  }
  fatti <<- fatti + 1L
  if (file.exists("esportASDNa-vecchio-formato.xlsx")) {
    h <- anteprima("esportASDNa-vecchio-formato.xlsx")
    if (!grepl("vecchio formato", h, fixed = TRUE) || !grepl("convertito nel modello", h, fixed = TRUE) || !grepl("Soggetto conservatore", h, fixed = TRUE))
      falliti <<- c(falliti, paste("anteprima import: il vecchio foglio non è dichiarato:", substr(h, 1, 160)))
  } else cat("SALTATA l'anteprima del vecchio formato: manca esportASDNa-vecchio-formato.xlsx\n")
  fatti <<- fatti + 1L
  h <- anteprima("dati/modello_export.xlsx")
  if (!grepl("Foglio nel modello", h, fixed = TRUE) || grepl("vecchio formato", h, fixed = TRUE) || grepl("Errore|non valido", h))
    falliti <<- c(falliti, paste("anteprima import: il modello deve dirsi tale:", substr(h, 1, 160)))
  # Export: una sola descrizione, nessuna scelta di profilo.
  fatti <<- fatti + 1L
  h <- tryCatch(paste(as.character(output$info_export$html), collapse = ""), error = function(e) conditionMessage(e))
  if (!grepl("28 colonne", h, fixed = TRUE) || grepl("selectize", h, fixed = TRUE))
    falliti <<- c(falliti, paste("export: deve dire il modello e non offrire profili:", substr(h, 1, 160)))

  # La tabella dei fascicoli con zero righe: una tabella vuota, non un errore.
  session$setInputs(cerca_fascicoli = "zzzqqq-nessuno"); session$flushReact()
  fatti <<- fatti + 1L
  r <- tryCatch({ stopifnot(nrow(righe_tabella()) == 0L); output$tabella; "" }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("tabella dei fascicoli a zero righe:", r))

  # Gli errori già nel database (13/10, strada B): un fondo con refusi mostra il banner in Fascicoli; la stessa finestra li corregge
  # dopo una copia di sicurezza, e nelle schede restano solo le cose da decidere.
  fatti <<- fatti + 1L
  r <- tryCatch({
    a0 <- leggi_arianna(normalizePath("dati/esempio/prova-normalizzazione.xlsx"))
    cx <- apri_db(copia); on.exit(DBI::dbDisconnect(cx), add = TRUE)
    importa_nuovo_archivio(cx, a0, "Archivio di Prova", "Fondo Anomalie", "")
    attiva_fondo("Fondo Anomalie"); session$setInputs(nav = "Fascicoli"); session$flushReact()
    # Il controllo parte dopo il disegno (later, 0,3 s) e gira in un processo a parte: qui si lascia girare il ciclo degli eventi.
    t0 <- Sys.time(); while (is.null(anomalie_stato()) && difftime(Sys.time(), t0, units = "secs") < 40) { later::run_now(0.5); session$flushReact() }
    st <- anomalie_stato()
    if (is.null(st) || is.null(st$an) || st$an$pr$totale < 10L) stop("il controllo delle schede non ha trovato i refusi")
    h0 <- paste(as.character(output$banner_anomalie$html), collapse = "")
    if (nzchar(h0)) stop("la fascia degli errori compare anche senza il filtro «Con errori»")   # dal 26/10 sta dietro la pillola
    session$setInputs(solo_errori = 1); session$flushReact()
    h <- paste(as.character(output$banner_anomalie$html), collapse = "")
    if (!grepl("possibili errori", h, fixed = TRUE) || !grepl("Controlla e correggi", h, fixed = TRUE)) stop("manca il banner in Fascicoli: ", substr(h, 1, 80))
    n_copie <- length(list.files(cartella_backup(), pattern = "correzione-schede"))
    session$setInputs(apri_anomalie = 1); session$flushReact()
    if (!isTRUE(revisione_import()$schede)) stop("la finestra delle schede non si è aperta")
    # I refusi di vocabolario partono NON spuntati (23/11/2026): chi guarda li include, qui tutte le celle.
    if (!any(!revisione_import()$pr$celle$spunta & revisione_import()$pr$celle$consigliata)) stop("i refusi di vocabolario dovrebbero partire non spuntati")
    rev_selezione(revisione_import()$pr$celle$id); session$flushReact()
    session$setInputs(rev_conferma = 1); session$flushReact()
    if (length(list.files(cartella_backup(), pattern = "correzione-schede")) != n_copie + 1L) stop("manca la copia di sicurezza prima di correggere")
    t <- tabella_arianna(cx, fondo = "Fondo Anomalie")
    if (any(grepl("cicra|miscredenxe|Maggia", unlist(t)))) stop("restano refusi nelle schede dopo la correzione: ", paste(grep("cicra|miscredenxe|Maggia", unlist(t), value = TRUE), collapse = " ; "))   # «Violo» resta: i nomi discordi si decidono, non si correggono per voti (D11, 20/10)
    t0 <- Sys.time(); while (is.null(anomalie_stato()) && difftime(Sys.time(), t0, units = "secs") < 30) { later::run_now(0.5); session$flushReact() }   # il ricontrollo gira in un processo a parte
    if (is.null(anomalie_stato()) || anomalie_stato()$an$pr$totale >= st$an$pr$totale) stop("dopo la correzione le anomalie non sono diminuite")
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("anomalie nelle schede (Fascicoli):", r))

  # «Pulisci e scarica» passa dalla stessa finestra di revisione (13/10): prima niente foglio pulito, dopo la conferma sì, e il database non cambia.
  fatti <<- fatti + 1L
  r <- tryCatch({
    n0 <- nrow(dati())
    session$setInputs(file_import = data.frame(name = "prova.xlsx", size = 1, type = "", datapath = normalizePath("dati/esempio/prova-normalizzazione.xlsx")))
    session$setInputs(pul_archivio = "Archivio di Prova", pul_fondo = "Tribunale dell\u2019Inquisizione di Prova", pul_serie = ""); session$flushReact()
    session$setInputs(avvia_pulito = 20); session$flushReact()
    rv <- revisione_import()
    if (is.null(rv) || !isTRUE(rv$pulisci) || rv$pr$totale < 10L) stop("la finestra di revisione non si è aperta per «Pulisci e scarica»")
    if (!is.null(risultato_pulito())) stop("ha pulito prima della conferma")
    rev_selezione(rv$pr$celle$id); session$flushReact()      # include anche i refusi di vocabolario, che partono non spuntati
    session$setInputs(rev_conferma = 1); session$flushReact()
    p <- risultato_pulito(); if (is.null(p)) stop("dopo la conferma non c'è il foglio pulito")
    if (!file.exists(p$percorso)) stop("il foglio pulito non esiste")
    z <- readxl::read_excel(p$percorso, sheet = 1)
    if (any(grepl("cicra|Bestemmia e miscredenxe", unlist(z)))) stop("restano refusi nel foglio pulito")   # «Violo» resta (D11)
    if (nrow(dati()) != n0) stop("il database è cambiato")
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("Pulisci e scarica con la finestra di revisione:", r))

  # Se il controllo prima di importare cade, non si scrive niente e compare la scelta esplicita (13/10, fail-closed).
  fatti <<- fatti + 1L
  r <- tryCatch({
    options(trama.simula_guasto_controllo = TRUE)
    n0 <- nrow(dati())
    session$setInputs(file_import = data.frame(name = "prova.xlsx", size = 1, type = "", datapath = normalizePath("dati/esempio/prova-normalizzazione.xlsx")))
    session$setInputs(modo_import = "accoda"); session$flushReact()
    session$setInputs(esegui_import = 10); session$flushReact()
    if (is.null(revisione_import())) stop("nessuna scelta offerta dopo il guasto del controllo")
    if (revisione_import()$pr$totale != 0L) stop("il guasto simulato non è scattato (ambiente ", environmentName(amb), ", totale ", revisione_import()$pr$totale, ")")
    if (!is.null(risultato_import())) stop("ha importato nonostante il guasto")
    session$setInputs(rev_annulla = 1); session$flushReact()
    if (!is.null(revisione_import())) stop("Annulla non chiude la scelta")
    options(trama.simula_guasto_controllo = NULL)
    if (nrow(dati()) != n0) stop("il database è cambiato dopo Annulla")
    ""
  }, error = function(e) conditionMessage(e))
  options(trama.simula_guasto_controllo = NULL)
  if (nzchar(r)) falliti <<- c(falliti, paste("guasto del controllo prima dell'import:", r))

  # Scegliere per gruppi nella finestra delle correzioni (22/10/2026): per colonna, per scheda, per tipo; «Applica»/«Non applicare» valgono per le mostrate,
  # la spunta delle altre resta, e togliere il filtro le riporta tutte.
  fatti <<- fatti + 1L
  r <- tryCatch({
    session$setInputs(file_import = data.frame(name = "prova.xlsx", size = 1, type = "", datapath = normalizePath("dati/esempio/prova-normalizzazione.xlsx")))
    session$setInputs(modo_import = "accoda"); session$flushReact()
    session$setInputs(esegui_import = 20); session$flushReact()
    rv <- revisione_import(); if (is.null(rv)) stop("la finestra di revisione non si è aperta")
    ce <- rv$pr$celle; h <- paste(as.character(modale_revisione(rv$pr)), collapse = "")
    if (!all(vapply(c("rev-app", "rev-voce", "rev_f_colonna", "rev_toggle", "Sicure", "Da controllare"), grepl, TRUE, x = h, fixed = TRUE))) stop("la finestra non ha i tre riquadri (filtri, elenco, dettaglio)")
    if (length(gregexpr("class=\"rev-voce\"", h, fixed = TRUE)[[1]]) != nrow(ce) + length(rv$pr$decisioni)) stop("l'elenco non ha una voce per ogni correzione e ogni decisione")
    col2 <- names(sort(table(ce$colonna), decreasing = TRUE))[1]; altre <- ce$id[ce$colonna != col2]; di_col <- ce$id[ce$colonna == col2]
    if (!length(altre)) stop("il foglio di prova ha una sola colonna: il test non distingue")
    prima <- rev_selezione()
    session$setInputs(rev_batch = list(ids = as.list(di_col), on = FALSE)); session$flushReact()
    if (length(intersect(rev_selezione(), di_col))) stop("«Non applicare» non ha tolto le celle mostrate")
    if (!setequal(rev_selezione(), setdiff(prima, di_col))) stop("«Non applicare» ha toccato anche le celle non mostrate")
    session$setInputs(rev_batch = list(ids = as.list(di_col), on = TRUE)); session$flushReact()
    if (!all(di_col %in% rev_selezione())) stop("«Applica» non ha spuntato le celle mostrate")
    if (!all(setdiff(prima, di_col) %in% rev_selezione())) stop("«Applica» ha tolto altre spunte")
    session$setInputs(rev_toggle = list(id = di_col[1], on = FALSE)); session$flushReact()
    if (di_col[1] %in% rev_selezione()) stop("togliere la spunta di una correzione non la toglie dalla scelta")
    session$setInputs(rev_annulla = 2); session$flushReact()
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("finestra delle correzioni: scelta per gruppi:", r))

  # La finestra «Errori e discordanze» dell'import (13/10): un foglio con segnature senza fondo respinge tutte le righe, e il
  # pulsante compare; la finestra si apre e le tre tabelle e il download si disegnano. (Lavora sulla copia del database.)
  fatti <<- fatti + 1L
  r <- tryCatch({
    session$setInputs(file_import = data.frame(name = "prova.xlsx", size = 1, type = "", datapath = normalizePath("dati/esempio/prova-normalizzazione.xlsx")))
    session$setInputs(modo_import = "accoda"); session$flushReact()
    session$setInputs(esegui_import = 1); session$flushReact()
    # Prima di scrivere si controlla (13/10): il foglio del Tab ha refusi, quindi la finestra di revisione si apre e il database non cambia.
    rv <- revisione_import()
    if (is.null(rv) || rv$pr$totale < 10L) stop("la finestra «Controlla prima di importare» non si è aperta o non ha trovato i refusi del Tab")
    if (!is.null(risultato_import())) stop("l'import ha scritto prima della conferma")
    if (!grepl("rev-app", paste(as.character(modale_revisione(rv$pr)), collapse = ""), fixed = TRUE)) stop("la finestra delle correzioni non si disegna")
    # un cambio dal browser (un clic su «Applica» o «Applicata») cambia le correzioni applicate
    id1 <- rv$pr$celle$id[1]; era <- id1 %in% rev_selezione()
    session$setInputs(rev_toggle = list(id = id1, on = !era)); session$flushReact()
    if (identical(id1 %in% rev_selezione(), era)) stop("il clic sulla correzione non cambia la selezione")
    session$setInputs(rev_toggle = list(id = id1, on = era)); session$flushReact()
    # senza toccare la tabella (non ancora disegnata) valgono tutte le correzioni; poi chi toglie una spunta la toglie davvero
    session$setInputs(rev_conferma = 1); session$flushReact()
    h <- paste(as.character(output$esito_import$html), collapse = "")
    if (!grepl("Errori e discordanze", h, fixed = TRUE)) stop("manca il pulsante «Errori e discordanze» dopo un import con righe respinte: ", substr(h, 1, 100))
    session$setInputs(apri_errori_import = 1); session$flushReact()
    for (o in c("errori_respinte", "errori_decidere", "errori_discordanze")) { x <- output[[o]]; if (is.null(x)) stop("tabella ", o, " non disegnata") }
    el <- elenco_errori(); if (nrow(el$respinte) != 9L) stop("le righe respinte dovrebbero essere 9, sono ", nrow(el$respinte))
    f <- tempfile(fileext = ".xlsx"); scrivi_errori_xlsx(el, f)
    if (!all(c("Respinte", "Da decidere", "Discordanze") %in% readxl::excel_sheets(f))) stop("l'elenco scaricabile non ha i tre fogli")
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("finestra errori e discordanze:", r))

  # «Elimina» sui luoghi ignoti (D9, 21/10/2026), dalla finestra vera: la spunta «Elimina dalla scheda» (rev_dec_del_N) arriva a rev_conferma col prefisso
  # \002 e il luogo esce dalla scheda a import riuscito; l'elenco dei luoghi tolti sta nell'esito e nella finestra «Errori e discordanze».
  fatti <<- fatti + 1L
  r <- tryCatch({
    col <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
    riga <- stats::setNames(as.list(rep(NA_character_, length(col))), col); riga[["Segnatura"]] <- "b. 1, fasc. 1"; riga[["Luoghi citati o indicizzati"]] <- "Napoli|Xyzzy"
    fx <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(as.data.frame(riga, check.names = FALSE), fx)
    session$setInputs(file_import = data.frame(name = "elimina.xlsx", size = 1, type = "", datapath = fx))
    session$setInputs(modo_import = "nuovo", import_archivio = "Archivio Prova Elimina", import_fondo = "Fondo Elimina", import_serie = ""); session$flushReact()
    session$setInputs(esegui_import = 2); session$flushReact()
    rv <- revisione_import(); if (is.null(rv)) stop("la finestra di revisione non si è aperta")
    dl <- Filter(function(d) identical(d$tipo, "luoghi_ignoti"), rv$pr$decisioni); if (length(dl) != 1L) stop("manca la decisione sui luoghi ignoti")
    h <- paste(as.character(modale_revisione(rv$pr)), collapse = "")
    if (!grepl(sprintf("rev_dec_del_%d", dl[[1]]$id), h, fixed = TRUE)) stop("la finestra non ha la spunta «Elimina»")
    do.call(session$setInputs, stats::setNames(list("Xyzzy"), sprintf("rev_dec_del_%d", dl[[1]]$id)))
    session$setInputs(rev_conferma = 2); session$flushReact()
    cx2 <- apri_db(copia); on.exit(DBI::dbDisconnect(cx2), add = TRUE)
    l <- DBI::dbGetQuery(cx2, "SELECT l.luogo FROM luoghi l JOIN fascicoli f ON f.id = l.fascicolo_id WHERE f.fondo = 'Fondo Elimina'")$luogo
    if (!identical(sort(l), "Napoli")) stop("i luoghi della scheda dopo Elimina sono: ", paste(l, collapse = ", "))
    e <- risultato_import(); if (is.null(e$luoghi_tolti) || nrow(e$luoghi_tolti) != 1L) stop("l'esito non ha l'elenco dei luoghi tolti")
    if (!identical(elenco_errori()$luoghi_tolti$Prima, "Napoli|Xyzzy")) stop("l'elenco degli errori non ha il luogo tolto")
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("Elimina dalla finestra di revisione:", r))

  # «Aggiungi alla lista» (D9): la spunta rev_dec_N porta il luogo nella lista dei luoghi personale (senza coordinate: la lista non ne ha) e lo lascia nella scheda.
  fatti <<- fatti + 1L
  r <- tryCatch({
    col <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
    riga <- stats::setNames(as.list(rep(NA_character_, length(col))), col); riga[["Segnatura"]] <- "b. 1, fasc. 1"; riga[["Luoghi citati o indicizzati"]] <- "Napoli|Qwertyville"
    fx <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(as.data.frame(riga, check.names = FALSE), fx)
    session$setInputs(file_import = data.frame(name = "aggiungi.xlsx", size = 1, type = "", datapath = fx))
    session$setInputs(modo_import = "nuovo", import_archivio = "Archivio Prova Aggiungi", import_fondo = "Fondo Aggiungi", import_serie = ""); session$flushReact()
    session$setInputs(esegui_import = 3); session$flushReact()
    rv <- revisione_import(); if (is.null(rv)) stop("la finestra di revisione non si è aperta")
    dl <- Filter(function(d) identical(d$tipo, "luoghi_ignoti"), rv$pr$decisioni); if (length(dl) != 1L) stop("manca la decisione sui luoghi ignoti")
    if (any(grepl("Qwertyville", leggi_lista_luoghi()$nome))) stop("il luogo era già nella lista prima della scelta")
    do.call(session$setInputs, stats::setNames(list("Qwertyville"), sprintf("rev_dec_%d", dl[[1]]$id)))
    session$setInputs(rev_conferma = 3); session$flushReact()
    if (!any(leggi_lista_luoghi()$nome == "Qwertyville")) stop("il luogo non è entrato nella lista dei luoghi")
    if (!identical(colnames(leggi_lista_luoghi()), LISTA_COLONNE)) stop("la lista dei luoghi ha colonne che non dovrebbe avere")
    cx3 <- apri_db(copia); on.exit(DBI::dbDisconnect(cx3), add = TRUE)
    l <- DBI::dbGetQuery(cx3, "SELECT l.luogo FROM luoghi l JOIN fascicoli f ON f.id = l.fascicolo_id WHERE f.fondo = 'Fondo Aggiungi'")$luogo
    if (!identical(sort(l), sort(c("Napoli", "Qwertyville")))) stop("i luoghi della scheda dopo Aggiungi sono: ", paste(l, collapse = ", "))
    ""
  }, error = function(e) conditionMessage(e))
  if (nzchar(r)) falliti <<- c(falliti, paste("Aggiungi alla lista dalla finestra di revisione:", r))
}))
unlink(copia)
cat(sprintf("interfaccia: %d disegni provati, %d falliti\n", fatti, length(falliti)))
if (length(falliti)) {
  cat(paste("NON TORNA:", falliti), sep = "\n"); cat("ESITO: NON TORNA\n")
  if (!interactive()) quit(status = 1) else stop("interfaccia in errore")
}
cat("ESITO: ok\n")
