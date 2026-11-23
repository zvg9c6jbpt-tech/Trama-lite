# ─────────────────────────────────────────────────────────────
# R/anomalie_server.R: gli errori già nel database, segnalati in Fascicoli e in Home
# Stessi controlli dell'import sul fondo a schermo; la correzione passa dalla finestra di revisione (`revisione_import`, `rev_selezione`).
# ─────────────────────────────────────────────────────────────

anomalie_server <- function(input, output, session, con, ricarica, fondo_attivo, solo_errori, revisione_import, rev_selezione) {
  # Stessi controlli dell'import sul fondo a schermo. Oltre 1.500 schede non si rifà da solo: il banner dice «Ricontrolla».
  anomalie_stato <- reactiveVal(NULL)
  # Il controllo di un fondo di 367 schede dura 7-8 s di calcolo: gira in un processo R separato (callr), così Home e Fascicoli restano
  # utilizzabili. Il risultato torna a pezzi di mezzo secondo (`later`) e, se il fondo è cambiato nel frattempo, si scarta e si rifà.
  lavoro <- new.env(); lavoro$p <- NULL; lavoro$fondo <- NULL; lavoro$chiave <- NULL
  .sorgenti_controllo <- function() {
    r <- readLines("app.R", warn = FALSE); r <- r[seq_len(grep('source\\("R/anomalie.R"\\)', r)[1])]
    f <- sub('^source\\("(R/[^"]+)"\\).*$', "\\1", grep('^source\\("R/', r, value = TRUE))
    f[!grepl("_(server|ui)\\.R$", f)]
  }
  .avvia_controllo <- function(fo, k) {
    lavoro$fondo <- fo; lavoro$chiave <- k
    lavoro$p <- callr::r_bg(function(radice, file, db, fo) {
      setwd(radice); suppressPackageStartupMessages({ library(shiny); library(bslib); library(DT) })
      for (f in file) source(f)
      con <- DBI::dbConnect(RSQLite::SQLite(), db); on.exit(DBI::dbDisconnect(con))
      anomalie_fondo(con, fo)
    }, args = list(radice = getwd(), file = .sorgenti_controllo(), db = percorso_db(), fo = fo), stderr = "2>&1")
    attesa <- function() {
      if (session$isClosed()) { try(lavoro$p$kill(), silent = TRUE); return(invisible()) }
      if (lavoro$p$is_alive()) return(invisible(later::later(attesa, delay = 0.5)))
      an <- tryCatch(lavoro$p$get_result(), error = function(e) { registra("ERRORE", "controllo_anomalie_fallito", conditionMessage(e)); NULL })
      lavoro$p <- NULL
      withReactiveDomain(session, isolate({
        if (identical(fondo_attivo(), fo)) anomalie_stato(list(fondo = fo, chiave = k, an = an, vecchio = FALSE))
        else controlla_anomalie()          # il fondo è cambiato mentre si controllava
      }))
    }
    later::later(attesa, delay = 0.5)
  }
  controlla_anomalie <- function(forza = FALSE) isolate({
    fo <- fondo_attivo(); if (.vuoto(fo)) return(invisible())
    k <- chiave_fondo(con, fo); st <- anomalie_stato()
    if (!is.null(lavoro$p)) {
      if (identical(lavoro$fondo, fo) && identical(lavoro$chiave, k)) return(invisible())   # già in corso, per queste stesse schede
      try(lavoro$p$kill(), silent = TRUE); lavoro$p <- NULL
    }
    if (!forza && !is.null(st) && identical(st$fondo, fo) && identical(st$chiave, k)) return(invisible())
    grande <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(fo))$n > 1500L
    if (!forza && grande && !is.null(st) && identical(st$fondo, fo)) { anomalie_stato(modifyList(st, list(vecchio = TRUE))); return(invisible()) }
    if (forza || is.null(st) || !identical(st$fondo, fo)) anomalie_stato(NULL)     # un risultato di altre schede non si mostra
    .avvia_controllo(fo, k)
    invisible()
  })
  # Il controllo parte dopo il disegno della pagina: la schermata compare subito, il banner dopo. `onFlushed` scatta prima che il browser
  # riceva i valori il controllo vero gira poi in un processo a parte.
  observe({
    req(input$nav %in% c("Home", "Fascicoli")); ricarica(); fondo_attivo()
    later::later(function() if (!session$isClosed()) withReactiveDomain(session, controlla_anomalie()), delay = 0.3)
  })
  output$banner_anomalie <- renderUI({
    st <- anomalie_stato(); if (is.null(st) || !identical(st$fondo, fondo_attivo())) return(NULL)
    if (!solo_errori()) return(NULL)    # la fascia compare solo con «Con errori»
    banner_anomalie_ui(st$an, st$vecchio)
  })
  observeEvent(input$ricontrolla_anomalie, controlla_anomalie(forza = TRUE))
  apri_finestra_anomalie <- function() {
    st <- anomalie_stato(); req(st, st$an)
    revisione_import(list(a = st$an$a, pr = st$an$pr, schede = TRUE, an = st$an))
    rev_selezione(celle_spuntate(st$an$pr$celle))
    showModal(modale_revisione(st$an$pr, schede = TRUE))
  }
  observeEvent(input$apri_anomalie, apri_finestra_anomalie())          # la fascia di Fascicoli
  observeEvent(input$home_apri_anomalie, apri_finestra_anomalie())     # il «prossimo passo» di Home

  list(anomalie_stato = anomalie_stato, controlla_anomalie = controlla_anomalie)
}
