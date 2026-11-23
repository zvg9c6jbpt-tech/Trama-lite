# ─────────────────────────────────────────────────────────────
# R/revisione_server.R: la finestra «Controlla prima di importare»
# Spunte, «Applica»/«Non applicare», conferma della revisione e della sostituzione. Lo stato (`revisione_import`, `rev_selezione`, `forza_pulisci`, `import_sostituzione_in_attesa`) nasce in import_server e arriva come argomento; `esegui_import_vero`, `esegui_pulizia` e `controlla_anomalie` sono le sue funzioni. Restituisce `rev_visibili` (i collaudi lo leggono).
# ─────────────────────────────────────────────────────────────

revisione_server <- function(input, output, session, con, ricarica, revisione_import, rev_selezione, forza_pulisci,
                             import_sostituzione_in_attesa, esegui_import_vero, esegui_pulizia, controlla_anomalie) {
  observeEvent(input$rev_scarica_pulito, { r <- revisione_import(); req(r); forza_pulisci(TRUE); revisione_import(modifyList(r, list(pulisci = TRUE))); conferma_revisione() })
  observeEvent(input$rev_cambia_modalita, { removeModal(); revisione_import(NULL) })
  # La finestra disegna le correzioni in tre riquadri (R/revisione_vista.R): il browser tiene il segno di ogni correzione applicata e dice qui ogni cambiamento.
  observeEvent(input$rev_toggle, {
    t <- input$rev_toggle; req(t$id)
    rev_selezione(if (isTRUE(t$on)) union(rev_selezione(), t$id) else setdiff(rev_selezione(), t$id))
  })
  observeEvent(input$rev_batch, {
    b <- input$rev_batch; ids <- as.integer(unlist(b$ids))
    rev_selezione(if (isTRUE(b$on)) union(rev_selezione(), ids) else setdiff(rev_selezione(), ids))
  })
  observeEvent(input$conferma_sostituzione, {
    r <- import_sostituzione_in_attesa(); req(r)
    if (r$orfane > 0L && !isTRUE(input$sost_accetta_perdite)) {
      showNotification("Spunta «Ho capito» per sostituire: alcune schede perderebbero dei dati.", type = "warning"); return()
    }
    removeModal(); import_sostituzione_in_attesa(NULL)
    esegui_import_vero(r$a, r$applicate, confermato = TRUE)
  })
  observeEvent(input$rev_annulla, { removeModal(); revisione_import(NULL) })
  observeEvent(input$rev_come_sta, { r <- revisione_import(); req(r); removeModal(); revisione_import(NULL); if (isTRUE(r$pulisci)) esegui_pulizia(r$a, revisionato = TRUE) else esegui_import_vero(r$a) })
  conferma_revisione <- function() {
    r <- revisione_import(); req(r)
    ok <- rev_selezione()
    scelte <- list()
    for (d in r$pr$decisioni) {
      lib <- input[[sprintf("rev_dec_libero_%d", d$id)]]
      v <- if (!is.null(lib) && nzchar(trimws(lib))) trimws(lib) else input[[sprintf("rev_dec_%d", d$id)]]
      if (identical(d$tipo, "luoghi_ignoti")) {                                      # D9: Aggiungi (nome) ed Elimina (prefisso \002) per luogo
        del <- input[[sprintf("rev_dec_del_%d", d$id)]]
        v <- c(v, if (length(del)) paste0("\002", del))
      }
      if (length(v) > 1L) v <- paste(v, collapse = "\001")                       # più spunte (luoghi ignoti)
      if (!is.null(v) && nzchar(v)) scelte[[as.character(d$id)]] <- v
    }
    if (isTRUE(r$schede)) {                       # correggere le schede già nel database: prima la copia di sicurezza
      copia <- tryCatch(backup_prima_ripristino(con, cartella_backup(), "correzione-schede"), error = function(e) NULL)
      if (is.null(copia)) { showNotification("Non riesco a fare la copia di sicurezza: non ho cambiato niente.", type = "error", duration = NULL); return() }
      res <- tryCatch(applica_anomalie_db(con, r$an, ok = ok, scelte = scelte), error = function(e) { registra("ERRORE", "correzione_schede_fallita", conditionMessage(e)); e })
      removeModal(); revisione_import(NULL)
      if (inherits(res, "error")) { showNotification(div(strong("Correzione non riuscita"), conditionMessage(res)), type = "error", duration = NULL); return() }
      ricarica(ricarica() + 1); controlla_anomalie(forza = TRUE)
      showNotification(esito_ok(sprintf("%d schede corrette \u00b7 copia di sicurezza fatta", res$schede)), type = "message", duration = 10)
      return()
    }
    a2 <- applica_proposte(r$a, r$pr, ok = ok, scelte = scelte)
    removeModal(); revisione_import(NULL)
    if (isTRUE(r$pulisci)) esegui_pulizia(a2, revisionato = TRUE)
    else esegui_import_vero(a2, applicate = attr(a2, "celle_cambiate") %||% (length(ok) + length(scelte)))
  }
  observeEvent(input$rev_conferma, conferma_revisione())
  list()
}
