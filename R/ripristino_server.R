# ─────────────────────────────────────────────────────────────
# R/ripristino_server.R: copie del database e ripristino
# Copia completa (zip) e ripristino, database in .sql/.sqlite, backup JSON e ripristino JSON (sostituisci / accoda).
# `dopo_ripristino()` chiude schede e filtri e fa avanzare le versioni (richiesta, coorti, piani); `naviga` passa dall'avviso delle modifiche non salvate; `.nome_file`, `registra_copia` e `.a_righe` sono gli aiuti di app.R.
# ─────────────────────────────────────────────────────────────

ripristino_server <- function(input, output, session, con, ricarica, fondo_attivo, fondo_da_esportare, naviga, .nome_file, registra_copia, .a_righe, dopo_ripristino) {
  output$scarica_backup_completo <- downloadHandler(
    filename = function() .nome_file("zip", "copia-completa", per_fondo = FALSE),
    content  = function(file) {
      tmp <- tempfile(fileext = ".zip")
      backup_completo(con, tmp, cart_luoghi = cartella_luoghi())
      file.copy(tmp, file, overwrite = TRUE)
    }
  )

  # ── ripristino della copia completa (5/10) ─────────────────────────
  copia_scelta <- reactive({
    f <- input$file_completo
    if (is.null(f)) return(NULL)
    tryCatch(c(leggi_backup_completo(f$datapath), list(percorso = f$datapath, nome = f$name)),
             error = function(e) list(errore = conditionMessage(e)))
  })
  output$anteprima_completo <- renderUI({
    k <- copia_scelta()
    if (is.null(k)) return(NULL)
    if (!is.null(k$errore)) return(div(class = "alert alert-danger py-2 small", k$errore))
    impedito <- ripristino_completo_impedito(percorso_db())
    tagList(
      div(class = "alert alert-secondary py-2 small",
          div(sprintf("Nella copia: %d fascicoli.", k$fascicoli)),
          div(if (k$luoghi) "Con la lista dei luoghi personale." else "Senza lista dei luoghi personale: quella attuale resta."),
          div(sprintf("Ora nel database: %d fascicoli.", dbGetQuery(con, "SELECT COUNT(*) AS n FROM fascicoli")$n))),
      if (!is.null(impedito)) div(class = "alert alert-warning py-2 small", impedito))
  })
  observeEvent(input$esegui_completo, {
    k <- copia_scelta()
    if (is.null(k)) return(showNotification("Scegli prima il file .zip della copia completa.", type = "warning"))
    if (!is.null(k$errore)) return(showNotification(k$errore, type = "error"))
    impedito <- ripristino_completo_impedito(percorso_db())
    if (!is.null(impedito)) return(showNotification(impedito, type = "error", duration = NULL))
    showModal(modalDialog(
      title = "Ripristinare la copia completa?", easyClose = TRUE,
      p(sprintf("Il database attuale verrà sostituito con quello della copia (%d fascicoli).", k$fascicoli)),
      p(class = "small text-muted", "Il database e la lista dei luoghi di adesso vengono prima salvati nella cartella dei backup. Poi Trama Lite si chiude: riaprila per lavorare sui dati ripristinati."),
      footer = tagList(modalButton("Annulla"), actionButton("conferma_completo", "Sostituisci e chiudi Trama", class = "btn-danger"))))
  })
  observeEvent(input$conferma_completo, {
    k <- copia_scelta()
    if (is.null(k) || !is.null(k$errore) || !is.null(ripristino_completo_impedito(percorso_db()))) return()
    removeModal()
    # La connessione si chiude prima: i file non si sostituiscono sotto un database aperto. Poi la sessione va riaperta.
    try(dbDisconnect(con), silent = TRUE)
    esito <- tryCatch(ripristina_backup_completo(k$percorso, dove = dirname(percorso_db())),
                      error = function(e) list(errore = conditionMessage(e)))
    if (is.null(esito$errore)) registra("INFO", "ripristino copia completa", sprintf("%d fascicoli", esito$fascicoli))
    else registra("ERRORE", "ripristino copia completa non riuscito", esito$errore)
    showModal(modalDialog(
      title = if (is.null(esito$errore)) "Copia completa ripristinata" else "Ripristino non riuscito", footer = NULL,
      if (is.null(esito$errore)) tagList(
        p(sprintf("Ripristinati %d fascicoli.", esito$fascicoli)),
        if (length(esito$copie_di_prima)) p(class = "small text-muted", span("I file di prima sono in: "), tags$code(dirname(esito$copie_di_prima[1]))))
      else tagList(p(esito$errore), p(class = "small text-muted", "I file di prima, se erano già stati salvati, sono nella cartella dei backup.")),
      p(tags$b("Trama si è chiusa: chiudi questa scheda e riaprila."))))
    later::later(function() stopApp(), 1.5)
  })

  db_fondo <- function() if (isTRUE(input$db_solo_fondo)) fondo_da_esportare() else NULL
  output$scarica_db_sql <- downloadHandler(
    filename = function() .nome_file("sql", "database", per_fondo = isTRUE(input$db_solo_fondo)),
    content  = function(file) {
      esporta_sql(con, file, fondo = db_fondo())
      registra_copia(file, .nome_file("sql", "database", per_fondo = isTRUE(input$db_solo_fondo)))
    })
  output$scarica_db_sqlite <- downloadHandler(
    filename = function() .nome_file("sqlite", "database", per_fondo = isTRUE(input$db_solo_fondo)),
    content  = function(file) {
      esporta_sqlite(con, file, fondo = db_fondo())
      registra_copia(file, .nome_file("sqlite", "database", per_fondo = isTRUE(input$db_solo_fondo)))
    })

  output$scarica_backup <- downloadHandler(
    filename = function() .nome_file("json", "backup", per_fondo = FALSE),
    content  = function(file) esporta_backup_json(con, file)
  )
  # I quattro download stanno in una sezione chiusa («Backup e ripristino»): finché non è aperta Shiny non li collega e il link resta
  # spento, quindi un clic subito dopo l'apertura non fa niente. Così sono pronti prima.
  for (o in c("scarica_backup", "scarica_backup_completo", "scarica_db_sql", "scarica_db_sqlite"))
    outputOptions(output, o, suspendWhenHidden = FALSE)

  risultato_ripristino <- reactiveVal(NULL)

  ripristino_in_attesa <- reactiveVal(NULL)
  esegui_ripristino <- function(f, modo) {
    risultato_ripristino(NULL)
    esito <- try(ripristina_backup_json(con, f$datapath, modo = modo,
                                      cartella_sicurezza = cartella_backup()), silent = TRUE)
    if (inherits(esito, "try-error")) {
      registra("ERRORE", "ripristino JSON fallito", conditionMessage(attr(esito, "condition")))
      showNotification(div(strong("Ripristino fallito"),
                           .a_righe(conditionMessage(attr(esito, "condition")))),
                       type = "error", duration = NULL)
      return()
    }
    esito$modo <- modo
    registra("INFO", "ripristino JSON", sprintf("modo %s", modo))
    risultato_ripristino(esito)
    # Nessuna bozza della versione precedente deve poter riscrivere il database.
    ids_fondi <- elenco_fondi(con)$id            # dopo «sostituisci» il fondo in uso può non esistere più
    if (!.vuoto(fondo_attivo()) && !fondo_attivo() %in% ids_fondi) fondo_attivo(if (length(ids_fondi)) ids_fondi[1] else NULL)
    dopo_ripristino()
    ricarica(ricarica() + 1)
    # L'esito è già nel riquadro della carta (niente avviso che lo ripeta): si porta in vista.
    session$sendCustomMessage("scorri_a", "esito_ripristino")
  }
  observeEvent(input$esegui_ripristino, {
    f <- input$file_ripristino; modo <- input$modo_ripristino
    if (is.null(f)) { showNotification("Nessun file scelto.", type = "warning"); return() }
    naviga(function() {
      if (modo == "sostituisci") {
        ripristino_in_attesa(list(file=f, modo=modo))
        showModal(modalDialog(title="Sostituire i dati con il backup?",
          p("File: ", strong(f$name)),
          p("Fascicoli, persone, piani, ricerche e ordine saranno sostituiti. Prima verrà salvata una copia integrale del database attuale nella cartella dei backup. Se il file non è valido, nulla verrà cambiato."),
          footer=tagList(modalButton("Annulla"), actionButton("conferma_ripristino", "Sostituisci con il backup", class="btn-danger"))))
      } else esegui_ripristino(f, modo)
    })
  })
  observeEvent(input$conferma_ripristino, {
    x <- ripristino_in_attesa(); req(x)
    ripristino_in_attesa(NULL); removeModal()
    esegui_ripristino(x$file, x$modo)
  })

  # «0 importati · 5533 respinti» è il risultato giusto di un «Accoda» su un database che ha già tutto: si dice perché.
  output$esito_ripristino <- renderUI({
    e <- risultato_ripristino()
    if (is.null(e)) return(NULL)
    n <- length(e$respinti)
    div(class = paste("alert py-2 mt-3", if (n == 0) "alert-success" else "alert-warning"),
        sprintf("%d letti · %d importati · %d già presenti", e$letti, e$importati, n),
        if (n > 0) p("Accoda conserva i fascicoli già presenti per identificativo o segnatura. Il dettaglio sotto indica il motivo."),
        if (!is.null(e$backup_sicurezza)) p("Copia di sicurezza: ", tags$code(e$backup_sicurezza)),
        if (nrow(e$dettagli)) tags$details(tags$summary(sprintf("Dettaglio delle voci mantenute (%d)", nrow(e$dettagli))),
          p("Sono mostrate le prime 100 voci."),
          tags$div(style="max-height:20rem;overflow:auto", tags$table(class="table table-sm",
            tags$thead(tags$tr(tags$th("Tipo"), tags$th("Riferimento"), tags$th("Motivo"))),
            tags$tbody(lapply(seq_len(min(100L,nrow(e$dettagli))), function(i)
              tags$tr(lapply(e$dettagli[i, ], tags$td))))))))
  })
}
