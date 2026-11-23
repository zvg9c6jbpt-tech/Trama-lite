# ─────────────────────────────────────────────────────────────
# R/fondo_server.R: il fondo di lavoro
# Tutto ciò che si vede e si esporta è di UN fondo. `naviga(azione)` passa dall'avviso delle modifiche non salvate; `al_cambio()` chiude ciò che era del fondo di prima.
# Restituisce list(fondo_attivo, fondi_noti, attiva_fondo).
# ─────────────────────────────────────────────────────────────

fondo_server <- function(input, output, session, con, ricarica, naviga, al_cambio) {
  # Tutto ciò che si vede e si esporta è di UN fondo; «(tutti i fondi)» è solo una vista d'insieme.
  fondi_noti <- reactive({
    ricarica()
    elenco_fondi(con)
  })
  fondo_attivo <- reactiveVal(NULL)
  output$archivio_attivo <- renderUI({
    ricarica(); a <- archivio_del_fondo(con, fondo_attivo())
    # Se il menu del fondo dice già l'ente («Fondo X · Archivio Y (n)»), non lo si ripete accanto (13/10).
    nel_menu <- !is.null(a) && grepl(a, etichetta_fondi(con, fondo_attivo(), 0L), fixed = TRUE)
    if (is.null(a) || nel_menu) NULL else span(class = "no-trad", title = a, a)
  })

  observe({
    g <- fondi_noti()
    salvato <- leggi_impostazione(con, "fondo_attivo")
    scelto  <- if (!is.null(salvato) && salvato %in% g$id) salvato
               else if (nrow(g)) g$id[1] else ""
    isolate({
      if (is.null(fondo_attivo())) fondo_attivo(scelto)
      updateSelectizeInput(session, "fondo_attivo",
        choices  = c(stats::setNames(g$id, etichetta_fondi(con, g$id, g$n)), "(tutti i fondi)" = ""),
        selected = fondo_attivo())
    })
  })

  # Dal menu in alto o dalla Home (anche subito dopo Nuovo archivio).
  attiva_fondo <- function(f) {
    if (identical(f, fondo_attivo())) return()
    naviga(function() {
      fondo_attivo(f)
      scrivi_impostazione(con, "fondo_attivo", f)
      al_cambio()    # filtri azzerati, scheda aperta e scheda nuova del fondo di prima chiuse
      updateSelectizeInput(session, "fondo_attivo", selected = f)
      showNotification(if (.vuoto(f)) "Ora vedi tutti i fondi."
                       else tagList("Ora lavori su «", span(class = "no-trad", nome_fondo_esteso(con, f)), "»."), type = "message")
    })
  }
  observeEvent(input$fondo_attivo, ignoreInit = TRUE, attiva_fondo(input$fondo_attivo))

  # Il fondo a schermo, scritto per esteso sul pulsante della barra, e la finestra per sceglierne un altro.
  output$fondo_etichetta <- renderUI({
    g <- fondi_noti(); f <- fondo_attivo()
    if (.vuoto(f)) return(span("(tutti i fondi)"))
    i <- match(f, g$id)
    span(class = "no-trad", etichetta_fondi(con, f, if (is.na(i)) 0L else g$n[i]))
  })
  observeEvent(input$fondo_apri, {
    g <- fondi_noti(); f <- if (.vuoto(fondo_attivo())) "" else fondo_attivo()
    riga <- function(id, titolo, sotto, n, cerca) tags$button(type = "button",
      class = paste("riga-fondo list-group-item list-group-item-action d-flex justify-content-between align-items-center gap-2", if (identical(id, f)) "active"),
      `data-cerca` = tolower(cerca), onclick = sprintf("Shiny.setInputValue('fondo_scelto', '%s', {priority: 'event'});", id),
      div(div(class = "fw-semibold no-trad", titolo), if (nzchar(sotto)) div(class = "small opacity-75 no-trad", sotto)),
      if (!is.na(n)) span(class = "badge text-bg-secondary", n))
    righe <- lapply(seq_len(nrow(g)), function(i) riga(g$id[i], con_parola_fondo(g$fondo[i]), g$archivio[i], g$n[i], paste(g$fondo[i], g$archivio[i])))
    showModal(modalDialog(title = "Scegli il fondo", easyClose = TRUE, size = "m", footer = modalButton("Chiudi"),
      if (nrow(g) > 8) tags$input(type = "search", class = "form-control mb-2", placeholder = "Cerca un fondo\u2026",
        oninput = "var q=this.value.toLowerCase();this.closest('.modal-body').querySelectorAll('.riga-fondo').forEach(function(r){if(!q||r.dataset.cerca.indexOf(q)>=0)r.style.removeProperty('display');else r.style.setProperty('display','none','important');});"),
      div(class = "list-group elenco-fondi-finestra", righe,
          riga("", "(tutti i fondi)", "", NA_integer_, "tutti i fondi"))))
  })
  observeEvent(input$fondo_scelto, ignoreInit = TRUE, { removeModal(); attiva_fondo(input$fondo_scelto) })
  list(fondo_attivo = fondo_attivo, fondi_noti = fondi_noti, attiva_fondo = attiva_fondo)
}
