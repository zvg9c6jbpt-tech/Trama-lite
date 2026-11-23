# ─────────────────────────────────────────────────────────────
# R/piano_server.R: il piano di busta
# Registra i fascicoli attesi di una busta; non crea schede. `piani_versione` fa aggiornare Qualità senza ricostruire il modulo.
# ─────────────────────────────────────────────────────────────

piano_server <- function(input, output, session, con, contesto_attuale, id_fondo_di, piani_versione) {
  # Registra i fascicoli attesi; non crea schede.
  piano_fondo    <- reactiveVal(NA_character_)

  observeEvent(input$piano, {
    ctx <- contesto_attuale()
    b   <- input$busta
    fid <- id_fondo_di(ctx)
    pb  <- if (.vuoto(fid) || .vuoto(b)) data.frame() else
           dbGetQuery(con, "SELECT primo, ultimo FROM piani_busta WHERE fondo_id = ? AND busta = ? AND busta_suffisso = ?",
                      list(fid, as.integer(b), if (.vuoto(input$busta_suffisso)) "" else input$busta_suffisso))
    piano_fondo(if (.vuoto(fid)) NA_character_ else fid)
    showModal(modalDialog(
      title = "Piano di busta",
      p(class = "text-muted small",
        "I fascicoli attesi in una busta. Non crea schede: ognuna nasce con Salva.",
        "Il piano dice che cosa manca e porta Prossimo al primo numero da schedare."),
      p(strong("Fondo:"), " ", span(class = "no-trad", if (.vuoto(ctx$fondo)) "—" else ctx$fondo)),
      layout_column_wrap(
        width = 1/4,
        numerico("piano_busta",  "Busta",         value = if (.vuoto(b)) NA else b, min = 1, step = 1),
        textInput("piano_suffisso", "Suffisso (-bis, -ter…)", value = if (.vuoto(input$busta_suffisso)) "" else input$busta_suffisso),
        numerico("piano_primo",  "Dal fascicolo", value = if (nrow(pb)) pb$primo  else NA, min = 1, step = 1),
        numerico("piano_ultimo", "Al fascicolo",  value = if (nrow(pb)) pb$ultimo else NA, min = 1, step = 1)
      ),
      uiOutput("piano_anteprima"),
      footer = tagList(modalButton("Annulla"),
                       actionButton("piano_registra", "Registra piano", class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  output$piano_anteprima <- renderUI({
    piani_versione()
    a <- anteprima_piano(con, piano_fondo(), input$piano_busta, input$piano_primo, input$piano_ultimo,
                         suffisso_b = trimws(input$piano_suffisso %||% ""))
    if (!a$ok) return(div(class = "alert alert-warning py-2 mb-0", a$messaggio))
    div(class = "alert alert-light py-2 mb-0",
        strong(sprintf("%d fascicol%s attes%s", a$attesi,
                       if (a$attesi == 1) "o" else "i", if (a$attesi == 1) "o" else "i")),
        sprintf(" · già schedati %d · ", length(a$schedati)),
        strong(sprintf("da schedare %d", length(a$mancanti))),
        if (length(a$mancanti)) div(class = "small text-muted", "mancano: ", .intervalli(a$mancanti)),
        if (length(a$fuori)) div(class = "small text-muted", "schede della busta fuori dal piano: ",
                                 .intervalli(a$fuori)),
        if (!is.na(a$sostituisce)) div(class = "small", "sostituisce il piano ", a$sostituisce))
  })

  observeEvent(input$piano_registra, {
    esito <- try(salva_piano(con, piano_fondo(), input$piano_busta,
                             input$piano_primo, input$piano_ultimo,
                             suffisso_b = trimws(input$piano_suffisso %||% "")), silent = TRUE)
    if (inherits(esito, "try-error")) {
      showNotification(conditionMessage(attr(esito, "condition")), type = "warning")
      return()
    }
    removeModal()
    piani_versione(piani_versione() + 1)
    showNotification(
      if (esito$primo == esito$ultimo)
        sprintf("Piano registrato: b. %s, fascicolo %d · da schedare %d", .busta_etichetta(esito$busta, esito$suffisso_b), esito$primo, length(esito$mancanti))
      else sprintf("Piano registrato: b. %s, fascicoli %d–%d · da schedare %d",
                   .busta_etichetta(esito$busta, esito$suffisso_b), esito$primo, esito$ultimo, length(esito$mancanti)),
      type = "message")
  })
}
