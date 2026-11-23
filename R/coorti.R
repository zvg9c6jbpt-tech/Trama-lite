# ─────────────────────────────────────────────────────────────
# R/coorti.R: le coorti: ricerche salvate con un nome
# L'elenco Fascicoli ha la sua barra e il suo pulsante «Salva». `coorti_versione` resta in app.R: la fanno avanzare anche il ripristino e il cambio di fondo.
# ─────────────────────────────────────────────────────────────

coorti_server <- function(input, output, session, con, coorti_versione) {
  # Le stesse coorti sopra la tabella Fascicoli: qui riempiono la sua ricerca.
  output$coorti_elenco <- renderUI({
    coorti_versione()
    co <- elenco_coorti(con)
    if (nrow(co) == 0) return(NULL)
    tagList(span(class = "small text-muted", "Coorti:"), lapply(seq_len(nrow(co)), function(i)
      span(class = "coorte-chip",
           a(href = "#", title = paste("Applica:", co$query[i]),
             onclick = sprintf("Shiny.setInputValue('coorte_elenco_clic', %d, {priority:'event'}); return false;", co$id[i]),
             co$nome[i]),
           a(href = "#", class = "coorte-chip-x", title = "Elimina",
             onclick = sprintf("Shiny.setInputValue('coorte_elimina', %d, {priority:'event'}); return false;", co$id[i]),
             "\u00d7"))))
  })
  observeEvent(input$coorte_elenco_clic, {
    co <- elenco_coorti(con)
    q  <- co$query[co$id == input$coorte_elenco_clic]
    if (length(q)) updateTextInput(session, "cerca_fascicoli", value = q)
  })

  observeEvent(input$coorte_elimina, {
    elimina_coorte(con, input$coorte_elimina)
    coorti_versione(coorti_versione() + 1)
  })

  # La ricerca da salvare: quella della tabella Fascicoli.
  coorte_da_salvare <- reactiveVal("")
  apri_salva_coorte <- function(q) {
    if (.vuoto(q)) {
      showNotification("Scrivi prima una ricerca da salvare.", type = "warning")
      return()
    }
    coorte_da_salvare(q)
    showModal(modalDialog(
      title = "Salva questa ricerca come coorte",
      p(class = "text-muted small", tags$code(q)),
      textInput("coorte_nome_nuovo", "Nome"),
      footer = tagList(modalButton("Annulla"),
                       actionButton("coorte_conferma", "Salva", class = "btn-primary")),
      easyClose = TRUE
    ))
  }
  observeEvent(input$salva_coorte_elenco, apri_salva_coorte(input$cerca_fascicoli))

  observeEvent(input$coorte_conferma, {
    if (.vuoto(input$coorte_nome_nuovo)) return()
    salva_coorte(con, input$coorte_nome_nuovo, coorte_da_salvare())
    removeModal()
    coorti_versione(coorti_versione() + 1)
  })
}
