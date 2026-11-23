# ─────────────────────────────────────────────────────────────
# R/errori_import_server.R: la finestra «Errori e discordanze» dell'import
# Righe respinte, nomi e luoghi da decidere, discordanze: si apre dall'esito dell'import o della pulizia. Restituisce `elenco_errori` (i collaudi lo leggono). `e_pulisci` è la funzione di import_server che dice se si sta solo pulendo.
# ─────────────────────────────────────────────────────────────

errori_import_server <- function(input, output, session, risultato_import, risultato_pulito, e_pulisci) {
  .e_pulisci <- e_pulisci
  # Finestra «Errori e discordanze»: righe respinte, nomi e luoghi da decidere, discordanze.
  elenco_errori <- reactive(elenco_errori_import(esito = risultato_import(), pulito = if (.e_pulisci()) risultato_pulito() else NULL))
  observeEvent(input$apri_errori_import, {
    el <- elenco_errori()
    tabella <- function(d, id, vuoto) if (nrow(d)) DTOutput(id) else p(class = "text-muted small", vuoto)
    showModal(modalDialog(
      title = "Errori e discordanze", size = "xl", easyClose = TRUE,
      p(class = "small text-muted", "Tutto ciò che l'import non ha potuto decidere da solo, in un posto: le righe respinte col loro motivo, i nomi e i luoghi da decidere e le discordanze fra le colonne di una riga. Si scarica anche in Excel."),
      tabsetPanel(
        tabPanel(sprintf("Righe respinte (%d)", nrow(el$respinte)), div(class = "mt-2", tabella(el$respinte, "errori_respinte", "Nessuna riga respinta."))),
        tabPanel(sprintf("Da decidere (%d)", nrow(el$da_decidere)), div(class = "mt-2", tabella(el$da_decidere, "errori_decidere", "Niente da decidere."),
          if (nrow(el$da_decidere) > 0) actionButton("errori_per_colonna", tagList(icon("table-columns"), "Rivedi per colonna"), class = "btn-sm btn-outline-secondary mt-2"))),
        if (nrow(el$luoghi_tolti)) tabPanel(sprintf("Luoghi tolti (%d)", nrow(el$luoghi_tolti)), div(class = "mt-2", DTOutput("errori_tolti"),
          p(class = "small text-muted mt-2", "Luoghi eliminati dalle schede con «Elimina» nella finestra di controllo: sono già fuori dal database."))),
        if (nrow(el$luoghi_scelti)) tabPanel(sprintf("Luoghi scelti (%d)", nrow(el$luoghi_scelti)), div(class = "mt-2", DTOutput("errori_scelti"),
          p(class = "small text-muted mt-2", "Omonimi decisi nella finestra di controllo: le celle sono rimaste come scritte, qui c'è il luogo scelto con identificativo e coordinate."))),
        tabPanel(sprintf("Discordanze (%d)", nrow(el$discordanze)), div(class = "mt-2", tabella(el$discordanze, "errori_discordanze", "Nessuna discordanza."),
          if (nrow(el$discordanze) > 0 && !.e_pulisci()) p(class = "small text-muted mt-2", "Si correggono con «Scarica pulito, senza importare»: nel database sono entrate com'erano scritte.")))),
      footer = tagList(downloadButton("scarica_errori_import", "Scarica l'elenco (.xlsx)"), modalButton("Chiudi"))))
  })
  mostra_tab <- function(d, colnames) datatable(d, rownames = FALSE, selection = "none", colnames = colnames, options = list(dom = "tp", pageLength = 8))
  output$errori_respinte    <- renderDT(mostra_tab(elenco_errori()$respinte, c("Riga", "Segnatura", "Motivo")))
  output$errori_decidere    <- renderDT(mostra_tab(elenco_errori()$da_decidere, c("Colonna", "Valore", "Schede")))
  output$errori_tolti      <- renderDT(mostra_tab(elenco_errori()$luoghi_tolti, c("Riga", "Colonna", "Prima", "Dopo")))
  output$errori_scelti     <- renderDT(mostra_tab(elenco_errori()$luoghi_scelti, c("Riga", "Colonna", "Luogo", "Scelta", "Wikidata", "Coordinate")))
  output$errori_discordanze <- renderDT(mostra_tab(elenco_errori()$discordanze, c("Riga", "Colonna", "Correzione")))
  output$scarica_errori_import <- downloadHandler(
    filename = function() paste0("errori-e-discordanze-", format(Sys.Date()), ".xlsx"),
    content  = function(file) scrivi_errori_xlsx(elenco_errori(), file))
  observeEvent(input$errori_per_colonna, { removeModal(); nav_select("nav", "Qualità"); nav_select("vista_qualita", "colonne") })
  list(elenco_errori = elenco_errori)
}
