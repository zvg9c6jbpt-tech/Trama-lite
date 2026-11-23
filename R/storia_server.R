# ─────────────────────────────────────────────────────────────
# R/storia_server.R: la storia della scheda e «Annulla l'ultima modifica»
# `naviga(azione)` passa dall'avviso delle modifiche non salvate; `richiesta` fa riaprire il modulo della Scheda.
# ─────────────────────────────────────────────────────────────

storia_server <- function(input, output, session, con, aperto, ricarica, richiesta, naviga) {
  output$storia_scheda <- renderUI({
    ricarica()
    id <- aperto()
    s <- storia_fascicolo(con, id)
    if (!nrow(s)) return(NULL)
    quando <- format(as.POSIXct(s$quando, format = "%Y-%m-%dT%H:%M:%S"), "%d/%m/%Y %H:%M")
    tags$details(class = "impostazioni-scheda storia-scheda",
      tags$summary(sprintf("Storia della scheda (%d)", nrow(s))),
      div(class = "d-flex flex-wrap align-items-center gap-3 mt-2",
          actionButton("annulla_ultima", "Annulla l'ultima modifica", class = "btn-outline-secondary btn-sm"),
          span(class = "small text-muted", "Rimette la scheda com'era prima dell'ultimo salvataggio di questo elenco.")),
      tags$ol(class = "storia-elenco", lapply(seq_len(nrow(s)), function(i)
        # Un elemento per campo: in inglese ognuno si traduce per intero.
        tags$li(span(class = "storia-quando", quando[i]),
                span(class = "storia-campi", lapply(strsplit(s$campi[i], " \u00b7 ", fixed = TRUE)[[1]], function(k) span(class = "storia-campo", k)))))))
  })
  observeEvent(input$annulla_ultima, naviga(function() {
    id <- aperto()
    campi <- tryCatch(annulla_ultima_modifica(con, id), error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
    if (is.null(campi)) return()
    ricarica(ricarica() + 1)
    richiesta(richiesta() + 1)
    showNotification(tagList(span("Tornati com'erano:"), " ", lapply(campi, function(k) tagList(span(class = "storia-campo", k), " "))),
                     type = "message", duration = 6)
  }))
}
