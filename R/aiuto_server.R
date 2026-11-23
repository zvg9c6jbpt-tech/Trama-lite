# ─────────────────────────────────────────────────────────────
# R/aiuto_server.R: le scorciatoie da tastiera e l'apertura della cartella dei dati
# ─────────────────────────────────────────────────────────────

aiuto_server <- function(input, output, session, PERCORSO_DB) {
  # Le scorciatoie c'erano ma non erano scritte da nessuna parte (2/10).
  observeEvent(input$aiuto_tasti, showModal(modalDialog(
    title = "Scorciatoie da tastiera e gesti", easyClose = TRUE, footer = modalButton("Chiudi"),
    tags$dl(class = "aiuto-tasti",
      tags$dt("Scheda"),
      tags$dd(tags$kbd("Ctrl"), " o ", tags$kbd("Cmd"), " + ", tags$kbd("Invio"), ": salva, se ci sono modifiche, e passa al prossimo fascicolo."),
      tags$dd(tags$kbd("Tab"), ": passa da un gruppo di pillole al successivo; ", tags$kbd("\u2190"), " ", tags$kbd("\u2192"), ": da una pillola all'altra; ", tags$kbd("1"), "\u2026", tags$kbd("9"), ": sceglie la pillola con quel numero."),
      tags$dd("Estremi cronologici: entrando nel campo compare l'anno del fascicolo precedente, già selezionato; la prima cifra lo sostituisce."),
      tags$dt("Persone, luoghi e gli altri campi a etichette"),
      tags$dd(tags$kbd("Invio"), ": aggiunge ciò che hai scritto; ", tags$kbd(";"), ": separa più voci."),
      tags$dd("Doppio clic su un'etichetta: la rimette nella casella per correggerla. La \u00d7 la toglie."),
      tags$dd("Un nome scritto senza virgola («paolo gargiulo») diventa «Gargiulo, Paolo»; con la virgola resta nell'ordine scritto."),
      tags$dt("Fascicoli"),
      tags$dd("Clic su una riga: apre la scheda. La casella a sinistra spunta soltanto."),
      tags$dd("Ricerca: campo:valore (reato:eresia anno:1560-1565); i valori con spazi fra virgolette.")))))

  observeEvent(input$mostra_cartella, {
    cartella <- if (identical(input$mostra_cartella$tipo, "export")) cartella_export() else file.path(dirname(PERCORSO_DB), "backup")
    tryCatch(apri_cartella(cartella), error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
  observeEvent(input$apri_cartella_dati, tryCatch(apri_cartella(dirname(PERCORSO_DB)),
    error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)))
}
