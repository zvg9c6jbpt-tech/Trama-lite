# ─────────────────────────────────────────────────────────────
# R/scheda_ui.R: la Scheda: testata, modulo e barra di salvataggio
# ─────────────────────────────────────────────────────────────

scheda_ui <- function(BARRA) nav_panel(
    title = tagList(icon("file-pen"), "Scheda"),
    value = "Scheda",
    # La testata va a capo se la finestra è stretta e resta in cima scorrendo.
    # Da sinistra: dove si torna, che cosa si scheda, a che punto è; a destra le schede nuove.
    div(
      class = "testa-scheda", style = BARRA,
      actionButton("torna_elenco", tagList(icon("arrow-left"), "Indietro ai Fascicoli"), class = "btn-pillola btn-sm",
                   title = "Torna all'elenco dei fascicoli"),
      uiOutput("intestazione_scheda", class = "testa-titolo"),
      # Seconda riga: posizione nell'elenco e campi fondamentali compilati o mancanti.
      div(class = "testa-stato",
          span(class = "testa-posizione", title = "Posizione nell'elenco di Fascicoli", icon("list-ol"), textOutput("posizione_scheda", inline = TRUE)),
          uiOutput("completezza_scheda", inline = TRUE)),
      # Le tre schede nuove stanno in un solo menu.
      div(
        class = "testa-crea dropdown barra-altro",
        tags$button(type = "button", class = "btn btn-outline-secondary btn-sm dropdown-toggle", `data-bs-toggle` = "dropdown",
                    `aria-expanded` = "false", title = "Crea un fascicolo nuovo", icon("plus"), "Nuovo fascicolo"),
        div(class = "dropdown-menu dropdown-menu-end",
          actionButton("svuota",      "Scheda vuota", class = "dropdown-item",
                       title = "Una scheda nuova nello stesso fondo, senza busta né numero"),
          actionButton("successivo",  "Fascicolo seguente", class = "dropdown-item",
                       title = "Una scheda nuova con il numero che segue, nella stessa busta"),
          actionButton("nuova_busta", "Nuova busta", class = "dropdown-item",
                       title = "Una scheda nuova nella prima busta libera del fondo, fascicolo 1")))
    ),
    div(class = "carta-scheda",
        uiOutput("modulo"),
        # Sezioni chiuse: i campi restano nella pagina.
        tags$details(class = "impostazioni-scheda",
          tags$summary("Impostazioni: piano di busta"),
        div(class = "d-flex flex-wrap gap-3 align-items-end mt-2",
            div(class = "mb-3",
                actionButton("piano", "Piano di busta\u2026", class = "btn-outline-secondary")),
            div(style = "display:none", textInput("profilo_visibile", NULL, "Generale")))),
        # Storia della scheda: ultime modifiche e un passo indietro.
        uiOutput("storia_scheda"),
        # In fondo: si salva e si passa alla scheda accanto nell'elenco.
        div(class = "barra-fondo-scheda",
            # Una sola barra: i pulsanti di spostamento salvano solo se c'è qualcosa da salvare (etichette nei data-*, scambiate dal messaggio 'modifiche').
            # In primo piano Salva e Prossimo; le altre destinazioni stanno nel menu «Altro».
            actionButton("salva", "Salva", class = "btn-primary btn-salva"),
            span(class = "stato-salvataggio align-self-center", role = "status", `aria-live` = "polite"),
            span(class = "barra-spazio"),
            actionButton("salva_prec", "\u2039 Precedente", class = "btn-outline-secondary",
                         `data-pulito` = "\u2039 Precedente", `data-sporco` = "\u2039 Salva e precedente",
                         title = "Il fascicolo prima di questo nell'elenco di Fascicoli"),
            actionButton("salva_succ", "Prossimo \u203a", class = "btn-outline-primary",
                         `data-pulito` = "Prossimo \u203a", `data-sporco` = "Salva e prossimo \u203a",
                         title = "Il fascicolo dopo questo nell'elenco di Fascicoli (Ctrl o Cmd + Invio)"),
            div(class = "dropup barra-altro",
                tags$button(type = "button", class = "btn btn-outline-secondary dropdown-toggle", `data-bs-toggle` = "dropdown",
                            `aria-expanded` = "false", title = "Altre azioni: prossimo incompleto, chiudi, annulla", icon("ellipsis"), "Altro"),
                div(class = "dropdown-menu dropdown-menu-end",
                    actionButton("salva_incompleto", "Prossimo incompleto", class = "dropdown-item",
                                 `data-pulito` = "Prossimo incompleto", `data-sporco` = "Salva \u2192 prossimo incompleto",
                                 title = "Il primo fascicolo dopo questo, nell'elenco, con meno di 4 campi fondamentali"),
                    # Finito di schedare: salva, se serve, e torna alla Home (Stefano, 2/10).
                    actionButton("salva_chiudi", "Chiudi", class = "dropdown-item",
                                 `data-pulito` = "Chiudi", `data-sporco` = "Salva e chiudi", `data-solo-modifiche` = "1",
                                 title = "Salva le modifiche, se ce ne sono, e torna alla Home"),
                    div(class = "dropdown-divider"),
                    actionButton("annulla_scheda", "Annulla", class = "dropdown-item",
                                 title = "Riporta la scheda com'era quando l'hai aperta")))))
  )
