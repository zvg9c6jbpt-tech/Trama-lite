# ─────────────────────────────────────────────────────────────
# R/qualita_ui.R: Qualità: controlli e colonne
# ─────────────────────────────────────────────────────────────

qualita_ui <- function() nav_panel(
    title = tagList(icon("shield-halved"), "Qualit\u00e0", uiOutput("conta_qualita", inline = TRUE)),
    value = "Qualit\u00e0",
    titolo_pagina("Qualit\u00e0"),
    # Carte alte quanto il contenuto: card_body(fillable = FALSE) e heights_equal = "row".
    # Controlli a tutta larghezza sopra; sotto, le grafie divergenti.
    navset_pill(id = "vista_qualita",
    nav_panel("Controlli", value = "controlli", tagList(
    card(class = "mb-3",
      card_header(class = "d-flex justify-content-between align-items-center",
                  "Anomalie: da vedere",
                  span(class = "small text-muted fw-normal",
                       textOutput("qualita_ambito", inline = TRUE),
                       " \u00b7 clic su una riga: i suoi fascicoli in Fascicoli")),
      card_body(fillable = FALSE, DTOutput("tab_qualita"), uiOutput("qualita_a_posto"))
    ),
    # Il lavoro di fondo (schedatura che manca) sta a parte: non è un'anomalia e non conta nel numero del menu.
    uiOutput("qualita_fondo_riquadro"),
    layout_column_wrap(
      width = 1, heights_equal = "row", class = "griglia-pagina",

      card(
        card_header("Grafie divergenti"),
        card_body(fillable = FALSE,
          div(class = "text-muted small mt-2 mb-1", "Persone"),
          DTOutput("tab_grafie_p"),
          div(class = "text-muted small mt-3 mb-1", "Luoghi"),
          DTOutput("tab_grafie_l"),
          uiOutput("fusione_luoghi"))
      )
    ))),
    # Per colonna (7/10, R/colonne.R): pronto per l'export e valori controllati.
    nav_panel("Per colonna", value = "colonne", colonne_pagina_ui()))
  )
