# ─────────────────────────────────────────────────────────────
# R/estrazione_server.R: estrazione dal titolo: propone, non applica in blocco
# Solo sul titolo scritto a mano (conditionalPanel in modulo.R). Legge il titolo con estrai_da_titolo (R/estrazione.R) e riempie persone e luogo della Scheda.
# ─────────────────────────────────────────────────────────────

estrazione_server <- function(input, output, session) {
  observeEvent(input$estrai, {
    r <- estrai_da_titolo(input$titolo)

    if (r$tipo == "vuoto") {
      showNotification("Titolo vuoto: niente da estrarre.", type = "warning")
      return()
    }
    if (r$tipo == "nessuno") {
      showNotification("Il titolo non segue uno schema riconosciuto (circa/contro/su/di, o “Atti per diversi”): niente proposto.", type = "warning")
      return()
    }

    righe_esito <- list()

    if (r$tipo == "collettivo") {
      updateCheckboxInput(session, "identita_imputato_dubbia", value = TRUE)
      righe_esito <- c(righe_esito, list("titolo collettivo → identità delle persone segnata dubbia"))
    } else {
      if (length(r$nomi)) {
        # I nomi del titolo entrano fra le persone citate (forma indicizzata), senza distinzione di ruolo.
        attuali <- as.character(unlist(input$persona)); attuali <- attuali[!is.na(attuali) & nzchar(attuali)]
        nuovi <- unique(c(attuali, vapply(r$nomi, indicizza_nome, "", USE.NAMES = FALSE)))
        .manda_voci(session, "persona", nuovi)
        updateSelectizeInput(session, "persona", choices = nuovi, selected = nuovi)
        righe_esito <- c(righe_esito, list(sprintf("persona proposta: <strong>%s</strong>", htmltools::htmlEscape(r$nomi))))
      }
      if (length(r$alias))
        righe_esito <- c(righe_esito, list(sprintf("alias nel titolo (non inserito da solo): %s", htmltools::htmlEscape(paste(r$alias, collapse = "; ")))))
      if (length(r$soprannomi))
        righe_esito <- c(righe_esito, list(sprintf("soprannome nel titolo (non inserito da solo): %s", htmltools::htmlEscape(paste(r$soprannomi, collapse = "; ")))))
    }

    if (length(r$luoghi)) {
      if (!length(.luoghi_pieni(list(luoghi = input$luogo)))) {
        .manda_voci(session, "luogo", r$luoghi[1])
        updateSelectizeInput(session, "luogo", selected = r$luoghi[1])
      }
      righe_esito <- c(righe_esito, list(sprintf("luogo proposto: <strong>%s</strong>", htmltools::htmlEscape(paste(r$luoghi, collapse = ", ")))))
    }
    if (length(r$scarti))
      righe_esito <- c(righe_esito, list(sprintf("nel titolo ma scartato (non un nome né un luogo): %s", htmltools::htmlEscape(paste(r$scarti, collapse = "; ")))))

    output$esito_estrazione <- renderUI({
      div(class = "titolo-composto", lapply(righe_esito, function(x) div(HTML(paste0("• ", x)))))
    })
  })
}
