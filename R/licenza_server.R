# ─────────────────────────────────────────────────────────────
# R/licenza_server.R: licenza e attribuzione dei dati
# Chi ha schedato e per quale progetto; la fonte dei dati è del fondo.
# ─────────────────────────────────────────────────────────────

licenza_server <- function(input, output, session, con, fondo_attivo) {
  # ── licenza e attribuzione dei dati ───────────
  # Chi ha schedato e per quale progetto: entrano nelle proprietà dei fogli e nel backup.
  local({
    a <- attribuzione_dati(con)
    updateTextInput(session, "attribuzione_schedatore", value = a$schedatore)
    updateTextInput(session, "attribuzione_progetto", value = a$progetto)
  })
  attribuzione_versione <- reactiveVal(0)
  # La fonte dei dati è del fondo: cambiando fondo la casella mostra la sua.
  observeEvent(fondo_attivo(), {
    updateTextInput(session, "fonte_fondo", value = if (.vuoto(fondo_attivo())) "" else fonte_del_fondo(con, fondo_attivo()))
  }, ignoreNULL = FALSE)
  observeEvent(input$fonte_fondo, {
    f <- fondo_attivo(); if (.vuoto(f)) return(invisible())
    v <- trimws(if (is.null(input$fonte_fondo)) "" else input$fonte_fondo)
    if (identical(v, fonte_del_fondo(con, f))) return(invisible())
    tryCatch({ imposta_fonte_fondo(con, f, v); attribuzione_versione(attribuzione_versione() + 1) },
             error = function(e) showNotification(conditionMessage(e), type = "error"))
  }, ignoreInit = TRUE)
  for (k in c("attribuzione_schedatore", "attribuzione_progetto")) local({
    k <- k
    observeEvent(input[[k]], {
      v <- trimws(if (is.null(input[[k]])) "" else input[[k]])
      if (!identical(v, attribuzione_dati(con)[[sub("attribuzione_", "", k)]])) {
        scrivi_impostazione(con, k, v); attribuzione_versione(attribuzione_versione() + 1)
      }
    }, ignoreInit = TRUE)
  })
  output$attribuzione_riga <- renderUI({
    attribuzione_versione()
    r <- riga_attribuzione(attribuzione_dati(con))
    fo <- if (.vuoto(fondo_attivo())) "" else fonte_del_fondo(con, fondo_attivo())
    div(class = "small",
        if (nzchar(r)) tagList(span(class = "text-muted", "Nel file: "), span(class = "no-trad", r))
        else span(class = "text-muted", "Nessuna attribuzione: i file diranno solo la licenza."),
        if (nzchar(fo)) tagList(br(), span(class = "text-muted", "Fonte dei dati: "), span(class = "no-trad", fo)))
  })
}
