# ─────────────────────────────────────────────────────────────
# R/preferenze_server.R: tema chiaro/scuro e lingua del vocabolario
# Scuro e English si ricordano nel database fra una sessione e l'altra. Restituisce `lingua()` ("it"/"en").
# ─────────────────────────────────────────────────────────────

preferenze_server <- function(input, session, con) {
  # ── tema chiaro/scuro ────────────────────────────────────────
  observe(session$setCurrentTheme(tema_app(if (isTRUE(input$scuro)) "scuro" else "chiaro"))) |>
    bindEvent(input$scuro, ignoreInit = TRUE)

  # Scuro e English si ricordano nel database fra una sessione e l'altra; si ripristinano all'avvio.
  if (identical(leggi_impostazione(con, "scuro"), "1"))   bslib::update_switch("scuro", value = TRUE)
  if (identical(leggi_impostazione(con, "inglese"), "1")) bslib::update_switch("inglese", value = TRUE)
  observeEvent(input$scuro,   scrivi_impostazione(con, "scuro",   if (isTRUE(input$scuro)) "1" else "0"),   ignoreInit = TRUE)
  observeEvent(input$inglese, scrivi_impostazione(con, "inglese", if (isTRUE(input$inglese)) "1" else "0"), ignoreInit = TRUE)

  # ── vocabolario in inglese ─────────────────
  # Solo il vocabolario controllato; il database resta italiano (VOCABOLARI_EN, R/i18n.R).
  lingua <- reactive(if (isTRUE(input$inglese)) "en" else "it")
  lingua
}
