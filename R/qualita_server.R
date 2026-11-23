# ─────────────────────────────────────────────────────────────
# R/qualita_server.R: il server della pagina Qualità
# I CONTROLLI (tabella, «a posto», clic che porta ai fascicoli), le grafie divergenti e la fusione delle grafie dei LUOGHI.
# Dipende da qualita.R e fusioni.R. Gli stati condivisi (ricarica, piani_versione, filtro_schedatura, aperto) arrivano dall'app.
# ─────────────────────────────────────────────────────────────
qualita_server <- function(input, output, session, con, fondo_attivo, fondi_noti, ricarica, piani_versione,
                           filtro_schedatura, aperto, azzera_filtri_elenco, naviga, fusione_consentita) {
  # ── qualità ───────────────────────────────────────────────

  # Qualità guarda il fondo su cui si lavora (21/09): con Napoli e Modena
  # insieme, i numeri dei due mescolati non descrivono nessuno dei due.
  output$qualita_ambito <- renderText({
    f <- fondo_attivo()
    g <- fondi_noti()
    if (.vuoto(f)) return(sprintf("tutti i %d fondi", max(1L, nrow(g))))
    i <- match(f, g$id)
    etichetta_fondi(con, f, if (is.na(i)) 0L else g$n[i])   # il nome del fondo, non il suo identificativo
  })

  # Il numero accanto a «Qualità» nel menu: i controlli con casi «da vedere» (non il lavoro di fondo).
  output$conta_qualita <- renderUI({
    ricarica(); piani_versione()
    n <- sum(qualita_ordinata(qualita(con, fondo_attivo()))$gruppo == "Da vedere")
    if (n > 0L) span(class = "nav-badge", title = sprintf("%d controlli con casi da vedere", n), n)
  })

  # Due tabelle: le anomalie («Da vedere») e il lavoro di fondo, che non è un errore e non entra nel numero del menu.
  tabella_qualita <- function(gruppo, tinta) renderDT({
    ricarica(); piani_versione()
    d <- qualita_ordinata(qualita(con, fondo_attivo())); d <- d[d$gruppo == gruppo, c("controllo", "casi", "nota"), drop = FALSE]
    tb <- datatable(d, rownames = FALSE, selection = "single", colnames = c("Controllo", "Casi", "Nota"),
                    options = list(dom = "t", pageLength = 30, ordering = FALSE))
    # I colori del tema, non due esadecimali (contrasto controllato il 5/10).
    formatStyle(tb, "casi", fontWeight = if (identical(gruppo, "Da vedere")) "bold" else "normal", color = tinta)
  })
  output$tab_qualita       <- tabella_qualita("Da vedere", "var(--bs-primary)")
  output$tab_qualita_fondo <- tabella_qualita("Lavoro di fondo, non anomalie", "var(--bs-secondary-color)")
  output$qualita_fondo_riquadro <- renderUI({
    ricarica(); piani_versione()
    if (!any(qualita_ordinata(qualita(con, fondo_attivo()))$gruppo == "Lavoro di fondo, non anomalie")) return(NULL)
    card(class = "mb-3 qualita-fondo",
      card_header(class = "d-flex justify-content-between align-items-center", "Lavoro di fondo",
                  span(class = "small text-muted fw-normal", "non sono anomalie: è la schedatura che manca")),
      card_body(fillable = FALSE, DTOutput("tab_qualita_fondo")))
  })

  # I controlli senza casi: una riga chiusa, non venti righe a zero (15/10).
  output$qualita_a_posto <- renderUI({
    ricarica(); piani_versione()
    ok <- qualita_ordinata(qualita(con, fondo_attivo()), a_posto = TRUE)
    if (!nrow(ok)) return(NULL)
    tags$details(class = "qualita-a-posto mt-2",
      tags$summary(sprintf("A posto: %d controlli senza casi", nrow(ok))),
      tags$ul(class = "small text-muted mb-0 mt-2", lapply(seq_len(nrow(ok)), function(i) tags$li(ok$controllo[i]))))
  })

  # Clic su una riga: i fascicoli dietro quel controllo, in Fascicoli (stessa fascetta «Mostra tutti» della mappa). I controlli che non sono insiemi di fascicoli lo dicono e basta.
  # Clic su una riga delle due tabelle.
  clic_qualita <- function(righe, gruppo, proxy) {
    i <- righe
    if (length(i) == 0) return(NULL)
    selectRows(dataTableProxy(proxy), NULL)
    d <- qualita_ordinata(qualita(con, fondo_attivo())); d <- d[d$gruppo == gruppo, , drop = FALSE]
    d$controllo[i]
  }
  osserva_qualita <- function(righe, gruppo, proxy) {
    controllo <- clic_qualita(righe, gruppo, proxy)
    if (is.null(controllo)) return()
    ids <- qualita_fascicoli(con, controllo, fondo_attivo())
    if (is.null(ids)) {
      showNotification("Questo controllo non riguarda singoli fascicoli: il dettaglio è nella nota o nei riquadri qui sotto.",
                       type = "message")
      return()
    }
    if (length(ids) == 0) {
      showNotification("Nessun fascicolo in questo controllo.", type = "message")
      return()
    }
    azzera_filtri_elenco()
    session$sendCustomMessage("vista_elenco", "tabella")
    filtro_schedatura(list(etichetta = paste("Qualità:", controllo), ids = ids))
    nav_select("nav", "Fascicoli")
  }
  observeEvent(input$tab_qualita_rows_selected, osserva_qualita(input$tab_qualita_rows_selected, "Da vedere", "tab_qualita"))
  observeEvent(input$tab_qualita_fondo_rows_selected, osserva_qualita(input$tab_qualita_fondo_rows_selected, "Lavoro di fondo, non anomalie", "tab_qualita_fondo"))

  .tab_grafie <- function(tabella) renderDT({
    ricarica()
    g <- grafie_vicine(con, tabella, fondo_attivo())
    if (nrow(g) == 0)
      return(datatable(data.frame(esito = "nessuna divergenza"), class = "senza-testa",
                       rownames = FALSE, colnames = "", selection = "none",
                       options = list(dom = "t", ordering = FALSE)))
    datatable(g, rownames = FALSE, selection = "none",
              colnames = c("Forme che collassano sulla stessa chiave", "n"),
              options = list(dom = "tp", pageLength = 8))
  })

  output$tab_grafie_p <- .tab_grafie("persone")
  output$tab_grafie_l <- .tab_grafie("luoghi")

  # ── fusione delle grafie divergenti ───────────────────────
  # Propone e basta: vince la forma più frequente (l'elenco sopra mostra quali si fondono, prima di premere). Un solo pulsante per tabella, tutte le proposte insieme.
  .fusione_ui <- function(tabella, bottone) renderUI({
    ricarica()
    pf <- proposta_fusione(con, tabella, fondo_attivo())
    if (nrow(pf) == 0) return(NULL)
    actionButton(bottone,
                 sprintf("Fondi %d grupp%s propost%s", nrow(pf),
                         if (nrow(pf) == 1) "o" else "i", if (nrow(pf) == 1) "o" else "i"),
                 class = "btn-outline-secondary btn-sm mb-2")
  })
  output$fusione_luoghi  <- .fusione_ui("luoghi",  "fondi_luoghi")

  # Si fondono solo i LUOGHI: una grafia sbagliata è un errore di trascrizione, non un'identità. Le persone non si toccano (Lite non dichiara identità).
  .applica_fusioni <- function(tabella, fondi) {
    if (!fusione_consentita()) return()
    pf <- proposta_fusione(con, tabella, fondo_attivo())
    gruppi <- nrow(pf)
    toccati <- 0L
    for (i in seq_len(gruppi))
      for (a in strsplit(pf$assorbite[i], " \\| ")[[1]])
        toccati <- toccati + fondi(pf$destinazione[i], a)
    ricarica(ricarica() + 1)
    piani_versione(piani_versione() + 1)
    showNotification(sprintf("Fus%s %d grupp%s · %d %s",
                             if (gruppi == 1) "o" else "i", gruppi,
                             if (gruppi == 1) "o" else "i", toccati,
                             "fascicoli riscritti"),
                     type = "message")
  }
  observeEvent(input$fondi_luoghi,  .applica_fusioni("luoghi",  function(d, a) unisci_luoghi(con, d, a)))
  invisible(NULL)
}
