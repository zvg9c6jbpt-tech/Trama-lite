# ─────────────────────────────────────────────────────────────
# R/pagine_server.R: il collegamento delle pagine Gestione, Home, Colonne e Luoghi con il resto dell'app
# Ogni pagina ha il suo modulo; qui si danno loro le azioni sul resto (aprire una scheda, tornare all'elenco, cambiare fondo).
# Va chiamato dopo tutti gli altri moduli: gli argomenti sono già definiti.
# ─────────────────────────────────────────────────────────────

pagine_server <- function(input, output, session, con,
                          anomalie_stato, aperto, apri_da_elenco, apri_nuovo, attiva_fondo, azzera_filtri_elenco, contesto_attuale, dati, filtro_schedatura, fondo_attivo, fusione_consentita, naviga, nuovo, produttore_predefinito, ricarica, richiesta, solo_errori, solo_incompleti) {
  gestione_server(input, output, session, con, fondo_attivo = function() fondo_attivo(),
                  ricarica = ricarica, naviga = function(azione) naviga(azione),
                  # Dopo aver cancellato un fondo o tutto: si sceglie di nuovo il fondo e si torna alla Home.
                  dopo_cancellazione = function(tipo, fondo) {
                    # Eliminato il fondo in uso: si passa al primo che resta (con «tutti i fondi» Qualità ricalcolava tutto).
                    if (tipo == "tutto") fondo_attivo(NULL)
                    else if (identical(fondo, fondo_attivo())) {
                      resto <- elenco_fondi(con)$id; resto <- resto[resto != fondo]      # l'id, non la denominazione: «fondo_attivo» è sempre un id
                      fondo_attivo(if (length(resto)) resto[1] else NULL)
                    }
                    aperto(NULL); nuovo(NULL); azzera_filtri_elenco()
                    nav_select("nav", "Home")
                  })
  home_server(input, output, session, con, dati = function() dati(), fondo_attivo = function() fondo_attivo(),
              ricarica = ricarica, attiva_fondo = function(f) attiva_fondo(f),
              riempimento = function() riempimento(),
              anomalie = function() anomalie_stato(),
              apri = function(id) apri_da_elenco(id),
              nuova = function() naviga(function() {
                apri_nuovo(valori_nuovo(contesto_attuale(), produttore = produttore_predefinito()))
                nav_select("nav", "Scheda")
              }),
              # «Continua»: la scheda nuova dopo l'ultima della busta, o la prima della busta aperta successiva.
              continua = function(dove) naviga(function() {
                apri_nuovo(valori_nuovo(contesto_attuale(), busta = dove$busta, fascicolo = dove$fascicolo,
                                        produttore = produttore_predefinito(), busta_suffisso = dove$suffisso_b))
                nav_select("nav", "Scheda")
              }),
              incompleti = function() {
                azzera_filtri_elenco(); solo_errori(FALSE); solo_incompleti(TRUE)
                session$sendCustomMessage("solo_incompleti", TRUE)
                nav_select("nav", "Fascicoli")
              },
              cerca = function(q) {
                azzera_filtri_elenco()
                updateTextInput(session, "cerca_fascicoli", value = q)
                nav_select("nav", "Fascicoli")
              })
  selezione_server(input, output, session, con, ricarica = ricarica,
                   # La scheda aperta fra quelle toccate si ricarica; se è stata eliminata, si chiude.
                   dopo_modifica = function(ids) if (!is.null(aperto()) && aperto() %in% ids) {
                     if (is.null(leggi_fascicolo(con, aperto()))) aperto(NULL) else richiesta(richiesta() + 1)
                   })
  # Schede con un valore in ciascuna delle 28 colonne (R/colonne.R).
  riempimento <- reactive({
    ricarica(); f <- fondo_attivo()
    if (.vuoto(f)) return(NULL)
    tryCatch(riempimento_colonne(con, f), error = function(e) NULL)   # un fondo senza schede non ha colonne da contare
  })
  colonne_server(input, output, session, con, fondo_attivo = function() fondo_attivo(), ricarica = ricarica,
                 riempimento = function() riempimento(),
                 mostra_schede = function(etichetta, ids) {
                   azzera_filtri_elenco()
                   session$sendCustomMessage("vista_elenco", "tabella")
                   filtro_schedatura(list(etichetta = etichetta, ids = ids))
                   nav_select("nav", "Fascicoli")
                 },
                 dopo_modifica = function(ids) if (!is.null(aperto()) && aperto() %in% ids) richiesta(richiesta() + 1))
  output$pronto_export <- renderUI(tagList(pronto_ui(riempimento()),
    if (!is.null(riempimento())) div(class = "mb-3", actionLink("pronto_dettaglio", "Colonna per colonna"))))
  observeEvent(input$pronto_dettaglio, { nav_select("nav", "Qualità"); nav_select("vista_qualita", "colonne") })
  luoghi_server(input, output, session, con, fondo_attivo = function() fondo_attivo(),
                 ricarica = ricarica, dati = function() dati(),
                 fusione_consentita = function() fusione_consentita())
}
