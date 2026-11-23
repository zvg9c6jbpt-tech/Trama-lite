# ─────────────────────────────────────────────────────────────
# R/modulo_server.R: il modulo della Scheda: disegno, caricamento dei valori e conferma del browser
# `campi_modificati`, `imputati_prima`, `persone_prima` e `.indicizzati` stanno più avanti in app.R: arrivano come funzioni.
# ─────────────────────────────────────────────────────────────

modulo_server <- function(input, output, session, con, ricarica, fondo_attivo, aperto, nuovo, richiesta, lingua,
                          produttore_predefinito, fascicolo_caricato, campi_modificati, imputati_prima, persone_prima, .indicizzati, PROFILO_MODULO) {
  fascicolo_corrente <- reactive({
    ricarica()
    id <- aperto()
    if (is.null(id)) NULL else leggi_fascicolo(con, id)
  })
  
  # Busta e fascicolo per primi: se lo spazio manca, si tronca l'archivio.
  output$intestazione_scheda <- renderUI({
    f <- fascicolo_corrente()
    v <- if (is.null(f)) valori_iniziali() else f
    numeri <- if (.vuoto(v$busta)) NULL else posizione_scritta(v$sigla_busta, v$busta, v$sigla_fascicolo, v$fascicolo, v$fascicolo_suffisso, v$busta_suffisso)
    if (is.null(f))
      return(h5(class = "mb-0 text-truncate", strong("Nuovo fascicolo"),
                if (!is.null(numeri)) paste0(" · ", numeri)))
    dove <- paste(Filter(function(x) !.vuoto(x), list(f$archivio, f$fondo, f$serie)), collapse = ", ")
    h5(class = "mb-0 text-truncate",
       if (!is.null(numeri)) strong(class = "segnatura", numeri),
       # Archivio, fondo e serie sono nomi: non si traducono (5/10).
       span(class = "text-muted", if (!is.null(numeri)) " · ", span(class = "no-trad", dove)))
  })

  valori_iniziali <- reactive({
    richiesta()
    f <- fascicolo_corrente()
    if (!is.null(f)) {
      v <- valori_da_fascicolo(f, PROFILO_MODULO)
      # Una scheda esistente ma vuota riprende il contesto dalla scheda di riferimento del fondo.
      if (scheda_senza_contenuto(f) && !is.null(r <- contesto_di_riferimento(con, f))) {
        rv <- valori_da_fascicolo(r, PROFILO_MODULO)[CONTESTO_EREDITABILE]
        dare <- CONTESTO_EREDITABILE[vapply(CONTESTO_EREDITABILE, function(k) .vuoto(v[[k]]) && !.vuoto(rv[[k]]), TRUE)]
        if (length(dare)) { attr(v, "ereditati") <- lapply(v[dare], identity); v[dare] <- rv[dare] }
      }
      return(v)
    }
    if (!is.null(nuovo())) return(nuovo())
    u <- ultimo_modificato(con, fondo_attivo())
    valori_nuovo(if (!is.null(u)) valori_da_fascicolo(u, PROFILO_MODULO)[EREDITATI]
                 else if (!is.null(a <- contesto_archivio(con, fondo_attivo()))) a else list(),
                 produttore = produttore_predefinito())
  })
  
  # Ricalcolati a ogni salvataggio: un nome nuovo è subito suggeribile.
  suggerimenti <- reactive({ ricarica(); suggerimenti_da_db(con, fondo_attivo()) })

  # Il modulo si disegna UNA volta (isolate); poi se ne aggiornano i campi (aggiorna_modulo(), R/modulo.R).
  output$modulo <- renderUI(isolate(
    tagList(modulo_ui(identity,
                      valori = valori_iniziali(),
                      lingua = lingua(), p = PROFILO_MODULO,
                      rapidi = list(luogo = luoghi_frequenti(con, fondo_attivo()))))))
  # Il modulo sta in una scheda nascosta all'avvio: va disegnato subito, o la conferma del primo disegno arriva fuori tempo.
  outputOptions(output, "modulo", suspendWhenHidden = FALSE)

  # Valori dell'ultimo caricamento: distinguono un caricamento da un cambiamento dell'utente.
  caricati <- reactiveVal(NULL)
  invariati <- function(ids) all(vapply(ids, function(id) come_caricato(id, input[[id]], caricati()), logical(1)))

  # Cosa mostra davvero il modulo. Salva scrive i campi a schermo sul fascicolo selezionato: serve che il server abbia
  # caricato la selezione e che il browser ne abbia confermato l'arrivo, o si scrive la scheda sbagliata.
  caricato_per      <- reactiveVal(NULL)
  numero_caricamento <- reactiveVal(0)
  modulo_pronto <- function()
    identical(caricato_per(), list(aperto(), richiesta())) &&
      identical(as.numeric(input$caricato), as.numeric(numero_caricamento()))

  # Un aggiornamento da altrove (`ricarica()`) non deve sovrascrivere una bozza della stessa scheda.
  # `atteso` resta la fotografia d'apertura, così un Salva su scheda cambiata viene respinto; `dopo_salva` segna il ricaricamento del Salva.
  dopo_salva <- reactiveVal(FALSE)
  observeEvent(list(valori_iniziali(), suggerimenti()), {
    if (!isTRUE(isolate(dopo_salva())) && identical(isolate(caricato_per()), list(isolate(aperto()), isolate(richiesta()))) &&
        length(isolate(campi_modificati()))) {
      showNotification("I dati sono cambiati altrove: le tue modifiche a questa scheda restano. Salvando, Trama controlla che la scheda non sia cambiata nel frattempo.",
                       type = "warning", duration = 8, id = "bozza_conservata")
      return()
    }
    dopo_salva(FALSE)
    v <- valori_iniziali()
    aggiorna_modulo(session, v, suggerimenti(), isolate(lingua()), p = PROFILO_MODULO, fondo = isolate(fondo_attivo()))
    caricati(v)
    fascicolo_caricato(isolate(fascicolo_corrente()))
    imputati_prima(.indicizzati(v[["imputati"]]))
    persone_prima({ x <- as.character(unlist(v[["persona"]])); x[!is.na(x) & nzchar(x)] })
    output$esito_estrazione <- renderUI(NULL)   # l'esito dell'estrazione è del fascicolo precedente
    caricato_per(list(aperto(), richiesta()))
    primo <- numero_caricamento() == 0
    numero_caricamento(numero_caricamento() + 1)
    # Il primo caricamento è il disegno del modulo: lo segnala il browser a campi collegati (gestore 'shiny:value').
    # La conferma parte dopo il ciclo (onFlushed): sendCustomMessage arriva prima degli aggiornamenti dei campi.
    n <- numero_caricamento()
    if (!primo) session$onFlushed(function() session$sendCustomMessage("caricato", n), once = TRUE)
  })

  observeEvent(lingua(), traduci_modulo(session, input, lingua(), p = PROFILO_MODULO), ignoreInit = TRUE)

  list(fascicolo_corrente = fascicolo_corrente, valori_iniziali = valori_iniziali, suggerimenti = suggerimenti, caricati = caricati, invariati = invariati, caricato_per = caricato_per, numero_caricamento = numero_caricamento, modulo_pronto = modulo_pronto, dopo_salva = dopo_salva)
}
