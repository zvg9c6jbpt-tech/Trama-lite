# ─────────────────────────────────────────────────────────────
# R/modifiche_server.R: modifiche non salvate: la fotografia dei campi, l'avviso e `naviga`
# Ogni cambio di scheda o di pagina passa da `naviga(azione)`: se ci sono modifiche, prima chiede. Ripristina anche le viste (filtri, busta).
# ─────────────────────────────────────────────────────────────

modifiche_server <- function(input, output, session, con, ricarica, fondo_attivo, aperto, busta_elenco, filtro_schedatura,
                             cerca, solo_incompleti, solo_errori, numero_caricamento, valori_iniziali, PROFILO_MODULO) {
  # «Modificato» rispetto a una fotografia dei campi scattata alla conferma del caricamento, non rispetto al database
  # (un campo vuoto lì diventa la prima voce del menu e risulterebbe cambiato).
  campi_modulo <- local({
    p <- PROFILO_MODULO; p <- p[p$dove != "derivato", , drop = FALSE]
    data.frame(id = vapply(seq_len(nrow(p)), function(i) id_input(p[i, ]), ""),
               etichetta = p$etichetta, stringsAsFactors = FALSE)
  })

  istantanea <- reactiveVal(NULL)
  observeEvent(input$caricato, {
    if (!identical(as.numeric(input$caricato), as.numeric(numero_caricamento()))) return()
    s <- setNames(lapply(campi_modulo$id, function(id) input[[id]]), campi_modulo$id)
    # I campi ripresi dalla scheda di riferimento restano «modifiche da salvare»: la fotografia li tiene vuoti.
    er <- attr(valori_iniziali(), "ereditati")
    for (k in intersect(names(er), names(s))) s[k] <- list(er[[k]])
    istantanea(s)
    # «Altri campi» aperto solo se la scheda appena caricata ne ha di compilati.
    session$sendCustomMessage("apri_altri", length(altri_compilati()) > 0)
    session$sendCustomMessage("apri_sezione", list(classe = "segnalazioni", aperto = length(segnalazioni_accese()) > 0))
    session$sendCustomMessage("apri_sezione", list(classe = "tortura-sentenza-sezione", aperto = tortura_sentenza_toccata()))
  })

  # Segnalazioni sulla fonte e tortura/sentenza: righe chiuse che dicono da sole che cosa c'è dentro (15/10).
  segnalazioni_accese <- reactive({
    ids <- c("identita_imputato_dubbia" = "identità dubbia", "reato_ignoto" = "reato ignoto", "luogo_ignoto" = "luogo ignoto",
             "non_procedimento" = "non è un procedimento")
    unname(ids[vapply(names(ids), function(id) isTRUE(as.logical(input[[id]])), logical(1))])
  })
  output$riepilogo_segnalazioni <- renderUI({
    e <- segnalazioni_accese()
    if (!length(e)) return(span(class = "nota-campo altri-vuoti", "\u2014 nessuna"))
    span(class = "altri-compilati", elenco_span(e, ", "))
  })
  outputOptions(output, "riepilogo_segnalazioni", suspendWhenHidden = FALSE)
  tortura_sentenza_toccata <- function() isolate({
    nd <- function(x) is.null(x) || !nzchar(as.character(x)) || identical(as.character(x), "n.d.")
    !nd(input$tortura) || !nd(input$sentenza) || isTRUE(as.logical(input$tortura_esaminata)) || isTRUE(as.logical(input$sentenza_esaminata))
  })
  output$riepilogo_tortura_sentenza <- renderUI({
    nd <- function(x) is.null(x) || !nzchar(as.character(x)) || identical(as.character(x), "n.d.")
    if (nd(input$tortura) && nd(input$sentenza)) return(span(class = "nota-campo altri-vuoti", "\u2014 n.d."))
    span(class = "altri-compilati", elenco_span(c(if (!nd(input$tortura)) "tortura", if (!nd(input$sentenza)) "sentenza"), ", "))
  })
  outputOptions(output, "riepilogo_tortura_sentenza", suspendWhenHidden = FALSE)

  # Campi di «Altri campi» compilati, nel titolo della sezione.
  ALTRI <- campi_altri(PROFILO_MODULO)
  altri_compilati <- reactive(ALTRI$etichetta[vapply(ALTRI$id, function(id) compilato(id, input[[id]]), logical(1))])
  output$riepilogo_altri <- renderUI({
    e <- altri_compilati()
    if (!length(e)) return(span(class = "nota-campo altri-vuoti", "\u2014 nessuno compilato"))
    span(class = "altri-compilati", title = paste(e, collapse = "\n"),
         sprintf("%d compilat%s:", length(e), if (length(e) == 1) "o" else "i"), " ",
         elenco_span(head(e, 3), ", "), if (length(e) > 3) "\u2026")
  })
  outputOptions(output, "riepilogo_altri", suspendWhenHidden = FALSE)

  # I cinque luoghi rapidi seguono il fondo di lavoro e i salvataggi.
  observe({
    ricarica()
    session$sendCustomMessage("luoghi_rapidi", as.list(luoghi_frequenti(con, fondo_attivo())))
  })

  campi_modificati <- reactive({
    s <- istantanea()
    if (is.null(s)) return(character(0))
    cambiati <- vapply(campi_modulo$id, function(id)
      !identical(.normalizza_valore(input[[id]]), .normalizza_valore(s[[id]])), logical(1))
    campi_modulo$etichetta[cambiati]
  })

  # Pallino su Salva, stato scritto accanto e conferma alla chiusura della pagina.
  observe(session$sendCustomMessage("modifiche", list(presenti = length(campi_modificati()) > 0,
                                                      nuova = is.null(aperto()))))

  # Se cambiando scheda c'è qualcosa da perdere si chiede; «Resta» è il pulsante evidenziato.
  # La fusione ricarica anche la scheda aperta senza eliminarne la bozza.
  fusione_consentita <- function() {
    if (length(campi_modificati())) {
      showNotification("Prima di unire le grafie, salva o annulla le modifiche al fascicolo aperto.", type = "warning")
      return(FALSE)
    }
    bozze <- as.character(unlist(input$note_biografiche_bozze))
    if (length(bozze)) {
      showNotification(paste0("Prima di unire le grafie, salva le note biografiche di: ",
                              paste(bozze, collapse = "; "), "."), type = "warning", duration = NULL,
                       id = "avviso_bozze_note")
      return(FALSE)
    }
    TRUE
  }

  in_attesa <- reactiveVal(NULL)
  naviga <- function(azione) {
    m <- campi_modificati()
    if (!length(m)) return(azione())
    in_attesa(azione)
    showModal(modalDialog(
      title = "Modifiche non salvate",
      p("In questa scheda hai cambiato:"),
      p(strong(elenco_span(head(m, 6), " · ")),
        if (length(m) > 6) sprintf(" e altr%s %d camp%s", if (length(m) == 7) "o" else "i", length(m) - 6,
                                   if (length(m) == 7) "o" else "i")),
      p(class = "text-muted small",
        "Se continui, queste modifiche vanno perse. Per tenerle, resta e premi Salva."),
      footer = tagList(actionButton("naviga_scarta", "Scarta e continua", class = "btn-outline-secondary"),
                       actionButton("naviga_resta", "Resta sulla scheda", class = "btn-primary")),
      easyClose = FALSE))
  }

  # Fotografia di Fascicoli che Indietro ricorda (R/cronologia.R); valori vuoti come stringa vuota o FALSE, mai NULL.
  stato_viste <- reactive({
    f <- filtro_schedatura(); b <- busta_elenco()
    list(
      Fascicoli = list(q = cerca(), inc = isTRUE(solo_incompleti()), err = isTRUE(solo_errori()), busta = if (is.null(b)) "" else b,
                       vista = if (identical(input$vista_elenco, "scaffale")) "scaffale" else "tabella",
                       filtro = if (is.null(f)) FALSE else list(etichetta = f$etichetta, ids = as.list(f$ids))))
  })
  observe(session$sendCustomMessage("stato_viste", stato_viste()))

  ripristina_viste <- function(v) {
    f <- v$Fascicoli
    if (is.list(f)) {
      q <- as.character(f$q %||% ""); cerca(q); updateTextInput(session, "cerca_fascicoli", value = q)
      solo_incompleti(isTRUE(f$inc)); session$sendCustomMessage("solo_incompleti", isTRUE(f$inc)); solo_errori(isTRUE(f$err))
      b <- as.character(unlist(f$busta))[1]; busta_elenco(if (length(b) && !is.na(b) && nzchar(b)) b else NULL)
      filtro_schedatura(if (is.list(f$filtro) && length(f$filtro$ids))
                          list(etichetta = as.character(f$filtro$etichetta), ids = as.character(unlist(f$filtro$ids))) else NULL)
      if (identical(f$vista, "scaffale") || identical(f$vista, "tabella")) session$sendCustomMessage("vista_elenco", f$vista)
    }
  }

  # Il tasto Indietro del browser e il pulsante «Indietro» (R/cronologia.R).
  cron <- cronologia_server(input, session, aperto, naviga,
    esiste   = function(id) {
      r <- dbGetQuery(con, "SELECT fondo_id FROM fascicoli WHERE id = ?", list(id))
      nrow(r) == 1 && (.vuoto(fondo_attivo()) || identical(r$fondo_id[1], fondo_attivo()))
    },
    caricato = reactive(input$caricato), ripristina = ripristina_viste)

  observeEvent(input$naviga_scarta, {
    azione <- in_attesa(); in_attesa(NULL); removeModal(); cron$scarta()
    if (is.function(azione)) azione()
  })

  observeEvent(input$naviga_resta, {
    in_attesa(NULL); removeModal(); cron$resta()
    # Se l'avviso è scattato cambiando fondo dal menu, lo si rimette sul fondo in uso.
    updateSelectizeInput(session, "fondo_attivo", selected = fondo_attivo())
  })

  list(campi_modulo = campi_modulo, in_attesa = in_attesa, campi_modificati = campi_modificati, fusione_consentita = fusione_consentita, istantanea = istantanea, naviga = naviga)
}
