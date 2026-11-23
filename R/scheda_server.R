# ─────────────────────────────────────────────────────────────
# R/scheda_server.R: la Scheda mentre la scrivi: riepiloghi, proposta d'anno e regesto
# Collocazione e soggetti in sola lettura; un valore diverso dal fascicolo precedente si evidenzia.
# ─────────────────────────────────────────────────────────────

scheda_server <- function(input, output, session, con, ricarica, fondo_attivo, aperto, id_fondo_di, invariati, scheda_dal_modulo, PROFILO_MODULO) {
  # Collocazione e soggetti in sola lettura; un valore diverso dal fascicolo precedente si evidenzia. I campi si aprono con «modifica».
  fascicolo_prima <- reactive({
    ricarica()
    # Si confronta col fondo di lavoro, non con quello scritto nel campo.
    fascicolo_precedente(con, if (.vuoto(fondo_attivo())) id_fondo_di(list(archivio = input$archivio, fondo = input$fondo)) else fondo_attivo(),
                         input$busta, input$fascicolo,
                         escludi = if (is.null(aperto())) NA_character_ else aperto(),
                         suffisso = input$fascicolo_suffisso, suffisso_b = input$busta_suffisso)
  })
  output$riepilogo_collocazione <- renderUI({
    testo <- function(x) { x <- as.character(unlist(x)); x <- x[!is.na(x) & nzchar(trimws(x))]
                           if (length(x)) paste(x, collapse = ", ") else "" }
    voci <- list(
      Archivio      = list(ora = input$archivio, prima = "archivio"),
      Fondo         = list(ora = input$fondo, prima = "fondo"),
      Produttore    = list(ora = input$soggetto, prima = "produttori"),
      Conservazione = list(ora = input$luogo_conservazione, prima = "luogo_conservazione"),
      Lingua        = list(ora = input$lingua, prima = "lingue"),
      Materiale     = list(ora = c(input$definizione, input$supporto), prima = c("definizione", "supporto")))
    pf <- fascicolo_prima()
    righe <- lapply(names(voci), function(k) {
      v <- testo(voci[[k]]$ora)
      diverso <- !is.null(pf) && !identical(tolower(v), tolower(testo(lapply(voci[[k]]$prima, function(c) pf[[c]]))))
      # Sono dati (archivio, fondo, lingue…): solo corrispondenze intere.
      div(class = "coppia", tags$dt(k), tags$dd(class = paste(c("dato", if (diverso) "diverso"), collapse = " "), if (nzchar(v)) v else "\u2014"))
    })
    n_div <- if (is.null(pf)) 0 else sum(vapply(names(voci), function(k)
      !identical(tolower(testo(voci[[k]]$ora)), tolower(testo(lapply(voci[[k]]$prima, function(c) pf[[c]])))), logical(1)))
    da <- if (!is.null(pf)) sprintf("%s%s%s %s%s%s", sigla_busta(pf$sigla_busta), pf$busta, if (.vuoto(pf$busta_suffisso)) "" else pf$busta_suffisso, sub("fasc\\.", "f.", sigla_fascicolo(pf$sigla_fascicolo)), pf$fascicolo, pf$fascicolo_suffisso)
    div(class = "riepilogo-collocazione",
        div(class = "riepilogo-testa", span("Collocazione e soggetti"), span(class = "riepilogo-modifica", "Modifica \u270e")),
        tags$dl(righe),
        div(class = "nota-campo mb-0",
            if (is.null(da)) { if (.vuoto(input$busta) || .vuoto(input$fascicolo)) "Indica busta e fascicolo per il confronto con il precedente."
                               else "Primo fascicolo del fondo." }
            else if (n_div == 0) sprintf("Uguali al fascicolo precedente (%s).", da)
            # Due frasi intere, non un plurale a pezzi: così si traducono (1/10).
            else if (n_div == 1) sprintf("1 valore diverso dal fascicolo precedente (%s), evidenziato.", da)
            else sprintf("%d valori diversi dal fascicolo precedente (%s), evidenziati.", n_div, da)))
  })
  # La data topica segue il soggetto produttore: si aggiungono le città mancanti, niente si toglie. Solo per cambi dell'utente.
  observeEvent(input$soggetto, {
    if (invariati("soggetto")) return()
    nuova <- data_topica_con_produttori(input$luogo_produzione, input$soggetto)
    if (is.na(nuova) || identical(nuova, as.character(input$luogo_produzione))) return()
    .manda_voci(session, "luogo_produzione", nuova)
    updateSelectizeInput(session, "luogo_produzione", selected = nuova)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  output$riepilogo_data_topica <- renderUI({
    v <- input$luogo_produzione
    dal_produttore <- !.vuoto(v) && length(citta_da_produttore(input$soggetto)) &&
      all(citta_da_produttore(input$soggetto) %in% trimws(strsplit(v, "|", fixed = TRUE)[[1]]))
    span(class = "nota-campo", "Data topica: ", strong(if (.vuoto(v)) "\u2014" else gsub("|", ", ", v, fixed = TRUE)),
         if (dal_produttore) span(class = "text-muted", " (dal soggetto produttore)"), " \u00b7 ",
         span(class = "link-cambia", if (.vuoto(v)) "Aggiungi" else "Cambia"))
  })

  # I 4 campi fondamentali (completezza_core, R/ricerca.R) e i quattro stati di tortura e sentenza (stato_esame).
  stato_esame_ui <- function(valore, esaminata) {
    s <- stato_esame(valore, esaminata)
    span(class = paste("stato-esame", s$codice), role = "status",
         title = switch(s$codice,
           da_esaminare = "n.d. senza spunta: nessuno ha ancora guardato la fonte",
           muta         = "n.d. con la spunta: la fonte è stata guardata e non lo dice",
           NULL),
         span(class = "simbolo", `aria-hidden` = "true", s$simbolo), s$testo)
  }
  output$stato_tortura  <- renderUI(stato_esame_ui(input$tortura,  input$tortura_esaminata))
  output$stato_sentenza <- renderUI(stato_esame_ui(input$sentenza, input$sentenza_esaminata))

  output$completezza_scheda <- renderUI({
    pieno <- function(x) !.vuoto(x) && any(nzchar(trimws(as.character(x))))
    chi <- pieno(input$persona) || pieno(input$imputati) || pieno(input$titolo)
    anno <- pieno(input$estremo_remoto)
    dove <- pieno(input$luogo)
    rea <- pieno(input$reato)
    n <- sum(chi, anno, dove, rea)
    manca <- core_mancanti(chi, anno, dove, rea)
    tagList(
      span(class = "completezza-scheda",
           title = sprintf("%d camp%s fondamental%s su 4 compilat%s. Compilato non vuol dire verificato sulla fonte.", n,
                           if (n == 1) "o" else "i", if (n == 1) "e" else "i", if (n == 1) "o" else "i"),
           span(class = "completezza-barra", span(style = sprintf("width:%d%%", n * 25L),
                                                  class = if (n == 4) "piena")),
           span(class = "completezza-testo", if (n == 4) "Completo" else sprintf("%d su 4", n))),
      if (nrow(manca) > 0)
        span(class = "completezza-manca small", "Manca: ",
             tagList(lapply(seq_len(nrow(manca)), function(i) tagList(
               if (i > 1) " · ",
               tags$a(href = "#", class = "manca-campo", title = "Vai al campo",
                      onclick = sprintf("tramaVai('%s'); return false;", manca$campo[i]),
                      manca$etichetta[i]))))))
  })

  # Anno del fascicolo precedente, proposto per l'estremo remoto (JS in R/modulo.R).
  proposta_anno <- reactive({
    ricarica()
    anno_precedente(con, id_fondo_di(list(archivio = input$archivio, fondo = input$fondo)), input$busta, input$fascicolo,
                    escludi = if (is.null(aperto())) NA_character_ else aperto(),
                    suffisso = input$fascicolo_suffisso, suffisso_b = input$busta_suffisso)
  })
  observe(session$sendCustomMessage("proposta_anno", if (is.null(proposta_anno())) "" else proposta_anno()$anno))
  output$proposta_anno <- renderUI({
    a <- proposta_anno()
    if (is.null(a) || !.vuoto(input$estremo_remoto) || isTRUE(input$data_nd)) return(NULL)
    # Il pulsante sta in una riga sua sotto i campi, separata da «Aggiungi mese e giorno».
    div(class = "proposta-anno",
        actionLink("usa_anno_precedente", sprintf("Usa %d", a$anno), class = "pillola"),
        span(class = "small text-muted", paste0("come il precedente (", a$da, ") \u2014 anche entrando nel campo")))
  })
  observeEvent(input$usa_anno_precedente, {
    a <- proposta_anno(); req(a)
    updateNumericInput(session, "estremo_remoto", value = a$anno)
  })

  # «ricomponi dai campi»: il regesto torna quello composto e la spunta «scritto a mano» si toglie.
  observeEvent(input$ricomponi_regesto, {
    testo <- componi_regesto_scheda(scheda_dal_modulo(), forza = TRUE)
    updateCheckboxInput(session, "regesto_manuale", value = FALSE)
    updateTextAreaInput(session, "regesto", value = if (is.na(testo)) "" else testo)
  })
  list(proposta_anno = proposta_anno)
}
