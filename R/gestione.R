gestione_ui <- function() nav_panel(title = tagList(icon("screwdriver-wrench"), "Gestione"), value = "Gestione", titolo_pagina("Gestione"), navset_pill(
  nav_panel("Strumenti",
    card(card_header("Dove stanno i dati"), uiOutput("dati_posizione")),
    card(card_header("Genera fascicoli vuoti"),
      p("Crea in blocco le schede vuote di una busta, pronte da aprire e compilare una a una, con l'identit\u00e0 archivistica dell'ultimo fascicolo lavorato del fondo. I numeri gi\u00e0 presenti non si toccano."),
      uiOutput("vuoti_identita"),
      layout_columns(col_widths = c(3, 3, 3),
        numerico("vuoti_busta", "Busta", NA, min = 1, step = 1),
        numerico("vuoti_da", "Dal fascicolo", NA, min = 1, step = 1),
        numerico("vuoti_a", "Al fascicolo (facoltativo)", NA, min = 1, step = 1)),
      uiOutput("vuoti_anteprima"),
      # Spento finché non ci sono busta e primo fascicolo (13/10): prima era acceso con i campi vuoti.
      div(conditionalPanel("!(input.vuoti_busta && input.vuoti_da)",
            tags$button(type = "button", class = "btn btn-primary", disabled = NA, tagList(icon("file-circle-plus"), "Genera fascicoli vuoti"))),
          conditionalPanel("input.vuoti_busta && input.vuoti_da",
            actionButton("vuoti_genera", tagList(icon("file-circle-plus"), "Genera fascicoli vuoti"), class = "btn-primary")))),
    card(card_header("Note rapide da archivio"),
      p("Per trascrivere velocemente gli appunti presi leggendo i fascicoli: una riga per fascicolo, campi separati da ",
        tags$code("|"), ". Aggiorna la scheda con quel numero (es. i fascicoli vuoti generati sopra) o ne crea una nuova. ",
        "Riempie solo i campi vuoti: un valore gi\u00e0 scritto resta, e l'anteprima lo segnala."),
      div(class = "alert alert-secondary small py-2",
        strong("Ordine dei dati da annotare leggendo il fascicolo:"),
        tags$ol(class = "mb-0", tags$li("Numero di fascicolo"), tags$li("Persona principale (nome e cognome)"),
                tags$li("Anno (o anni: 1623-1625)"), tags$li("Tipo di reato (pi\u00f9 voci separate da virgola)"),
                tags$li("Tortura (s\u00ec / no / n.d.)"), tags$li("Sentenza (s\u00ec / no / n.d.)"), tags$li("Note libere"))),
      numerico("note_busta", "Busta", NA, min = 1, step = 1, width = "10rem"),
      textAreaInput("note_testo", "Appunti (una riga per fascicolo)", rows = 6, width = "100%",
                    placeholder = "3 | Mario Rossi | 1623 | Eresia, Magia | s\u00ec | no | controllare data\n4 | | 1631 | Censura | n.d. | s\u00ec"),
      uiOutput("note_anteprima"),
      uiOutput("note_applica_ui")),
    card(card_header("Rimuovi duplicati"),
      p("Individua i fascicoli con la stessa busta e lo stesso numero schedati due volte perché il fondo è scritto in due modi (es. «Sant'Ufficio» e «Sant'Uffizio»), e te li mostra a gruppi: tu spunti quelli che sono davvero copie e scegli quale resta. Due fondi davvero diversi (anche «Serie 1» e «Serie 2») non sono duplicati."),
      div(actionButton("dup_analizza", tagList(icon("magnifying-glass"), "Analizza duplicati"), class = "btn-outline-secondary")),
      uiOutput("dup_esito")),
    # Zona delicata (7/10): le due cose che non si disfano da sole. Prima si chiede se esportare, poi
    # si fa sempre una copia di sicurezza, poi si scrive il numero delle schede.
    card(class = "border-danger", card_header("Zona delicata"),
      p("Cancellare non si disfa da solo. Prima di toccare qualcosa Trama ti chiede se vuoi esportare, poi fa sempre una copia di sicurezza nella cartella dei backup, da cui si può ripristinare."),
      div(class = "d-flex gap-2 flex-wrap",
          actionButton("canc_fondo_apri", tagList(icon("trash-can"), "Elimina questo fondo…"), class = "btn-outline-danger"),
          actionButton("canc_tutto_apri", tagList(icon("eraser"), "Svuota tutto e ricomincia…"), class = "btn-outline-danger")))),
  nav_panel("Ordine e segnature",
    p("Il fondo è quello scelto nella barra in alto. Trascina una riga o usa le frecce: cambi solo l'ordine di consultazione nella busta. Per modificare la segnatura usa Correggi o Rinumera."),
    textOutput("ord_fondo"),
    selectInput("ord_busta", "Busta", choices = NULL),
    div(class = "d-flex flex-wrap gap-2 mb-3",
      actionButton("ord_numerico", "Ordina per segnatura"),
      actionButton("ord_annulla", "Annulla ultimo spostamento"),
      actionButton("ord_rinumera", "Rinumera nell'ordine mostrato…"),
      actionButton("ord_annulla_correzione", "Annulla ultima correzione")),
    textOutput("ord_esito"),
    div(style = "max-height:65vh;overflow:auto", uiOutput("ord_elenco")),
    tags$script(HTML("(function(){
      var trascinato = null;
      $(document).on('dragstart', '#ord_elenco tr[data-id]', function(e){
        trascinato = this.dataset.id;
        e.originalEvent.dataTransfer.setData('text/plain', trascinato);
        e.originalEvent.dataTransfer.effectAllowed = 'move';
      });
      $(document).on('dragover', '#ord_elenco tr[data-id]', function(e){e.preventDefault();});
      $(document).on('drop', '#ord_elenco tr[data-id]', function(e){
        e.preventDefault();
        if(trascinato && trascinato !== this.dataset.id)
          Shiny.setInputValue('ord_sposta', {id:trascinato, prima:this.dataset.id}, {priority:'event'});
        trascinato = null;
      });
      $(document).on('click', '#ord_elenco button[data-azione]', function(){
        Shiny.setInputValue('ord_azione', {id:this.dataset.id, azione:this.dataset.azione}, {priority:'event'});
      });
    })();"))
  )
))

gestione_server <- function(input, output, session, con, fondo_attivo, ricarica, naviga,
                            dopo_cancellazione = function(tipo, fondo) NULL) {
  errore <- function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)

  # ── Zona delicata: elimina un fondo, svuota tutto (7/10, crud.R) ──
  canc <- reactiveVal(NULL)    # list(tipo = "fondo"|"tutto", fondo, n)
  nome_prima <- function(che, ext) paste0(paste(c("trama", che, format(Sys.Date())), collapse = "-"), ".", ext)
  output$prima_zip <- downloadHandler(filename = function() nome_prima("copia-completa", "zip"),
    content = function(file) { tmp <- tempfile(fileext = ".zip"); backup_completo(con, tmp, cart_luoghi = cartella_luoghi()); file.copy(tmp, file, overwrite = TRUE) })
  output$prima_json <- downloadHandler(filename = function() nome_prima("backup", "json"),
    content = function(file) esporta_backup_json(con, file))
  output$prima_sql <- downloadHandler(filename = function() nome_prima("database", "sql"),
    content = function(file) esporta_sql(con, file, fondo = canc()$fondo))
  output$prima_sqlite <- downloadHandler(filename = function() nome_prima("database", "sqlite"),
    content = function(file) esporta_sqlite(con, file, fondo = canc()$fondo))
  output$prima_xlsx <- downloadHandler(filename = function() nome_prima("foglio", "xlsx"),
    content = function(file) esporta_arianna(con, file, fondo = canc()$fondo))

  finestra_cancella <- function(tipo) {
    f <- if (tipo == "fondo") fondo_attivo() else NULL
    if (tipo == "fondo" && .vuoto(f)) {
      showNotification("Scegli prima il fondo da eliminare nella barra in alto.", type = "warning"); return()
    }
    r <- riepilogo_cancellazione(con, f)
    canc(list(tipo = tipo, fondo = f, nome = if (is.null(f)) NA_character_ else nome_fondo_esteso(con, f), n = r$fascicoli))
    scarica <- function(id, testo) downloadButton(id, testo, class = "btn-outline-secondary btn-sm")
    showModal(modalDialog(size = "l", easyClose = FALSE,
      title = if (tipo == "fondo") "Eliminare questo fondo?" else "Svuotare tutto il database?",
      if (tipo == "fondo") p(span("Sparisce il fondo "), span(class = "no-trad", nome_fondo_esteso(con, f)),
                              span(sprintf(" con le sue %d schede, il suo piano di busta e le decisioni del suo vocabolario.", r$fascicoli)))
      else p(sprintf("Spariscono %d fondi, %d schede, i piani di busta e le ricerche salvate. Restano la lista dei luoghi e, se vuoi, le impostazioni.",
                     r$fondi, r$fascicoli)),
      div(class = "alert alert-warning small py-2",
          "Trama fa sempre una copia di sicurezza prima di cancellare: sta nella cartella dei backup e da lì si ripristina."),
      h6("Vuoi esportare prima?"),
      div(class = "d-flex gap-2 flex-wrap mb-3",
          if (tipo == "fondo" && r$fascicoli > 0) scarica("prima_xlsx", "Foglio Excel del fondo (.xlsx)"),
          scarica("prima_zip", "Copia completa (.zip)"),
          scarica("prima_sql", "Database (.sql)"),
          scarica("prima_sqlite", "Database (.sqlite)"),
          if (tipo == "tutto") scarica("prima_json", "Backup (.json)")),
      radioButtons("canc_scelta", NULL, selected = character(0),
                   c("Ho esportato quello che mi serve" = "fatto",
                     "Non serve: mi basta la copia di sicurezza automatica" = "salta")),
      if (tipo == "tutto") checkboxInput("canc_tieni", "Tieni le impostazioni (schedatore e progetto)", TRUE),
      if (r$fascicoli > 0) textInput("canc_conferma", sprintf("Per confermare scrivi %d", r$fascicoli)),
      footer = tagList(modalButton("Annulla"), uiOutput("canc_applica_ui", inline = TRUE))))
  }
  observeEvent(input$canc_fondo_apri, finestra_cancella("fondo"))
  observeEvent(input$canc_tutto_apri, finestra_cancella("tutto"))

  canc_pronto <- function() {
    c <- canc()
    !is.null(c) && !is.null(input$canc_scelta) && (c$n == 0 || identical(trimws(input$canc_conferma %||% ""), as.character(c$n)))
  }
  output$canc_applica_ui <- renderUI({
    c <- canc(); req(c)
    b <- actionButton("canc_applica", if (c$tipo == "fondo") "Elimina il fondo" else "Svuota tutto", class = "btn-danger")
    if (canc_pronto()) b else htmltools::tagAppendAttributes(b, disabled = "disabled")
  })
  observeEvent(input$canc_applica, {
    c <- canc(); req(c)
    if (!canc_pronto()) { showNotification("Scegli se hai esportato e scrivi il numero delle schede.", type = "warning"); return() }
    tryCatch({
      copia <- backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"),
                                       if (c$tipo == "fondo") "eliminazione-fondo" else "svuotamento")
      if (c$tipo == "fondo") elimina_fondo(con, c$fondo) else svuota_database(con, tieni_impostazioni = isTRUE(input$canc_tieni))
      registra("INFO", if (c$tipo == "fondo") "fondo eliminato" else "database svuotato",
               if (c$tipo == "fondo") sprintf("%s (%d schede); copia %s", c$nome, c$n, basename(copia)) else basename(copia))
      removeModal(); canc(NULL)
      dopo_cancellazione(c$tipo, c$fondo)
      ricarica(ricarica() + 1)
      notifica_con_cartella(tagList(
        if (c$tipo == "fondo") tagList(span("Fondo eliminato: "), span(class = "no-trad", paste0(c$nome, "."))) else span("Database svuotato."),
        span(" Copia di sicurezza fatta.")), "backup", duration = 20)
    }, error = function(e) { registra("ERRORE", "cancellazione non riuscita", conditionMessage(e)); errore(e) })
  })

  # ── Strumenti: genera fascicoli vuoti (crud.R) ──
  # I nomi (archivio, fondo, serie) stanno in un elemento `no-trad`: nella stessa frase il traduttore li storpiava.
  output$vuoti_identita <- renderUI({
    ricarica(); f <- fondo_attivo()
    if (.vuoto(f)) return(span("Scegli prima un fondo nella barra in alto."))
    u <- ultimo_modificato(con, f); b <- if (!is.null(u)) u else contesto_archivio(con, f)
    if (is.null(b)) return(span(span("Il fondo "), span(class = "no-trad", paste0("«", nome_fondo_esteso(con, f), "»")), span(" non ha ancora schede né un archivio dichiarato.")))
    x <- as.character(c(b$archivio, b$fondo, b$serie)); x <- x[!is.na(x) & nzchar(trimws(x))]
    span(span("Identità: "), span(class = "no-trad", paste0(paste(x, collapse = ", "), ".")))
  })
  output$vuoti_anteprima <- renderUI({
    ricarica(); b <- input$vuoti_busta; da <- input$vuoti_da; a <- if (is.na(input$vuoti_a %||% NA)) da else input$vuoti_a
    if (is.na(b %||% NA) || is.na(da %||% NA)) return(NULL)      # niente avviso a campi vuoti: non è un errore (13/10)
    if (a < da) return(p(class = "small text-danger", "L'ultimo fascicolo viene prima del primo."))
    presenti <- dbGetQuery(con, "SELECT fascicolo FROM fascicoli WHERE fondo_id = ? AND busta = ? AND busta_suffisso = '' AND fascicolo_suffisso = ''",
                           list(fondo_attivo() %||% "", as.integer(b)))$fascicolo
    gia <- intersect(da:a, presenti)
    p(class = "small", sprintf("Creerà %d schede vuote (b. %d, fasc. %d\u2013%d).", length(setdiff(da:a, presenti)), as.integer(b), as.integer(da), as.integer(a)),
      if (length(gia)) span(class = "text-muted", sprintf(" Già presenti, saltati: %s.", paste(gia, collapse = ", "))))
  })
  # ── Strumenti: note rapide da archivio (crud.R) ──
  note_prova <- reactive({
    ricarica()
    if (is.na(input$note_busta %||% NA) || !nzchar(trimws(input$note_testo %||% ""))) return(NULL)
    applica_note_rapide(con, fondo_attivo(), input$note_busta, input$note_testo,
                        produttore = produttore_del_fondo(con, fondo_attivo()), prova = TRUE)
  })
  output$note_anteprima <- renderUI({
    a <- tryCatch(note_prova(), error = function(e) conditionMessage(e))
    if (is.null(a)) return(p(class = "small text-muted", "Indica la busta e scrivi gli appunti: qui compare l'anteprima."))
    if (is.character(a)) return(p(class = "text-danger small", a))
    tags$table(class = "table table-sm small",
      tags$tr(tags$th("Riga"), tags$th("Fascicolo"), tags$th("Azione"), tags$th("Cosa cambia"), tags$th("Conflitti")),
      lapply(seq_len(nrow(a)), function(i) tags$tr(class = if (a$azione[i] == "errore") "table-danger" else if (nzchar(a$conflitti[i])) "table-warning",
        tags$td(a$riga[i]), tags$td(a$fascicolo[i]), tags$td(a$azione[i]), tags$td(a$cambia[i]), tags$td(a$conflitti[i]))))
  })
  # Spento finché non c'è un'anteprima valida; dice a quante schede scriverà (23/11/2026).
  output$note_applica_ui <- renderUI({
    a <- tryCatch(note_prova(), error = function(e) NULL)
    ok <- !is.null(a) && nrow(a) > 0 && !any(a$azione == "errore")
    b <- actionButton("note_applica", tagList(icon("check"), if (ok) sprintf("Applica gli appunti (%d)", nrow(a)) else "Applica gli appunti"), class = "btn-primary")
    div(if (ok) b else tagAppendAttributes(b, disabled = NA, title = "Prima serve un'anteprima senza righe in rosso"))
  })
  observeEvent(input$note_applica, tryCatch({
    a <- note_prova(); if (is.null(a)) return()
    if (any(a$azione == "errore")) stop("Correggi prima le righe segnate in rosso.", call. = FALSE)
    backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"), "note-rapide")
    e <- applica_note_rapide(con, fondo_attivo(), input$note_busta, input$note_testo,
                             produttore = produttore_del_fondo(con, fondo_attivo()))
    updateTextAreaInput(session, "note_testo", value = "")
    ricarica(ricarica() + 1)
    showNotification(esito_ok(sprintf("Appunti applicati: %d schede nuove, %d aggiornate.", sum(e$azione == "nuova scheda"),
                                      sum(e$azione == "aggiorna"))), type = "message", duration = 8)
  }, error = errore))

  fondo_o_tutti <- function() { f <- fondo_attivo(); if (.vuoto(f)) NULL else f }
  # (19/10 sera: «Ricostruisci imputati dai titoli» non c'è più: nessuna distinzione fra imputati e altre figure.)

  # ── Dove stanno i dati: il percorso ──
  output$dati_posizione <- renderUI({
    p(class = "small mb-1", span("Il database e le copie di sicurezza stanno in "), span(class = "no-trad", tags$code(dirname(percorso_db()))), span("."))
  })

  # ── Strumenti: rimuovi duplicati (crud.R) ──
  dup <- reactiveVal(NULL)
  observeEvent(input$dup_analizza, dup(duplicati_fascicoli(con)))
  output$dup_esito <- renderUI({
    d <- dup(); if (is.null(d)) return(NULL)
    if (!nrow(d)) return(p(class = "small text-muted mt-2", "Nessun duplicato: ogni fascicolo c'è una volta sola."))
    gruppi <- unique(d$gruppo)
    tagList(
      p(class = "small mt-2", if (length(gruppi) == 1) "1 gruppo di possibili duplicati." else sprintf("%d gruppi di possibili duplicati.", length(gruppi)),
        " Non si elimina niente da solo: spunta i gruppi che sono davvero la stessa scheda scritta due volte, e scegli quale scheda resta (proposta: la più compilata)."),
      lapply(gruppi, function(g) {
        m <- d[d$gruppo == g, , drop = FALSE]
        div(class = "border rounded p-2 mb-2",
          checkboxInput(paste0("dup_si_", g), sprintf("Gruppo %d: elimina le copie in più", g), FALSE),
          if (isTRUE(m$diversi[1])) p(class = "small text-warning-emphasis", "I titoli sono diversi: controlla che siano davvero la stessa scheda."),
          radioButtons(paste0("dup_tieni_", g), "Resta:",
            choiceNames = lapply(seq_len(nrow(m)), function(i) span(class = "small", m$segnatura[i], if (nzchar(m$titolo[i])) paste0(" \u2014 ", m$titolo[i]),
                                                                    sprintf(" (%d campi compilati)", m$compilati[i]))),
            choiceValues = m$id, selected = m$id[m$tieni][1]))
      }),
      actionButton("dup_rimuovi", tagList(icon("trash-can"), "Elimina le copie dei gruppi spuntati"), class = "btn-danger"))
  })
  observeEvent(input$dup_rimuovi, tryCatch({
    d <- dup(); req(d)
    gruppi <- unique(d$gruppo)
    spuntati <- gruppi[vapply(gruppi, function(g) isTRUE(input[[paste0("dup_si_", g)]]), TRUE)]
    if (!length(spuntati)) { showNotification("Spunta almeno un gruppo da eliminare.", type = "warning"); return() }
    tieni <- stats::setNames(lapply(spuntati, function(g) input[[paste0("dup_tieni_", g)]]), as.character(spuntati))
    ids <- scelta_duplicati(d, spuntati, tieni)
    copia <- backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"), "duplicati")
    n <- rimuovi_duplicati(con, ids)
    dup(duplicati_fascicoli(con)); ricarica(ricarica() + 1)
    notifica_con_cartella(sprintf("%s. Copia di sicurezza fatta.", if (n == 1) "Eliminata 1 copia" else sprintf("Eliminate %d copie", n)),
                          "backup", duration = 10)
  }, error = errore))

  observeEvent(input$vuoti_genera, tryCatch({
    g <- genera_fascicoli_vuoti(con, fondo_attivo(), input$vuoti_busta, input$vuoti_da, input$vuoti_a,
                                produttore = produttore_del_fondo(con, fondo_attivo()))
    ricarica(ricarica() + 1)
    showNotification(sprintf("Create %d schede vuote%s.", g$creati,
                             if (length(g$saltati)) sprintf(" (saltati i già presenti: %s)", paste(g$saltati, collapse = ", ")) else ""),
                     type = "message", duration = 8)
  }, error = errore))
  ordine_versione <- reactiveVal(0)
  ultimo_ordine <- reactiveVal(NULL)
  ultima_correzione <- reactiveVal(NULL)
  correzione_singola <- reactiveVal(FALSE)
  # Il valore del menu è «busta|suffisso» («232|-bis»): 232 e 232-bis sono due buste.
  busta <- reactive({
    v <- input$ord_busta
    if (is.null(v) || v == "__senza__") NA_integer_ else as.integer(sub("\\|.*$", "", v))
  })
  busta_suf <- reactive({
    v <- input$ord_busta
    if (is.null(v) || v == "__senza__" || !grepl("|", v, fixed = TRUE)) "" else sub("^[^|]*\\|", "", v)
  })
  output$ord_fondo <- renderText(paste("Fondo:", if (.vuoto(fondo_attivo())) "scegli un fondo nella barra in alto" else nome_fondo_esteso(con, fondo_attivo())))
  observe({
    ricarica(); f <- fondo_attivo()
    d <- if (.vuoto(f)) data.frame(busta = integer()) else dbGetQuery(con, "SELECT DISTINCT busta, busta_suffisso FROM fascicoli WHERE fondo_id = ? ORDER BY busta", list(f))
    if (nrow(d)) d <- d[order(d$busta, .rango_suffisso(d$busta_suffisso), d$busta_suffisso), , drop = FALSE]
    v <- ifelse(is.na(d$busta), "__senza__", paste0(d$busta, "|", d$busta_suffisso))
    et <- ifelse(is.na(d$busta), "Senza busta", paste("Busta", paste0(d$busta, d$busta_suffisso)))
    selezionata <- isolate(input$ord_busta)
    updateSelectInput(session, "ord_busta", choices = setNames(v, et),
                      selected = if (length(selezionata) && selezionata %in% v) selezionata else head(v, 1))
  })
  elenco <- reactive({
    ordine_versione(); ricarica()
    if (.vuoto(fondo_attivo()) || is.null(input$ord_busta)) return(data.frame())
    elenco_busta(con, fondo_attivo(), busta(), busta_suf())
  })
  output$ord_elenco <- renderUI({
    d <- elenco()
    if (!nrow(d)) return(p("Nessun fascicolo in questa busta."))
    pulsante <- function(id, azione, label, title) tags$button(type = "button", class = "btn btn-sm btn-outline-secondary",
      `data-id` = id, `data-azione` = azione, title = title, `aria-label` = title, label)
    tags$table(class = "table table-sm table-hover align-middle",
      tags$thead(tags$tr(tags$th("Sposta"), tags$th("Fascicolo"), tags$th("Titolo"), tags$th("Segnatura"))),
      tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(draggable = "true", `data-id` = d$id[i],
        tags$td(div(class = "d-flex gap-1", pulsante(d$id[i], "su", "↑", "Sposta su"), pulsante(d$id[i], "giu", "↓", "Sposta giù"))),
        tags$td(paste0(d$fascicolo[i], d$fascicolo_suffisso[i])),
        tags$td(if (is.na(d$titolo[i])) "—" else d$titolo[i]),
        tags$td(pulsante(d$id[i], "correggi", "Correggi…", "Correggi busta e numero"))))))
  })
  salva_ordine <- function(ids) {
    d <- elenco()
    tryCatch({
      salva_ordine_busta(con, fondo_attivo(), busta(), ids, busta_suf())
      ultimo_ordine(list(fondo = fondo_attivo(), busta = busta(), suf = busta_suf(), ids = d$id))
      ordine_versione(ordine_versione() + 1)
      # Solo la tabella di consultazione dipende da questo contatore.
      output$ord_esito <- renderText("Ordine di consultazione salvato; segnature invariate.")
    }, error = errore)
  }
  session$userData$ordine_versione <- ordine_versione
  observeEvent(input$ord_sposta, {
    m <- input$ord_sposta; ids <- elenco()$id
    if (!all(c(m$id, m$prima) %in% ids) || identical(m$id, m$prima)) return()
    ids <- setdiff(ids, m$id)
    salva_ordine(append(ids, m$id, after = match(m$prima, ids) - 1L))
  })
  correzione <- reactiveVal(NULL)
  correzione_singola <- reactiveVal(FALSE)
  apri_correzione <- function(d, singolo = FALSE) naviga(function() {
    correzione(d)
    correzione_singola(singolo)
    showModal(modalDialog(title = if (singolo) "Correggi segnatura" else "Rinumera i fascicoli nell'ordine mostrato", size = "l",
      p("L'identificativo e i contenuti dei fascicoli restano invariati. Controlla l'anteprima prima di applicare."),
      numerico("corr_busta", "Busta di destinazione (vuota = senza busta)", value = d$busta[1], min = 1, step = 1),
      numericInput("corr_primo", if (singolo) "Numero del fascicolo" else "Primo numero", value = if (singolo) d$fascicolo[1] else 1L, min = 1, step = 1),
      if (singolo) textInput("corr_suffisso", "Suffisso", value = d$fascicolo_suffisso[1])
      else p("Ogni fascicolo conserva il proprio suffisso."),
      uiOutput("corr_anteprima"),
      footer = tagList(modalButton("Annulla"), actionButton("corr_applica", "Applica correzione", class = "btn-primary"))))
  })
  observeEvent(input$ord_azione, {
    m <- input$ord_azione; d <- elenco(); i <- match(m$id, d$id)
    if (is.na(i)) return()
    if (m$azione == "correggi") return(apri_correzione(d[i, , drop = FALSE], TRUE))
    j <- i + if (m$azione == "su") -1L else 1L
    if (!m$azione %in% c("su", "giu") || j < 1 || j > nrow(d)) return()
    ids <- d$id; ids[c(i, j)] <- ids[c(j, i)]; salva_ordine(ids)
  })
  observeEvent(input$ord_numerico, {
    d <- elenco(); if (nrow(d)) salva_ordine(d$id[order(d$fascicolo, d$fascicolo_suffisso, d$id)])
  })
  observeEvent(input$ord_annulla, {
    u <- ultimo_ordine(); if (is.null(u)) return()
    tryCatch({
      salva_ordine_busta(con, u$fondo, u$busta, u$ids, u$suf)
      ultimo_ordine(NULL); ordine_versione(ordine_versione() + 1)
      output$ord_esito <- renderText("Ultimo spostamento annullato.")
    }, error = errore)
  })
  observeEvent(input$ord_rinumera, { d <- elenco(); if (nrow(d)) apri_correzione(d) })
  piano <- reactive({
    d <- correzione(); req(d, input$corr_primo)
    piano_collocazioni(d, input$corr_busta, input$corr_primo,
                       if (correzione_singola()) input$corr_suffisso else NULL)
  })
  output$corr_anteprima <- renderUI({
    tryCatch({
      p <- piano()
      coll <- function(b, f, s) paste0("b. ", ifelse(is.na(b), "?", b), ", fasc. ", f, s)
      tags$table(class = "table table-sm", tags$thead(tags$tr(tags$th("Prima"), tags$th("Dopo"))),
        tags$tbody(lapply(seq_len(nrow(p)), function(i) tags$tr(
          tags$td(coll(p$busta[i], p$fascicolo[i], p$fascicolo_suffisso[i])),
          tags$td(coll(p$nuova_busta[i], p$nuovo_fascicolo[i], p$nuovo_suffisso[i]))))))
    }, error = function(e) div(class = "text-danger", conditionMessage(e)))
  })
  observeEvent(input$corr_applica, {
    tryCatch({
      p <- piano(); applica_collocazioni(con, p)
      inverso <- p
      inverso$busta <- p$nuova_busta; inverso$fascicolo <- p$nuovo_fascicolo; inverso$fascicolo_suffisso <- p$nuovo_suffisso
      inverso$nuova_busta <- p$busta; inverso$nuovo_fascicolo <- p$fascicolo; inverso$nuovo_suffisso <- p$fascicolo_suffisso
      mod <- dbGetQuery(con, "SELECT id, modificato FROM fascicoli")   # una lettura sola, non una scheda intera per riga
      inverso$modificato <- mod$modificato[match(inverso$id, mod$id)]
      ultima_correzione(inverso); ultimo_ordine(NULL)
      removeModal(); ricarica(ricarica() + 1)
      output$ord_esito <- renderText(sprintf("Corrette %d segnature.", nrow(p)))
    }, error = errore)
  })
  observeEvent(input$ord_annulla_correzione, naviga(function() {
    p <- ultima_correzione(); if (is.null(p)) return()
    tryCatch({
      applica_collocazioni(con, p); ultima_correzione(NULL)
      ricarica(ricarica() + 1); output$ord_esito <- renderText("Ultima correzione annullata.")
    }, error = errore)
  }))
}
