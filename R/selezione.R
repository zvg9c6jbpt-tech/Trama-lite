# ─────────────────────────────────────────────────────────────
# selezione.R: casella per riga nella tabella Fascicoli e correzione selettiva «Imposta un campo»
# La scrittura è imposta_campo_in_blocco() (crud.R): qui solo interfaccia.
# ─────────────────────────────────────────────────────────────

# Colonne dei campi a più valori nelle tabelle figlie.
.COLONNA_FIGLIA <- c(persone = "persona", luoghi = "luogo", reati = "reato", produttori = "soggetto", lingue = "lingua")

valori_noti_campo <- function(con, campo, dove) {
  v <- if (dove == "fascicoli") dbGetQuery(con, sprintf("SELECT DISTINCT %s AS v FROM fascicoli", campo))$v
       else dbGetQuery(con, sprintf("SELECT DISTINCT %s AS v FROM %s", .COLONNA_FIGLIA[[dove]], dove))$v
  v <- as.character(v); v <- v[!is.na(v) & nzchar(v)]
  voc <- tryCatch(as.character(.scelte(.voc(campo), NULL)), error = function(e) character(0))
  unique(c(voc, sort(v)))
}

selezione_ui <- function() tagList(
  uiOutput("barra_selezione"),
  tags$script(HTML("
    window.spuntati = window.spuntati || new Set();
    function mandaSpuntati(){ Shiny.setInputValue('fascicoli_spuntati', Array.from(window.spuntati), {priority: 'event'}); }
    function ridisegnaSpunte(){
      $('#tabella .spunta-riga').each(function(){ this.checked = window.spuntati.has(this.dataset.id); });
      $('#tabella tbody tr').each(function(){
        var c = $(this).find('.spunta-riga')[0];
        $(this).toggleClass('riga-spuntata', !!(c && c.checked));
      });
      // La casella di una busta è accesa se lo sono tutte le sue righe; quella in testa alla colonna, se lo sono tutte le righe trovate.
      $('#tabella .spunta-busta').each(function(){
        var ids = this.dataset.ids.split(',');
        this.checked = ids.length > 0 && ids.every(function(i){ return window.spuntati.has(i); });
      });
      var t = window.tabellaFascicoli, tutte = document.getElementById('spunta_tutte');
      if (t && tutte) {
        var trovate = t.rows({search: 'applied'}).data().toArray().map(function(r){ return r[0]; });
        tutte.checked = trovate.length > 0 && trovate.every(function(i){ return window.spuntati.has(i); });
      }
    }
    $(document).on('change', '#tabella .spunta-riga', function(){
      if (this.checked) window.spuntati.add(this.dataset.id); else window.spuntati.delete(this.dataset.id);
      ridisegnaSpunte(); mandaSpuntati();
    });
    $(document).on('change', '#spunta_tutte', function(){ Shiny.setInputValue('sel_spunte', this.checked ? 'visibili' : 'nessuna', {priority: 'event'}); });
    $(document).on('change', '#tabella .spunta-busta', function(){
      var on = this.checked;
      this.dataset.ids.split(',').forEach(function(i){ if (on) window.spuntati.add(i); else window.spuntati.delete(i); });
      ridisegnaSpunte(); mandaSpuntati();
    });
    Shiny.addCustomMessageHandler('spunte', function(m){
      if (m === 'nessuna') window.spuntati.clear();
      if (m === 'visibili' && window.tabellaFascicoli)
        window.tabellaFascicoli.rows({search: 'applied'}).data().each(function(r){ window.spuntati.add(r[0]); });
      ridisegnaSpunte(); mandaSpuntati();
    });
  "))
)

selezione_server <- function(input, output, session, con, ricarica, dopo_modifica = function(ids) NULL) {
  spuntati <- reactive({ x <- as.character(unlist(input$fascicoli_spuntati)); x[nzchar(x)] })
  campi <- CAMPI_IN_BLOCCO()

  output$barra_selezione <- renderUI({
    n <- length(spuntati())
    div(class = "barra-selezione",
      # Senza righe spuntate resta solo «Spunta le visibili»: le caselle si vedono, la frase che le spiega non serve.
      if (n) strong(class = "barra-selezione-n", sprintf("%d fascicol%s spuntat%s", n, if (n == 1) "o" else "i", if (n == 1) "o" else "i")),
      if (n) actionButton("sel_imposta", tagList(icon("pen-to-square"), "Imposta un campo…"), class = "btn-pillola"),
      if (n) actionButton("sel_ricomponi", tagList(icon("rotate"), "Ricomponi titolo e regesto…"), class = "btn-pillola"),
      if (n) actionButton("sel_elimina", tagList(icon("trash-can"), "Elimina…"), class = "btn-pillola text-danger"),
      # Senza righe spuntate la barra non c'è (compare in basso, ferma, quando serve). «Spunta le visibili» è la casella in testa alla colonna.
      if (n) tags$button(type = "button", class = "btn btn-pillola barra-selezione-via", onclick = "Shiny.setInputValue('sel_spunte', 'nessuna', {priority:'event'})",
                         "Deseleziona"))
  })
  observeEvent(input$sel_spunte, session$sendCustomMessage("spunte", input$sel_spunte))

  observeEvent(input$sel_imposta, {
    showModal(modalDialog(
      title = sprintf("Imposta un campo su %d fascicoli", length(spuntati())), size = "l", easyClose = TRUE,
      layout_columns(col_widths = c(5, 7),
        selectInput("sel_campo", "Campo", stats::setNames(campi$campo, campi$etichetta)),
        uiOutput("sel_modo_ui")),
      selectizeInput("sel_valore", "Valore", choices = NULL, multiple = TRUE, width = "100%",
                     options = list(create = TRUE, delimiter = ";", placeholder = "scegli o scrivi; vuoto = svuota il campo")),
      p(class = "small text-muted", "Titolo e regesto composti si ricompongono; quelli scritti a mano restano. ",
        "Prima di scrivere si fa una copia di sicurezza del database."),
      h6("Anteprima"), uiOutput("sel_anteprima"),
      footer = tagList(modalButton("Annulla"), actionButton("sel_applica", "Applica", class = "btn-primary"))))
  })
  def <- reactive({ req(input$sel_campo); campi[campi$campo == input$sel_campo, , drop = FALSE] })
  output$sel_modo_ui <- renderUI({
    if (!def()$multi) return(p(class = "small text-muted mt-4", "Il valore sostituisce quello di ogni fascicolo."))
    radioButtons("sel_modo", "Modo", inline = TRUE,
                 c("Aggiungi" = "aggiungi", "Togli" = "togli", "Sostituisci tutto" = "sostituisci"))
  })
  observeEvent(input$sel_campo, {
    d <- def()
    updateSelectizeInput(session, "sel_valore", choices = valori_noti_campo(con, d$campo, d$dove), selected = character(0),
                         options = list(maxItems = if (d$multi) NULL else 1, create = TRUE, delimiter = ";"), server = FALSE)
  })
  modo <- reactive(if (isTRUE(def()$multi)) input$sel_modo %||% "aggiungi" else "sostituisci")
  anteprima <- reactive({
    req(input$sel_campo)
    imposta_campo_in_blocco(con, spuntati(), input$sel_campo, input$sel_valore, modo(), prova = TRUE)
  })
  output$sel_anteprima <- renderUI({
    a <- tryCatch(anteprima(), error = function(e) conditionMessage(e))
    if (is.character(a)) return(p(class = "text-danger", a))
    if (!nrow(a)) return(p(class = "text-muted", "Nessun fascicolo cambierebbe."))
    tagList(p(sprintf("Cambiano %d fascicoli su %d.", nrow(a), length(spuntati()))),
      div(style = "max-height:40vh;overflow:auto", tags$table(class = "table table-sm",
        tags$tr(tags$th("Fascicolo"), tags$th("Prima"), tags$th("Dopo")),
        lapply(seq_len(nrow(a)), function(i) tags$tr(tags$td(a$segnatura[i]), tags$td(a$prima[i]), tags$td(strong(a$dopo[i])))))))
  })
  # Ricomponi titolo e regesto delle spuntate (crud.R, ricomponi_in_blocco).
  observeEvent(input$sel_ricomponi, showModal(modalDialog(
    title = sprintf("Ricomponi titolo e regesto di %d fascicoli", length(spuntati())), size = "l", easyClose = TRUE,
    checkboxInput("sel_anche_titoli", "Anche i titoli scritti a mano (tornano composti)", FALSE),
    checkboxInput("sel_anche_regesti", "Anche i regesti scritti a mano (tornano composti)", FALSE),
    uiOutput("sel_ricomponi_anteprima"),
    uiOutput("sel_ricomponi_conferma"),
    footer = tagList(modalButton("Annulla"), actionButton("sel_ricomponi_applica", "Ricomponi", class = "btn-primary")))))
  # Il ricalcolo senza scrivere (anteprima e conferma leggono lo stesso risultato). I testi scritti a mano restano fuori finché non si spuntano, titoli e regesti separatamente;
  # un risultato vuoto non sostituisce mai un testo.
  prova_ricomposizione <- reactive(ricomponi_in_blocco(con, spuntati(), prova = TRUE,
                                                       anche_titoli = isTRUE(input$sel_anche_titoli),
                                                       anche_regesti = isTRUE(input$sel_anche_regesti)))
  n_protetti <- function(a) c(titoli = sum(grepl("titolo", a$protetti)), regesti = sum(grepl("regesto", a$protetti)))
  output$sel_ricomponi_anteprima <- renderUI({
    a <- prova_ricomposizione()
    if (!nrow(a)) return(p(class = "text-muted", "Nessun fascicolo cambierebbe."))
    np <- n_protetti(a)
    tagList(p(sprintf("Cambiano %d fascicoli (titolo / regesto).", nrow(a))),
      if (sum(np)) div(class = "alert alert-danger py-2 small",
        strong(sprintf("Sostituirebbe testi scritti a mano: %d titoli e %d regesti.", np[["titoli"]], np[["regesti"]])),
        " Le righe interessate sono segnate con «scritto a mano» nella colonna «Prima». Il testo di prima non si recupera da qui: resta nella copia di sicurezza."),
      # Nessun imputato implicito: senza imputati dichiarati il regesto composto non nomina nessuno come accusato.
      p(class = "small text-muted", "Dove gli imputati non sono dichiarati il nuovo regesto non nomina nessuno come accusato («contro X» sparisce) e il titolo resta com'è: il ruolo delle persone citate è da verificare."),
      div(style = "max-height:45vh;overflow:auto", tags$table(class = "table table-sm small",
        tags$tr(tags$th("Fascicolo"), tags$th("Prima"), tags$th("Dopo")),
        lapply(seq_len(nrow(a)), function(i) tags$tr(tags$td(a$segnatura[i]),
                                                      tags$td(if (nzchar(a$protetti[i])) "(scritto a mano) ", a$prima[i]), tags$td(strong(a$dopo[i])))))))
  })
  output$sel_ricomponi_conferma <- renderUI({
    np <- sum(n_protetti(prova_ricomposizione()))
    if (np) checkboxInput("sel_conferma_manuali", sprintf("Ho guardato le differenze: sostituisco %d testi scritti a mano", np), FALSE)
  })
  observeEvent(input$sel_ricomponi_applica, tryCatch({
    if (sum(n_protetti(prova_ricomposizione())) && !isTRUE(input$sel_conferma_manuali))
      stop("Spunta la conferma: la ricomposizione sostituirebbe testi scritti a mano.", call. = FALSE)
    copia <- backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"), "ricomposizione")
    e <- ricomponi_in_blocco(con, spuntati(), anche_titoli = isTRUE(input$sel_anche_titoli), anche_regesti = isTRUE(input$sel_anche_regesti))
    removeModal(); ricarica(ricarica() + 1); dopo_modifica(e$id)
    notifica_con_cartella(sprintf("%s. Copia di sicurezza fatta.", if (nrow(e) == 1) "Ricomposto 1 fascicolo" else sprintf("Ricomposti %d fascicoli", nrow(e))), "backup")
  }, error = function(err) showNotification(conditionMessage(err), type = "error", duration = NULL)))

  # Elimina le spuntate: conferma scrivendo il numero, copia di sicurezza prima.
  observeEvent(input$sel_elimina, {
    n <- length(spuntati())
    showModal(modalDialog(
      title = sprintf("Eliminare %d fascicoli?", n), easyClose = TRUE,
      p("Le schede spuntate spariscono dal database con persone, reati, luoghi e collegamenti. ",
        "Prima si fa una copia di sicurezza completa, da cui si possono recuperare."),
      textInput("sel_elimina_conferma", sprintf("Per confermare scrivi %d", n)),
      footer = tagList(modalButton("Annulla"), actionButton("sel_elimina_applica", "Elimina", class = "btn-danger"))))
  })
  observeEvent(input$sel_elimina_applica, tryCatch({
    if (!identical(trimws(input$sel_elimina_conferma), as.character(length(spuntati()))))
      stop("Scrivi il numero dei fascicoli da eliminare per confermare.", call. = FALSE)
    copia <- backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"), "eliminazione")
    ids <- spuntati(); n <- elimina_fascicoli(con, ids)
    removeModal(); session$sendCustomMessage("spunte", "nessuna"); ricarica(ricarica() + 1); dopo_modifica(ids)
    notifica_con_cartella(sprintf("%s. Copia di sicurezza fatta.", if (n == 1) "Eliminato 1 fascicolo" else sprintf("Eliminati %d fascicoli", n)), "backup", duration = 10)
  }, error = function(err) showNotification(conditionMessage(err), type = "error", duration = NULL)))

  observeEvent(input$sel_applica, {
    tryCatch({
      # Accanto al database in uso: con TRAMA_DB (prove) la copia resta
      # vicino alla copia di prova, non fra i backup veri.
      copia <- backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"), "correzione")
      e <- imposta_campo_in_blocco(con, spuntati(), input$sel_campo, input$sel_valore, modo())
      removeModal(); ricarica(ricarica() + 1); dopo_modifica(e$id)
      notifica_con_cartella(sprintf("Aggiornati %d fascicoli. Copia di sicurezza fatta.", nrow(e)), "backup")
    }, error = function(err) showNotification(conditionMessage(err), type = "error", duration = NULL))
  })
}
