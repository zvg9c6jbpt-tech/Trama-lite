# ─────────────────────────────────────────────────────────────
# R/elenco_server.R: la scheda Fascicoli: ricerca, filtri, tabella e scaffale
# Tabella di Fascicoli: tutto il fondo, o un luogo o un controllo di Qualità; gli indici di riga sono di questa vista, non di dati().
# `anomalie_stato`, `naviga` e `nuovo` sono funzioni (definiti più avanti in app.R). Restituisce gli stati che servono fuori:
# filtro_schedatura, solo_incompleti, solo_errori, busta_elenco, cerca, azzera_filtri_elenco, righe_tabella, ordine_elenco, apri_da_elenco, righe_ricerca.
# ─────────────────────────────────────────────────────────────

elenco_server <- function(input, output, session, con, ricarica, dati, fondo_attivo, aperto, anomalie_stato, naviga, nuovo) {
  
  # Tabella di Fascicoli: tutto il fondo, o un luogo o un controllo di Qualità.
  # Gli indici di riga sono di questa vista, non di dati().
  filtro_schedatura <- reactiveVal(NULL)    # list(etichetta, ids) oppure NULL
  solo_incompleti   <- reactiveVal(FALSE)
  solo_errori       <- reactiveVal(FALSE)   # «Con errori»: schede segnalate dal controllo
  busta_elenco <- reactiveVal(NULL)
  cerca <- reactiveVal("")
  observeEvent(input$cerca_fascicoli, cerca(input$cerca_fascicoli))
  azzera_filtri_elenco <- function() {
    cerca(""); updateTextInput(session, "cerca_fascicoli", value = "")
    busta_elenco(NULL); filtro_schedatura(NULL); solo_incompleti(FALSE); solo_errori(FALSE)
    session$sendCustomMessage("solo_incompleti", FALSE)
  }
  observeEvent(input$azzera_filtri_elenco, azzera_filtri_elenco())
  # «Azzera ricerca e filtri» compare solo se c'è qualcosa da azzerare (15/10).
  output$filtri_attivi <- reactive(nzchar(cerca()) || solo_incompleti() || solo_errori() || !is.null(busta_elenco()) || !is.null(filtro_schedatura()))
  outputOptions(output, "filtri_attivi", suspendWhenHidden = FALSE)

  righe_ricerca <- reactive({
    session$userData$ordine_versione()
    d <- ordina_consultazione(con, dati())
    f <- filtro_schedatura()
    if (!is.null(f)) d <- d[d$id %in% f$ids, , drop = FALSE]
    d <- filtra_avanzato(d, cerca())
    if (solo_incompleti()) d <- d[which(completezza_core(d) < 4L), , drop = FALSE]
    if (solo_errori()) d <- d[d$id %in% errori_ids(), , drop = FALSE]
    d
  })

  # Id delle schede con una correzione o una decisione (R/anomalie.R); vuoto finché il controllo non è finito.
  errori_ids <- reactive({
    st <- anomalie_stato()
    if (is.null(st) || !identical(st$fondo, fondo_attivo()) || is.null(st$an)) return(character(0))
    an <- st$an
    righe <- unique(c(an$pr$celle$riga, vapply(an$pr$decisioni, function(x) x$riga, 1L)))
    righe <- righe - 1L    # `riga` conta l'intestazione: la prima scheda è la riga 2
    as.character(an$ids[righe[righe >= 1L & righe <= length(an$ids)]])
  })

  # I filtri a pillola sopra la tabella, con i loro conti: Tutti · Da completare · Con errori.
  output$chips_filtri <- renderUI({
    d <- dati(); ricarica()
    n_inc <- length(which(completezza_core(d) < 4L)); n_err <- length(intersect(errori_ids(), as.character(d$id)))
    chip <- function(id, etichetta, n, attiva, classe = "")
      actionButton(id, tagList(etichetta, span(class = "n", n)), class = paste("btn-pillola chip-filtro", classe, if (attiva) "attiva"),
                   `aria-pressed` = tolower(as.character(attiva)))
    tagList(chip("filtri_tutti", "Tutti", nrow(d), !solo_incompleti() && !solo_errori()),
            chip("solo_incompleti", "Da completare", n_inc, solo_incompleti(), "chip-avviso"),
            chip("solo_errori", "Con errori", n_err, solo_errori(), "chip-errore"))
  })
  observeEvent(input$filtri_tutti, { solo_incompleti(FALSE); solo_errori(FALSE); session$sendCustomMessage("solo_incompleti", FALSE) })
  observeEvent(input$solo_errori, {   # «Da completare» e «Con errori» si escludono
    solo_errori(!solo_errori())
    if (solo_errori() && solo_incompleti()) { solo_incompleti(FALSE); session$sendCustomMessage("solo_incompleti", FALSE) }
  })

  righe_tabella <- reactive({
    d <- righe_ricerca()
    b <- busta_elenco()
    if (!is.null(b)) d <- d[which(chiave_busta(d$busta, d$suffisso_b) == b), , drop = FALSE]
    d
  })

  # Numero accanto a «Fascicoli» nel menu: le schede ancora da completare.
  output$conta_fascicoli <- renderUI({
    ricarica(); n <- length(which(completezza_core(dati()) < 4L))
    if (n > 0L) span(class = "nav-badge", title = sprintf("%d schede da completare (meno di 4 campi fondamentali)", n), n)
  })
  # Un filtro `campo:valore` illeggibile va segnalato, altrimenti la tabella si svuota senza spiegazione.
  avviso_ricerca_ui <- function(q) { a <- avvisi_ricerca(q)
    if (length(a)) div(class = "avviso-ricerca small mb-2", role = "status", icon("triangle-exclamation"),
                       lapply(a, function(x) span(class = "ms-1 me-2", x))) }
  output$avviso_ricerca_elenco    <- renderUI(avviso_ricerca_ui(cerca()))
  output$conta_elenco <- renderText(sprintf("%d su %d fascicol%s", nrow(righe_tabella()), nrow(dati()),
                                            if (nrow(dati()) == 1) "o" else "i"))

  output$filtro_schedatura_info <- renderUI({
    f <- filtro_schedatura()
    tagList(
      if (!is.null(f)) div(class = "filtro-schedatura", strong(f$etichetta),
        actionLink("filtro_schedatura_via", "Togli questo filtro", class = "ms-2")),
      if (!is.null(busta_elenco())) div(class = "filtro-schedatura",
        strong(local({
          sig <- dati()$sigla_busta[which(chiave_busta(dati()$busta, dati()$suffisso_b) == busta_elenco())[1]]
          if (.vuoto(sig)) paste("Busta", busta_elenco()) else paste(sig, busta_elenco())
        })),
        actionLink("busta_elenco_via", "Tutte le buste", class = "ms-2")),
      if (solo_incompleti()) p(class = "small text-muted",
        "Solo incompleti: meno di 4 campi fondamentali compilati. Premi di nuovo «incompleti» per includere tutti."))
  })
  observeEvent(input$filtro_schedatura_via, filtro_schedatura(NULL))
  observeEvent(input$busta_elenco_via, busta_elenco(NULL))
  observeEvent(input$vista_elenco, {
    if (identical(input$vista_elenco, "scaffale")) busta_elenco(NULL)
  })

  observeEvent(input$solo_incompleti, {
    solo_incompleti(!solo_incompleti())
    if (solo_incompleti()) solo_errori(FALSE)
    session$sendCustomMessage("solo_incompleti", solo_incompleti())
  })

  # La busta è un filtro a parte: tornando allo scaffale si toglie.
  observeEvent(input$scaffale_busta, {
    busta_elenco(input$scaffale_busta)
    session$sendCustomMessage("vista_elenco", "tabella")
  })
  output$scaffale <- renderUI(scaffale_ui(righe_ricerca()))

  output$tabella <- renderDT({
    d <- tabella_elenco(righe_tabella())
    nascoste <- match(c("id", "k_segn", "k_stato", "k_anno", "k_busta"), names(d)) - 1L
    # Lo stato sta accanto alla segnatura (solo dove manca qualcosa): la colonna Stato non si mostra più.
    nascoste <- c(nascoste, match("Stato", names(d)) - 1L)
    # Tortura e sentenza solo se c'è qualcosa da dire.
    r_ <- righe_tabella()
    tutti_nd <- function(v) all(is.na(v) | !nzchar(v) | v == "n.d.")
    if (nrow(r_) && tutti_nd(r_$tortura)) nascoste <- c(nascoste, match("Tortura", names(d)) - 1L)
    if (nrow(r_) && tutti_nd(r_$sentenza)) nascoste <- c(nascoste, match("Sentenza", names(d)) - 1L)
    col <- function(nome) match(nome, names(d)) - 1L
    # Le buste come gruppi: riga di testa con fascicoli, avanzamento e casella «tutti». Vale con l'ordine per segnatura.
    testa_busta <- JS(sprintf("function(rows, group) {
      if (window.gruppiBusta === false) return $('<tr class=\"gruppo-busta\" style=\"display:none\"/>');   // elenco piatto: una riga invisibile al posto della testa
      var dati = rows.data().toArray(), n = dati.length, ok = 0;
      dati.forEach(function(r) { if (r[%d] >= 4) ok++; });
      var pct = Math.round(100 * ok / n), ids = dati.map(function(r) { return r[0]; });
      var tutte = ids.every(function(i) { return window.spuntati && window.spuntati.has(i); });
      var nodi = rows.nodes(), colonne = nodi.length ? $(nodi[0]).children('td').length : 8;   // quante colonne si vedono davvero
      var avanzamento = pct >= 100 ? ''   // una busta a posto non dice niente: si segnala solo quel che manca
        : '<span class=\"busta-barra\" role=\"img\" aria-label=\"' + pct + '%% completa\"><span style=\"width:' + pct + '%%\"></span></span><span class=\"busta-pct\">' + pct + '%% completa</span>';
      return $('<tr class=\"gruppo-busta\"/>').append('<td colspan=\"' + colonne + '\"><label class=\"busta-spunta\"><input type=\"checkbox\" class=\"spunta-busta\" data-ids=\"' +
        ids.join(',') + '\"' + (tutte ? ' checked' : '') + ' aria-label=\"Spunta tutta la busta\"></label><span class=\"busta-nome\">' + $('<div/>').text(group).html() +
        '</span><span class=\"busta-n\">' + n + (n === 1 ? ' fascicolo' : ' fascicoli') + '</span>' + avanzamento + '</td>');
    }", col("k_stato")))
    datatable(
      d, escape = FALSE, rownames = FALSE, selection = "none", extensions = "RowGroup",
      class = "tabella-fascicoli hover",
      # La casella in testa alla colonna spunta o toglie tutte le righe trovate.
      colnames = c("Luoghi citati" = "Luoghi", "Titolo e persone" = "Titolo",
                   '<input type="checkbox" id="spunta_tutte" aria-label="Spunta tutte le righe trovate">' = "Sel"),
      # Clic sulla riga: apre la scheda; sulla casella: solo spunta (R/selezione.R). La testa di busta non apre niente.
      callback = JS(sprintf("window.tabellaFascicoli = table;
                     table.on('click', 'a.link-apri', function(e) { e.preventDefault(); });   // il titolo è un link: raggiungibile con Tab, Invio apre la scheda (il clic sale alla riga)
                     table.on('click', 'tbody tr', function(e) {
                       if ($(e.target).closest('td.col-spunta, tr.gruppo-busta').length) return;
                       var r = table.row(this).data();
                       if (r) Shiny.setInputValue('apri_fascicolo', r[0], {priority: 'event'});
                     });
                     // Ordinando per anno, reato o altro le buste si spezzerebbero in più pezzi: allora l'elenco torna piatto.
                     window.gruppiBusta = true;
                     table.on('order.dt', function() {
                       var o = table.order();
                       window.gruppiBusta = !o.length || o[0][0] === %d;   // l'evento arriva prima del ridisegno: la testa lo legge
                       $(table.table().node()).toggleClass('elenco-piatto', !window.gruppiBusta);   // fuori dai gruppi la segnatura si scrive intera
                     });", col("Segnatura"))),
      options = list(
        dom = "tip", pageLength = 100, order = list(), autoWidth = FALSE,
        rowGroup = list(dataSrc = col("k_busta"), startRender = testa_busta),
        language = if (nrow(dati()) == 0) modifyList(DT_ITALIANO, list(emptyTable = "Nessun fascicolo in questo fondo."))
                   else DT_ITALIANO,
        drawCallback = JS("function() { if (window.evidenziaRigaAperta) window.evidenziaRigaAperta();
                                         if (window.ridisegnaSpunte) window.ridisegnaSpunte(); }"),
        columnDefs = list(
          list(visible = FALSE, targets = nascoste),
          list(orderData = match("k_segn",  names(d)) - 1L, targets = match("Segnatura", names(d)) - 1L),
          list(orderData = match("k_stato", names(d)) - 1L, targets = match("Stato", names(d)) - 1L),
          list(orderData = match("k_anno",  names(d)) - 1L, targets = match("Anno", names(d)) - 1L),
          # Sulle finestre strette escono tortura e sentenza (tema.R).
          list(className = "col-dati col-anno",   targets = match("Anno",   names(d)) - 1L),
          list(className = "col-dati col-reato",  targets = match("Reato",  names(d)) - 1L),
          list(className = "col-dati col-luoghi", targets = match("Luoghi", names(d)) - 1L),
          list(className = "col-segn",            targets = match("Segnatura", names(d)) - 1L),
          list(className = "col-titolo",          targets = match("Titolo", names(d)) - 1L),
          list(className = "col-esito",   targets = match(c("Tortura", "Sentenza"), names(d)) - 1L),
          list(orderable = FALSE, className = "col-spunta", targets = match("Sel", names(d)) - 1L)))
    )
  }, server = FALSE)

  # La riga della scheda aperta resta evidenziata (anche dopo un ridisegno).
  observe(session$sendCustomMessage("riga_aperta", if (is.null(aperto())) "" else aperto()))

  # Le schede nell'ordine della tabella: è l'elenco che prec. e succ. scorrono.
  ordine_elenco <- reactive({
    ids <- righe_tabella()$id
    i <- input$tabella_rows_all
    if (length(i) == length(ids) && all(i >= 1 & i <= length(ids))) ids[i] else ids
  })

  apri_da_elenco <- function(id) {
    if (identical(id, aperto())) return(nav_select("nav", "Scheda"))
    naviga(function() { aperto(id); nav_select("nav", "Scheda") })
  }
  observeEvent(input$torna_elenco, nav_select("nav", "Fascicoli"))
  observeEvent(input$apri_fascicolo, apri_da_elenco(input$apri_fascicolo))

  output$posizione_scheda <- renderText({
    ids <- ordine_elenco()
    i <- if (is.null(aperto())) NA else match(aperto(), ids)
    if (is.na(i)) "" else sprintf("%d / %d", i, length(ids))   # una scheda nuova non ha posto nell'elenco: niente «— / 24»
  })
  list(filtro_schedatura = filtro_schedatura, solo_incompleti = solo_incompleti, solo_errori = solo_errori, busta_elenco = busta_elenco,
       cerca = cerca, azzera_filtri_elenco = azzera_filtri_elenco, righe_tabella = righe_tabella, ordine_elenco = ordine_elenco,
       apri_da_elenco = apri_da_elenco, righe_ricerca = righe_ricerca)
}
