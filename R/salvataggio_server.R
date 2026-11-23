# ─────────────────────────────────────────────────────────────
# R/salvataggio_server.R: Salva, la scelta D30 sugli estremi uguali, Salva e vai, Annulla
# `modulo_pronto` e `campi_modificati` vengono dal modulo della Scheda; `naviga` passa dall'avviso delle modifiche non salvate.
# ─────────────────────────────────────────────────────────────

salvataggio_server <- function(input, output, session, con, ricarica, dati, aperto, richiesta, nuovo, istantanea, campi_modificati,
                               modulo_pronto, fascicolo_caricato, filtro_schedatura, dopo_salva, ordine_elenco, righe_tabella,
                               naviga, PROFILO_MODULO) {
  # Il vincolo unico sulla segnatura scoppia apposta (niente INSERT OR REPLACE, vedi salva_fascicolo in R/crud.R);
  # qui diventa la segnatura occupata, il titolo di chi la occupa e le due strade possibili.
  .perche_non_salvata <- function(f, esito) {
    m <- conditionMessage(attr(esito, "condition"))
    if (!grepl("UNIQUE constraint failed: (fascicoli|index .idx_segnatura)", m))
      return(paste("Non salvata:", m))

    testo <- function(x) if (.vuoto(x)) "" else as.character(x)
    occupa <- dbGetQuery(con,
      # busta e fascicolo sono INTEGER: CAST su entrambi i lati, SQLite non converte i tipi.
      "SELECT titolo FROM fascicoli
        WHERE IFNULL(fondo_id,'') = ?
          AND IFNULL(CAST(busta AS TEXT),'') = ?
          AND IFNULL(busta_suffisso,'') = ?
          AND IFNULL(CAST(fascicolo AS TEXT),'') = ?
          AND IFNULL(fascicolo_suffisso,'') = ?
          AND IFNULL(id,'') <> ?",
      list(testo(fondo_id_da(con, f$archivio, f$fondo)), testo(f$busta), testo(f$busta_suffisso), testo(f$fascicolo),
           testo(f$fascicolo_suffisso), testo(f$id)))$titolo

    paste0(componi_segnatura(f$archivio, f$fondo, f$serie,
                             f$busta, f$fascicolo, f$fascicolo_suffisso, suffisso_busta = f$busta_suffisso),
           " è già in uso",
           if (length(occupa) && nzchar(occupa[1])) paste0(" da «", occupa[1], "»"), ". ",
           "Cambia il numero del fascicolo, oppure scrivi un suffisso (bis, ter…) ",
           "nel campo accanto. Nulla è stato scritto.")
  }

  # Salva: restituisce l'id scritto, o NULL se non ha scritto niente (il motivo è già in un avviso).
  salva_scheda <- function() {
    if (!modulo_pronto()) {
      showNotification("Il fascicolo si sta ancora caricando: nulla è stato scritto, riprova fra un istante.",
                       type = "warning")
      return(NULL)
    }
    # Una scheda esistente non cambiata non si riscrive; una nuova si salva sempre (stesso rilevamento del pallino).
    if (!is.null(aperto()) && !is.null(istantanea()) && !length(campi_modificati())) {
      showNotification("Nessuna modifica: la scheda \u00e8 gi\u00e0 salvata cos\u00ec.", type = "message")
      return(aperto())
    }
    f <- raccogli_modulo(input, PROFILO_MODULO)
    if (!is.null(aperto())) f$id <- aperto()
    
    # Busta e fascicolo: obbligatori per una scheda nuova, solo un avviso per una già nel database (controllo_collocazione, R/crud.R).
    ctrl <- controllo_collocazione(f, nuova = is.null(aperto()))
    if (!is.null(ctrl$errore)) {
      showNotification(ctrl$errore, type = "warning")
      return(NULL)
    }
    # Il soggetto produttore c'è sempre in una scheda nuova (in un fondo nuovo si scrive nella prima). Solo qui, non in import.
    if (is.null(aperto()) && !length(Filter(function(x) !.vuoto(x), as.list(f$produttori)))) {
      showNotification("Indica il soggetto produttore (un ente, per esempio «Inquisizione di Napoli»): la prima scheda di un fondo lo fissa per le successive.", type = "warning", duration = 10)
      return(NULL)
    }
    for (avviso in ctrl$avvisi) showNotification(avviso, type = "warning")
    # La data o c'è, o si dichiara l'assenza ("n.d."). Solo qui, non in salva_fascicolo().
    nd <- isTRUE(suppressWarnings(as.integer(f$data_nd)) == 1)
    con_data <- !.vuoto(f$estremo_remoto) || !.vuoto(f$estremo_recente)
    if (.vuoto(f$estremo_remoto) && !nd) {
      showNotification(if (con_data)
                         "Manca l'estremo remoto: c'è solo il recente. Inseriscilo (anche uguale al recente)."
                       else
                         "Manca la data: inserisci l'estremo remoto, oppure spunta «n.d.» se la fonte non la dà. Nulla è stato scritto.",
                       type = "error", duration = 8)
      return(NULL)
    }
    if (con_data && nd) {
      showNotification("Data inserita e «n.d.» spuntato insieme: togli uno dei due. Nulla è stato scritto.",
                       type = "error", duration = 8)
      return(NULL)
    }
    # Giorno e mese facoltativi: devono essere una data valida con l'anno del campo; si salvano in forma canonica.
    for (campo_d in names(EXTRA_DATA_PRECISA)) {
      nome_d <- EXTRA_DATA_PRECISA[[campo_d]]; testo_d <- trimws(as.character(unlist(f$extra[[nome_d]]))[1])
      if (is.na(testo_d) || !nzchar(testo_d)) { f$extra[[nome_d]] <- NULL; next }
      dp <- .data_precisa(testo_d); anno_d <- suppressWarnings(as.integer(f[[campo_d]]))
      if (is.null(dp)) {
        showNotification(sprintf("«%s» non è una data: scrivi «aaaa-mm-gg» (1644-03-29) o «mese aaaa» (marzo 1644). Nulla è stato scritto.", testo_d), type = "error", duration = 8)
        return(NULL)
      }
      if (is.na(anno_d) || dp$anno != anno_d) {
        showNotification(sprintf("La data «%s» è del %d, ma l'%s è %s: devono coincidere. Nulla è stato scritto.", testo_d, dp$anno,
                                 if (identical(campo_d, "estremo_remoto")) "estremo remoto" else "estremo recente", if (is.na(anno_d)) "vuoto" else as.character(anno_d)),
                         type = "error", duration = 8)
        return(NULL)
      }
      f$extra[[nome_d]] <- dp$canonica
    }
    # L'estremo recente non viene prima del remoto; gli anni stanno fra 1200 e 1899.
    r1 <- suppressWarnings(as.integer(f$estremo_remoto)); r2 <- suppressWarnings(as.integer(f$estremo_recente))
    fuori <- c(r1, r2)[which(!is.na(c(r1, r2)) & (c(r1, r2) < ANNI_AMMESSI[1] | c(r1, r2) > ANNI_AMMESSI[2]))]
    if (length(fuori)) {
      showNotification(sprintf("L'anno %d è fuori dall'intervallo %d\u2013%d: correggilo. Nulla è stato scritto.", fuori[1], ANNI_AMMESSI[1], ANNI_AMMESSI[2]),
                       type = "error", duration = 8)
      return(NULL)
    }
    if (!is.na(r1) && !is.na(r2) && r2 < r1) {
      showNotification(sprintf("L'estremo recente (%d) viene prima del remoto (%d): correggi le date. Nulla è stato scritto.", r2, r1),
                       type = "error", duration = 8)
      return(NULL)
    }
    # D30: i due estremi sono la stessa data. Nella Scheda non c'è anteprima, quindi si chiede (Sì / No); il «No» vale per questa coppia.
    uguali <- estremi_uguali(f)
    togli_recente <- FALSE
    if (!is.null(uguali)) {
      chiave <- paste(if (is.null(aperto())) "nuova" else aperto(), uguali)
      if (identical(d30_scelta(), "togli")) togli_recente <- TRUE
      else if (!identical(d30_scelta(), "tieni") && !identical(d30_tenuto(), chiave)) {
        showModal(modalDialog(
          title = "Due estremi uguali",
          p(sprintf("L'estremo remoto e il recente sono la stessa data (%s): è una data sola, scritta due volte.", uguali)),
          p("Tolgo l'estremo recente?"),
          p(class = "text-muted small", "Se lo tieni, la scheda si salva com'è."),
          footer = tagList(actionButton("d30_no", "No, tieni tutti e due", class = "btn-outline-secondary"),
                           actionButton("d30_si", "Sì, togli il recente", class = "btn-primary")),
          easyClose = FALSE))
        return(NULL)
      }
    }
    d30_scelta(NULL)
    if (togli_recente) { f$estremo_recente <- NA_integer_; f$extra[[EXTRA_DATA_PRECISA[["estremo_recente"]]]] <- NULL }
    esito <- try(salva_fascicolo(con, f, atteso = fascicolo_caricato(), storia = TRUE), silent = TRUE)
    if (inherits(esito, "try-error")) {
      registra("ERRORE", "salvataggio scheda non riuscito", conditionMessage(attr(esito, "condition")))
      showNotification(.perche_non_salvata(f, esito), type = "error", duration = NULL)
      return(NULL)
    }
    aperto(f$id)
    if (togli_recente) richiesta(richiesta() + 1)   # il campo «Estremo recente» si svuota anche a schermo
    else if (!is.null(uguali)) d30_tenuto(paste(f$id, uguali))   # «No»: per questa scheda e questa data non lo richiede
    # Una scheda nuova salvata con la tabella filtrata su un luogo entra nella vista.
    fs <- filtro_schedatura()
    if (!is.null(fs) && !(f$id %in% fs$ids)) filtro_schedatura(list(etichetta = fs$etichetta, ids = c(fs$ids, f$id)))
    dopo_salva(TRUE)
    ricarica(ricarica() + 1)
    showNotification(
      esito_ok(paste("Salvata:", componi_segnatura(f$archivio, f$fondo, f$serie,
                                                   f$busta, f$fascicolo, f$fascicolo_suffisso, suffisso_busta = f$busta_suffisso))), type = "message")
    f$id
  }

  # D30: la scelta Sì / No e la ripresa dell'azione che l'aveva chiesta (Salva, Salva e prossimo, Chiudi).
  d30_scelta <- reactiveVal(NULL); d30_tenuto <- reactiveVal(NULL); d30_ripresa <- reactiveVal(NULL)
  d30_risposta <- function(scelta) {
    removeModal(); d30_scelta(scelta)
    azione <- d30_ripresa(); if (!is.null(azione)) azione()
    d30_scelta(NULL)
  }
  observeEvent(input$d30_si, d30_risposta("togli"))
  observeEvent(input$d30_no, d30_risposta("tieni"))
  observeEvent(input$salva, { d30_ripresa(function() salva_scheda()); salva_scheda() })

  # Salva e passa alla scheda accanto; i vicini si guardano prima di salvare (con «incompleti» la scheda uscirebbe dall'elenco).
  salva_e_vai <- function(passo, solo_incompleti = FALSE) {
    d30_ripresa(function() salva_e_vai(passo, solo_incompleti))
    ids <- ordine_elenco()
    i <- if (is.null(aperto())) NA else match(aperto(), ids)
    # Niente da scrivere: ci si sposta senza toccare la data di modifica.
    salvato <- if (!is.null(aperto()) && !length(campi_modificati()) && modulo_pronto()) aperto()
               else salva_scheda()
    if (is.null(salvato)) return()
    if (is.na(i)) {                       # scheda nuova: dove è finita
      ids <- righe_tabella()$id
      i <- match(salvato, ids)
      if (is.na(i)) return(showNotification("Salvata; non è nell'elenco di Fascicoli (c'è una ricerca attiva).",
                                            type = "message"))
    }
    candidati <- if (passo > 0) ids[seq_along(ids) > i] else rev(ids[seq_along(ids) < i])
    if (solo_incompleti) {
      d <- dati()
      candidati <- candidati[candidati %in% d$id[which(completezza_core(d) < 4L)]]
    }
    if (!length(candidati))
      return(showNotification(if (solo_incompleti) "Nessun fascicolo incompleto dopo questo nell'elenco."
                              else if (passo > 0) "Era l'ultimo fascicolo dell'elenco." else "Era il primo fascicolo dell'elenco.",
                              type = "message"))
    aperto(candidati[1])
  }
  observeEvent(input$salva_prec,       salva_e_vai(-1L))
  observeEvent(input$salva_succ,       salva_e_vai(1L))
  observeEvent(input$salva_incompleto, salva_e_vai(1L, solo_incompleti = TRUE))
  # «Chiudi»: senza modifiche alla Home; con modifiche si salva, e se non riesce si resta qui.
  observeEvent(input$salva_chiudi, {
    d30_ripresa(function() { if (length(campi_modificati()) && is.null(salva_scheda())) return(); nav_select("nav", "Home") })
    if (length(campi_modificati()) && is.null(salva_scheda())) return()
    nav_select("nav", "Home")
  })

  # Annulla: la scheda torna com'era all'apertura (chiedendo, se c'è qualcosa da perdere).
  observeEvent(input$annulla_scheda, naviga(function() richiesta(richiesta() + 1)))

  list(salva_scheda = salva_scheda)
}
