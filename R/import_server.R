# ─────────────────────────────────────────────────────────────
# R/import_server.R: importare un foglio, controllarlo prima, correggere le schede già nel database
# Anteprima, import (accoda / nuovo fondo / sostituisci) e «Pulisci e scarica». La finestra di revisione è in R/revisione_server.R (pagina: R/revisione_import.R),
# «Errori e discordanze» in R/errori_import_server.R, gli errori già nel database in R/anomalie_server.R.
# Restituisce gli stati che servono fuori (anomalie_stato, .a_righe) e quelli che i collaudi leggono (lettura, revisione_import, rev_selezione, elenco_errori, risultato_import, risultato_pulito). `solo_errori` è il filtro «Con errori» dell'elenco; `attiva_fondo` serve a passare al fondo appena importato.
# ─────────────────────────────────────────────────────────────

import_server <- function(input, output, session, con, ricarica, fondo_attivo, attiva_fondo, solo_errori) {
  # Intestazioni esatte del foglio caricato: l'export le restituisce identiche.
  risultato_import      <- reactiveVal(NULL)
  forza_pulisci         <- reactiveVal(FALSE)  # «Scarica pulito» scelto
  .e_pulisci <- function() isTRUE(forza_pulisci())
  risultato_pulito      <- reactiveVal(NULL)   # l'esito di «Pulisci e scarica» (R/arianna_pulisci.R, pulisci_foglio)

  lettura <- reactive({
    f <- input$file_import
    if (is.null(f)) return(NULL)
    try(leggi_arianna(f$datapath), silent = TRUE)
  })

  # I messaggi di rifiuto vanno a capo con \n: in HTML le righe vuote fanno spazio, quelle con spazi restano allineate.
  .a_righe <- function(testo) {
    r <- strsplit(testo, "\n", fixed = TRUE)[[1]]
    lapply(r, function(x)
      if (!nzchar(trimws(x))) div(style = "height:.5rem")
      else if (startsWith(x, " ")) div(class = "font-monospace small", HTML(gsub(" ", "&nbsp;", htmltools::htmlEscape(x), fixed = TRUE)))
      else div(x))
  }

  output$anteprima_import <- renderUI({
    a <- lettura()
    if (is.null(a)) return(NULL)
    if (inherits(a, "try-error"))
      return(div(class = "alert alert-danger py-2",
                 .a_righe(conditionMessage(attr(a, "condition")))))
    # Prima di importare si dice tutto: colonne riconosciute per sinonimo, ignorate, correzioni che l'import applicherà.
    k <- correzioni_import(a, con)
    elenco <- function(x) paste(x, collapse = " \u00b7 ")
    # Le segnature che non so leggere per intero (5/10): le eccezioni non si perdono in silenzio.
    cs <- a$colonna[["Segnatura"]]
    g <- if (!is.na(cs) && nrow(a$dati)) segnature_da_guardare(a$dati[[cs]]) else data.frame()
    rif <- righe_segnatura_non_ammessa(a)       # le righe che NON entrano finché non si corregge la segnatura (20/10)
    # Gli spazi doppi nella segnatura non respingono la riga: li corregge la finestra «Controlla prima di importare» (D28, 23/10). Qui restano solo le righe che nessuna correzione sistema.
    sistema_la_finestra <- function(x) !length(.scomponi_segnatura(x)$illeggibile) && all(.segnatura_forma(x)$motivi %in% "doppio spazio non ammesso")
    corregge_ <- if (nrow(rif)) vapply(rif$segnatura, sistema_la_finestra, TRUE) else logical(0)
    n_spazi <- sum(corregge_); rif <- rif[!corregge_, , drop = FALSE]
    tagList(
      # Un solo modello (2/10): un foglio nel vecchio formato si converte, e si dice.
      p(if (a$profilo %in% PROFILI_LETTURA)
          tagList(strong("Foglio nel vecchio formato "), a$profilo, ": verrà convertito nel modello \u00b7 ")
        else tagList(strong("Foglio nel modello"), " \u00b7 "),
        nrow(a$dati), " righe"),
      # Di quale archivio è il foglio: lo dicono le sue segnature (7/10).
      local({
        d <- archivi_nel_foglio(a)
        if (nrow(d)) div(class = "small mb-2",
          span("Archivio e fondo nel foglio:"), " ",
          lapply(seq_len(nrow(d)), function(i) span(class = "me-3",
            span(class = "no-trad", paste(c(d$archivio[i], d$fondo[i])[nzchar(c(d$archivio[i], d$fondo[i]))], collapse = " \u00b7 ")),
            span(class = "text-muted", sprintf(" (%d righe)", d$righe[i])))))
      }),
      if (nrow(a$fuori_modello))
        div(class = "alert alert-secondary py-2 small",
            strong("Colonne che il modello non ha (i dati restano nel database, non escono nell'export): "),
            elenco(sprintf("%s (%d celle piene)", a$fuori_modello$colonna, a$fuori_modello$celle))),
      if (nrow(a$sinonimi))
        div(class = "alert alert-secondary py-2 small",
            strong("Colonne riconosciute per sinonimo: "),
            elenco(sprintf("%s = %s", a$sinonimi$foglio, a$sinonimi$profilo))),
      if (length(a$spurie))
        div(class = "alert alert-secondary py-2 small",
            strong("Colonne ignorate (vuote o senza intestazione): "),
            elenco(substr(a$spurie, 1, 60))),
      if (length(a$ignote))
        div(class = "alert alert-warning py-2",
            strong("Colonne non riconosciute: "), elenco(a$ignote)),
      if (length(a$mancanti))
        div(class = "alert alert-secondary py-2 small",
            strong("Colonne del modello che il foglio non ha (si aggiungono, vuote, nel foglio pulito e nell'export): "), elenco(a$mancanti)),
      if (n_spazi > 0L)
        div(class = "alert alert-secondary py-2 small",
            strong(sprintf("%d segnature hanno spazi doppi: ", n_spazi)), "le correggi nella finestra «Controlla prima di importare», dove vedi il prima e il dopo. Con «Importa com\u2019è, senza correggere» queste righe non entrerebbero."),
      if (nrow(rif))
        div(class = "alert alert-danger py-2 small",
            strong(sprintf("Segnature non ammesse (%d righe non entrano finché non le correggi nel foglio): ", nrow(rif))),
            tags$ul(class = "mb-0 mt-1", lapply(seq_len(min(nrow(rif), 6L)), function(i)
              tags$li(span(sprintf("riga %d: ", rif$i[i] + 1L)), span(class = "no-trad", rif$segnatura[i]), span(" \u2014 "), span(rif$motivo[i])))),
            if (nrow(rif) > 6L) span(sprintf("… e altre %d.", nrow(rif) - 6L))),
      if (nrow(g))
        div(class = "alert alert-warning py-2 small",
            strong(sprintf("Segnature con parti che non so leggere (%d): ", nrow(g))),
            span("l'import le legge fin dove può; controllale prima, o correggi la scheda dopo."),
            tags$ul(class = "mb-0 mt-1", lapply(seq_len(min(nrow(g), 6L)), function(i)
              tags$li(span(sprintf("riga %d: ", g$riga[i])), span(class = "no-trad", g$segnatura[i]), span(" \u2014 "), span(class = "no-trad", g$motivo[i])))),
            if (nrow(g) > 6L) span(sprintf("… e altre %d.", nrow(g) - 6L))),
      if (nrow(k))
        div(class = "alert alert-secondary py-2 small",
            strong("Correzioni che verranno applicate: "),
            tags$ul(class = "mb-0",
                    lapply(seq_len(nrow(k)), function(i)
                      tags$li(sprintf("%s \u2014 %s: %d cell%s", k$colonna[i], k$regola[i],
                                      k$celle[i], if (k$celle[i] == 1) "a" else "e")))))
    )
  })

  # Import vero (dopo il controllo): scrive e mostra l'esito; `applicate` = correzioni accettate nella finestra.
  import_sostituzione_in_attesa <- reactiveVal(NULL)
  # Dove andrà il foglio (riscritto il 18/11/2026): lo dicono le sue segnature. Un fondo che nel database non c'è è un fondo NUOVO, e «Accoda»
  # senza un fondo a cui accodare lo creava di nascosto senza attivarlo; ora il foglio dice «nuovo» e si va dritti lì.
  destinazione <- reactive({
    ricarica(); d <- dal_foglio(); a <- lettura()
    n <- if (is.null(a) || inherits(a, "try-error")) 0L else nrow(a$dati)
    if (!nzchar(d$fondo)) return(list(stato = "ignota", archivio = "", fondo = "", id = NA_character_, n = n))
    id <- fondo_id_da(con, d$archivio, d$fondo)
    list(stato = if (is.na(id)) "nuovo" else "presente", archivio = d$archivio, fondo = d$fondo, id = id, n = n)
  })
  # «Sostituisci» vale solo per il fondo attivo: se il foglio è di un altro fondo (o di uno nuovo) tutte le righe verrebbero respinte.
  sostituzione_fuori_fondo <- function() {
    d <- destinazione()
    identical(input$modo_import, "sostituisci") && !identical(d$stato, "ignota") && !identical(d$id, fondo_attivo())
  }
  modo_effettivo <- function() {
    m <- input$modo_import %||% "accoda"
    if (identical(m, "accoda") && identical(destinazione()$stato, "nuovo")) "nuovo" else m
  }
  output$destinazione_import <- renderUI({
    d <- destinazione()
    if (d$n == 0L) return(NULL)
    if (identical(d$stato, "ignota")) {
      # Un foglio con più fondi: «Accoda» li crea tutti, e quelli che non ci sono vanno dichiarati prima, non scoperti in Home dopo.
      k <- tryCatch(archivi_nel_foglio(lettura()), error = function(e) NULL)
      if (!is.null(k) && nrow(k) > 1L) {
        nuovo <- vapply(seq_len(nrow(k)), function(i) is.na(fondo_id_da(con, k$archivio[i], k$fondo[i])), TRUE)
        voce <- function(i) sprintf("%s \u00b7 %s (%d righe)", k$fondo[i], k$archivio[i], k$righe[i])
        return(div(class = if (any(nuovo)) "alert alert-warning py-2" else "alert alert-info py-2",
                   strong(sprintf("Il foglio contiene %d fondi. ", nrow(k))),
                   if (any(nuovo)) tagList("Con «Accoda» l'import ", strong(sprintf("crea %d fondi nuovi", sum(nuovo))), ": ", paste(vapply(which(nuovo), voce, ""), collapse = "; "), ". ") ,
                   if (any(!nuovo)) tagList("Già presenti: ", paste(vapply(which(!nuovo), voce, ""), collapse = "; "), ". ")))
      }
      return(div(class = "alert alert-secondary py-2 small",
                 "Il foglio non dice un solo archivio e un solo fondo nelle segnature: scegli qui sotto che cosa fare e, per un fondo nuovo, scrivi archivio e fondo."))
    }
    nome <- paste(d$fondo, d$archivio, sep = " \u00b7 ")
    if (sostituzione_fuori_fondo())
      return(div(class = "alert alert-warning py-2", strong("Questo foglio non è del fondo attivo. "),
                 "È ", span(class = "no-trad", nome), if (identical(d$stato, "nuovo")) " (un fondo che non c'è ancora)" else " (un altro fondo)",
                 ": «Sostituisci» lo respingerebbe tutto. Scegli «", if (identical(d$stato, "nuovo")) "Importa nuovo fondo" else "Accoda",
                 "», oppure cambia il fondo attivo in alto."))
    if (identical(d$stato, "nuovo"))
      div(class = "alert alert-info py-2", strong("Fondo nuovo: "), span(class = "no-trad", nome), sprintf(" (%d righe).", d$n),
          " Nel database non c'\u00e8 ancora: l'import lo crea e poi lavori su quello.")
    else {
      n <- tryCatch(dbGetQuery(con, "SELECT COUNT(*) n FROM fascicoli WHERE fondo_id = ?", list(d$id))$n, error = function(e) NA)
      div(class = "alert alert-info py-2", strong("Fondo gi\u00e0 presente: "), span(class = "no-trad", nome), sprintf(" (%s schede nel database, %d righe nel foglio).", n, d$n),
          " Con \u00abAccoda\u00bb si aggiungono le schede nuove e quelle con la stessa segnatura restano com'erano.")
    }
  })
  # Quanto si perde con «Sostituisci», detto prima di sceglierlo: le schede del fondo attivo, col suo nome.
  output$perdita_sostituzione <- renderUI({
    ricarica(); f <- fondo_attivo()
    if (.vuoto(f)) return(NULL)
    n <- tryCatch(riepilogo_cancellazione(con, f)$fascicoli, error = function(e) NA_integer_)
    if (is.na(n)) return(NULL)
    div(class = "mt-1", div(span("Fondo attivo: "), span(class = "no-trad", nome_fondo_esteso(con, f))), strong(sprintf("%d schede da sostituire.", n)))
  })
  esegui_import_vero <- function(a, applicate = 0L, confermato = FALSE) {
    m <- modo_effettivo(); dest0 <- destinazione()
    # «Sostituisci questo fondo» cancella le schede del fondo attivo: conferma, poi copia di sicurezza.
    if (identical(m, "sostituisci") && .vuoto(fondo_attivo())) {
      showNotification("Scegli prima il fondo da sostituire nella barra in alto.", type = "warning"); return(invisible())
    }
    if (identical(m, "sostituisci") && sostituzione_fuori_fondo()) {
      showNotification("Questo foglio non è del fondo attivo: «Sostituisci» non può funzionare. Scegli «Importa nuovo fondo» o «Accoda», o cambia il fondo in alto.", type = "warning", duration = 15)
      return(invisible())
    }
    if (identical(m, "sostituisci") && !confermato) {
      orfane <- tryCatch(schede_senza_corrispondenza(con, a, fondo_attivo()), error = function(e) NULL)
      import_sostituzione_in_attesa(list(a = a, applicate = applicate, orfane = if (is.null(orfane)) 0L else nrow(orfane)))
      showModal(modalDialog(title = "Sostituire tutte le schede?",
        p(span("Le schede del fondo "), span(class = "no-trad", nome_fondo_esteso(con, fondo_attivo())),
          span(sprintf(" (%d) verranno cancellate e rimpiazzate con le righe del foglio che sono di quel fondo (ne ho lette %d). Gli altri fondi non si toccano. Se una riga viene respinta (segnatura non ammessa, riga di un altro fondo, rifiuto del database), la sostituzione è annullata e non si cancella niente. Le righe senza segnatura qui sotto restano fuori e non la annullano.",
                       riepilogo_cancellazione(con, fondo_attivo())$fascicoli, nrow(a$dati)))),
        {
          senza <- righe_senza_segnatura(a)
          if (length(senza)) p(class = "text-warning-emphasis",
            if (length(senza) == 1L) sprintf("1 riga senza segnatura (note in fondo al foglio?) non entra: %s.", senza + 1L)
            else sprintf("%d righe senza segnatura (note in fondo al foglio?) non entrano: %s.", length(senza), paste(utils::head(senza + 1L, 12), collapse = ", ")),
            if (length(senza) > 12L) " …")
        },
        p(class = "small", "Il foglio non porta tortura, sentenza, attribuzione e le spunte \u00abesaminata\u00bb: per le righe con la stessa segnatura di una scheda di prima si tengono, a meno che il foglio non dica altro. Le righe nuove li hanno vuoti."),
        if (!is.null(orfane) && nrow(orfane)) tagList(
          div(class = "alert alert-warning small py-2",
            strong(sprintf("%d schede di prima non hanno una riga con la stessa segnatura nel foglio", nrow(orfane))),
            span(": hanno dati che il foglio non porta e che con la sostituzione sparirebbero. Se hai corretto una segnatura nel foglio, la scheda nuova parte senza quei dati. Prime: "),
            span(class = "no-trad", paste(utils::head(sprintf("%s (%s)", orfane$segnatura, orfane$perde), 5), collapse = "; ")),
            if (nrow(orfane) > 5) span(sprintf(" \u2026 e altre %d.", nrow(orfane) - 5))),
          checkboxInput("sost_accetta_perdite", "Ho capito: quei dati si perdono (resta la copia di sicurezza)", FALSE)),
        p("Prima Trama salva una copia integrale del database nella cartella dei backup. Se qualcosa non va, i dati di prima restano."),
        footer = tagList(modalButton("Annulla"), actionButton("conferma_sostituzione", "S\u00ec, sostituisci il fondo", class = "btn-danger"))))
      return(invisible())
    }
    if (identical(m, "sostituisci")) {
      copia <- tryCatch(backup_prima_ripristino(con, cartella_backup(), "sostituzione"), error = function(e) NULL)
      if (is.null(copia)) { showNotification("Non riesco a fare la copia di sicurezza: non ho cambiato niente.", type = "error", duration = NULL); return(invisible()) }
    }
    if (identical(m, "nuovo")) {
      arc_n <- if (nzchar(input$import_archivio %||% "")) input$import_archivio else dest0$archivio
      fon_n <- if (nzchar(input$import_fondo %||% "")) input$import_fondo else dest0$fondo
      if (!nzchar(trimws(arc_n)) || !nzchar(trimws(fon_n))) {
        showNotification("Scrivi archivio e fondo del nuovo dataset: il foglio non li dice.", type = "warning", duration = 12); return(invisible())
      }
    }
    esito <- try(if (identical(m, "nuovo"))
      importa_nuovo_archivio(con, a, if (nzchar(input$import_archivio %||% "")) input$import_archivio else dal_foglio()$archivio, if (nzchar(input$import_fondo %||% "")) input$import_fondo else dal_foglio()$fondo, input$import_serie, fonte = input$import_fonte)
      else importa_arianna(con, a, modo = m, fondo = fondo_attivo(), senza_segnatura_ok = identical(m, "sostituisci") && confermato), silent = TRUE)
    if (inherits(esito, "try-error")) {
      registra("ERRORE", "import fallito", conditionMessage(attr(esito, "condition")))
      showNotification(div(strong("Import fallito"),
                           .a_righe(conditionMessage(attr(esito, "condition")))),
                       type = "error", duration = NULL)
      return()
    }

    registra("INFO", "import", sprintf("modo %s: %d letti, %d importati, %d respinti (di cui %d già presenti)", m,
                                       esito$letti, esito$importati, nrow(esito$respinti), conta_non_entrate(esito$respinti)$presenti))
    cz <- tryCatch(concordanza_righe(a), error = function(e) NULL)       # discordanze della riga: si mostrano, «Pulisci e scarica» le corregge
    if (!is.null(cz)) { esito$discordanze <- cz$registro; esito$da_decidere_nomi <- cz$da_decidere }
    esito$correzioni_applicate <- applicate
    # Le scelte sui luoghi ignoti (D9) si eseguono solo a import riuscito. «Aggiungi»: lista personale; «Elimina»: resta nell'esito.
    # Nel registro solo numero e righe, mai il testo delle schede.
    if (!isTRUE(esito$sostituzione_annullata)) {
      nuovi_luoghi <- attr(a, "luoghi_da_aggiungere")
      if (length(nuovi_luoghi)) {
        fatti <- aggiungi_luoghi_alla_lista(nuovi_luoghi)
        if (length(fatti)) showNotification({ elenco <- paste(utils::head(fatti, 5), collapse = ", "); if (length(fatti) == 1L) sprintf("1 luogo aggiunto alla lista dei luoghi: %s", elenco) else sprintf("%d luoghi aggiunti alla lista dei luoghi: %s", length(fatti), elenco) }, type = "message", duration = 12)
        if (length(fatti) < length(unique(nuovi_luoghi))) showNotification("Alcuni luoghi non sono entrati nella lista dei luoghi (già presenti o file occupato): restano nei dati.", type = "warning", duration = 12)
      }
      tolti <- attr(a, "luoghi_tolti"); rimasti <- attr(a, "luoghi_rimasti")
      if (!is.null(tolti) && nrow(tolti)) {
        esito$luoghi_tolti <- tolti
        registra("INFO", "luoghi_eliminati", sprintf("%d celle, righe %s", nrow(tolti), paste(utils::head(unique(tolti$Riga), 50), collapse = ",")))
        showNotification(if (nrow(tolti) == 1L) "Luoghi tolti da 1 cella (l'elenco è in «Errori e discordanze»)." else sprintf("Luoghi tolti da %d celle (l'elenco è in «Errori e discordanze»).", nrow(tolti)), type = "message", duration = 8)
      }
      om <- attr(a, "omonimi_scelti")
      if (!is.null(om) && nrow(om)) { esito$omonimi_scelti <- om; esito$da_decidere_nomi <- .senza_omonimi_scelti(esito$da_decidere_nomi, a); registra("INFO", "omonimi_scelti", sprintf("%d luoghi, righe %s", nrow(om), paste(utils::head(unique(om$Riga), 50), collapse = ","))) }
      if (!is.null(rimasti) && nrow(rimasti)) {
        esito$luoghi_rimasti <- rimasti
        showNotification(if (nrow(rimasti) == 1L) "1 cella con un luogo da eliminare non è stata toccata: la correzione del separatore non era applicata (vedi «Errori e discordanze»)."
                         else sprintf("%d celle con un luogo da eliminare non sono state toccate: la correzione del separatore non era applicata (vedi «Errori e discordanze»).", nrow(rimasti)),
                         type = "warning", duration = NULL)
      }
    }
    risultato_import(esito)
    ricarica(ricarica() + 1)
    # Il fondo dove sono entrate le schede diventa quello su cui si lavora (un fondo nuovo, o quello a cui si è accodato).
    dove <- if (identical(m, "nuovo")) esito$fondo else if (identical(m, "accoda") && identical(dest0$stato, "presente")) dest0$id else NULL
    if (!is.null(dove) && !isTRUE(esito$sostituzione_annullata)) attiva_fondo(dove)

    if (!isTRUE(esito$sostituzione_annullata)) {
      cosa <- paste0(riassunto_import(esito$letti, esito$importati, esito$respinti),
                     if (applicate > 0) sprintf(" \u00b7 %d correzioni applicate", applicate) else "")
      showModal(modalDialog(title = if (identical(m, "nuovo")) "Fondo nuovo importato" else "Import concluso", easyClose = TRUE,
        p(strong(cosa)),
        if (!is.null(dove)) p(if (identical(m, "nuovo")) "Il fondo \u00e8 stato creato e ora lavori su " else "Ora lavori su ",
                              strong(class = "no-trad", nome_fondo_esteso(con, dove)), "."),
        if (nrow(esito$respinti) > 0) p(class = "small text-muted", "Le righe non entrate, con il motivo, sono nella tabella dell'esito."),
        footer = tagList(modalButton("Resta qui"), actionButton("import_vai_home", "Apri in Home", class = "btn-primary"))))
    }
    if (!is.null(esito$date_non_inserite) && nrow(esito$date_non_inserite))
      showNotification(div(strong(sprintf("%d date non inserite", nrow(esito$date_non_inserite))), br(),
                           sprintf("Una data non valida non diventa un anno: il testo resta com'era nel foglio (per esempio «%s», riga %d), da correggere.",
                                   esito$date_non_inserite$valore[1], esito$date_non_inserite$riga[1])),
                       type = "warning", duration = NULL)
    if (isTRUE(esito$sostituzione_annullata))
      showNotification(div(strong("Sostituzione non eseguita"), br(), "Il foglio contiene righe respinte: sostituzione annullata, nessuna riga importata. Le schede di prima sono intatte. Correggi le righe segnalate e riprova."),
                       type = "warning", duration = NULL)
  }

  # «Pulisci e scarica»: il foglio esce nel modello, il database non si tocca; `revisionato` = correzioni già scelte nella finestra.
  esegui_pulizia <- function(a, revisionato = FALSE) {
      p <- try(pulisci_foglio(a, da = con, revisionato = revisionato, nome = input$file_import$name,
                              archivio = input$pul_archivio, fondo = input$pul_fondo, serie = input$pul_serie), silent = TRUE)
      if (inherits(p, "try-error")) {
        risultato_pulito(NULL)
        showNotification(div(strong("Non riesco a pulire il foglio"), .a_righe(conditionMessage(attr(p, "condition")))),
                         type = "error", duration = NULL)
      } else {
        # Una copia resta anche in ~/Documents/Trama/puliti, se il browser non fa partire lo scarico.
        p$salvato <- tryCatch({
          d <- file.path(cartella_dati(), "puliti"); dir.create(d, showWarnings = FALSE, recursive = TRUE)
          dest <- file.path(d, p$nome); if (file.copy(p$percorso, dest, overwrite = TRUE)) dest else NULL
        }, error = function(e) NULL)
        risultato_import(NULL); risultato_pulito(p)
        showNotification(esito_ok(sprintf("%d lette \u00b7 %d pulite \u00b7 %d respinte \u00b7 niente scritto nel database",
                                          p$letti, p$importati, nrow(p$respinti))), type = "message", duration = 10)
      }
  }

  # Parte da «Importa» o da «Scarica pulito, senza importare»: la finestra si apre sempre (19/11/2026: il controllo automatico alla scelta del foglio non c'è più).
  controlla_import <- function() {
    if (!.e_pulisci() && sostituzione_fuori_fondo()) {
      showNotification("Questo foglio non è del fondo attivo: «Sostituisci» non può funzionare. Scegli «Importa nuovo fondo» o «Accoda», o cambia il fondo in alto.", type = "warning", duration = 15)
      return()
    }

    a <- lettura()
    if (is.null(a) || inherits(a, "try-error")) {
      showNotification("Nessun foglio valido caricato.", type = "warning")
      return()
    }

    # Controllare prima di importare: correzioni cella per cella, si scrive dopo la conferma.
    # Se il controllo fallisce non si importa in silenzio: si dice e si lascia scegliere.
    errore_controllo <- NULL; pulisci <- .e_pulisci()
    pr <- tryCatch({
      if (isTRUE(getOption("trama.simula_guasto_controllo"))) stop("guasto simulato")   # solo per verifiche/interfaccia.R
      proposte_import(a, con, switch(modo_effettivo(), nuovo = NULL, accoda = if (identical(destinazione()$stato, "presente")) destinazione()$id else fondo_attivo(), fondo_attivo()), luoghi_ignoti = !pulisci)
    }, error = function(e) { errore_controllo <<- conditionMessage(e); NULL })
    if (!is.null(errore_controllo)) {
      registra("ERRORE", "controllo_import_fallito", errore_controllo)
      revisione_import(list(a = a, pulisci = pulisci, pr = list(celle = data.frame(id = integer(0)), decisioni = list(), totale = 0L)))
      showModal(modalDialog(
        title = "Il controllo non è riuscito",
        p("Trama non è riuscita a controllare il foglio prima di importarlo, quindi non ha scritto niente."),
        p(tags$code(errore_controllo)),
        p("Puoi annullare e riprovare, oppure importare il foglio com’è, senza controlli."),
        footer = tagList(actionButton("rev_annulla", "Annulla"),
                         actionButton("rev_come_sta", "Importa com’è, senza controllo", class = "btn-primary")),
        easyClose = FALSE))
      return()
    }
    if (!is.null(pr) && pr$totale > 0) {
      revisione_import(list(a = a, pr = pr, pulisci = pulisci))
      rev_selezione(celle_spuntate(pr$celle))          # all'apertura le consigliate sono spuntate
      showModal(modale_revisione(pr, pulisci = pulisci, a = a, modo_testo = if (!pulisci) paste0(.ETICHETTA_MODO[[modo_effettivo()]], {
        # «Accoda» con più fondi nel foglio li crea: lo si dice anche qui, dove si può ancora annullare.
        k <- if (identical(modo_effettivo(), "accoda") && identical(destinazione()$stato, "ignota")) tryCatch(archivi_nel_foglio(lettura()), error = function(e) NULL)
        n_nuovi <- if (!is.null(k) && nrow(k) > 1L) sum(vapply(seq_len(nrow(k)), function(i) is.na(fondo_id_da(con, k$archivio[i], k$fondo[i])), TRUE)) else 0L
        if (n_nuovi > 0L) sprintf(" (crea %d fondi nuovi: Annulla per fermarti)", n_nuovi) else "" })))
      return()
    }
    if (pulisci) esegui_pulizia(a) else esegui_import_vero(a)
  }
  observeEvent(input$esegui_import, { forza_pulisci(FALSE); controlla_import() })
  observeEvent(input$avvia_pulito, { forza_pulisci(TRUE); controlla_import() })
  observeEvent(input$modo_import, forza_pulisci(FALSE), ignoreInit = TRUE)
  output$modo_pulisci <- reactive(.e_pulisci()); outputOptions(output, "modo_pulisci", suspendWhenHidden = FALSE)
  # Alla scelta del foglio non si apre niente: si legge, si dice dove andrà (destinazione_import) e si sceglie la modalit\u00e0 giusta;
  # il controllo parte con «Importa», una volta sola (18/11/2026: la finestra automatica confermava un «Accoda» che l'utente non aveva scelto).
  observeEvent(input$file_import, {
    forza_pulisci(FALSE)      # un altro foglio: si riparte da «Importa»
    d <- destinazione()
    # Si tocca la modalità solo quando il foglio lo chiede: un fondo nuovo passa a «Importa nuovo fondo», uno già presente toglie
    # un «nuovo» rimasto da prima. Una modalità scelta a mano (Accoda, Sostituisci, Pulisci) non si sovrascrive.
    m <- input$modo_import %||% "accoda"
    if (identical(d$stato, "nuovo") && identical(m, "accoda")) updateRadioButtons(session, "modo_import", selected = "nuovo")
    else if (identical(d$stato, "presente") && identical(m, "nuovo")) updateRadioButtons(session, "modo_import", selected = "accoda")
  }, ignoreInit = TRUE)
  observeEvent(input$import_vai_home, { removeModal(); nav_select("nav", "Home") })

  .ETICHETTA_MODO <- list(accoda = "Accoda ai fascicoli esistenti", nuovo = "Importa nuovo fondo", sostituisci = "Sostituisci questo fondo", pulisci = "Pulisci e scarica")
  revisione_import <- reactiveVal(NULL)

  rev_selezione <- reactiveVal(integer(0))     # id delle correzioni spuntate
  # ── Gli errori già nel database, in Fascicoli (R/anomalie_server.R) ──
  anomalie <- anomalie_server(input, output, session, con, ricarica, fondo_attivo, solo_errori, revisione_import, rev_selezione)
  anomalie_stato <- anomalie$anomalie_stato; controlla_anomalie <- anomalie$controlla_anomalie
  # La finestra di revisione (R/revisione_server.R) e quella degli errori (R/errori_import_server.R).
  revisione <- revisione_server(input, output, session, con, ricarica, revisione_import, rev_selezione, forza_pulisci,
                                import_sostituzione_in_attesa, esegui_import_vero, esegui_pulizia, controlla_anomalie)
  errori <- errori_import_server(input, output, session, risultato_import, risultato_pulito, .e_pulisci)
  elenco_errori <- errori$elenco_errori

  output$esito_pulisci <- renderUI({
    p <- risultato_pulito()
    if (is.null(p) || !.e_pulisci()) return(NULL)
    tagList(
      div(class = paste("alert py-2 mt-3", if (nrow(p$respinti) == 0) "alert-success" else "alert-warning"),
          sprintf("%d lette \u00b7 %d pulite \u00b7 %d respinte", p$letti, p$importati, nrow(p$respinti))),
      p(class = "small text-muted",
        span("Il foglio esce nel modello a 28 colonne, con le correzioni dell'anteprima e i refusi corretti. Nel database non è stato scritto niente."), " ",
        span("Tre fogli in più dicono che cosa è stato riscritto («Correzioni»), quali valori restano da decidere («Da decidere») e quali righe non sono entrate («Respinte»).")),
      if (!is.null(p$da_decidere) && nrow(p$da_decidere) > 0)
        div(class = "alert alert-warning py-2",
            span("Valori che non assomigliano a niente nel vocabolario sono rimasti come sono, nel foglio «Da decidere»: "), strong(nrow(p$da_decidere))),
      if (!is.null(p$salvato))
        p(class = "small text-muted mb-2", span("Una copia del file è anche in questa cartella, se lo scarico non parte: "), span(class = "no-trad", dirname(p$salvato))),
      downloadButton("scarica_pulito", if (length(p$fondi) > 1) "Scarica i fogli puliti (.zip)" else "Scarica il foglio pulito (.xlsx)",
                     class = "btn-primary"),
      if (elenco_errori_import(pulito = p)$totale > 0)
        actionButton("apri_errori_import", tagList(icon("list-check"), "Errori e discordanze\u2026"), class = "btn-outline-primary ms-2"),
      if (nrow(p$respinti) > 0) div(class = "mt-3", DTOutput("respinti_pulisci")))
  })
  output$scarica_pulito <- downloadHandler(
    filename = function() risultato_pulito()$nome,
    content  = function(file) file.copy(risultato_pulito()$percorso, file, overwrite = TRUE))
  output$respinti_pulisci <- renderDT({
    p <- risultato_pulito(); req(p, nrow(p$respinti) > 0)
    datatable(p$respinti, rownames = FALSE, selection = "none", colnames = c("Riga", "Segnatura", "Motivo"),
              options = list(dom = "tp", pageLength = 5))
  })
  observeEvent(input$file_import, { risultato_pulito(NULL); forza_pulisci(FALSE) })   # un altro foglio: l'esito di prima non vale più
  # update_task_button() non cambia l'etichetta: le due scritte (pronto, occupato) partono con un messaggio proprio.
  observeEvent(input$modo_import, session$sendCustomMessage("etichetta_task", list(id = "esegui_import",
    pronto   = switch(input$modo_import %||% "", sostituisci = "Sostituisci questo fondo\u2026", "Importa"),
    occupato = switch(input$modo_import %||% "", sostituisci = "Sostituisco\u2026", "Importo\u2026"))), ignoreInit = FALSE)

  output$esito_import <- renderUI({
    e <- risultato_import()
    if (is.null(e)) return(NULL)
    div(class = paste("alert py-2 mt-3",
                      if (nrow(e$respinti) == 0) "alert-success" else "alert-warning"),
        paste0(riassunto_import(e$letti, e$importati, e$respinti),
               if (isTRUE(e$correzioni_applicate > 0)) sprintf(" \u00b7 %d correzioni applicate prima di importare", e$correzioni_applicate) else ""),
        if (conta_non_entrate(e$respinti)$presenti > 0)
          p(class = "small mb-0 mt-1", "Accoda conserva i fascicoli già presenti (stessa segnatura): il motivo di ogni riga è nella tabella qui sotto."),
        if (elenco_errori_import(esito = e)$totale > 0)
          div(class = "mt-2", actionButton("apri_errori_import", tagList(icon("list-check"), "Errori e discordanze\u2026"), class = "btn-sm btn-outline-primary")))
  })

  output$respinti <- renderDT({
    e <- risultato_import()
    req(e, nrow(e$respinti) > 0)
    datatable(e$respinti, rownames = FALSE, selection = "none",
              colnames = c("Riga", "Segnatura", "Motivo"),
              options = list(dom = "tp", pageLength = 5))
  })

  # Un solo modello di uscita: le 28 colonne di dati/modello_export.xlsx, in xlsx e CSV.
  # Un fileInput non porta il file al browser: la presenza del file la dice il server.
  # Archivio e fondo del foglio, se univoci e presenti in tutte le righe: allora non si chiedono.
  dal_foglio <- reactive({
    a <- lettura(); if (is.null(a) || inherits(a, "try-error")) return(list(archivio = "", fondo = ""))
    k <- try(archivi_nel_foglio(a), silent = TRUE)
    if (inherits(k, "try-error") || nrow(k) != 1L || !nzchar(k$archivio) || !nzchar(k$fondo)) return(list(archivio = "", fondo = ""))
    list(archivio = k$archivio, fondo = k$fondo)
  })
  output$archivio_fondo_noti <- reactive(nzchar(dal_foglio()$fondo)); outputOptions(output, "archivio_fondo_noti", suspendWhenHidden = FALSE)
  output$archivio_fondo_liberi <- reactive({ d <- dal_foglio(); ricarica(); nzchar(d$fondo) && is.na(fondo_id_da(con, d$archivio, d$fondo)) })
  outputOptions(output, "archivio_fondo_liberi", suspendWhenHidden = FALSE)
  output$archivio_fondo_dal_foglio <- renderUI(p(class = "small text-muted", "Archivio e fondo dal foglio: ", strong(paste(dal_foglio()$archivio, dal_foglio()$fondo, sep = " \u00b7 ")), "."))
  output$file_import_presente <- reactive(!is.null(input$file_import))
  outputOptions(output, "file_import_presente", suspendWhenHidden = FALSE)
  list(anomalie_stato = anomalie_stato, .a_righe = .a_righe, lettura = lettura, revisione_import = revisione_import,
       rev_selezione = rev_selezione, elenco_errori = elenco_errori,
       risultato_import = risultato_import, risultato_pulito = risultato_pulito)
}
