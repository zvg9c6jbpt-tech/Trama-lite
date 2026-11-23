# ─────────────────────────────────────────────────────────────
# R/importexport_ui.R: Import / Export: importazione guidata, esportazione, backup e ripristino
# ─────────────────────────────────────────────────────────────

importexport_ui <- function(PERCORSO_DB) nav_panel(
    title = tagList(icon("right-left"), "Import / Export"),
    value = "Import / Export",
    titolo_pagina("Import / Export"),
    uiOutput("copia_export"),
    # Carte alte quanto il contenuto, perché i pulsanti non restino sotto il bordo.
    # Importa ed Esporta guidati: scegli il foglio, controlla, importa; esporta l'Excel normalizzato.
    layout_column_wrap(
      width = 1/2, heights_equal = "row", class = "griglia-pagina griglia-import",

      card(
        card_header("Importa nell'app"),
        card_body(fillable = FALSE,
          passo_guidato(1, "Scegli il foglio",
            # Anche CSV (separatore ";", UTF-8): lo stesso che esce da "Scarica CSV".
            div(class = "zona-trascina",
                div(class = "zona-testo", icon("file-arrow-up"), " Trascina qui il foglio, o clicca per sceglierlo"),
                fileInput("file_import", NULL, accept = c(".xlsx", ".xls", ".csv"),
                          buttonLabel = "Scegli\u2026", placeholder = "nessun file"))),
          conditionalPanel("!output.file_import_presente",
            passo_guidato(2, "Controlla che cosa cambia", attesa = TRUE),
            passo_guidato(3, "Importa", attesa = TRUE)),
          conditionalPanel("output.file_import_presente",
            passo_guidato(2, "Controlla che cosa cambia",
              uiOutput("anteprima_import"),
              uiOutput("destinazione_import"),
              # Prima le modalità che scrivono, poi «Sostituisci questo fondo» (distruttivo), a parte lo strumento che non scrive.
              radioButtons("modo_import", "Modalit\u00e0",
                           choiceNames = list("Accoda ai fascicoli esistenti", "Importa nuovo fondo",
                                              span(class = "modo-pericolo", icon("triangle-exclamation"), "Sostituisci questo fondo")),
                           choiceValues = c("accoda", "nuovo", "sostituisci"), selected = "accoda"),
              conditionalPanel("input.modo_import === 'sostituisci'",
                div(class = "alert alert-danger small py-2", role = "alert",
                    strong("Cancella le schede del fondo scelto in alto"), " e le rimpiazza con quelle del foglio, che devono essere di quel fondo. Gli altri fondi non si toccano. Prima fa una copia di sicurezza e chiede conferma."),
                    uiOutput("perdita_sostituzione")),
              conditionalPanel("input.modo_import === 'nuovo'",
                p("Crea un dataset separato, selezionabile dal menu dei fondi. Le schede esistenti restano invariate."),
                # Archivio e fondo si chiedono solo se il foglio non li dice nelle segnature, o se quel fondo esiste già.
                conditionalPanel("!output.archivio_fondo_liberi",
                  senza_suggerimenti(textInput("import_archivio", "Archivio", placeholder = "es. Archivio di Stato di Modena")),
                  senza_suggerimenti(textInput("import_fondo", "Fondo / dataset", placeholder = "Come lo chiamerai nel menu dei fondi"))),
                conditionalPanel("output.archivio_fondo_liberi", uiOutput("archivio_fondo_dal_foglio")),
                senza_suggerimenti(textInput("import_serie", "Serie (facoltativa)", placeholder = "Vuoto: conserva le serie del foglio")),
                senza_suggerimenti(textInput("import_fonte", "Da dove vengono i dati (facoltativo)", placeholder = "una riga, es. Inventario dell'archivio, trascritto dal gruppo di lavoro")),
                p(class = "small text-muted", "Archivio e fondo indicati qui si applicano a tutte le righe. Le denominazioni originali restano nei campi aggiuntivi. In caso di errore l'intera nuova importazione viene annullata.")),
              conditionalPanel("output.modo_pulisci",
                # Solo se nelle segnature manca l'archivio o il fondo; altrimenti si leggono dal foglio.
                conditionalPanel("!output.archivio_fondo_noti",
                  p(class = "small text-muted", "Nella segnatura manca l'archivio o il fondo (un nome solo prima di «b.»): scrivili qui, valgono per tutte le righe. Senza, quelle righe vengono respinte."),
                  senza_suggerimenti(textInput("pul_archivio", "Archivio", placeholder = "es. Archivio di Stato di Modena")),
                  senza_suggerimenti(textInput("pul_fondo", "Fondo", placeholder = "es. Inquisizione")),
                  senza_suggerimenti(textInput("pul_serie", "Serie (facoltativa)"))))),
            passo_guidato(3, "Importa",
              # Pulsante a stato (bslib): mentre il server lavora è spento e dice «Importo…».
              div(class = "d-flex flex-wrap gap-2 align-items-center",
                input_task_button("esegui_import", "Importa", label_busy = "Importo\u2026"),
                # «Pulisci e scarica» è un pulsante, anche dentro la finestra di controllo.
                actionButton("avvia_pulito", tagList(icon("file-arrow-down"), "Scarica pulito, senza importare"), class = "btn-outline-secondary")),
              uiOutput("esito_import"),
              uiOutput("esito_pulisci"),
              DTOutput("respinti"))))
      ),

      card(
        card_header("Esporta"),
        card_body(fillable = FALSE,
          passo_guidato(1, "Il fondo",
            uiOutput("info_export"),
            uiOutput("pronto_export")),
          passo_guidato(2, "Scarica l'Excel normalizzato",
            div(class = "d-flex flex-wrap gap-2 azioni-scarico",
                downloadButton("scarica_xlsx", "Foglio .xlsx", class = "btn-primary"),
                # Stesse colonne dell'xlsx, separatore ";", UTF-8.
                downloadButton("scarica_csv", "Foglio .csv", class = "btn-outline-secondary")),
            p(class = "small text-muted mt-2 mb-0", "Una copia del file resta anche nella cartella dei dati, in «export». Il nome di chi ha schedato e il progetto, che entrano nelle proprietà del foglio, si scrivono sotto, in «Backup e ripristino» (Attribuzione).")))
      )
    ),

    # Il resto (formati di uscita, copie, ripristino) sta in una sezione chiusa; gli elementi restano nella pagina.
    tags$details(class = "altro-export",
      tags$summary("Backup e ripristino: JSON e copie del database"),
    layout_column_wrap(
      width = 1/2, heights_equal = "row", class = "griglia-pagina",

      card(
        card_header("Attribuzione e schede in JSON"),
        card_body(fillable = FALSE,
          # Chi ha schedato e per quale progetto: entra nei fogli Excel, nel backup JSON e nel LEGGIMI.
          layout_columns(col_widths = c(6, 6),
            textInput("attribuzione_schedatore", "Schedatura di", placeholder = "nome e cognome di chi ha schedato"),
            textInput("attribuzione_progetto", "Progetto", placeholder = "il progetto di cui i dati fanno parte")),
          uiOutput("attribuzione_riga"),
          # Da dove vengono i dati del fondo scelto: facoltativo, entra nel foglio Excel e nel backup JSON.
          textInput("fonte_fondo", "Da dove vengono i dati del fondo scelto in alto", placeholder = "una riga, es. Inventario dell'archivio, trascritto dal gruppo di lavoro", width = "100%"),
          tags$details(class = "spiega mt-1 mb-2", tags$summary("Dove compare questa attribuzione"),
            p(class = "text-muted small mt-1 mb-0", "Entra nelle proprietà dei fogli Excel (autore e oggetto: nessuna cella cambia), nel backup JSON e nel LEGGIMI della copia completa.")),
          p(class = "text-muted small mt-2",
            span("Licenza dei dati: "), tags$a(href = LICENZA_DATI$uri, target = "_blank", rel = "noopener", class = "no-trad", LICENZA_DATI$nome), span(".")),
          div(class = "d-flex flex-wrap gap-2 azioni-scarico",
              downloadButton("scarica_json_schede", "Schede .json", class = "btn-outline-secondary")))
      ),

      card(
        card_header("Backup JSON"),
        card_body(fillable = FALSE,
          # Che cosa porta ogni formato, una frase per elemento.
          p(class = "text-muted small",
            span(tags$b("Excel e CSV:"), " "), span("scambio delle schede nel modello a 28 colonne, un fondo per volta; alcuni segni di lavoro di Trama non ci sono."), " ",
            span(tags$b("JSON:"), " "), span("i dati strutturati di Trama Lite; non porta la lista dei luoghi personale, la cronologia delle modifiche né le impostazioni."), " ",
            span(tags$b("Copia completa:"), " "), span("tutto l'ambiente di lavoro, per riprendere su un altro computer o dopo un guasto.")),
          p(class = "text-muted small",
            "Fascicoli, persone, collegamenti, piani, ricerche e ordine di consultazione, con campi extra e date di modifica."),
          p(class = "text-muted small",
            "Oltre a questo l'app copia il database nella cartella ", tags$code(cartella_backup()),
            ": una copia a ogni avvio (le ultime 14 giornaliere) e, mentre lavori, una ogni mezz'ora se qualcosa è cambiato (le ultime 20). ",
            uiOutput("ultimo_backup", inline = TRUE)),
          # Dove sono i dati: il pulsante apre la cartella nel Finder / Esplora file.
          p(class = "text-muted small",
            span("I tuoi dati stanno in: "), tags$code(class = "no-trad", dirname(PERCORSO_DB)), " ",
            actionButton("apri_cartella_dati", tagList(icon("folder-open"), "Apri la cartella"), class = "btn-pillola btn-sm")),
          p(class = "text-muted small",
            "La copia completa è un solo file .zip con il database, il backup JSON e la lista dei luoghi personale, che il JSON non porta. ",
            "Serve per ricominciare su un altro computer o dopo un guasto."),
          div(class = "d-flex gap-2 flex-wrap",
              downloadButton("scarica_backup", "Scarica backup (.json)", class = "btn-primary"),
              downloadButton("scarica_backup_completo", "Scarica copia completa (.zip)", class = "btn-outline-secondary")))
      ),

      # Copia del database (tabelle interne, per altri programmi): tutti i fondi o solo quello attivo.
      card(
        card_header("Database (SQL, SQLite)"),
        card_body(fillable = FALSE,
          p(class = "text-muted small",
            # Ogni frase nel suo elemento: il traduttore confronta nodi di testo interi.
            span("Il database com'è, per altri programmi (DB Browser for SQLite, DBeaver, R, Python): struttura e dati in SQL (dialetto SQLite) o il file SQLite."), " ",
            span("Non è il foglio del modello a 28 colonne: sono le tabelle interne di Trama (schede, persone, luoghi, reati…).")),
          checkboxInput("db_solo_fondo", "Solo il fondo attivo", FALSE),
          div(class = "d-flex gap-2 flex-wrap",
              downloadButton("scarica_db_sql", "Scarica database (.sql)", class = "btn-outline-secondary"),
              downloadButton("scarica_db_sqlite", "Scarica database (.sqlite)", class = "btn-outline-secondary")))
      ),

      card(
        card_header("Ripristino da JSON"),
        card_body(fillable = FALSE,
          fileInput("file_ripristino", NULL, accept = ".json",
                    buttonLabel = "Scegli…", placeholder = "nessun file"),
          radioButtons("modo_ripristino", "Modalità",
                       c("Accoda ai fascicoli esistenti" = "accoda",
                         "Sostituisci tutto"             = "sostituisci"),
                       selected = "accoda"),
          actionButton("esegui_ripristino", "Ripristina", class = "btn-primary"),
          uiOutput("esito_ripristino"))
      ),

      # Ripristino della copia completa: si sceglie lo zip, si vede il contenuto, si conferma.
      card(
        card_header("Ripristino da copia completa (.zip)"),
        card_body(fillable = FALSE,
          p(class = "text-muted small",
            span("Sostituisce il database e la lista dei luoghi personale con quelli della copia. "),
            span("Prima salva i file che ci sono nella cartella dei backup; alla fine Trama si chiude e va riaperta.")),
          fileInput("file_completo", NULL, accept = ".zip",
                    buttonLabel = "Scegli…", placeholder = "nessun file"),
          uiOutput("anteprima_completo"),
          actionButton("esegui_completo", "Ripristina la copia completa…", class = "btn-outline-danger"))
      )
    ))
  )
