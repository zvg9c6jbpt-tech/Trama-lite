# ─────────────────────────────────────────────────────────────
# R/sincronizzazioni_server.R: i campi della Scheda che si aggiornano mentre scrivi
# Ciò che salva_fascicolo scrive, reso visibile prima del Salva: coppie persona/reato, imputati fra le persone citate, omonimi dei luoghi,
# regesto, connettore e titolo composti. Scatta solo per cambi dell'utente (`invariati`: il valore è quello caricato).
# Restituisce gli aiuti che servono fuori: scheda_dal_modulo, .indicizzati, imputati_prima, persone_prima.
# ─────────────────────────────────────────────────────────────

sincronizzazioni_server <- function(input, output, session, caricati, invariati) {
  # Le griglie delle coppie seguono imputati, persone, capi e luoghi; le spunte le conserva la griglia (R/modulo.R).
  observeEvent(list(input$imputati, input$imputati_esaminati, input$persona, input$reato, input$luogo), {
    v <- list(imputati = input$imputati, imputati_esaminati = input$imputati_esaminati, persona = input$persona, reato = input$reato,
              luogo = input$luogo)
    aggiorna_coppie(session, "persona_reati", v)
    aggiorna_coppie(session, "reato_luoghi",  v)
  }, ignoreInit = TRUE)

  # L'imputato entra fra le persone citate (in testa) e non ne esce quando smette di esserlo;
  # togliere una persona è la × sul suo nome, che la toglie anche dagli imputati (JS in R/modulo.R).
  .indicizzati <- function(x) {
    x <- as.character(unlist(x)); x <- x[!is.na(x) & nzchar(trimws(x))]
    unique(vapply(x, indicizza_nome, character(1), USE.NAMES = FALSE))
  }
  imputati_prima <- reactiveVal(character(0))

  observeEvent(input$imputati, {
    ind   <- .indicizzati(input$imputati)
    prima <- imputati_prima()
    imputati_prima(ind)
    if (setequal(ind, prima)) return()
    persone <- as.character(unlist(input$persona))
    mancanti <- setdiff(ind, persone)
    if (!length(mancanti)) return()
    sel <- unique(c(mancanti, persone))
    sel <- sel[!is.na(sel) & nzchar(sel)]
    .manda_voci(session, "persona", sel)
    updateSelectizeInput(session, "persona", selected = sel)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)   # togliere l'ultimo imputato dà NULL: deve contare

  # Una persona aggiunta non nasce imputata. Qui resta solo la normalizzazione del nome.
  persone_prima <- reactiveVal(character(0))
  observeEvent(input$persona, {
    ora   <- as.character(unlist(input$persona)); ora <- ora[!is.na(ora) & nzchar(ora)]
    nuove <- setdiff(ora, persone_prima())
    # Un nome nuovo prende maiuscole e ordine «Cognome, Nome» (normalizza_persona); mai al caricamento.
    if (length(nuove) && !invariati("persona")) {
      giuste <- normalizza_persona(nuove)
      if (!identical(giuste, nuove)) {
        sel <- ora; sel[match(nuove, sel)] <- giuste; sel <- unique(sel)
        .manda_voci(session, "persona", sel)
        updateSelectizeInput(session, "persona", selected = sel)
        return()
      }
    }
    persone_prima(ora)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Campi da cui si compongono titolo e regesto, nella forma di leggi_fascicolo(): stesse funzioni del Salva (R/compose.R).
  scheda_dal_modulo <- function() {
    imp <- as.character(unlist(input$imputati)); imp <- imp[!is.na(imp) & nzchar(trimws(imp))]
    list(tipologia_documentaria = input$tipologia_documentaria, reati = input$reato,
         imputati = data.frame(esteso = imp, indicizzato = vapply(imp, indicizza_nome, "", USE.NAMES = FALSE),
                               stringsAsFactors = FALSE),
         imputati_esaminati = as.integer(isTRUE(input$imputati_esaminati)),
         reato_ignoto = as.integer(isTRUE(input$reato_ignoto)), luogo_ignoto = as.integer(isTRUE(input$luogo_ignoto)),
         non_procedimento = as.integer(isTRUE(input$non_procedimento)),
         persone = input$persona, tipo_titolo = input$tipo_titolo, connettore = input$connettore,
         identita_imputato_dubbia = as.integer(isTRUE(input$identita_imputato_dubbia)),
         regesto = input$regesto, luoghi = as.character(unlist(input$luogo)),
         tortura = input$tortura, sentenza = input$sentenza,
         # Testo e flag attuali: servono a non cancellare un titolo già scritto quando la regola non ne compone uno.
         titolo = input$titolo, titolo_manuale = as.integer(isTRUE(input$titolo_manuale)),
         regesto_manuale = as.integer(isTRUE(input$regesto_manuale)))
  }

  # Il regesto si ricompone dai campi finché non lo dichiari scritto a mano (narrativo, o strutturato se già così).
  observeEvent(list(input$tipologia_documentaria, input$reato, input$imputati, input$persona, input$imputati_esaminati,
                    input$luogo, input$tortura, input$sentenza, input$regesto_manuale,
                    input$reato_ignoto, input$luogo_ignoto, input$non_procedimento), {
    # Come il titolo: un regesto vuoto si compone anche se segnato a mano.
    vuoto <- .vuoto(input$regesto)
    if (isTRUE(input$regesto_manuale) && !vuoto) return()
    if (invariati(c("tipologia_documentaria", "reato", "imputati", "persona", "imputati_esaminati", "luogo",
                    "tortura", "sentenza", "regesto_manuale", "reato_ignoto", "luogo_ignoto", "non_procedimento"))) return()
    testo <- componi_regesto_scheda(scheda_dal_modulo())
    if (isTRUE(input$regesto_manuale)) updateCheckboxInput(session, "regesto_manuale", value = FALSE)
    updateTextAreaInput(session, "regesto",
                        value = if (is.na(testo)) "" else testo)
  }, ignoreInit = TRUE)

  # Il connettore proposto segue il tipo scelto; a mano resta finché non si cambia tipo.
  observeEvent(input$tipo_titolo, {
    # Guardia: aprire una scheda non deve cambiare il connettore.
    # Tornando al tipo caricato torna anche il connettore caricato (mai il predefinito).
    if (invariati("tipo_titolo")) {
      if (!invariati("connettore")) {
        cc <- caricati()[["connettore"]]
        updateSelectizeInput(session, "connettore", selected = if (.vuoto(cc)) "" else cc)
      }
      return()
    }
    predefinito <- TIPI_TITOLO[[input$tipo_titolo]]
    if (!is.null(predefinito))
      updateSelectizeInput(session, "connettore", selected = predefinito)
  }, ignoreInit = TRUE)

  # Il titolo si ricompone da tipologia, connettore e imputati finché non lo dichiari scritto a mano.
  observeEvent(list(input$tipologia_documentaria, input$tipo_titolo, input$connettore, input$imputati,
                    input$persona, input$imputati_esaminati, input$identita_imputato_dubbia, input$titolo_manuale), {
    # Un titolo vuoto non è «scritto a mano»: si compone anche con la spunta accesa, che si spegne.
    vuoto <- .vuoto(input$titolo)
    if (isTRUE(input$titolo_manuale) && !vuoto) return()
    if (invariati(c("tipologia_documentaria", "tipo_titolo", "connettore", "imputati", "persona", "imputati_esaminati",
                    "identita_imputato_dubbia", "titolo_manuale"))) return()
    testo <- componi_titolo_scheda(scheda_dal_modulo())
    if (is.na(testo) && vuoto) return()
    if (isTRUE(input$titolo_manuale)) updateCheckboxInput(session, "titolo_manuale", value = FALSE)
    updateTextInput(session, "titolo", value = if (is.na(testo)) "" else testo)
  }, ignoreInit = TRUE)
  list(scheda_dal_modulo = scheda_dal_modulo, .indicizzati = .indicizzati, imputati_prima = imputati_prima, persone_prima = persone_prima)
}
