# ── I metadati colonna per colonna ────────────────────────
# Due modi di lavorare sulle stesse 28 colonne: compilare (maschera ed export; qui «Pronto per l'export», `riempimento_colonne`)
# e normalizzare (si importa un foglio d'altri: `valori_colonna`, `vocabolario_fondo` per le decisioni che si ricordano, `sostituisci_valore` per la correzione in blocco).

# Le colonne standard sono quelle di Napoli, ciò che il foglio ASDNa compila davvero; le aggiuntive (dimensioni, numerazione, consistenza, soggetti citati,
# eventi, Manifest IIIF, immagini, estremo recente, altra segnatura, note) possono restare vuote senza che il fondo smetta di essere pronto.
# Alcune colonne standard restano vuote per REGOLA: senza cognome non è una persona citata, quindi «Atti circa Carlo» non ha persone.
# Le schede senza persone non tolgono il «pronto»; se il titolo nomina qualcuno con cognome lo dice già il controllo di Qualità.
COLONNE_VUOTE_PER_REGOLA <- "Persone citate o indicizzate"

COLONNE_STANDARD <- c(
  "Segnatura", "Titolo", "Estremo remoto", "definizione.tipologia", "Supporto (materiale)",
  "Tipologia documentaria", "Regesto o descrizione sintetica", "Persone citate o indicizzate",
  "Luogo di conservazione", "Luogo di produzione (data topica)", "Luoghi citati o indicizzati",
  "Soggetto produttore", "Soggettario", "Lingua")

# ── pronto per l'export ───────────────────────────────────────────

# Per ogni colonna del modello, in quante schede del fondo ha un valore nel foglio che uscirebbe (è `tabella_arianna`). `ids` segue le righe.
riempimento_colonne <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  t <- tabella_arianna(con, fondo = fondo)
  ids <- dbGetQuery(con, "SELECT id FROM fascicoli WHERE fondo_id = ? ORDER BY busta, fascicolo", list(fondo))$id
  vuota <- lapply(t, function(x) is.na(x) | !nzchar(trimws(as.character(x))))
  # Una cella vuota che un flag spiega non manca: «reato ignoto» e «non è un procedimento» spiegano il Soggettario vuoto; «non è un procedimento» anche il regesto.
  fl <- dbGetQuery(con, "SELECT id, reato_ignoto, non_procedimento FROM fascicoli WHERE fondo_id = ?", list(fondo))
  fl <- fl[match(ids, fl$id), , drop = FALSE]
  spiegata <- list(Soggettario = fl$reato_ignoto == 1L | fl$non_procedimento == 1L,
                   "Regesto o descrizione sintetica" = fl$non_procedimento == 1L)
  for (nome in names(spiegata)) {
    k <- which(.chiave(names(t)) == .chiave(nome))
    if (length(k) == 1L && length(spiegata[[nome]]) == length(vuota[[k]])) vuota[[k]] <- vuota[[k]] & !spiegata[[nome]]
  }
  r <- data.frame(colonna = names(t),
                  standard = .chiave(names(t)) %in% .chiave(COLONNE_STANDARD),
                  totale = nrow(t),
                  compilate = vapply(vuota, function(v) sum(!v), 1L),
                  stringsAsFactors = FALSE, row.names = NULL)
  r$mancanti <- r$totale - r$compilate
  # Una colonna standard che in questo fondo resta vuota per scelta (le persone di «Atti circa fra
  # Giulio»: senza cognome non sono persone citate) si dichiara accettata, e conta come completa.
  r$accettata <- r$colonna %in% colonne_accettate(con, fondo)
  r$per_regola <- .chiave(r$colonna) %in% .chiave(COLONNE_VUOTE_PER_REGOLA)
  attr(r, "ids_mancanti") <- stats::setNames(lapply(vuota, function(v) ids[v]), names(t))
  r
}

# Il fondo è pronto quando ogni colonna standard è compilata in tutte le schede.
# Non blocca niente: l'export resta sempre possibile, questo dice che cosa manca.
pronto_export <- function(r) {
  s <- r[r$standard, , drop = FALSE]
  ok <- s$mancanti == 0L | s$accettata | s$per_regola
  list(pronto = nrow(s) > 0 && all(ok),
       complete = sum(ok), standard = nrow(s),
       celle_mancanti = sum(s$mancanti[!ok]), colonne_incomplete = s$colonna[!ok])
}

# Le decisioni «questa colonna può restare vuota in questo fondo» stanno nella stessa tabella del
# vocabolario per fondo (campo «colonna_accettata»): vanno nel backup, e valgono per fondo.
colonne_accettate <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) return(character(0))
  dbGetQuery(con, "SELECT valore FROM vocabolario_fondo WHERE fondo_id = ? AND campo = 'colonna_accettata'", list(fondo))$valore
}
accetta_colonna <- function(con, fondo, colonna, accetta = TRUE) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) stop("Scegli prima il fondo.", call. = FALSE)
  if (accetta) dbExecute(con, "INSERT OR IGNORE INTO vocabolario_fondo (fondo_id, campo, valore, forma, creato) VALUES (?, 'colonna_accettata', ?, NULL, ?)",
                         list(fondo, colonna, format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  else dbExecute(con, "DELETE FROM vocabolario_fondo WHERE fondo_id = ? AND campo = 'colonna_accettata' AND valore = ?", list(fondo, colonna))
  invisible(TRUE)
}


# ── vocabolario per fondo e decisioni ricordate ───────────────────

registra_valore <- function(con, fondo, campo, valore, forma = NA_character_) {
  fondo <- .fid(con, fondo)
  if (.vuoto(fondo)) stop("Scegli prima il fondo.", call. = FALSE)
  if (!campo %in% CAMPI_CONTROLLATI$campo) stop("Campo non controllato: ", campo, call. = FALSE)
  valore <- trimws(valore); forma <- if (.vuoto(forma)) NA_character_ else trimws(forma)
  if (!nzchar(valore)) stop("Il valore è vuoto.", call. = FALSE)
  if (!is.na(forma) && identical(forma, valore)) stop("La forma giusta è uguale al valore.", call. = FALSE)
  dbExecute(con, "INSERT INTO vocabolario_fondo (fondo_id, campo, valore, forma, creato) VALUES (?, ?, ?, ?, ?)
                  ON CONFLICT (fondo_id, campo, valore) DO UPDATE SET forma = excluded.forma",
            list(fondo, campo, valore, forma, format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))
  invisible(TRUE)
}

togli_regola <- function(con, fondo, campo, valore) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  dbExecute(con, "DELETE FROM vocabolario_fondo WHERE fondo_id = ? AND campo = ? AND valore = ?",
            list(fondo, campo, trimws(valore)))
}


# ── classificare i valori ─────────────────────────────────────────

# Il valore più vicino a uno del vocabolario, se la distanza è piccola (un refuso).
.vicino <- function(x, voc) {
  if (!length(voc) || .vuoto(x)) return(NA_character_)
  d <- drop(utils::adist(tolower(x), tolower(voc)))
  i <- which.min(d)
  if (d[i] <= max(1, min(2, floor(nchar(x) / 6)))) voc[i] else NA_character_
}

# Stato di ogni valore rispetto al vocabolario (base + ammessi del fondo) e alle varianti registrate; `proposta` = la forma giusta, se c'è.
#   ok · ammesso (per il fondo) · variante (registrata) · maiuscole · vicino (refuso probabile) · nuovo (da decidere).
classifica_valori <- function(valori, base, ammessi = character(0), alias = character(0)) {
  voc <- unique(c(base, ammessi))
  una <- function(x) {
    y <- trimws(gsub("\\s+", " ", x))
    if (y %in% base) return(c("ok", NA))
    if (y %in% ammessi) return(c("ammesso", NA))
    if (y %in% names(alias)) return(c("variante", unname(alias[[y]])))
    i <- match(tolower(y), tolower(voc))
    if (!is.na(i)) return(c("maiuscole", voc[i]))
    v <- .vicino(y, voc)
    if (!is.na(v)) return(c("vicino", v))
    # Un refuso di una variante nota («Amminisrazione» per «Amministrazione», termine del tesauro) si propone nella forma a cui quella variante rimanda.
    w <- if (length(alias)) .vicino(y, names(alias)) else NA
    if (!is.na(w)) return(c("vicino", unname(alias[[w]])))
    c("nuovo", NA)
  }
  m <- vapply(valori, una, c("", ""))
  data.frame(valore = valori, stato = m[1, ], proposta = m[2, ], stringsAsFactors = FALSE, row.names = NULL)
}

# ── i valori di una colonna, in un fondo ──────────────────────────

.sql_valori <- function(campo, dove) switch(dove,
  reati  = "SELECT r.reato AS valore, COUNT(DISTINCT r.fascicolo_id) AS schede FROM reati r
            JOIN fascicoli f ON f.id = r.fascicolo_id WHERE f.fondo_id = ? GROUP BY r.reato",
  lingue = "SELECT l.lingua AS valore, COUNT(DISTINCT l.fascicolo_id) AS schede FROM lingue l
            JOIN fascicoli f ON f.id = l.fascicolo_id WHERE f.fondo_id = ? GROUP BY l.lingua",
  sprintf("SELECT %1$s AS valore, COUNT(*) AS schede FROM fascicoli
           WHERE fondo_id = ? AND %1$s IS NOT NULL AND %1$s <> '' GROUP BY %1$s", campo))

# Una riga per valore: quante schede, e che cosa ne pensa il vocabolario. Da vedere per primi.
valori_colonna <- function(con, fondo, campo) {
  fondo <- .fid(con, fondo)
  def <- .riga_campo(campo)
  if (!nrow(def)) stop("Campo non controllato: ", campo, call. = FALSE)
  v <- dbGetQuery(con, .sql_valori(campo, def$dove), list(fondo))
  if (!nrow(v)) return(data.frame(valore = character(0), schede = integer(0), stato = character(0), proposta = character(0)))
  r <- regole_fondo(con, fondo); r <- r[r$campo == campo, , drop = FALSE]
  cl <- classifica_valori(v$valore, vocabolario(campo), r$valore[is.na(r$forma)],
                          stats::setNames(r$forma[!is.na(r$forma)], r$valore[!is.na(r$forma)]))
  d <- cbind(cl[, "valore", drop = FALSE], schede = v$schede, cl[, c("stato", "proposta")])
  rango <- c(nuovo = 1, vicino = 2, maiuscole = 3, variante = 4, ammesso = 5, ok = 6)
  d[order(rango[d$stato], -d$schede, d$valore), , drop = FALSE]
}

# Quanti valori restano da decidere nel fondo (nuovo, vicino, maiuscole): serve a Home e a Qualità.
valori_da_decidere <- function(con, fondo) {
  if (.vuoto(fondo)) return(data.frame(campo = character(0), valori = integer(0)))
  x <- lapply(CAMPI_CONTROLLATI$campo, function(k) {
    v <- valori_colonna(con, fondo, k)
    data.frame(campo = k, valori = sum(v$stato %in% c("nuovo", "vicino", "maiuscole")), stringsAsFactors = FALSE)
  })
  x <- do.call(rbind, x)
  x[x$valori > 0, , drop = FALSE]
}


# ── correggere un valore in tutte le schede del fondo ─────────────

# Sostituisce `da` con `a` in tutte le schede del fondo che lo hanno, come la maschera: si cambia solo quel valore, titolo e regesto composti si ricompongono.
# prova = TRUE: solo anteprima (una riga per scheda che cambia). Le schede si trovano con una query sola.
.ids_con_valori <- function(con, fondo, def, campo, valori) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  ph <- paste(rep("?", length(valori)), collapse = ", ")
  sql <- switch(def$dove,
    reati  = "SELECT DISTINCT f.id FROM fascicoli f JOIN reati x ON x.fascicolo_id = f.id WHERE f.fondo_id = ? AND x.reato IN (%s) ORDER BY f.busta, f.fascicolo",
    lingue = "SELECT DISTINCT f.id FROM fascicoli f JOIN lingue x ON x.fascicolo_id = f.id WHERE f.fondo_id = ? AND x.lingua IN (%s) ORDER BY f.busta, f.fascicolo",
    paste0("SELECT f.id FROM fascicoli f WHERE f.fondo_id = ? AND f.", campo, " IN (%s) ORDER BY f.busta, f.fascicolo"))
  dbGetQuery(con, sprintf(sql, ph), c(list(fondo), as.list(valori)))$id
}

# Sostituisce più valori nel fondo: `mappa` ha nome (il valore com'è) e valore (come deve essere). Una lettura per scheda toccata; serve a «Pulisci un foglio».
sostituisci_valori <- function(con, fondo, campo, mappa, prova = FALSE) {
  def <- .riga_campo(campo)
  if (!nrow(def)) stop("Campo non controllato: ", campo, call. = FALSE)
  da <- trimws(names(mappa)); a <- trimws(as.character(mappa))
  if (any(!nzchar(a))) stop("Scrivi la forma giusta.", call. = FALSE)
  mappa <- stats::setNames(a, da)
  chiave <- if (def$dove == "fascicoli") campo else def$dove
  esito <- data.frame(id = character(0), segnatura = character(0), prima = character(0), dopo = character(0),
                      stringsAsFactors = FALSE)
  ids <- if (length(mappa)) .ids_con_valori(con, fondo, def, campo, da) else character(0)
  schede <- list()
  for (id in ids) {
    f <- leggi_fascicolo(con, id); if (is.null(f)) next
    v <- as.character(unlist(f[[chiave]])); v <- v[!is.na(v) & nzchar(v)]
    if (!any(v %in% da)) next
    nuovo <- unique(ifelse(v %in% da, unname(mappa[match(v, da)]), v))
    nuovo <- nuovo[nuovo != FORMA_ELIMINA]                      # «(elimina)»: il valore si toglie dalla scheda
    f[[chiave]] <- if (def$dove == "fascicoli") (if (length(nuovo)) nuovo[1] else NA_character_) else nuovo
    if (!isTRUE(f$titolo_manuale == 1L)) { t <- componi_titolo_scheda(f); if (!is.na(t)) f$titolo <- t }
    if (!isTRUE(f$regesto_manuale == 1L)) { r <- componi_regesto_scheda(f); if (!is.na(r)) f$regesto <- r }
    esito[nrow(esito) + 1L, ] <- list(id, posizione_scritta(f$sigla_busta, f$busta, f$sigla_fascicolo, f$fascicolo, f$fascicolo_suffisso, f$busta_suffisso),
                                      paste(v, collapse = "; "), paste(nuovo, collapse = "; "))
    schede[[id]] <- f
  }
  if (!prova && length(schede)) .atomico(con, for (f in schede) salva_fascicolo(con, f))
  esito
}

# Sostituisce `da` con `a` in tutte le schede del fondo che lo hanno, come la maschera: titolo e regesto composti si ricompongono.
sostituisci_valore <- function(con, fondo, campo, da, a, prova = FALSE) {
  if (!nzchar(trimws(a))) stop("Scrivi la forma giusta.", call. = FALSE)
  sostituisci_valori(con, fondo, campo, stats::setNames(a, da), prova)
}


# ── disegno ───────────────────────────────────────────────────────

ETICHETTA_STATO <- c(ok = "nel vocabolario", ammesso = "ammesso per il fondo", variante = "variante registrata",
                     maiuscole = "stessa voce, scritta diversamente", vicino = "forse un refuso", nuovo = "da decidere")

# La carta che dice se il fondo è pronto per l'export (Import / Export, Home, Per colonna).
pronto_ui <- function(r) {
  if (is.null(r)) return(p(class = "text-muted small", "Scegli un fondo per vedere se è pronto per l'export."))
  s <- pronto_export(r)
  tagList(
    div(class = paste("pronto-esito", if (s$pronto) "pronto" else "da-completare"),
        icon(if (s$pronto) "circle-check" else "circle-half-stroke"),
        span(class = "pronto-titolo", if (s$pronto) "Pronto per l'export" else "Da completare"),
        span(class = "pronto-conto",
             sprintf("%d colonn%s standard su %d complet%s", s$complete, if (s$complete == 1) "a" else "e",
                     s$standard, if (s$complete == 1) "a" else "e"))),
    if (!s$pronto) p(class = "small text-muted mb-0",
                     "L'export resta possibile: le colonne standard (quelle di Napoli) ancora vuote in qualche scheda si vedono in Qualità, «Per colonna»."),
    # Completezza non è attendibilità: una scritta positiva non deve sembrare una certificazione dei dati.
    p(class = "small text-muted mb-0", "Dice quanto è compilato, non che i dati siano giusti: omonimie, date sospette e luoghi incerti si vedono in Qualità.")
  )
}

# La tabella delle colonne: standard in cima, aggiuntive sotto. Il clic su «Mostra» porta in
# Fascicoli alle schede dove la colonna è vuota.
colonne_tabella_ui <- function(r) {
  riga <- function(i) {
    x <- r[i, ]
    pct <- if (x$totale) round(100 * x$compilate / x$totale) else 0
    tags$tr(class = if (x$standard && x$mancanti > 0 && !x$accettata && !x$per_regola) "colonna-incompleta",
      tags$td(class = "no-trad", x$colonna),
      tags$td(span(class = "small text-muted", if (x$standard) "standard" else "aggiuntiva"),
              if (x$accettata) span(class = "small text-muted ms-2", "accettata"),
              if (x$per_regola && x$mancanti > 0)
                span(class = "small text-muted ms-2", title = "Senza cognome non è una persona citata: la colonna resta vuota.", "per regola")),
      tags$td(div(class = "colonna-barra", span(style = sprintf("width:%d%%", pct))),
              span(class = "small text-nowrap", sprintf("%d su %d", x$compilate, x$totale))),
      tags$td(
        if (x$mancanti > 0 && x$compilate > 0 || (x$standard && x$mancanti > 0))
          tags$button(type = "button", class = "btn btn-sm btn-link p-0 me-2",
            onclick = sprintf("Shiny.setInputValue('colonna_mancanti', %s, {priority: 'event'})",
                              jsonlite::toJSON(x$colonna, auto_unbox = TRUE)),
            "Mostra le schede senza"),
        if (x$standard && x$mancanti > 0 && !x$per_regola)
          tags$button(type = "button", class = "btn btn-sm btn-link p-0",
            onclick = sprintf("Shiny.setInputValue('colonna_accetta', %s, {priority: 'event'})",
                              jsonlite::toJSON(list(colonna = x$colonna, accetta = !x$accettata, n = stats::runif(1)), auto_unbox = TRUE)),
            if (x$accettata) "Non accettare più" else "Può restare vuota")))
  }
  o <- c(which(r$standard), which(!r$standard))
  tags$table(class = "table table-sm colonne-tabella",
    tags$thead(tags$tr(tags$th("Colonna"), tags$th("Tipo"), tags$th("Compilate"), tags$th(""))),
    lapply(o, riga))
}


# La pagina «Per colonna» di Qualità: a sinistra il pronto per l'export e le 28 colonne, a destra
# i valori controllati con le decisioni (ammettere per il fondo, correggere in tutte le schede).
colonne_pagina_ui <- function() layout_column_wrap(
  width = 1/2, heights_equal = "row", class = "griglia-pagina mt-3",
  card(
    card_header("Pronto per l'export"),
    card_body(fillable = FALSE,
      uiOutput("col_pronto"),
      uiOutput("col_tabella"))),
  card(
    card_header("Valori controllati"),
    card_body(fillable = FALSE,
      p(class = "small text-muted",
        "Soggettario, lingua e le altre colonne a vocabolario, confrontati con il vocabolario. «Ammetti» vale solo per questo fondo e si ricorda; «Correggi» riscrive il valore in tutte le schede del fondo, e si ricorda per i prossimi import."),
      selectInput("col_campo", "Colonna", stats::setNames(CAMPI_CONTROLLATI$campo, CAMPI_CONTROLLATI$etichetta)),
      uiOutput("col_valori"))))

colonne_server <- function(input, output, session, con, fondo_attivo, ricarica, riempimento,
                           mostra_schede = function(etichetta, ids) NULL, dopo_modifica = function(ids) NULL) {
  senza_fondo <- function() p(class = "text-muted small", "Scegli un fondo in alto: questa pagina guarda un fondo solo.")

  output$col_pronto <- renderUI(pronto_ui(riempimento()))
  output$col_tabella <- renderUI({ r <- riempimento(); if (is.null(r)) NULL else colonne_tabella_ui(r) })

  observeEvent(input$colonna_accetta, {
    a <- input$colonna_accetta; f <- fondo_attivo(); if (.vuoto(f)) return()
    accetta_colonna(con, f, a$colonna, isTRUE(a$accetta)); ricarica(ricarica() + 1)
  })

  observeEvent(input$colonna_mancanti, {
    r <- riempimento(); if (is.null(r)) return()
    ids <- attr(r, "ids_mancanti")[[input$colonna_mancanti]]
    if (!length(ids)) { showNotification("Nessuna scheda senza questa colonna.", type = "message"); return() }
    mostra_schede(paste("Senza", input$colonna_mancanti), ids)
  })

  valori <- reactive({
    ricarica(); f <- fondo_attivo(); req(input$col_campo)
    if (.vuoto(f)) return(NULL)
    valori_colonna(con, f, input$col_campo)
  })

  azione <- function(a, valore, forma = "")
    sprintf("Shiny.setInputValue('val_azione', %s, {priority: 'event'})",
            jsonlite::toJSON(list(azione = a, valore = valore, forma = forma, n = stats::runif(1)), auto_unbox = TRUE))
  bottone <- function(testo, js) tags$button(type = "button", class = "btn btn-sm btn-outline-secondary me-1", onclick = js, testo)

  output$col_valori <- renderUI({
    v <- valori()
    if (is.null(v)) return(senza_fondo())
    if (!nrow(v)) return(p(class = "text-muted small", "Questo fondo non ha valori in questa colonna."))
    tags$table(class = "table table-sm colonne-tabella",
      tags$thead(tags$tr(tags$th("Valore"), tags$th("Schede"), tags$th("Stato"), tags$th("Forma giusta"), tags$th(""))),
      lapply(seq_len(nrow(v)), function(i) {
        x <- v[i, ]
        tags$tr(class = if (x$stato %in% c("nuovo", "vicino", "maiuscole")) "colonna-incompleta",
          tags$td(x$valore), tags$td(x$schede), tags$td(class = "small", unname(ETICHETTA_STATO[x$stato])),
          tags$td(if (!is.na(x$proposta)) x$proposta),
          tags$td(
            if (x$stato %in% c("nuovo", "vicino", "maiuscole"))
              tagList(bottone("Correggi…", azione("correggi", x$valore, if (is.na(x$proposta)) "" else x$proposta)),
                      bottone("Ammetti per il fondo", azione("ammetti", x$valore)),
                      bottone("Non è un valore: elimina…", azione("correggi", x$valore, FORMA_ELIMINA))),
            if (x$stato %in% c("ammesso", "variante"))
              bottone("Togli la regola", azione("togli", x$valore))))
      }))
  })

  bersaglio <- reactiveVal(NULL)    # list(valore, forma) del valore che si sta correggendo
  observeEvent(input$val_azione, {
    a <- input$val_azione; f <- fondo_attivo(); campo <- input$col_campo
    if (.vuoto(f)) return()
    tryCatch(switch(a$azione,
      ammetti = {
        registra_valore(con, f, campo, a$valore); ricarica(ricarica() + 1)
        showNotification(tagList("Ammesso per il fondo: «", span(class = "no-trad", a$valore), "»."), type = "message")
      },
      togli = {
        togli_regola(con, f, campo, a$valore); ricarica(ricarica() + 1)
        showNotification(tagList("Regola tolta: «", span(class = "no-trad", a$valore), "»."), type = "message")
      },
      correggi = {
        bersaglio(list(valore = a$valore))
        showModal(modalDialog(title = "Correggi un valore", size = "l", easyClose = TRUE,
          p(class = "small text-muted", "Il valore si riscrive in tutte le schede di questo fondo. Titolo e regesto composti si ricompongono; quelli scritti a mano restano. Prima di scrivere si fa una copia di sicurezza del database."),
          p(span("Valore: "), strong(class = "no-trad", a$valore)),
          textInput("val_forma", "Forma giusta", value = a$forma, width = "100%"),
          p(class = "small text-muted", "Se non è un valore di questa colonna, la forma giusta è «(elimina)»: si toglie dalle schede."),
          checkboxInput("val_ricorda", "Ricorda per i prossimi import di questo fondo", TRUE),
          uiOutput("val_anteprima"),
          footer = tagList(modalButton("Annulla"), actionButton("val_applica", "Applica", class = "btn-primary"))))
      }),
      error = function(e) showNotification(conditionMessage(e), type = "error"))
  })

  output$val_anteprima <- renderUI({
    b <- bersaglio(); f <- fondo_attivo(); campo <- input$col_campo
    req(b, input$val_forma, nzchar(trimws(input$val_forma)))
    x <- tryCatch(sostituisci_valore(con, f, campo, b$valore, input$val_forma, prova = TRUE), error = function(e) conditionMessage(e))
    if (is.character(x)) return(p(class = "text-danger", x))
    nuovo <- !(trimws(input$val_forma) %in% c(vocabolario_di(con, campo, f), FORMA_ELIMINA))
    tagList(
      if (nuovo) p(class = "small text-muted", "La forma giusta non è nel vocabolario: verrà ammessa per questo fondo."),
      if (!nrow(x)) p(class = "text-muted", "Nessuna scheda cambierebbe.")
      else tagList(p(sprintf("Cambia%s %d sched%s.", if (nrow(x) == 1) "" else "no", nrow(x), if (nrow(x) == 1) "a" else "e")),
        div(style = "max-height:35vh;overflow:auto", tags$table(class = "table table-sm small",
          tags$tr(tags$th("Scheda"), tags$th("Prima"), tags$th("Dopo")),
          lapply(seq_len(nrow(x)), function(i) tags$tr(tags$td(x$segnatura[i]), tags$td(x$prima[i]), tags$td(strong(x$dopo[i]))))))))
  })

  observeEvent(input$val_applica, {
    b <- bersaglio(); f <- fondo_attivo(); campo <- input$col_campo
    req(b, !.vuoto(f))
    tryCatch({
      forma <- trimws(input$val_forma)
      copia <- backup_prima_ripristino(con, file.path(dirname(percorso_db()), "backup"), "valori")
      x <- sostituisci_valore(con, f, campo, b$valore, forma)
      if (!identical(forma, FORMA_ELIMINA) && !(forma %in% vocabolario_di(con, campo, f))) registra_valore(con, f, campo, forma)
      if (isTRUE(input$val_ricorda)) registra_valore(con, f, campo, b$valore, forma)
      removeModal(); bersaglio(NULL); ricarica(ricarica() + 1); dopo_modifica(x$id)
      notifica_con_cartella(sprintf("%s. Copia di sicurezza fatta.",
                               if (nrow(x) == 1) "Corretta 1 scheda" else sprintf("Corrette %d schede", nrow(x))), "backup")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
}
