# ─────────────────────────────────────────────────────────────
# luoghi_citati.R: la pagina «Luoghi citati»: elenco a sinistra, scheda a destra
# Dipende da crud.R (leggi_fascicolo), qualita.R (.solo_fondo, chiave_grafia), fusioni.R (proposta_fusione, unisci_luoghi), lista_luoghi.R (leggi_lista_luoghi)
# Tutto ciò che la scheda dice si LEGGE dai fascicoli (nel tempo, reati, luoghi citati insieme). Scrive una cosa sola: la fusione di due grafie dello stesso luogo (unisci_luoghi).
# ─────────────────────────────────────────────────────────────

# L'elenco: una riga per luogo, con quanti fascicoli e in che anni.
elenco_voci <- function(con, fondo = NULL) {
  d <- dbGetQuery(con, .solo_fondo(
    "SELECT x.luogo AS grafia, x.fascicolo_id AS id, f.estremo_remoto AS anno
       FROM luoghi x JOIN fascicoli f ON f.id = x.fascicolo_id WHERE TRIM(x.luogo) <> ''", fondo))
  vuoto <- data.frame(nome = character(0), n = integer(0), da = integer(0), a = integer(0),
                      cerca = character(0), stringsAsFactors = FALSE)
  if (!nrow(d)) return(vuoto)
  d$nome <- d$grafia
  g <- split(d, d$nome)
  anni <- function(x) { x <- x[!is.na(x)]; if (length(x)) range(x) else c(NA_integer_, NA_integer_) }
  data.frame(
    nome = names(g),
    n    = vapply(g, function(x) length(unique(x$id)), integer(1)),
    da   = vapply(g, function(x) anni(x$anno)[1], integer(1)),
    a    = vapply(g, function(x) anni(x$anno)[2], integer(1)),
    cerca = vapply(g, function(x) paste(unique(x$grafia), collapse = " "), ""),
    stringsAsFactors = FALSE, row.names = NULL)
}

# Cerca e ordina l'elenco (senza accenti, senza maiuscole).
filtra_voci <- function(v, cerca = "", ordine = c("az", "za", "citazioni")) {
  ordine <- match.arg(ordine)
  if (!is.null(cerca) && nzchar(trimws(cerca))) {
    q <- .senza_accenti(tolower(trimws(cerca)))
    v <- v[grepl(q, .senza_accenti(tolower(paste(v$nome, v$cerca))), fixed = TRUE), , drop = FALSE]
  }
  chiave <- .senza_accenti(tolower(v$nome))
  o <- switch(ordine,
              az = order(chiave),
              za = order(chiave, decreasing = TRUE),
              citazioni = order(-v$n, chiave))
  v[o, , drop = FALSE]
}

.dividi <- function(x) {
  x <- if (is.na(x) || !nzchar(x)) character(0) else strsplit(x, " | ", fixed = TRUE)[[1]]
  trimws(x[nzchar(trimws(x))])
}
.conta <- function(x) { t <- sort(table(x), decreasing = TRUE); stats::setNames(as.integer(t), names(t)) }
.conta_ordinata <- function(x) { t <- table(x); stats::setNames(as.integer(t), names(t)) }
.decenni <- function(anni) { anni <- anni[!is.na(anni)]; .conta_ordinata((anni %/% 10L) * 10L) }
# Etichette «1650s» per le barre; senza decenni paste0() darebbe un'etichetta «s» sola e setNames() fallirebbe.
.decenni_con_s <- function(d) if (length(d)) stats::setNames(d, paste0(names(d), "s")) else d

# La scheda di un luogo. `d` è elenco_fascicoli() del fondo (in app.R: dati()), per non rileggerlo a ogni clic.
profilo_luogo <- function(con, nome, d) {
  ids <- dbGetQuery(con, "SELECT DISTINCT fascicolo_id AS id FROM luoghi WHERE luogo = ?", list(nome))$id
  f <- d[d$id %in% ids, , drop = FALSE]
  f <- f[order(is.na(f$anno), f$anno, f$busta, if ("suffisso_b" %in% names(f)) .rango_suffisso(f$suffisso_b) else 0L, f$fascicolo), , drop = FALSE]
  noti <- c(leggi_lista_luoghi()$chiave, chiave_grafia(forme_locali_luoghi()$locale))
  list(nome = nome, fascicoli = f,
       anni = if (any(!is.na(f$anno))) range(f$anno, na.rm = TRUE) else NULL,
       decenni = .decenni(f$anno),
       reati = .conta(unlist(lapply(f$reati, .dividi))),
       # Non setdiff(): toglierebbe anche le ripetizioni, e ogni conto varrebbe 1.
       altri_luoghi = local({ x <- unlist(lapply(f$luoghi, .dividi)); .conta(x[x != nome]) }),
       in_lista = chiave_grafia(nome) %in% noti)
}

.segnatura_breve <- function(f) {
  col <- function(nomi) { k <- intersect(nomi, names(f))[1]; if (is.na(k)) rep(NA_character_, nrow(f)) else f[[k]] }
  sf <- sub("^fasc\\.$", "f.", sigle_fascicolo(col(c("sigla_f", "sigla_fascicolo"))))   # in breve «f.»
  sb <- col(c("suffisso_b", "busta_suffisso")); sb <- ifelse(is.na(sb), "", sb)
  sprintf("%s%s%s, %s%s%s", sigle_busta(col(c("sigla", "sigla_busta"))), ifelse(is.na(f$busta), "?", f$busta), sb, sf,
          ifelse(is.na(f$fascicolo), "?", f$fascicolo), ifelse(is.na(f$suffisso), "", f$suffisso))
}


# ── pezzi di interfaccia ───────────────────────────────────────────

.link_fascicolo <- function(id, testo)
  tags$a(href = "#", class = "link-segnatura", title = "Apri la scheda",
         onclick = sprintf("Shiny.setInputValue('apri_fascicolo', '%s', {priority:'event'}); return false;", id),
         testo)

# Barre orizzontali in HTML: etichetta, barra, numero. Niente ggplot: si ridisegnano a ogni clic e devono essere istantanee.
.barre <- function(conti, classe = "", massimo = max(conti)) {
  if (!length(conti)) return(div(class = "small text-muted", "—"))
  div(class = paste("barre", classe),
      lapply(seq_along(conti), function(i)
        div(class = "barra-riga",
            span(class = "barra-etichetta", names(conti)[i]),
            span(class = "barra-traccia", span(style = sprintf("width:%.0f%%", 100 * conti[[i]] / max(1, massimo)))),
            span(class = "barra-numero", conti[[i]]))))
}

.chip_conti <- function(conti, classe = "") {
  if (!length(conti)) return(div(class = "small text-muted", "—"))
  div(class = "riga-pillole", lapply(seq_along(conti), function(i)
    span(class = paste("chip-conto", classe), names(conti)[i], strong(conti[[i]]))))
}

.riquadro <- function(titolo, ...) div(class = "riquadro-scheda", div(class = "riquadro-titolo", titolo), ...)

.periodo <- function(a) {
  if (is.null(a) || any(!is.finite(a))) "senza data"
  else if (a[1] == a[2]) as.character(a[1]) else paste0(a[1], "–", a[2])
}

.fascicoli_ui <- function(f) {
  if (!nrow(f)) return(div(class = "small text-muted", "—"))
  div(class = "elenco-fascicoli-voce",
      lapply(seq_len(nrow(f)), function(i)
        div(class = "fascicolo-voce",
            .link_fascicolo(f$id[i], .segnatura_breve(f[i, ])),
            span(class = "text-muted", if (is.na(f$anno[i])) "s.d." else f$anno[i]),
            span(class = "cella-riga", if (is.na(f$titolo[i])) "(senza titolo)" else f$titolo[i]))))
}

scheda_luogo_ui <- function(pl) {
  f <- pl$fascicoli
  div(class = "scheda-voce",
    div(class = "scheda-voce-titolo", icon("location-dot"), pl$nome),
    div(class = "small text-muted mb-3",
        sprintf("%d fascicol%s · %s", nrow(f), if (nrow(f) == 1) "o" else "i", .periodo(pl$anni))),
    layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)), gap = ".75rem",
      .riquadro("Nel tempo", .barre(.decenni_con_s(pl$decenni), "barre-tempo")),
      .riquadro("Nel vocabolario dei luoghi",
                div(class = "small",
                  if (!pl$in_lista) "Non è nel vocabolario. Si aggiunge con «Aggiungi alla lista» quando importi un foglio (o si corregge la grafia)."
                  else "È nel vocabolario dei luoghi.")),
      .riquadro("Reati nei suoi fascicoli", .chip_conti(pl$reati)),
      .riquadro("Citati con questo luogo", .chip_conti(head(pl$altri_luoghi, 12)))),
    div(class = "riquadro-titolo mt-3", sprintf("Fascicoli (%d)", nrow(f))),
    .fascicoli_ui(f))
}

# L'elenco a sinistra: HTML semplice, un pulsante per voce; la voce scelta la evidenzia il browser ('voce_attiva'), così scegliere non ridisegna l'elenco.
# Ogni riga ha la sua barra (--p) in proporzione alle citazioni rispetto al luogo più citato del fondo (`massimo`): l'elenco è anche la classifica.
lista_voci_ui <- function(v, input_id, attiva = NULL, massimo = max(v$n, 1L)) {
  if (!nrow(v)) return(div(class = "small text-muted p-2", "Nessuna voce corrisponde alla ricerca."))
  div(class = "lista-voci", `data-input` = input_id,
      lapply(seq_len(nrow(v)), function(i)
        tags$button(type = "button", class = paste("voce-lista", if (identical(v$nome[i], attiva)) "attiva"),
                    style = sprintf("--p: %.0f%%", 100 * v$n[i] / massimo),
                    `data-nome` = v$nome[i], span(class = "cella-riga", v$nome[i]), span(class = "voce-n", v$n[i]))))
}

# Il pannello di destra prima di scegliere una voce: un riepilogo in tre numeri; un clic su una voce apre qui la sua scheda.
riepilogo_voci_ui <- function(v, input_id) {
  piu <- sum(v$n > 1)
  div(class = "riquadro-scheda riepilogo-voci",
      div(class = "riquadro-titolo", "Luoghi citati nel fondo"),
      div(class = "riepilogo-numeri",
          div(tags$b(format(nrow(v), big.mark = ".", decimal.mark = ",")), span("Luoghi")),
          div(tags$b(format(piu, big.mark = ".", decimal.mark = ",")), span("In più di un fascicolo")),
          div(tags$b(format(nrow(v) - piu, big.mark = ".", decimal.mark = ",")), span("In un fascicolo solo"))),
      p(class = "small text-muted mt-3 mb-0",
        "Scegli un luogo nell'elenco: fascicoli, periodo e reati che vi compaiono, e se è nel vocabolario dei luoghi. ",
        "La barra di ogni luogo è lunga in proporzione ai fascicoli che lo citano."))
}

# Le grafie quasi uguali: una scelta per gruppo, applicate tutte insieme.
doppioni_ui <- function(pf, prefisso) {
  if (!nrow(pf)) return(NULL)
  div(class = "riquadro-doppioni",
      div(class = "riquadro-titolo", icon("code-merge"), sprintf(" Possibili doppioni da digitazione (%d)", nrow(pf))),
      p(class = "small text-muted mb-2",
        "Nomi quasi identici: se sono lo stesso luogo, scegli quale grafia tenere per ciascun gruppo che vuoi correggere, poi applica tutte le scelte insieme. ",
        "Nessuna fusione avviene da sola. Nei fascicoli la grafia scartata viene riscritta con quella tenuta."),
      lapply(seq_len(nrow(pf)), function(i) {
        forme <- strsplit(pf$gruppo[i], "   ·   ", fixed = TRUE)[[1]]
        div(class = "gruppo-doppioni",
            radioButtons(paste0(prefisso, i), NULL, inline = TRUE, selected = character(0),
                         choiceNames = lapply(forme, function(x) paste0("Tieni «", x, "»")),
                         choiceValues = forme))
      }),
      actionButton(paste0(prefisso, "applica"), tagList(icon("code-merge"), "Applica le correzioni selezionate"),
                   class = "btn-primary btn-sm mt-1"))
}

luoghi_ui <- function() {
  nav_panel(
    title = tagList(icon("location-dot"), "Luoghi"),
    value = "Luoghi citati",
    titolo_pagina("Luoghi citati"),
    div(class = "pt-1",
      div(class = "small text-muted mb-2", textOutput("luoghi_conta", inline = TRUE)),
      uiOutput("luoghi_doppioni"),
      textInput("luoghi_cerca", NULL, width = "100%", placeholder = "Cerca un luogo…"),
      div(class = "d-flex align-items-center gap-2 mb-2",
          span(class = "small text-muted", "Ordina:"),
          div(class = "gruppo-pillole ordine-voci", `data-input` = "luoghi_ordine",
              tags$button(type = "button", class = "btn btn-pillola btn-sm attiva", `data-valore` = "az", "A–Z"),
              tags$button(type = "button", class = "btn btn-pillola btn-sm", `data-valore` = "za", "Z–A"),
              tags$button(type = "button", class = "btn btn-pillola btn-sm", `data-valore` = "citazioni", "Per citazioni"))),
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(4, 8)), gap = "1rem",
        div(class = "colonna-lista", uiOutput("luoghi_lista")),
        uiOutput("luoghi_scheda"))),
    tags$script(HTML("
      $(document).on('click', '.lista-voci .voce-lista', function() {
        var l = this.closest('.lista-voci');
        $(l).find('.voce-lista.attiva').removeClass('attiva'); $(this).addClass('attiva');
        Shiny.setInputValue(l.dataset.input, this.dataset.nome, {priority: 'event'});
      });
      $(document).on('click', '.ordine-voci .btn-pillola', function() {
        var g = this.closest('.ordine-voci');
        $(g).find('.attiva').removeClass('attiva'); $(this).addClass('attiva');
        Shiny.setInputValue(g.dataset.input, this.dataset.valore);
      });
      Shiny.addCustomMessageHandler('voce_attiva', function(m) {
        var l = document.querySelector('.lista-voci[data-input=\"' + m.input + '\"]'); if (!l) return;
        $(l).find('.voce-lista').each(function() { this.classList.toggle('attiva', this.dataset.nome === m.nome); });
        var a = l.querySelector('.voce-lista.attiva'); if (a) a.scrollIntoView({block: 'nearest'});
      });
    ")))
}


# ── server ─────────────────────────────────────────────────────────
# `dati`: elenco_fascicoli() del fondo attivo (reattivo, da app.R).
luoghi_server <- function(input, output, session, con, fondo_attivo, ricarica, dati, fusione_consentita = function() TRUE) {
  versione <- reactiveVal(0)       # fusioni scritte da qui
  errore <- function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)
  scelta_id <- "luogo_scelto_anagrafe"

  voci <- reactive({ ricarica(); versione(); elenco_voci(con, fondo_attivo()) })
  cerca <- debounce(reactive(input$luoghi_cerca), 250)
  visibili <- reactive(filtra_voci(voci(), cerca(), if (is.null(input$luoghi_ordine)) "az" else input$luoghi_ordine))

  output$luoghi_conta <- renderText({
    n <- nrow(voci()); m <- nrow(visibili())
    paste0(if (m < n) paste(m, "su ") else "", n, " luoghi citati")
  })
  output$luoghi_lista <- renderUI({
    if (!nrow(voci())) return(div(class = "small text-muted p-2", "Nessun luogo citato nei fascicoli di questo fondo."))
    lista_voci_ui(visibili(), scelta_id, isolate(input[[scelta_id]]), massimo = max(voci()$n, 1L))
  })
  output$luoghi_scheda <- renderUI({
    nome <- input[[scelta_id]]
    if (is.null(nome) || !nome %in% voci()$nome)
      return(if (nrow(voci())) riepilogo_voci_ui(voci(), scelta_id))
    session$sendCustomMessage("voce_attiva", list(input = scelta_id, nome = nome))
    scheda_luogo_ui(profilo_luogo(con, nome, dati()))
  })

  pf <- reactive({ ricarica(); versione(); proposta_fusione(con, "luoghi", fondo_attivo()) })
  output$luoghi_doppioni <- renderUI(doppioni_ui(pf(), "luoghi_dop_"))

  observeEvent(input$luoghi_dop_applica, {
    if (!fusione_consentita()) return()
    g <- pf(); fatti <- 0L; toccati <- 0L
    for (i in seq_len(nrow(g))) {
      tieni <- input[[paste0("luoghi_dop_", i)]]
      if (is.null(tieni) || !nzchar(tieni)) next
      forme <- strsplit(g$gruppo[i], "   ·   ", fixed = TRUE)[[1]]
      esito <- tryCatch({
        for (a in setdiff(forme, tieni)) toccati <- toccati + unisci_luoghi(con, tieni, a)
        TRUE
      }, error = function(e) { errore(e); FALSE })
      if (esito) fatti <- fatti + 1L
    }
    if (!fatti) return(showNotification("Scegli prima, per almeno un gruppo, quale grafia tenere.", type = "warning"))
    versione(versione() + 1); ricarica(ricarica() + 1)
    showNotification(sprintf("%d grupp%s corrett%s · %d fascicoli riscritti", fatti, if (fatti == 1) "o" else "i",
                             if (fatti == 1) "o" else "i", toccati), type = "message")
  })
}
