# ── Home ─────────────────────────────────────────────────────
# La prima scheda: dove sono, da dove riprendo, che cosa manca. Legge i dati e porta alle schede esistenti; unica azione propria: Nuovo archivio.

# Etichetta del pulsante «Riapri»: posizione dell'ultima scheda modificata, con i suffissi («b. 232-bis f. 5-ter»); con le sigle di sempre la forma corta «b. 3 f. 1».
etichetta_riapri <- function(u) {
  suf <- function(x) if (.vuoto(x)) "" else as.character(x)
  if (identical(sigla_busta(u$sigla_busta), "b.") && identical(sigla_fascicolo(u$sigla_fascicolo), "fasc."))
    sprintf("Riapri b. %s%s f. %s%s", u$busta, suf(u$busta_suffisso), u$fascicolo, suf(u$fascicolo_suffisso))
  else sprintf("Riapri %s", posizione_scritta(u$sigla_busta, u$busta, u$sigla_fascicolo, u$fascicolo, u$fascicolo_suffisso, u$busta_suffisso))
}


home_ui <- function() nav_panel(
  title = tagList(icon("house"), "Home"), value = "Home",
  div(class = "home",
    h1(class = "visually-hidden", "Home"),
    div(id = "home-carico", class = "home-carico text-muted", "Carico il fondo\u2026"),
    uiOutput("home_inizio"),
    uiOutput("home_archivi"),
    uiOutput("home_prossimo"),
    uiOutput("home_buste"),
    uiOutput("home_porte"),
    uiOutput("home_fasi"))
)

# Il titolo di un blocco della Home: una domanda e, sotto, una frase.
# La riga del fondo in uso fra quelli noti: NA se non ce n'è uno (all'avvio il fondo attivo è NULL, con «tutti i fondi» è vuoto, o può non esistere più); con NULL `match()` darebbe integer(0).
indice_fondo_attivo <- function(attivo, fondi) {
  if (is.null(attivo) || length(attivo) != 1L || is.na(attivo) || !nzchar(attivo)) return(NA_integer_)
  i <- match(attivo, fondi); if (length(i)) i[1] else NA_integer_
}

titolo_blocco <- function(domanda, spiega = NULL, ...) tagList(
  div(class = "home-titolo", h2(class = "home-h", domanda), ...), if (!is.null(spiega)) p(class = "home-spiega", spiega))

# Il fondo di esempio: un foglio nel modello, con dati inventati (dati/esempio/).
# Si carica solo in un database senza fascicoli: non si mescola mai al lavoro vero.
FOGLIO_ESEMPIO <- file.path("dati", "esempio", "esempio.xlsx")
carica_esempio <- function(con, foglio = FOGLIO_ESEMPIO) {
  if (dbGetQuery(con, "SELECT COUNT(*) AS n FROM fascicoli")$n > 0)
    stop("Il fondo di esempio si carica solo in un database vuoto.", call. = FALSE)
  importa_nuovo_archivio(con, leggi_arianna(foglio), "Archivio di Esempio", "Tribunale di esempio")
}

# `attiva_fondo(fondo)` cambia il fondo (con l'avviso delle modifiche non salvate), `apri(id)` apre una scheda, `nuova()` ne apre una nuova, `incompleti()` filtra Fascicoli sugli incompleti.
home_server <- function(input, output, session, con, dati, fondo_attivo, ricarica,
                        attiva_fondo, apri, nuova, incompleti, continua = function(dove) NULL, cerca, riempimento = function() NULL, anomalie = function() NULL) {
  vai <- function(scheda) nav_select("nav", scheda)
  manca <- function(x) span(class = "manca", x)

  qual <- reactive({ ricarica(); f <- fondo_attivo(); qualita(con, if (.vuoto(f)) NULL else f) })
  casi <- function(q, controllo) { x <- q$casi[q$controllo == controllo]; if (length(x)) x[1] else 0L }

  senza_fondi <- reactive({ ricarica(); nrow(elenco_fondi(con)) == 0 })
  # Dove sono nel lavoro: fondo vuoto, valori da decidere dopo un import, schede da completare, colonne mancanti, anomalie, pronto per esportare.
  # Decide il «prossimo passo» e la corsia evidenziata (A = compilo, B = normalizzo); i valori da decidere vengono prima delle incomplete.
  stato_lavoro <- reactive({
    ricarica(); f <- fondo_attivo(); d <- dati(); q <- qual(); r <- riempimento()
    n <- nrow(d); inc <- sum(completezza_core(d) < 4L)
    segn <- sum(q$casi > 0 & !(q$controllo %in% QUALITA_DI_FONDO))
    pr <- if (is.null(r)) NULL else pronto_export(r)
    vd <- if (.vuoto(f)) 0L else as.integer(sum(valori_da_decidere(con, f)$valori))
    # Gli errori già nelle schede (R/anomalie.R): la stessa cosa della fascia di Fascicoli, qui come prossimo passo.
    an <- anomalie(); err <- if (!is.null(an) && identical(an$fondo, f) && !is.null(an$an)) an$an$pr$totale else 0L
    caso <- if (n == 0) "vuoto" else if (vd > 0) "normalizzare" else if (err > 0) "errori" else if (inc > 0) "completare"
            else if (!is.null(pr) && !pr$pronto) "colonne" else if (segn > 0) "controllare" else "esportare"
    list(caso = caso, n = n, inc = inc, segn = segn, pronto = pr, vd = vd, err = err, err_schede = if (err > 0) schede_con_anomalie(an$an) else 0L,
         corsia = switch(caso, vuoto = NA_character_, normalizzare = "B", errori = "B", completare = "A", colonne = "A",
                         controllare = "B", esportare = "A"))
  })

  # Database senza fondi: la Home accompagna dal primo passo all'export.
  output$home_inizio <- renderUI({
    if (!senza_fondi()) return(NULL)
    passo <- function(n, titolo, testo) div(class = "home-passo", span(class = "n", n), h2(titolo), p(testo))
    div(class = "home-hero",
      p(class = "lead", "Si comincia da un fondo: un gruppo di fascicoli di un archivio. Trama ti accompagna dalla scheda al foglio Excel."),
      div(class = "home-passi",
        passo(1, "Crea il fondo", "Dai un nome all'archivio e al fondo: le schede nuove ne erediteranno i dati."),
        passo(2, "Compila o importa", "Scrivi le schede una a una, oppure importa un foglio già fatto: Trama lo corregge e ti dice che cosa ha cambiato."),
        passo(3, "Esporta", "Un solo modello Excel di 28 colonne, per ogni fondo.")),
      div(class = "home-hero-azioni",
        actionButton("home_nuovo_archivio", tagList(icon("plus"), "Crea il primo fondo"), class = "btn-primary btn-lg"),
        if (file.exists(FOGLIO_ESEMPIO))
          actionButton("home_esempio", tagList(icon("flask"), "Prova con un fondo di esempio"), class = "btn-outline-secondary btn-lg")),
      if (file.exists(FOGLIO_ESEMPIO))
        p(class = "small text-muted", "L'esempio ha 24 fascicoli inventati, per provare Trama. Si tolgono da Fascicoli, spuntandoli tutti."))
  })

  # Il prossimo passo: una sola cosa da fare adesso, scelta da dove si è nel lavoro.
  output$home_prossimo <- renderUI({
    if (senza_fondi()) return(NULL)
    st <- stato_lavoro(); d <- dati(); f <- fondo_attivo()
    u <- ultimo_modificato(con, if (.vuoto(f)) NULL else f)
    esame <- if (nrow(d)) sum(esame_da_fare(d)) else 0L
    riapri <- if (!is.null(u) && st$caso %in% c("completare", "colonne", "controllare")) actionButton("home_riprendi_ultima", tagList(icon("pen"),
      etichetta_riapri(u),
      if (!.vuoto(u$titolo)) span(class = "fw-normal", style = "opacity:.8", "· ", span(class = "dato", u$titolo))),
      class = "btn-outline-primary")
    nuova_b <- actionButton("home_nuova", tagList(icon("plus"), "Nuovo fascicolo"), class = "btn-outline-secondary")
    corpo <- switch(st$caso,
      vuoto = list("Il fondo è vuoto", "Crea il primo fascicolo, oppure importa un foglio già compilato.",
        list(actionButton("home_nuova", tagList(icon("plus"), "Crea il primo fascicolo"), class = "btn-primary"),
             actionButton("home_pp_importa", tagList(icon("file-import"), "Importa un foglio"), class = "btn-outline-secondary"))),
      normalizzare = list("Decidi i valori da normalizzare", "Dopo un import restano valori nuovi o dubbi: ammettili per il fondo o correggili in tutte le schede.",
        list(actionButton("home_pp_valori", tagList(icon("table-columns"), "Rivedi per colonna"), class = "btn-primary"),
             actionButton("home_pp_importa", tagList(icon("file-import"), "Importa un foglio"), class = "btn-outline-secondary")),
        tagList(tags$b(st$vd), span(class = "ms-1", "valori da decidere"))),
      errori = list("Correggi gli errori nelle schede", "Trama ha trovato refusi e discordanze nelle schede già salvate: si correggono con la stessa finestra dell'import, dopo una copia di sicurezza.",
        list(actionButton("home_apri_anomalie", "Controlla e correggi\u2026", class = "btn-primary btn-sm")),
        tagList(tags$b(st$err), span(class = "ms-1", sprintf("possibili errori in %d schede", st$err_schede)))),
      completare = list("Completa le schede", "Meno di 4 campi fondamentali compilati: persone o titolo, anno, luogo, reato.",
        list(actionButton("home_incompleti", tagList(icon("list-check"), "Apri l'elenco delle incomplete"), class = "btn-primary"), riapri, nuova_b),
        tagList(tags$b(st$inc), span(class = "ms-1", "schede da completare"))),
      colonne = list("Completa le colonne standard", "Le schede sono complete, ma qualche colonna standard del modello è ancora vuota.",
        list(actionButton("home_pp_colonne", tagList(icon("table-columns"), "Vedi quali mancano"), class = "btn-primary"), riapri, nuova_b),
        tagList(tags$b(st$pronto$standard - st$pronto$complete), span(class = "ms-1", "colonne da completare"))),
      controllare = list("Rivedi i controlli di Qualità", "Controlli di Qualità con casi segnalati: grafie divergenti, luoghi fuori vocabolario…",
        list(actionButton("home_anomalie", tagList(icon("shield-halved"), "Apri Qualità"), class = "btn-primary"), riapri, nuova_b),
        tagList(tags$b(st$segn), span(class = "ms-1", "controlli con casi"))),
      esportare = list("Esporta il fondo", "Le schede e le colonne standard sono complete: il foglio Excel è pronto.",
        list(actionButton("home_vai_export", tagList(icon("file-export"), "Vai all'export"), class = "btn-primary"), nuova_b)))
    # «Da guardare»: una fascia sottile che dice che cosa c'è da sistemare (errori, schede incomplete…) e porta lì con collegamenti.
    # Con tutto a posto resta solo, in neutro, la riga su tortura e sentenza.
    urgente <- !identical(st$caso, "esportare")
    if (!urgente && esame == 0) return(NULL)
    # La fascia gialla dice solo ciò che richiede un'azione; tortura e sentenza (lavoro di fondo) stanno in una riga neutra a parte.
    tagList(
      if (urgente) div(class = "home-guarda",
        icon("triangle-exclamation"),
        span(class = "testo", span(class = "etichetta-ora", "Da fare ora"), if (length(corpo) > 3) corpo[[4]] else corpo[[1]]),
        div(class = "azioni-guarda", corpo[[3]])),
      if (esame > 0) div(class = "home-guarda neutra",
        span(class = "sotto", span(class = "etichetta-ora", "Poi"),
          sprintf("Tortura o sentenza ancora da esaminare sulla fonte in %d fascicoli.", esame), " ",
          actionLink("home_esame", "Apri l'elenco"))))
  })

  # A che punto sono le buste: solo quelle aperte (fascicoli attesi non ancora schedati, o schede incomplete), al massimo otto; le altre si contano in una riga.
  output$home_buste <- renderUI({
    if (senza_fondi()) return(NULL)
    ricarica(); f <- fondo_attivo(); if (.vuoto(f)) return(NULL)
    av <- avanzamento_buste(con, f); if (!nrow(av)) return(NULL)
    aperta <- (!is.na(av$mancano) & av$mancano > 0L) | av$incomplete > 0L
    if (!any(aperta)) return(NULL)
    mostra <- which(aperta); resto <- length(mostra) - 8L; mostra <- head(mostra, 8L)
    riga <- function(i) {
      a <- av[i, ]; ha_piano <- !is.na(a$attese) && a$attese > 0L
      quanto <- c(if (ha_piano) sprintf("%d su %d schedati", a$schede, a$attese) else sprintf("%d schede", a$schede),
                  if (!is.na(a$mancano) && a$mancano > 0L) sprintf("mancano i n. %s", a$numeri),
                  if (a$incomplete > 0L) sprintf("%d da completare", a$incomplete))
      div(class = "home-busta",
        tags$b(class = "segnatura", etichetta_busta(a$busta, a$suffisso_b)),
        # Senza piano di busta la barra misura le schede complete su quelle schedate.
        div(class = "barra", `aria-hidden` = "true", div(class = "pieno", style = sprintf("width:%.0f%%",
          if (ha_piano) 100 * min(a$schede, a$attese) / a$attese else 100 * max(a$schede - a$incomplete, 0L) / max(a$schede, 1L)))),
        if (!is.na(a$prima_incompleta)) tags$a(href = "#", class = "azione small",
          onclick = sprintf("Shiny.setInputValue('home_apri_scheda', '%s', {priority:'event'}); return false;", gsub("[^A-Za-z0-9-]", "", a$prima_incompleta)),
          "Apri la prima da completare") else span(),
        span(class = "quanto", paste(quanto, collapse = " \u00b7 ")))
    }
    tagList(titolo_blocco("A che punto sono le buste", "I fascicoli schedati rispetto a quelli attesi dal piano di busta, e le schede ancora da completare."),
      div(class = "home-buste", lapply(mostra, riga),
          if (resto > 0L) p(class = "small text-muted mt-2", sprintf("E altre %d buste aperte: le vedi in Fascicoli.", resto))))
  })

  # Le tre azioni: importare e normalizzare, esportare, compilare le schede; ognuna dice in una riga a che punto è e porta a ciò che resta.
  output$home_porte <- renderUI({
    if (senza_fondi()) return(NULL)
    ricarica(); f <- fondo_attivo(); st <- stato_lavoro(); pr <- st$pronto
    # Tre box uguali: icona e titolo, una frase, una riga di stato (verde se a posto, ambra se c'è da fare) e i pulsanti.
    # Un solo pulsante pieno nella schermata: il «prossimo passo» nella fascia «Da guardare»; le tre porte hanno pulsanti a contorno.
    azione <- function(icona, titolo, testo, stato, ...) div(class = "home-porta",
      div(class = "testa", span(class = "ico", icon(icona)), h3(titolo)),
      p(testo), div(class = "stato", stato), div(class = "pulsanti", ...))
    fmt <- function(n) format(n, big.mark = ".", decimal.mark = ",")
    etichetta <- function(tipo, ...) span(class = paste("home-tag", tipo), ...)
    tagList(titolo_blocco("Operazioni"),
      div(class = "home-porte",
        azione("file-import", "Importa e normalizza",
          "Scegli un foglio Excel: Trama propone le correzioni, ti mostra il prima e il dopo e non scrive niente finché non confermi.",
          if (st$vd > 0) tagList(etichetta("att", tags$b(fmt(st$vd)), span(class = "ms-1", "valori da decidere")), actionLink("home_porta_colonna", "Decidi"))
          else etichetta("ok", icon("check"), span("nessun valore da decidere")),
          actionButton("home_porta_importa", tagList(icon("file-import"), "Importa un foglio"), class = "btn-outline-primary"),
          actionButton("home_porta_pulisci", tagList(icon("broom"), "Pulisci e scarica"), class = "btn-link btn-sm")),
        azione("file-export", "Esporta",
          "Il foglio Excel normalizzato nel modello a 28 colonne.",
          if (is.null(pr)) span(class = "text-muted", "—")
          else tagList(etichetta(if (pr$pronto) "ok" else "att", tags$b(sprintf("%d su %d", pr$complete, pr$standard)), span(class = "ms-1", "colonne standard complete")),
                       if (!pr$pronto) actionLink("home_porta_pronto", "Vedi quali mancano")),
          actionButton("home_vai_export", tagList(icon("file-export"), "Esporta"), class = "btn-outline-secondary")),
        azione("pen", "Schedatura",
          "Per scrivere o correggere un fascicolo alla volta. Non serve per i dati già in Excel.",
          # Le incomplete le dice già la fascia in alto quando sono il prossimo passo: qui non si ripetono.
          if (st$inc > 0) { if (!identical(st$caso, "completare")) tagList(etichetta("att", tags$b(fmt(st$inc)), span(class = "ms-1", "schede da completare")), actionLink("home_porta_incompleti", "Apri l'elenco")) }
          else etichetta("ok", icon("check"), span("nessuna scheda da completare")),
          if (!is.null(dove <- prossimo_da_schedare(con, f)))
            actionButton("home_continua", tagList(icon("forward"), sprintf("Continua: %s", posizione_scritta(dove$sigla_busta, dove$busta, dove$sigla_fascicolo, dove$fascicolo, "", dove$suffisso_b))), class = "btn-outline-primary")
          else if (piano_concluso(con, f)) tagList(etichetta("ok", icon("check"), span("Piano completato")),
            actionButton("home_fuori_piano", tagList(icon("forward"), "Aggiungi fuori piano"), class = "btn-outline-secondary")),
          # Un'azione sola in primo piano per box; le altre in secondo piano, come collegamenti (23/11/2026).
          actionButton("home_porta_nuova", tagList(icon("plus"), "Nuovo fascicolo"), class = "btn-link btn-sm"),
          actionButton("home_vai_fascicoli", tagList(icon("table-list"), "Vedi i fascicoli"), class = "btn-link btn-sm"))))
  })

  # Il resto dell'app, in piccolo: persone e luoghi, i fogli Excel, gli strumenti.
  output$home_fasi <- renderUI({
    if (senza_fondi()) return(NULL)
    d <- dati(); q <- qual()
    luoghi <- length(unique(unlist(strsplit(as.character(stats::na.omit(d$luoghi)), " | ", fixed = TRUE))))
    strumento <- function(titolo, testo, pulsante, id) div(class = "home-fase", h3(titolo), p(testo),
      actionButton(id, pulsante, class = "btn-sm btn-outline-secondary"))
    tagList(tags$details(class = "home-fasi-box", tags$summary("Altre funzioni"),
    div(class = "home-fasi",
      strumento("Luoghi", sprintf("%d luoghi citati.", luoghi), "Luoghi", "home_vai_luoghi"),
      strumento("Qualità", "Anomalie, grafie divergenti e colonne ancora vuote.", "Qualità", "home_vai_qualita"),
      strumento("Gestione e sicurezza", "Fondi, ordine di consultazione, copie e ripristino.",
           "Gestione", "home_vai_gestione"))),
      # Il piede (23/11/2026) tiene solo ciò che altrove non c'è: la copia e «Come si usa» stanno già nella barra e nel menu, il percorso in Gestione.
      div(class = "home-piede",
        actionButton("home_apri_cartella", tagList(icon("folder-open"), "Apri la cartella dei dati"), class = "btn-sm btn-outline-secondary")))
  })

  # «Fondo attivo»: i fondi in cima, il fondo in uso è la prima riga («In uso» e spunta, non solo colore). «Ultima scheda modificata» è la data più recente di `modificato` nel fondo
  # (un import la riscrive: non dice quando si è lavorato). Con un fondo solo, una frase invece della tabella.
  output$home_archivi <- renderUI({
    ricarica()
    g <- elenco_fondi(con)
    if (!nrow(g)) return(NULL)   # la Home vuota ha il suo invito (home_inizio)
    att <- fondo_attivo()
    ult <- dbGetQuery(con, "SELECT fondo_id, MAX(modificato) AS m FROM fascicoli WHERE fondo_id IS NOT NULL GROUP BY fondo_id")
    g$m <- ult$m[match(g$id, ult$fondo_id)]
    g$ente <- vapply(g$id, function(f) { e <- ente_conservatore_del_fondo(con, f); if (is.null(e)) "" else e }, "")
    g$compl <- vapply(g$id, function(f) if (g$n[g$id == f] == 0) NA_integer_
      else sum(completezza_core(elenco_fascicoli(con, fondo = f)) == 4L), 1L)
    g <- g[order(!(g$id %in% att), -g$n == 0, is.na(g$m), -xtfrm(g$m)), , drop = FALSE]
    fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",")
    giorno <- function(m) if (is.na(m)) "—" else format(as.Date(substr(m, 1, 10)), "%d/%m/%Y")
    nuovo_b <- actionButton("home_nuovo_archivio", tagList(icon("plus"), "Nuovo fondo"), class = "btn-outline-primary btn-sm")
    avanza <- function(i, classe = "barra") {
      if (is.na(g$compl[i])) return(span(class = "quanto", "—"))
      pc <- 100 * g$compl[i] / g$n[i]
      # Il testo accanto alla barra («21/24 · 88%») dice già tutto: la barra è decorazione.
      tagList(div(class = classe, `aria-hidden` = "true", div(class = "pieno", style = sprintf("width:%.1f%%", pc))),
              span(class = "quanto", sprintf("%s su %s complete · %.0f%%", fmt(g$compl[i]), fmt(g$n[i]), pc)))
    }
    nome_pieno <- function(i) tagList(if (!grepl("^fondo\\b", trimws(g$fondo[i]), ignore.case = TRUE)) "Fondo ", span(class = "no-trad", g$fondo[i]),
                                      if (nzchar(g$ente[i])) tagList(" · ", span(class = "no-trad", g$ente[i])))
    # Il fondo in uso in una riga: nome, avanzamento, ultima modifica e «Nuovo fondo».
    riga_attiva <- function(i) div(class = "home-uno riga",
      div(class = "chi", span(class = "etichetta-fondo", "Fondo attivo"), p(class = "ora", tags$b(nome_pieno(i)))),
      if (g$n[i] > 0) div(class = "prog", avanza(i)) else span(class = "quanto", "Nessuna scheda ancora."),
      if (g$n[i] > 0) span(class = "quanto ult", "Ultima scheda modificata: ", giorno(g$m[i])),
      nuovo_b)
    corpo <- if (nrow(g) == 1) {
      riga_attiva(1)
    } else {
      riga <- function(i) {
        in_uso <- identical(g$id[i], att)
        div(class = paste("home-fondo", if (in_uso) "attivo"), role = "row", `aria-current` = if (in_uso) "true",
          div(role = "cell", class = "nome", nome_pieno(i)),
          div(role = "cell", span(class = "et", "Ente conservatore"), span(class = "no-trad", g$ente[i])),
          div(role = "cell", span(class = "et", "Schede"), fmt(g$n[i])),
          div(role = "cell", span(class = "et", "Complete"), avanza(i)),
          div(role = "cell", span(class = "et", "Ultima scheda modificata"), giorno(g$m[i])),
          div(role = "cell",
            if (in_uso) span(class = "in-uso", icon("check"), "In uso")
            else tags$button(type = "button", class = "btn btn-sm btn-outline-primary",
              onclick = sprintf("Shiny.setInputValue('home_attiva_fondo', %s, {priority: 'event'})",
                                jsonlite::toJSON(g$id[i], auto_unbox = TRUE)),
              "Lavora su questo fondo", span(class = "visually-hidden", span(": "), span(nome_pieno(i))))))
      }
      testa <- div(class = "home-fondo capo", role = "row",
        div(role = "columnheader", "Fondo"), div(role = "columnheader", "Ente conservatore"), div(role = "columnheader", "Schede"),
        div(role = "columnheader", "Complete"), div(role = "columnheader", "Ultima scheda modificata"), div(role = "columnheader", ""))
      # Il fondo in uso in evidenza, come quando ce n'è uno solo; gli altri in una riga chiusa.
      i_att <- indice_fondo_attivo(att, g$id)
      altri <- setdiff(seq_len(nrow(g)), i_att)
      tagList(
        if (length(i_att) == 1L && !is.na(i_att)) riga_attiva(i_att)
        else titolo_blocco("Fondo attivo", "Gli altri fondi sono a un clic.", nuovo_b),
        if (length(altri)) tags$details(class = "home-altri-fondi mt-2", open = if (length(altri) <= 4L) NA,   # pochi fondi: si vedono tutti subito
          tags$summary(class = "py-2", sprintf("Cambia fondo (%d altri)", length(altri))),
          div(class = "home-fondi", role = "table", testa, lapply(altri, riga))))
    }
    div(class = "home-dove",
      corpo)
  })

  observeEvent(input$home_apri_cartella, tryCatch(apri_cartella(dirname(percorso_db())),
    error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL)))

  observeEvent(input$home_riprendi_ultima, {
    f <- fondo_attivo(); u <- ultimo_modificato(con, if (.vuoto(f)) NULL else f)
    if (!is.null(u)) apri(u$id)
  })
  observeEvent(input$home_incompleti, incompleti())
  observeEvent(input$home_apri_scheda, { id <- as.character(input$home_apri_scheda); if (length(id) == 1L && !is.na(id) && nzchar(id)) apri(id) })
  observeEvent(input$home_esame, cerca("esame:da-fare"))
  observeEvent(input$home_anomalie, vai("Qualità"))
  observeEvent(input$home_nuova, nuova())
  observeEvent(input$home_porta_nuova, nuova())
  observeEvent(input$home_fuori_piano, { f <- fondo_attivo(); if (!.vuoto(f) && !is.null(dove <- prossimo_fuori_piano(con, f))) continua(dove) })
  observeEvent(input$home_continua, { f <- fondo_attivo(); if (!.vuoto(f) && !is.null(dove <- prossimo_da_schedare(con, f))) continua(dove) })
  observeEvent(input$home_porta_importa, vai("Import / Export"))
  observeEvent(input$home_porta_pulisci, { vai("Import / Export"); updateRadioButtons(session, "modo_import", selected = "pulisci") })
  per_colonna <- function() { vai("Qualità"); nav_select("vista_qualita", "colonne") }
  observeEvent(input$home_porta_pronto, per_colonna())
  observeEvent(input$home_porta_colonna, per_colonna())
  observeEvent(input$home_porta_colonna_vai, per_colonna())
  observeEvent(input$home_pp_valori, per_colonna())
  observeEvent(input$home_pp_colonne, per_colonna())
  observeEvent(input$home_pp_importa, vai("Import / Export"))
  observeEvent(input$home_porta_incompleti, incompleti())
  observeEvent(input$home_porta_qualita, vai("Qualità"))
  observeEvent(input$home_porta_controlli, vai("Qualità"))
  observeEvent(input$home_vai_import, vai("Import / Export"))
  observeEvent(input$home_vai_export, vai("Import / Export"))
  observeEvent(input$home_vai_fascicoli, vai("Fascicoli"))
  observeEvent(input$home_vai_luoghi, vai("Luoghi citati"))
  observeEvent(input$home_vai_qualita, vai("Qualità"))
  observeEvent(input$home_vai_gestione, vai("Gestione"))
  observeEvent(input$home_attiva_fondo, attiva_fondo(input$home_attiva_fondo))

  observeEvent(input$home_nuovo_archivio, showModal(modalDialog(
    title = "Nuovo fondo", easyClose = TRUE,
    p(class = "small text-muted", "Il fondo nasce vuoto: le schede nuove ne erediteranno archivio, fondo e serie."),
    senza_suggerimenti(textInput("arch_archivio", "Archivio", placeholder = "es. Archivio di Stato di Modena", width = "100%")),
    senza_suggerimenti(textInput("arch_fondo", "Fondo", placeholder = "es. Inquisizione", width = "100%")),
    senza_suggerimenti(textInput("arch_serie", "Serie (facoltativa)", width = "100%")),
    senza_suggerimenti(textInput("arch_fonte", "Da dove vengono i dati (facoltativo)", placeholder = "una riga, es. Inventario dell'archivio, trascritto dal gruppo di lavoro", width = "100%")),
    footer = tagList(modalButton("Annulla"), actionButton("arch_crea", "Crea e lavora su questo", class = "btn-primary")))))

  observeEvent(input$home_esempio, {
    tryCatch({
      e <- carica_esempio(con)
      ricarica(ricarica() + 1)
      attiva_fondo(e$fondo)
      showNotification(sprintf("Fondo di esempio caricato: %d fascicoli inventati.", e$importati), type = "message", duration = 8)
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })

  observeEvent(input$arch_crea, {
    tryCatch({
      fondo <- crea_archivio(con, input$arch_archivio, input$arch_fondo, input$arch_serie, fonte = input$arch_fonte)
      removeModal()
      ricarica(ricarica() + 1)
      attiva_fondo(fondo)
    }, error = function(e) showNotification(conditionMessage(e), type = "error"))
  })
}
