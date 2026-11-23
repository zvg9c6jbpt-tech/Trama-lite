# ─────────────────────────────────────────────────────────────
# R/scheda_nuova_server.R: scheda nuova: Nuovo, Fascicolo successivo, Nuova busta
# Collocazione e forma vengono dal modulo a schermo o, se vuoto, dall'ultimo fascicolo modificato.
# `piani_versione` è lo stato condiviso con il piano di busta. Restituisce nuovo, richiesta, apri_nuovo, contesto_attuale, id_fondo_di, produttore_predefinito.
# ─────────────────────────────────────────────────────────────

scheda_nuova_server <- function(input, output, session, con, ricarica, fondo_attivo, aperto, naviga, piani_versione, PROFILO_MODULO) {
  # Collocazione e forma vengono dal modulo a schermo o, se vuoto, dall'ultimo fascicolo modificato.
  nuovo     <- reactiveVal(NULL)
  richiesta <- reactiveVal(0)    # ogni pressione riapre il modulo, anche a valori uguali

  # Il modulo a schermo vale solo se è del fondo di lavoro (dopo un cambio può mostrare l'altro).
  # Id del fondo di un contesto (archivio + denominazione): "" se manca o non esiste.
  id_fondo_di <- function(ctx) {
    if (.vuoto(ctx$fondo)) return("")
    x <- fondo_id_da(con, ctx$archivio, ctx$fondo); if (is.na(x)) "" else x
  }
  contesto_attuale <- function() {
    v <- setNames(lapply(EREDITATI, function(k) input[[k]]), EREDITATI)
    f <- fondo_attivo()
    if (.vuoto(v$fondo) || (!.vuoto(f) && !identical(id_fondo_di(v), f))) {
      u <- ultimo_modificato(con, f)
      if (!is.null(u)) v <- valori_da_fascicolo(u, PROFILO_MODULO)[EREDITATI]
      else if (!is.null(a <- contesto_archivio(con, f))) v <- a   # fondo appena creato, ancora vuoto
    }
    v
  }

  # Soggetto produttore di partenza: quello dell'ultima scheda salvata nel fondo a schermo; un fondo nuovo parte vuoto.
  produttore_predefinito <- function() produttore_del_fondo(con, fondo_attivo())

  apri_nuovo <- function(valori) {
    aperto(NULL)
    nuovo(valori)
    richiesta(richiesta() + 1)
  }

  observeEvent(input$svuota, naviga(function() apri_nuovo(valori_nuovo(contesto_attuale(), produttore = produttore_predefinito()))))
  # «nuovo fascicolo» dalla scheda Fascicoli: lo stesso Nuovo, e si va alla scheda.
  observeEvent(input$nuovo_da_elenco, naviga(function() {
    apri_nuovo(valori_nuovo(contesto_attuale(), produttore = produttore_predefinito()))
    nav_select("nav", "Scheda")
  }))

  # Dove nascerebbe la scheda di «Fascicolo seguente» e «Nuova busta» (serve al pulsante e al clic).
  fascicolo_seguente <- function(ctx = contesto_attuale()) {
    busta <- input$busta; sufb <- input$busta_suffisso
    fid <- id_fondo_di(ctx)
    if (.vuoto(busta) && !.vuoto(fid)) { u <- ultimo_modificato(con, fid); busta <- u$busta; sufb <- u$busta_suffisso }
    if (.vuoto(fid) || .vuoto(busta)) return(NULL)
    sufb <- if (.vuoto(sufb)) "" else sufb
    # Se la busta ha un piano, il primo numero che vi manca; altrimenti max + 1.
    n <- prossimo_nel_piano(con, fid, busta, corrente = input$fascicolo, suffisso_b = sufb)
    if (is.na(n)) {
      a_schermo <- if (is.null(aperto())) input$fascicolo else NA
      n <- prossimo_numero(con, fid, busta, anche = a_schermo, suffisso_b = sufb)
    }
    list(busta = as.integer(busta), suffisso_b = sufb, fascicolo = n)
  }
  busta_seguente <- function(ctx = contesto_attuale()) {
    fid <- id_fondo_di(ctx)
    if (.vuoto(fid)) return(NULL)
    prossimo_numero(con, fid, anche = if (is.null(aperto())) input$busta else NA)
  }
  observe({
    ricarica(); piani_versione(); fondo_attivo()
    s <- tryCatch(fascicolo_seguente(), error = function(e) NULL)
    b <- tryCatch(busta_seguente(), error = function(e) NULL)
    updateActionButton(session, "successivo",  label = if (is.null(s) || .vuoto(s$fascicolo)) "Fascicolo seguente" else sprintf("Fascicolo %d", s$fascicolo))
    updateActionButton(session, "nuova_busta", label = if (.vuoto(b)) "Nuova busta"
                       else if (.vuoto(input$sigla_busta)) sprintf("Busta %d", as.integer(b))
                       else sprintf("%s %d", trimws(input$sigla_busta), as.integer(b)))
  })

  observeEvent(input$successivo, naviga(function() {
    ctx <- contesto_attuale()
    s <- fascicolo_seguente(ctx)
    if (is.null(s)) {
      showNotification("Per il fascicolo successivo servono fondo e busta.", type = "warning")
      return()
    }
    apri_nuovo(valori_nuovo(ctx, busta = s$busta, fascicolo = s$fascicolo,
                            produttore = produttore_predefinito(), busta_suffisso = s$suffisso_b))
  }))

  observeEvent(input$nuova_busta, naviga(function() {
    ctx <- contesto_attuale()
    b <- busta_seguente(ctx)
    if (is.null(b)) {
      showNotification("Per una nuova busta serve il fondo.", type = "warning")
      return()
    }
    apri_nuovo(valori_nuovo(ctx, busta = b, fascicolo = 1L, produttore = produttore_predefinito()))
  }))
  list(nuovo = nuovo, richiesta = richiesta, apri_nuovo = apri_nuovo, contesto_attuale = contesto_attuale,
       id_fondo_di = id_fondo_di, produttore_predefinito = produttore_predefinito)
}
