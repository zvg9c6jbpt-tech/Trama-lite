# ─────────────────────────────────────────────────────────────
# R/backup_server.R: la striscia del backup e le copie di sicurezza
# `backup_oggi`: l'esito della copia di avvio (BACKUP_OGGI di app.R). La striscia in alto (ultima copia, «Copia ora») e le copie periodiche. L'ultima copia è il file più recente nella cartella backup accanto al database in uso.
# ─────────────────────────────────────────────────────────────

backup_server <- function(input, output, session, con, ricarica, backup_oggi) {
  cartella_copie <- function() file.path(dirname(percorso_db()), "backup")
  backup_versione <- reactiveVal(0)
  # L'ultimo tentativo non riuscito: la striscia lo dice in rosso finché una copia non va a buon fine.
  backup_errore <- reactiveVal({
    e <- if (inherits(backup_oggi, "try-error")) .spiega_errore_copia(conditionMessage(attr(backup_oggi, "condition"))) else backup_oggi$errore
    if (is.null(e)) NULL else e
  })
  output$banner_backup <- renderUI({
    backup_versione(); ricarica(); invalidateLater(5 * 60 * 1000)
    s <- stato_backup(elenco_copie(cartella_copie()), errore = backup_errore())
    prova <- nzchar(Sys.getenv("TRAMA_DB", ""))
    tagList(span(class = paste("backup-stato", if (s$livello != "ok" && !(prova && s$livello == "nessuna")) "backup-vecchio"), title = s$dettaglio,
                 icon(if (s$livello == "ok" || (prova && s$livello == "nessuna")) "shield-halved" else "triangle-exclamation"),
                 span(if (prova) "Copia di prova \u00b7" else if (s$livello == "ok") "Backup automatico attivo \u00b7" else "Attenzione \u00b7"),
                 lapply(s$parti, function(x) span(x))),
            actionButton("backup_ora", if (s$livello == "errore") "Riprova" else "Copia ora", class = "btn-pillola btn-sm",
                         title = "Fa subito una copia di sicurezza del database"))
  })
  # Copie periodiche: ogni 10 minuti, se il database è cambiato e sono passati 30 minuti dall'ultima (backup_periodico).
  # Non con un database di prova (TRAMA_DB).
  observe({
    invalidateLater(10 * 60 * 1000)
    if (nzchar(Sys.getenv("TRAMA_DB", ""))) return()
    r <- tryCatch(backup_periodico(con, cartella_copie()), error = function(e) list(nuovo = FALSE, errore = .spiega_errore_copia(conditionMessage(e))))
    if (!is.null(r$errore)) { backup_errore(r$errore); registra("ERRORE", "copia periodica non riuscita", r$errore) }
    else if (isTRUE(r$nuovo)) { backup_errore(NULL); backup_versione(isolate(backup_versione()) + 1) }
  })
  observeEvent(input$backup_ora, tryCatch({
    copia <- backup_prima_ripristino(con, cartella_copie(), "manuale")
    backup_errore(NULL)
    backup_versione(backup_versione() + 1)
    notifica_con_cartella("Copia di sicurezza salvata.", "backup", duration = 6)
  }, error = function(e) {
    m <- .spiega_errore_copia(conditionMessage(e))
    backup_errore(m)
    registra("ERRORE", "copia manuale non riuscita", m)
    showNotification(paste("Copia non riuscita:", m), type = "error", duration = NULL)
  }))

  output$ultimo_backup <- renderUI({
    backup_versione()
    s <- stato_backup(elenco_copie(cartella_copie()), errore = backup_errore())
    # Senza copie la frase di sopra e il dettaglio dicono la stessa cosa: resta il dettaglio.
    if (s$livello == "nessuna") return(span(s$dettaglio))
    tagList(lapply(s$parti, function(x) tagList(span(x), " ")), " \u00b7 ", span(s$dettaglio))
  })
}
