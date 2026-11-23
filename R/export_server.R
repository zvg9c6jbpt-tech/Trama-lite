# ─────────────────────────────────────────────────────────────
# R/export_server.R: scaricare le schede
# L'export è sempre di UN fondo. Ogni scarico lascia una copia nella cartella dati (`registra_copia`).
# Restituisce list(.nome_file, registra_copia, fondo_da_esportare), usati anche dal ripristino.
# ─────────────────────────────────────────────────────────────

export_server <- function(input, output, session, con, dati, fondo_attivo) {
  output$info_export <- renderUI({
    n <- nrow(dati())
    tagList(
      p(class = "text-muted small mb-2", "Modello a 28 colonne, uguale per tutti gli archivi."),
      # Un solo pezzo di HTML: tra i figli di un tag htmltools mette a capo che diventano spazi.
      if (.vuoto(fondo_attivo())) p(class = "text-danger", "Scegli un fondo in alto: l'export è sempre di un fondo solo.")
      else {
        # Archivio e fondo, per sapere se è Napoli, Bologna, Capua (7/10).
        arch <- archivio_del_fondo(con, fondo_attivo())
        div(
          if (!is.null(arch)) p(class = "mb-1", HTML(sprintf("<span>Archivio</span>: <span class=\"no-trad\">%s</span>", htmltools::htmlEscape(arch)))),
          p(HTML(sprintf("<span>Fondo</span>: <span class=\"no-trad\">%s</span> \u00b7 <span>%s</span>",
                         htmltools::htmlEscape(nome_fondo(con, fondo_attivo())), if (n == 1) "1 fascicolo" else sprintf("%d fascicoli", n)))))
      }
    )
  })

  # Nome del file scaricato: col fondo dentro, solo per gli export di un fondo solo.
  # Preposizioni e articoli tolti, per un nome breve («tribunale-inquisizione-modena»).
  .pezzo_fondo <- function(nome) {
    parti <- strsplit(.slug(nome), "_", fixed = TRUE)[[1]]
    tenute <- parti[!parti %in% c("di", "del", "dello", "della", "dei", "degli",
                                  "delle", "dell", "da", "dal", "il", "lo", "la",
                                  "i", "gli", "le", "l", "e", "ed", "in", "a")]
    paste(if (length(tenute)) tenute else parti, collapse = "-")
  }

  .nome_file <- function(ext, che = NULL, per_fondo = TRUE) {
    fondo <- if (!per_fondo) NULL
             else if (.vuoto(fondo_attivo())) "tutti-i-fondi"
             else .pezzo_fondo(nome_fondo(con, fondo_attivo()))
    paste0(paste(c("trama", che, fondo, format(Sys.Date())), collapse = "-"), ".", ext)
  }

  # Una copia di ogni export in ~/Documents/Trama/export, col suo percorso mostrato (12/10).
  ultima_copia <- reactiveVal(NULL)
  registra_copia <- function(file, nome) {
    d <- salva_copia_export(file, nome, pezzo_fondo = if (.vuoto(fondo_attivo())) NULL else .pezzo_fondo(nome_fondo(con, fondo_attivo())))
    ultima_copia(if (is.null(d)) NULL else list(percorso = d, quando = Sys.time()))
    if (!is.null(d)) notifica_con_cartella("Copia salvata anche nella cartella dei dati.", "export")
    invisible(d)
  }
  output$copia_export <- renderUI({
    u <- ultima_copia(); if (is.null(u)) return(NULL)
    div(class = "alert alert-light py-2 small mb-3", icon("folder-open"), " ",
        span("Ultimo export: una copia è anche in questa cartella, se lo scarico non parte: "),
        span(class = "no-trad", dirname(u$percorso)), " · ", span(class = "no-trad", basename(u$percorso)))
  })

  # L'export è sempre di un solo fondo: senza un fondo scelto non esce niente.
  fondo_da_esportare <- function() {
    f <- fondo_attivo()
    if (.vuoto(f)) {
      showNotification("Scegli prima un fondo in alto: l'export è sempre di un fondo solo.", type = "error", duration = 8)
      stop("nessun fondo scelto")
    }
    f
  }

  consegna_verificata <- function(tmp, file, fondo, nome) {
    ok <- tryCatch(verifica_export(con, tmp, fondo), error = function(e) {
      showNotification(div(strong("Export non consegnato"), br(), conditionMessage(e), br(), "Nessun file è stato creato: riprova, e se si ripete guarda il registro."),
                       type = "error", duration = NULL)
      registra("ERRORE", "export non consegnato", conditionMessage(e))
      stop(e)
    })
    file.copy(tmp, file, overwrite = TRUE)
    registra_copia(file, nome)
  }

  output$scarica_json_schede <- downloadHandler(
    filename = function() nome_file_schede(con, fondo_da_esportare(), "json"),
    content = function(file) {
      esporta_json_schede(con, file, fondo = fondo_da_esportare())
      registra_copia(file, nome_file_schede(con, fondo_da_esportare(), "json"))
    }
  )

  output$scarica_xlsx <- downloadHandler(
    filename = function() nome_file_schede(con, fondo_da_esportare(), "xlsx"),
    content  = function(file) {
      tmp <- tempfile(fileext = ".xlsx")
      f <- fondo_da_esportare()
      esporta_arianna(con, tmp, fondo = f)
      consegna_verificata(tmp, file, f, nome_file_schede(con, f, "xlsx"))
    }
  )

  output$scarica_csv <- downloadHandler(
    filename = function() nome_file_schede(con, fondo_da_esportare(), "csv"),
    content  = function(file) {
      tmp <- tempfile(fileext = ".csv")
      f <- fondo_da_esportare()
      esporta_csv(con, tmp, fondo = f)
      consegna_verificata(tmp, file, f, nome_file_schede(con, f, "csv"))
    }
  )
  list(.nome_file = .nome_file, registra_copia = registra_copia, fondo_da_esportare = fondo_da_esportare)
}
