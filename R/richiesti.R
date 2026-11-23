# ─────────────────────────────────────────────────────────────
# richiesti.R: i pacchetti che servono, e che cosa dire se mancano
# Su un computer nuovo R c'è ma i pacchetti no, e l'app moriva con «there is no package called 'bslib'»: qui si guarda prima di partire
# e si dice che cosa manca, a che serve e come installarlo.
# Non carica niente e non dipende da niente (solo requireNamespace, così funziona anche se manca shiny): va letto PRIMA di tutti gli altri.
# ─────────────────────────────────────────────────────────────

PACCHETTI_RICHIESTI <- c(
  shiny    = "l'applicazione",
  bslib    = "l'aspetto, le carte e il tema scuro",
  DT       = "le tabelle",
  DBI      = "il dialogo col database",
  RSQLite  = "il database",
  jsonlite = "il backup e l'export JSON",
  readxl   = "la lettura dei fogli Arianna (.xlsx)",
  openxlsx = "la scrittura dei fogli Arianna (.xlsx)",
  uuid     = "gli identificatori dei fascicoli"
)

# Oltre ai richiesti, il programma chiama questi pacchetti (lanciatori, controllo delle schede in un processo a parte, backup in zip, normalizzazione Unicode, rinvii): i costruttori Mac e Windows li aggiungono ai richiesti per il pacchetto.
PACCHETTI_DI_SERVIZIO <- c("httpuv", "later", "zip", "stringi", "callr")


pacchetti_mancanti <- function() {
  nomi <- names(PACCHETTI_RICHIESTI)
  nomi[!vapply(nomi, requireNamespace, logical(1), quietly = TRUE)]
}


# Ferma l'avvio con un messaggio leggibile. Si chiama dal lanciatore (avvia.command / avvia.bat) PRIMA di shiny::runApp, che senza shiny fallirebbe male; app.R lo richiama per chi parte da RStudio.
controlla_pacchetti <- function() {
  m <- pacchetti_mancanti()
  if (!length(m)) return(invisible(TRUE))

  riga <- strrep("─", 66)
  message("\n", riga)
  message("Trama Lite non può partire: manca", if (length(m) == 1) "" else "no",
          " ", length(m), " pacchett", if (length(m) == 1) "o" else "i", " di R.\n")
  for (p in m) message("   ", format(p, width = 10), " ", PACCHETTI_RICHIESTI[[p]])
  message("\nPer installarl", if (length(m) == 1) "o" else "i",
          " apri R e incolla questa riga (serve internet,")
  message("una volta sola; poi Trama Lite funziona anche scollegato):\n")
  elenco <- paste0("\"", m, "\"", collapse = ", ")
  message("   install.packages(", if (length(m) == 1) elenco else paste0("c(", elenco, ")"), ")\n")
  message(riga, "\n")

  stop("pacchetti mancanti: ", paste(m, collapse = ", "), call. = FALSE)
}
