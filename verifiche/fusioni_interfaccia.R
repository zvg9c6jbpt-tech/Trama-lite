# App reale su copia temporanea: nessuna scrittura nel database attivo.
source("verifiche/aiuti.R")
copia <- database_di_prova()
Sys.setenv(TRAMA_DB=copia)
shiny::testServer(".", {
  session$flushReact()
  proposta_anno() # Non nascondere errori negli output delle schede non visibili.
  istantanea(setNames(lapply(campi_modulo$id, function(k) input[[k]]), campi_modulo$id))
  session$setInputs(titolo="BOZZA DA PROTEGGERE")
  stopifnot(length(campi_modificati()) > 0, !fusione_consentita())
  prima <- ricarica()
  session$setInputs(persona_scelta="A", fondi_in_persona="B", fondi_persona_vai=1,
                    fondi_persone=1, persone_dop_applica=1, luoghi_dop_applica=1)
  stopifnot(identical(ricarica(), prima), input$titolo == "BOZZA DA PROTEGGERE")
  # Senza bozza fascicolo, la nota biografica impedisce ancora la fusione.
  istantanea(setNames(lapply(campi_modulo$id, function(k) input[[k]]), campi_modulo$id))
  session$setInputs(note_biografiche_bozze=list("A"))
  stopifnot(!fusione_consentita())
  session$setInputs(fondi_persona_vai=2, fondi_persona_conferma=1, persone_dop_applica=2)
  stopifnot(identical(ricarica(), prima))
  session$setInputs(note_biografiche_bozze=list())
  stopifnot(fusione_consentita())
  cat(nrow(dati()), "fascicoli: app avviata; fusioni bloccate con bozze, consentite senza.\n")
})
