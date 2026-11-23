# Navigazione su copia: ricerca, busta, incompleti e azzeramento dei filtri.
source("verifiche/aiuti.R")
copia <- database_di_prova()
Sys.setenv(TRAMA_DB=copia)
shiny::testServer(".", {
  session$flushReact()
  totale <- nrow(dati())
  b <- dati()$busta[which(!is.na(dati()$busta))[1]]
  session$setInputs(cerca_fascicoli=paste0("busta:", b))
  stopifnot(nrow(righe_tabella()) > 0, all(righe_tabella()$busta == b))
  session$setInputs(solo_incompleti=1, scaffale_busta=b)
  stopifnot(identical(busta_elenco(), b), solo_incompleti())
  session$setInputs(vista_elenco="scaffale")
  stopifnot(is.null(busta_elenco()), cerca() == paste0("busta:", b))
  filtro_schedatura(list(etichetta="Prova", ids=character()))
  session$setInputs(azzera_filtri_elenco=1)
  stopifnot(is.null(busta_elenco()), is.null(filtro_schedatura()),
            !solo_incompleti(), cerca() == "", nrow(righe_tabella()) == totale)
  session$setInputs(scaffale_busta=b, vista_elenco="tabella")
  stopifnot(all(righe_tabella()$busta == b), nrow(righe_ricerca()) == totale)
  session$setInputs(busta_elenco_via=1)
  stopifnot(nrow(righe_tabella()) == totale)
  cat("ESITO: ok - ricerca, buste, azzeramento dei filtri\n")
})
