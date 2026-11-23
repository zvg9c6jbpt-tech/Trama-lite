# ─────────────────────────────────────────────────────────────
# ricomposizione_interfaccia.R: «Ricomponi titolo e regesto» sulle spuntate, dall'app vera
#
# Nato il 20/10: la ricomposizione in blocco, con «anche i manuali», poteva sostituire
# centinaia di testi d'inventario (su Modena, Bologna e Napoli: 71 titoli e 123 regesti
# scritti a mano) senza dirlo. Qui, con testServer sulla cartella e una copia
# temporanea del database, si prova la finestra: i protetti restano fuori di default;
# includerli mostra quanti sono; senza la conferma non si scrive; con la conferma
# si scrive e il testo sostituito sta nella copia di sicurezza.
#
#     Rscript verifiche/ricomposizione_interfaccia.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
source("verifiche/aiuti.R")
copia <- database_di_prova()
Sys.setenv(TRAMA_DB = copia)
Sys.setenv(TRAMA_HOME = tempfile("trama_ricomposizione_"))
grDevices::pdf(NULL)
falliti <- character(0); fatti <- 0L
atteso <- function(nome, ottenuto, voluto) {
  fatti <<- fatti + 1L
  if (!identical(ottenuto, voluto)) { falliti <<- c(falliti, nome); cat("NON TORNA:", nome, "\n   ottenuto:", format(ottenuto), "\n   voluto:  ", format(voluto), "\n") }
}
suppressWarnings(shiny::testServer(".", {
  session$flushReact()
  cn <- apri_db(copia); on.exit(DBI::dbDisconnect(cn), add = TRUE)   # connessione del collaudo (quella dell'app non è visibile qui)
  # Una scheda che il composer sa scrivere, con titolo e regesto d'inventario protetti.
  scelta <- NULL
  for (i in head(DBI::dbGetQuery(cn, "SELECT id FROM fascicoli")$id, 80)) {
    f <- leggi_fascicolo(cn, i)
    if (!.vuoto(componi_titolo_scheda(f, forza = TRUE)) && !.vuoto(componi_regesto_scheda(f, forza = TRUE))) { scelta <- i; break }
  }
  if (is.null(scelta)) stop("nessuna scheda adatta nel database di prova")
  DBI::dbExecute(cn, "UPDATE fascicoli SET titolo = 'Titolo d''inventario', titolo_manuale = 1,
                       regesto = 'Regesto d''inventario', regesto_manuale = 1 WHERE id = ?", list(scelta))
  leggi <- function() { g <- leggi_fascicolo(cn, scelta); c(g$titolo, g$regesto) }
  testi <- c("Titolo d'inventario", "Regesto d'inventario")

  session$setInputs(fascicoli_spuntati = scelta, sel_ricomponi = 1)
  html <- function() as.character(output$sel_ricomponi_anteprima$html)
  atteso("finestra: di default i testi scritti a mano non cambiano", grepl("Nessun fascicolo cambierebbe", html()), TRUE)
  session$setInputs(sel_ricomponi_applica = 1)
  atteso("finestra: applicare senza includerli non li tocca", leggi(), testi)

  session$setInputs(sel_anche_titoli = TRUE)
  atteso("finestra: includere i titoli dice che sostituirebbe un titolo e nessun regesto",
         grepl("Sostituirebbe testi scritti a mano: 1 titoli e 0 regesti", html(), fixed = TRUE), TRUE)
  session$setInputs(sel_ricomponi_applica = 2)
  atteso("finestra: senza conferma non si scrive", leggi(), testi)

  session$setInputs(sel_conferma_manuali = TRUE)
  session$setInputs(sel_ricomponi_applica = 3)
  g <- leggi()
  atteso("finestra: con la conferma si sostituisce il solo titolo", c(g[1] != testi[1], g[2] == testi[2]), c(TRUE, TRUE))
}))
if (length(falliti)) { cat("\nverifiche:", fatti - length(falliti), "passate,", length(falliti), "fallite\nESITO: NON TORNA\n"); quit(status = 1) }
cat(sprintf("verifiche: %d passate, 0 fallite\nESITO: ok - ricomposizione in blocco: protezioni e conferma\n", fatti))
