# ─────────────────────────────────────────────────────────────
# app.R - trama
# ─────────────────────────────────────────────────────────────

# Senza i pacchetti l'app non parte: i lanciatori li controllano già, qui serve a chi parte da RStudio.
source("R/richiesti.R")
controlla_pacchetti()

library(shiny)
library(bslib)
library(DT)

source("R/percorsi.R")
source("R/profilo_dominio.R")  # il profilo del materiale: vocabolari, convenzioni, parametri (profili/<nome>/profilo.json)
source("R/registro.R")        # il registro (13/10): usato da schema.R, backup.R, gestione.R
source("R/schema.R")
source("R/fondi.R")           # l'id interno del fondo (19/10/2026)
source("R/compose.R")
source("R/estrazione.R")
source("R/crud.R")
source("R/profili.R")
source("R/i18n.R")
source("R/modulo.R")
source("R/arianna.R")
source("R/arianna_import.R")
source("R/arianna_export.R")
source("R/arianna_pulisci.R")
source("R/concordanza.R")     # i nomi della riga devono concordare (13/10)
source("R/analisi.R")
source("R/colori.R")           # i colori dei reati, per le pillole del Soggettario (20/10/2026)
source("R/colonne.R")
source("R/qualita.R")
source("R/qualita_server.R")   # il server della pagina Qualità (20/10/2026)
source("R/lista_luoghi.R")     # la lista dei luoghi ammessi, senza coordinate
source("R/luoghi.R")
source("R/tema.R")
source("R/backup_electron.R")
source("R/backup.R")
source("R/fusioni.R")         # la fusione delle grafie dei luoghi
source("R/attribuzione.R")
source("R/ricerca.R")
source("R/elenco.R")
source("R/luoghi_citati.R")   # la pagina «Luoghi citati»
source("R/cronologia.R")
source("R/ordinamento.R")
source("R/gestione.R")
source("R/errori_import.R")   # la finestra «Errori e discordanze» (13/10)
source("R/refusi_lessico.R")     # sospetti refusi nel testo libero, dal lessico del corpus (22/10/2026)
source("R/revisione_import.R")
source("R/revisione_vista.R")
source("R/anomalie.R")           # gli errori già nel database, segnalati in Fascicoli e in Home (13/10)
source("R/registro_modifiche.R")  # registro cella per cella fra foglio originale e pulito
source("R/avanzamento.R")        # avanzamento per busta, schede incomplete, «Continua» (22/10/2026)
source("R/home.R")
source("R/selezione.R")
source("R/coorti.R")            # le ricerche salvate (20/10/2026)
source("R/preferenze_server.R")  # tema chiaro/scuro e lingua
source("R/fondo_server.R")       # il fondo di lavoro
source("R/backup_server.R")       # la striscia del backup e le copie di sicurezza
source("R/licenza_server.R")      # licenza e attribuzione dei dati
source("R/ripristino_server.R")   # copie del database e ripristino
source("R/import_server.R")       # import, controllo prima di importare, anomalie
source("R/revisione_server.R")    # la finestra «Controlla prima di importare»
source("R/errori_import_server.R") # la finestra «Errori e discordanze»
source("R/anomalie_server.R")      # errori già nel database
source("R/export_server.R")       # scaricare le schede
source("R/sincronizzazioni_server.R")  # i campi della Scheda che si aggiornano mentre scrivi
source("R/estrazione_server.R")      # estrazione dal titolo
source("R/storia_server.R")           # storia della scheda
source("R/elenco_server.R")          # la scheda Fascicoli: ricerca, filtri, tabella
source("R/scheda_nuova_server.R")    # scheda nuova
source("R/piano_server.R")           # piano di busta
source("R/modulo_server.R")         # il modulo della Scheda
source("R/scheda_server.R")         # la Scheda mentre la scrivi
source("R/salvataggio_server.R")    # Salva, D30, Salva e vai
source("R/modifiche_server.R")       # modifiche non salvate e naviga
source("R/scheda_ui.R")              # pagina Scheda
source("R/importexport_ui.R")        # pagina Import / Export
source("R/qualita_ui.R")             # pagina Qualità
source("R/testata_ui.R")             # testata della pagina
source("R/pagine_server.R")         # collegamento di Gestione, Home, Colonne, Luoghi
source("R/aiuto_server.R")           # scorciatoie e cartella dei dati

options(shiny.maxRequestSize = 50 * 1024^2)   # fogli grandi (Modena)

# TRAMA_DB: copia del database per le prove, senza backup automatico.
# Database, backup e lista dei luoghi stanno nella cartella dell'utente (R/percorsi.R), non in quella dell'app.
PERCORSO_DB <- percorso_db()
# La guida HTML (un file solo, senza rete) si apre dalla Home: «Come si usa».
if (file.exists(file.path("pubblicazione", "guida", "guida-trama-lite.html"))) addResourcePath("guida", "pubblicazione/guida")
if (!nzchar(Sys.getenv("TRAMA_DB", ""))) dir.create(dirname(PERCORSO_DB), recursive = TRUE, showWarnings = FALSE)

registra("INFO", "avvio", "Trama Lite aperta")
con <- apri_db(PERCORSO_DB)
onStop(function() dbDisconnect(con))
SESSIONI_APERTE <- new.env(); SESSIONI_APERTE$n <- 0L
PROFILO_MODULO <- profilo_completo("Generale", dbGetQuery(con, "SELECT DISTINCT campo FROM campi_extra")$campo)

# Lista dei luoghi: la copia personale nasce da dati/luoghi.csv; le voci nuove della base si aggiungono, le tue restano.
prepara_cartella_dati()

# Una copia al giorno in backup/ (ultime 14); se fallisce l'app parte lo stesso.
BACKUP_OGGI <- if (!nzchar(Sys.getenv("TRAMA_DB", ""))) try(backup_automatico(con), silent = TRUE)
if (!nzchar(Sys.getenv("TRAMA_DB", ""))) try(ruota_copie_prima(), silent = TRUE)

BARRA <- paste("position: sticky; top: var(--rg-barra-h, 46px); z-index: 1020;",
               "background: var(--bs-body-bg); padding: .5rem 0;",
               "border-bottom: 1px solid var(--bs-border-color);")


ui <- page_navbar(
  id           = "nav",
  title        = MARCHIO_TRAMA,
  window_title = "Trama Lite",
  header       = testata_ui(),
  # La griglia delle coppie persona/reato e reato/luogo (R/modulo.R).
  footer = tagList(tags$script(HTML(JS_GRIGLIA_COPPIE)), tags$script(HTML(JS_PILLOLE)), tags$script(HTML(JS_TERMINI)), tags$script(HTML(JS_CRONOLOGIA)),
                   tags$script(HTML(JS_AUTOCORREZIONE)), tags$script(HTML(JS_TRASCINA))),
  theme        = tema_app("chiaro"),
  # Nessuna scheda riempie la finestra (fillable = FALSE): sono pagine che scorrono.
  fillable     = FALSE,

  home_ui(),
  fascicoli_ui(),

  scheda_ui(BARRA),

  luoghi_ui(),
  # Le voci di uso quotidiano sopra, quelle di servizio sotto (29/10/2026).
  nav_item(div(class = "nav-sep", role = "separator")),

  importexport_ui(PERCORSO_DB),

  qualita_ui(),

  gestione_ui(),
  # In fondo al menu: fondo attivo e backup (spostati da `sposta_contesto` quando la finestra è larga) e la guida.
  nav_spacer(),
  nav_item(div(class = "nav-contesto",
               if (file.exists(file.path("pubblicazione", "guida", "guida-trama-lite.html")))
                 tags$a(href = "guida/guida-trama-lite.html", target = "_blank", rel = "noopener", class = "nav-aiuto",
                        icon("book"), "Come si usa")))
)


server <- function(input, output, session) {
  # Lanciatore autonomo (TRAMA_AUTOSTOP=1): chiusa l'ultima scheda del browser l'app si ferma.
  if (nzchar(Sys.getenv("TRAMA_AUTOSTOP"))) {
    SESSIONI_APERTE$n <- SESSIONI_APERTE$n + 1L
    session$onSessionEnded(function() {
      SESSIONI_APERTE$n <- SESSIONI_APERTE$n - 1L
      later::later(function() if (SESSIONI_APERTE$n <= 0L) shiny::stopApp(), 8)
    })
  }

  ricarica <- reactiveVal(0)
  aperto   <- reactiveVal(NULL)
  fascicolo_caricato <- reactiveVal(NULL)
  # ── tema, lingua e fondo di lavoro (R/preferenze_server.R, R/fondo_server.R) ──
  lingua <- preferenze_server(input, session, con)
  fondo <- fondo_server(input, output, session, con, ricarica,
                        naviga = function(azione) naviga(azione),
                        al_cambio = function() { azzera_filtri_elenco(); aperto(NULL); nuovo(NULL) })
  fondo_attivo <- fondo$fondo_attivo; fondi_noti <- fondo$fondi_noti; attiva_fondo <- fondo$attiva_fondo


  dati <- reactive({
    ricarica()
    f <- fondo_attivo()
    elenco_fascicoli(con, fondo = if (.vuoto(f)) NULL else f)
  })
  
  # ── elenco Fascicoli, scheda nuova e piano di busta (R/elenco_server.R, R/scheda_nuova_server.R, R/piano_server.R) ──
  elenco <- elenco_server(input, output, session, con, ricarica, dati = function() dati(), fondo_attivo = fondo_attivo, aperto = aperto,
                          anomalie_stato = function() anomalie_stato(), naviga = function(azione) naviga(azione), nuovo = function() nuovo())
  filtro_schedatura <- elenco$filtro_schedatura; solo_incompleti <- elenco$solo_incompleti; solo_errori <- elenco$solo_errori
  busta_elenco <- elenco$busta_elenco; cerca <- elenco$cerca; azzera_filtri_elenco <- elenco$azzera_filtri_elenco
  righe_tabella <- elenco$righe_tabella; ordine_elenco <- elenco$ordine_elenco; apri_da_elenco <- elenco$apri_da_elenco
  righe_ricerca <- elenco$righe_ricerca
  piani_versione <- reactiveVal(0)    # Qualità si aggiorna senza ricostruire il modulo
  sn <- scheda_nuova_server(input, output, session, con, ricarica, fondo_attivo, aperto, naviga = function(azione) naviga(azione), piani_versione = piani_versione, PROFILO_MODULO = PROFILO_MODULO)
  nuovo <- sn$nuovo; richiesta <- sn$richiesta; apri_nuovo <- sn$apri_nuovo; contesto_attuale <- sn$contesto_attuale
  id_fondo_di <- sn$id_fondo_di; produttore_predefinito <- sn$produttore_predefinito
  piano_server(input, output, session, con, contesto_attuale, id_fondo_di, piani_versione)

  # ── il modulo della Scheda (R/modulo_server.R) ──
  modulo <- modulo_server(input, output, session, con, ricarica, fondo_attivo, aperto, nuovo, richiesta, lingua,
                          produttore_predefinito, fascicolo_caricato,
                          campi_modificati = function() campi_modificati(),
                          imputati_prima = function(...) imputati_prima(...), persone_prima = function(...) persone_prima(...),
                          .indicizzati = function(...) .indicizzati(...), PROFILO_MODULO = PROFILO_MODULO)
  fascicolo_corrente <- modulo$fascicolo_corrente
  valori_iniziali <- modulo$valori_iniziali
  suggerimenti <- modulo$suggerimenti
  caricati <- modulo$caricati
  invariati <- modulo$invariati
  caricato_per <- modulo$caricato_per
  numero_caricamento <- modulo$numero_caricamento
  modulo_pronto <- modulo$modulo_pronto
  dopo_salva <- modulo$dopo_salva

  # ── la Scheda mentre la scrivi (R/scheda_server.R) ──
  scheda <- scheda_server(input, output, session, con, ricarica, fondo_attivo, aperto, id_fondo_di, invariati,
                scheda_dal_modulo = function() scheda_dal_modulo(), PROFILO_MODULO = PROFILO_MODULO)
  proposta_anno <- scheda$proposta_anno

  # ── modifiche non salvate (R/modifiche_server.R) ──
  modifiche <- modifiche_server(input, output, session, con, ricarica, fondo_attivo, aperto, busta_elenco, filtro_schedatura,
                                cerca, solo_incompleti, solo_errori, numero_caricamento, valori_iniziali, PROFILO_MODULO = PROFILO_MODULO)
  campi_modificati <- modifiche$campi_modificati
  campi_modulo <- modifiche$campi_modulo; in_attesa <- modifiche$in_attesa
  fusione_consentita <- modifiche$fusione_consentita
  istantanea <- modifiche$istantanea
  naviga <- modifiche$naviga

  # ── sincronizzazioni in diretta (R/sincronizzazioni_server.R) ──
  sync <- sincronizzazioni_server(input, output, session, caricati, invariati)
  scheda_dal_modulo <- sync$scheda_dal_modulo; .indicizzati <- sync$.indicizzati
  imputati_prima <- sync$imputati_prima; persone_prima <- sync$persone_prima

  # ── estrazione dal titolo (R/estrazione_server.R) ──
  estrazione_server(input, output, session)

  # ── salvataggio della scheda (R/salvataggio_server.R) ──
  salvataggio <- salvataggio_server(input, output, session, con, ricarica, dati = function() dati(), aperto, richiesta, nuovo, istantanea,
                                    campi_modificati, modulo_pronto, fascicolo_caricato, filtro_schedatura, dopo_salva,
                                    ordine_elenco, righe_tabella, naviga = function(azione) naviga(azione), PROFILO_MODULO = PROFILO_MODULO)
  salva_scheda <- salvataggio$salva_scheda

  # ── storia della scheda (R/storia_server.R) ──
  storia_server(input, output, session, con, aperto, ricarica, richiesta, naviga = function(azione) naviga(azione))

  # ── coorti (R/coorti.R, 20/10/2026): le ricerche salvate dell'elenco Fascicoli ──
  coorti_versione <- reactiveVal(0)
  coorti_server(input, output, session, con, coorti_versione)

  # ── qualità (R/qualita_server.R, 20/10/2026) ──
  qualita_server(input, output, session, con, fondo_attivo = fondo_attivo, fondi_noti = fondi_noti, ricarica = ricarica,
                 piani_versione = piani_versione, filtro_schedatura = filtro_schedatura,
                 aperto = aperto, azzera_filtri_elenco = azzera_filtri_elenco, naviga = naviga, fusione_consentita = fusione_consentita)

  # ── import, export, backup, licenza e ripristino (R/import_server.R, R/export_server.R, …) ──
  import <- import_server(input, output, session, con, ricarica, fondo_attivo, attiva_fondo, solo_errori)
  anomalie_stato <- import$anomalie_stato
  lettura <- import$lettura; revisione_import <- import$revisione_import; elenco_errori <- import$elenco_errori   # i collaudi li leggono qui
  rev_selezione <- import$rev_selezione
  risultato_import <- import$risultato_import; risultato_pulito <- import$risultato_pulito
  esporta <- export_server(input, output, session, con, dati = function() dati(), fondo_attivo = fondo_attivo)
  fondo_da_esportare <- esporta$fondo_da_esportare
  backup_server(input, output, session, con, ricarica, backup_oggi = BACKUP_OGGI)
  licenza_server(input, output, session, con, fondo_attivo)
  ripristino_server(input, output, session, con, ricarica, fondo_attivo,
                    fondo_da_esportare = fondo_da_esportare, naviga = function(azione) naviga(azione),
                    .nome_file = esporta$.nome_file, registra_copia = esporta$registra_copia, .a_righe = import$.a_righe,
                    dopo_ripristino = function() {
                      aperto(NULL); nuovo(NULL); filtro_schedatura(NULL)
                      richiesta(richiesta() + 1); coorti_versione(coorti_versione() + 1); piani_versione(piani_versione() + 1)
                    })

  # ── pagine Gestione, Home, Colonne e Luoghi (R/pagine_server.R) e aiuto (R/aiuto_server.R) ──
  pagine_server(input, output, session, con,
                anomalie_stato = anomalie_stato, aperto = aperto, apri_da_elenco = apri_da_elenco, apri_nuovo = apri_nuovo,
                attiva_fondo = attiva_fondo, azzera_filtri_elenco = azzera_filtri_elenco, contesto_attuale = contesto_attuale,
                dati = dati, filtro_schedatura = filtro_schedatura, fondo_attivo = fondo_attivo, fusione_consentita = fusione_consentita,
                naviga = naviga, nuovo = nuovo, produttore_predefinito = produttore_predefinito, ricarica = ricarica,
                richiesta = richiesta, solo_errori = solo_errori, solo_incompleti = solo_incompleti)
  aiuto_server(input, output, session, PERCORSO_DB)
}

shinyApp(ui, server)
