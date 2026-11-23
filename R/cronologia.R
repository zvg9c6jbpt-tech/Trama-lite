# Il tasto Indietro.
# Trama è una pagina sola: ogni posto è un passaggio della cronologia del browser, così Indietro e Avanti (o il gesto sul trackpad) non portano fuori da Trama.
#   - un posto è una scheda della barra e, per la Scheda, il fascicolo aperto (`#Scheda/<id>`, `#Fascicoli`, `#Qualit%C3%A0`…);
#   - passare da un fascicolo all'altro con Precedente/Prossimo NON è un passo (si sostituisce la voce): Indietro torna all'elenco;
#   - Indietro, Avanti e il pulsante «Indietro» riportano al posto, con l'avviso «Modifiche non salvate» se serve; se si resta, la cronologia torna dov'era;
#   - un indirizzo con `#Scheda/<id>` si apre direttamente (segnalibri, messaggi).
# Cambia solo ciò che si vede e dove si va: nessun dato.
# Ricerca e filtri: a ogni cambiamento il server manda al browser una fotografia dei filtri di Fascicoli (ricerca, «incompleti», busta, vista, filtro da Qualità);
# il browser la tiene in `history.state` (non nell'indirizzo) e tornandoci la rimanda, e il server rimette i filtri com'erano.

POSTI_VALIDI <- c("Home", "Fascicoli", "Scheda", "Luoghi citati", "Import / Export", "Qualità", "Gestione")

# Il posto richiesto dal browser → dove andare davvero. Un fascicolo che non c'è più o è di un altro fondo, o una scheda inesistente: si va all'elenco, mai a un errore.
# NULL = ignora la richiesta. `v` (la fotografia dei filtri di quel passo) passa com'è, o NULL.
posto_da_richiesta <- function(x, esiste = function(id) FALSE) {
  nav <- as.character(unlist(x$nav))[1]
  id  <- as.character(unlist(x$id))[1]
  if (length(nav) == 0 || is.na(nav) || !nav %in% POSTI_VALIDI) return(NULL)
  if (is.na(id)) id <- ""
  v <- if (is.list(x$v) && length(x$v)) x$v else NULL
  posto <- function(nav, id) { p <- list(nav = nav, id = id); if (!is.null(v)) p$v <- v; p }
  if (!identical(nav, "Scheda")) return(posto(nav, ""))
  if (!nzchar(id)) return(posto("Scheda", ""))
  if (isTRUE(esiste(id))) posto("Scheda", id) else posto("Fascicoli", "")
}

JS_CRONOLOGIA <- "
(function () {
  var nav = null, iniziale = null, id = '', registrato = null, prima = null, timer = null, n = 0, viste = null, timerViste = null;

  function chiave(s) { return s.nav + '|' + (s.nav === 'Scheda' ? s.id : ''); }
  function indirizzo(s) {
    return '#' + encodeURIComponent(s.nav) + (s.nav === 'Scheda' && s.id ? '/' + encodeURIComponent(s.id) : '');
  }
  function daIndirizzo() {
    var h = (location.hash || '').replace(/^#/, ''); if (!h) return null;
    var p = h.split('/');
    try { return {nav: decodeURIComponent(p[0]), id: p[1] ? decodeURIComponent(p[1]) : ''}; } catch (e) { return null; }
  }
  function pulsante() {
    var vuoto = !registrato || registrato.d === 0;
    $('.btn-indietro').prop('disabled', vuoto);
  }

  // Si registra un posto solo dopo un attimo: aprire un fascicolo cambia sia la
  // scheda sia il fascicolo, in due messaggi, e deve contare una volta sola.
  function registra() {
    timer = null; prima = null;
    if (nav === null) return;
    var s = {nav: nav, id: nav === 'Scheda' ? id : '', d: registrato ? registrato.d : 0, v: viste};
    if (!registrato) { history.replaceState(s, '', indirizzo(s)); registrato = s; pulsante(); return; }
    if (chiave(s) === chiave(registrato)) return;
    if (s.nav === 'Scheda' && registrato.nav === 'Scheda') {   // Prossimo, Precedente: non è un passo
      history.replaceState(s, '', indirizzo(s)); registrato = s; return;
    }
    s.d = registrato.d + 1;
    history.pushState(s, '', indirizzo(s)); registrato = s; pulsante();
  }
  function programma() { if (timer) clearTimeout(timer); timer = setTimeout(registra, 80); }

  // La fotografia dei filtri: la tiene il passo in cui si è. Si riscrive al massimo ogni 400 ms
  // (i browser limitano le chiamate a history) e senza cambiare indirizzo.
  Shiny.addCustomMessageHandler('stato_viste', function (m) {
    viste = m;
    if (timerViste) return;
    timerViste = setTimeout(function () {
      timerViste = null;
      if (!registrato) return;
      registrato.v = viste;
      try { history.replaceState(registrato, '', indirizzo(registrato)); } catch (e) {}
    }, 400);
  });

  $(document).on('shiny:inputchanged', function (e) { if (e.name === 'nav') { nav = e.value; programma(); } });
  // La scheda con cui si parte: Shiny ne manda il valore prima che questo script lo ascolti,
  // e senza di essa Indietro non potrebbe mai tornare alla Home.
  (function () {
    var a = document.querySelector('.navbar .nav-link.active');
    nav = a && a.getAttribute('data-value') ? a.getAttribute('data-value') : 'Home';
    iniziale = nav;
    programma();
  })();
  Shiny.addCustomMessageHandler('posto_aperto', function (m) { id = m || ''; programma(); });

  // Indietro e Avanti del browser: si va al posto, e si avvisa il server, che
  // cambia scheda e apre il fascicolo (con l'avviso delle modifiche non salvate).
  window.addEventListener('popstate', function (e) {
    var s = e.state;
    if (!s || !s.nav) {      // un indirizzo scritto o incollato a mano (#Scheda/<id>): è un posto nuovo
      s = daIndirizzo(); if (!s || !s.nav) return;
      s.d = registrato ? registrato.d + 1 : 0;
      history.replaceState(s, '', indirizzo(s));
    }
    prima = registrato; registrato = s; pulsante();
    Shiny.setInputValue('indietro', {nav: s.nav, id: s.id || '', v: s.v || null, n: ++n}, {priority: 'event'});
  });
  // Si è scelto di restare dov'eravamo: la cronologia torna dov'era.
  Shiny.addCustomMessageHandler('posto_ripristina', function (m) {   // Shiny vuole un argomento
    if (!prima) return;
    history.pushState(prima, '', indirizzo(prima)); registrato = prima; prima = null; pulsante();
  });
  $(document).on('click', '.btn-indietro', function () { history.back(); });

  // Un indirizzo con #Scheda/<id>: lo si apre appena il server è pronto.
  $(document).one('shiny:connected', function () {
    var s = daIndirizzo();
    // Ricaricando la pagina il browser conserva history.state: anche i filtri tornano.
    var v = history.state && history.state.v ? history.state.v : null;
    // L'indirizzo vale solo se nessuno ha ancora toccato la barra: un clic su un'altra scheda fatto prima della connessione
    // è più recente dell'indirizzo. Se dice la scheda già aperta e non porta né un fascicolo né filtri, non c'è niente da applicare.
    // `avvio` e `da` servono al server: se quando la richiesta arriva la scheda non è più quella, non la applica.
    var a = document.querySelector('.navbar .nav-link.active'), attiva = a ? a.getAttribute('data-value') : null;
    if (attiva === iniziale && s && s.nav && (s.nav !== attiva || s.id || v))
      Shiny.setInputValue('indietro', {nav: s.nav, id: s.id, v: v, n: ++n, avvio: true, da: attiva}, {priority: 'event'});
  });
})();
"

# Il pulsante nella barra del contesto: spento finché non c'è un posto prima.
pulsante_indietro <- function()
  tags$button(type = "button", class = "btn btn-pillola btn-sm btn-indietro", disabled = NA,
              title = "Torna al punto di prima", icon("arrow-left"), "Indietro")

# La parte server. `aperto` (reactiveVal dell'id aperto), `naviga` (l'avviso delle
# modifiche non salvate), `esiste(id)` e `apri(id)` vengono da app.R.
cronologia_server <- function(input, session, aperto, naviga, esiste, caricato, ripristina = function(v) NULL) {
  da_indietro <- reactiveVal(FALSE)
  atteso <- reactiveVal(NULL)       # un indirizzo arrivato prima che la Scheda fosse pronta

  # Il server dice al browser qual è il fascicolo aperto (la cronologia lo scrive nell'indirizzo).
  observe(session$sendCustomMessage("posto_aperto", if (is.null(aperto())) "" else aperto()))

  vai <- function(p) {
    # Un indirizzo d'avvio che arriva dopo che l'utente ha già cambiato scheda è vecchio: non deve annullare il clic.
    if (isTRUE(p$avvio) && !identical(input$nav, p$da)) return()
    azione <- function() {
      if (!is.null(p$v)) ripristina(p$v)   # i filtri com'erano in quel passo
      if (identical(p$nav, "Scheda") && nzchar(p$id) && !identical(p$id, aperto())) aperto(p$id)
      nav_select("nav", p$nav, session = session)
    }
    cambia_fascicolo <- identical(p$nav, "Scheda") && nzchar(p$id) && !identical(p$id, aperto())
    if (cambia_fascicolo) { da_indietro(TRUE); naviga(azione) } else azione()
  }

  observeEvent(input$indietro, {
    p <- posto_da_richiesta(input$indietro, esiste)
    if (is.null(p)) return()
    if (isTRUE(input$indietro$avvio)) { p$avvio <- TRUE; p$da <- as.character(unlist(input$indietro$da))[1] }
    if (is.null(caricato())) { atteso(p); return() }   # il primo disegno della Scheda non è finito
    vai(p)
  })
  observeEvent(caricato(), {
    p <- atteso(); if (is.null(p)) return()
    atteso(NULL); vai(p)
  })

  # Dopo l'avviso: se si scarta si va (lo fa app.R), se si resta la cronologia torna dov'era.
  list(
    scarta = function() da_indietro(FALSE),
    resta  = function() { if (isTRUE(da_indietro())) session$sendCustomMessage("posto_ripristina", TRUE); da_indietro(FALSE) })
}
