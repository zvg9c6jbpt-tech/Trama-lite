# ─────────────────────────────────────────────────────────────
# R/testata_ui.R: la testata della pagina: stile dei caratteri, script di servizio e barra del contesto
# Rietichetta sul posto le opzioni di un selectize (cambio lingua): il valore resta, cambia il testo.
# ─────────────────────────────────────────────────────────────

# Il browser tiene in cache i file di www/: l'indirizzo porta la data del file, così uno nuovo ha un indirizzo nuovo.
versione_file <- function(nome) {
  p <- file.path("www", nome); m <- suppressWarnings(file.mtime(p))
  if (is.na(m)) nome else paste0(nome, "?v=", as.integer(m))
}

# Sotto la barra: fondo a sinistra, English e Scuro a destra.
barra_contesto <- function() div(class = "barra-contesto",
  # Indietro (5/10): torna al posto di prima; spento se non c'è. R/cronologia.R.
  pulsante_indietro(),
  div(class = "contesto-fondo",
      tags$label(icon("box-archive"), "Fondo"),
      # Il fondo si sceglie in una finestra; `fondo_attivo`, nascosto, resta l'input che tutta l'app legge.
      actionLink("fondo_apri", class = "fondo-apri", title = "Cambia fondo",
                 label = tagList(uiOutput("fondo_etichetta", inline = TRUE), icon("chevron-down"))),
      div(class = "fondo-nascosto", selectizeInput("fondo_attivo", NULL, choices = NULL, width = "auto"))),
  # L'archivio del fondo, in chiaro accanto al nome (7/10): lo stesso nome di fondo c'è in più archivi.
  uiOutput("archivio_attivo", inline = TRUE, class = "contesto-archivio"),
  # Striscia del backup: ultima copia e «Salva ora».
  uiOutput("banner_backup", class = "contesto-backup"),
  # Tasti, English e Scuro in un solo menu «Preferenze»; gli input restano nella pagina.
  div(class = "contesto-interruttori dropdown",
      tags$button(type = "button", class = "btn-pillola btn-sm dropdown-toggle", id = "menu_preferenze",
                  `data-bs-toggle` = "dropdown", `data-bs-auto-close` = "outside", `aria-expanded` = "false",
                  icon("sliders"), "Preferenze"),
      div(class = "dropdown-menu dropdown-menu-end preferenze-voci", `aria-labelledby` = "menu_preferenze",
          actionButton("aiuto_tasti", tagList(icon("keyboard"), "Tasti"), class = "btn-pillola btn-sm", title = "Scorciatoie da tastiera e gesti"),
          input_switch("inglese", tagList(icon("language"), "English"), value = FALSE),
          input_switch("scuro", tagList(icon("moon"), "Scuro"), value = FALSE),
          if (file.exists(file.path("pubblicazione", "guida", "guida-trama-lite.html")))
            tags$a(href = "guida/guida-trama-lite.html", target = "_blank", rel = "noopener", class = "btn-pillola btn-sm voce-guida",
                   icon("book"), "Come si usa"))))

testata_ui <- function() tagList(tags$link(rel = "stylesheet", href = "font-trama.css"), tags$script(HTML(sprintf("window.TRAMA_VERSIONE='%s';", as.integer(suppressWarnings(file.mtime("www/trama-en.json")))))), tags$script(src = versione_file("traduci.js")),tags$script(HTML("
    Shiny.addCustomMessageHandler('rietichetta', function(m) {
      var el = document.getElementById(m.id);
      if (!el || !el.selectize) return;
      var s = el.selectize;
      Object.keys(m.etichette).forEach(function(val) {
        var dato = {value: val, label: m.etichette[val]};
        if (s.options[val]) s.updateOption(val, dato); else s.addOption(dato);
      });
      s.refreshOptions(false);
    });
    // Le voci dei menu grandi (persone, imputati, luoghi) arrivano una
    // volta sola e poi solo per differenza - vedi .manda_voci() in
    // R/modulo.R. Si aggiungono al menu esistente: non lo si ricrea e
    // non si tocca ciò che è selezionato. Niente refreshOptions: la
    // tendina si ridisegna da sé quando si apre o si digita, e
    // ridisegnarla qui a ogni fascicolo sarebbe il lavoro che stiamo
    // togliendo. Al primo caricamento il modulo non c'è ancora: le voci
    // aspettano in un cassetto e si applicano quando il campo si
    // collega ('shiny:bound'), prima che Shiny gli consegni il valore.
    var vociInAttesa = {};
    // La barra di salvataggio della Scheda resta fissa in basso (26/10/2026): un campo che si sta usando e che sta in parte sotto di lei
    // (un regesto lungo, le note) viene portato sopra la barra; il browser non lo fa, perché lo considera già in vista.
    // Col mouse no: lo spostamento partirebbe tra il clic e il rilascio e il testo scapperebbe da sotto il puntatore mentre si seleziona.
    var ultimoClic = 0;
    document.addEventListener('pointerdown', function() { ultimoClic = Date.now(); }, true);
    document.addEventListener('focusin', function(e) {
      var bar = document.querySelector('.barra-fondo-scheda'), testa = document.querySelector('.testa-scheda');
      if (!bar || !e.target.closest || !e.target.closest('.carta-scheda') || e.target.closest('.barra-fondo-scheda')) return;
      if (Date.now() - ultimoClic < 500) return;
      requestAnimationFrame(function() {
        var r = e.target.getBoundingClientRect(), b = bar.getBoundingClientRect();
        if (b.top >= window.innerHeight || r.bottom <= b.top || r.top >= b.bottom) return;
        var alto = (testa ? testa.getBoundingClientRect().bottom : 0) + 12;   // l'altezza vera della testata, che a schermo stretto va su più righe
        window.scrollBy(0, Math.max(0, Math.min(r.bottom - b.top + 10, r.top - alto)));   // mai tanto da portare l'inizio del campo sotto la testata
      });
    });
    function metti_voci(id, voci) {
      var el = document.getElementById(id);
      if (!el || !el.selectize) {
        vociInAttesa[id] = (vociInAttesa[id] || []).concat(voci);
        return;
      }
      var s = el.selectize, nuove = [];
      for (var i = 0; i < voci.length; i++)
        if (!s.options[voci[i]]) nuove.push({value: voci[i], label: voci[i]});
      if (nuove.length) s.addOption(nuove);
    }
    // Cambiando fondo i menu dei nomi si svuotano: ogni archivio va da solo, e le persone, i luoghi e gli imputati
    // di un altro archivio non devono restare fra i suggerimenti (14/10, Stefano). Poi arrivano solo le voci del fondo.
    $(document).on('hidden.bs.modal', '#shiny-modal', function () { Shiny.setInputValue('finestra_chiusa', Math.random(), {priority: 'event'}); });
    Shiny.addCustomMessageHandler('voci_azzera', function(m) {
      [].concat(m.ids).forEach(function(id) {
        delete vociInAttesa[id];
        var el = document.getElementById(id);
        if (el && el.selectize) { el.selectize.clear(true); el.selectize.clearOptions(); }
      });
    });
    Shiny.addCustomMessageHandler('voci_menu', function(m) {
      metti_voci(m.id, [].concat(m.voci));
    });
    $(document).on('shiny:bound', function(e) {
      var id = e.target && e.target.id;
      if (!id || !vociInAttesa[id]) return;
      var voci = vociInAttesa[id];
      delete vociInAttesa[id];
      metti_voci(id, voci);
    });
    // Il marchio «Trama» è la Home (5/10): un clic apre la scheda Home, che nella barra non
    // si vede (tema.R). Si illumina quando si è sulla Home. Non si segue il suo href=\"#\":
    // cambierebbe l'indirizzo e la cronologia (R/cronologia.R).
    $(document).on('click', '.navbar-brand', function (e) {
      e.preventDefault();
      var h = document.querySelector('.navbar .nav-link[data-value=\"Home\"]');
      if (h) h.click();
    });
    $(document).on('shown.bs.tab', 'a[data-bs-toggle=tab]', function (e) {
      if (e.target.closest('.navbar-nav') || e.target.closest('.navbar'))
        $('.navbar-brand').toggleClass('attivo', e.target.getAttribute('data-value') === 'Home');
    });
    $(function () { $('.navbar-brand').addClass('attivo').attr({role: 'link', 'aria-label': 'Home'}); });
    // Sul telefono le voci scorrono di lato: la voce attiva si porta in vista, e finché c'è altro da scorrere
    // il bordo destro sfuma (classe `scorre-ancora`, tema/07-fascicoli.css), così non sembra che le voci finiscano lì.
    (function () {
      function barra() { return document.querySelector('.navbar .navbar-nav'); }
      function aggiorna() {
        var n = barra(); if (!n) return;
        n.classList.toggle('scorre-ancora', n.scrollWidth - n.clientWidth - n.scrollLeft > 4);
      }
      function inVista() {
        var n = barra(), a = n && n.querySelector('.nav-link.active'); if (!a) return;
        var r = a.getBoundingClientRect(), c = n.getBoundingClientRect();
        if (r.left < c.left || r.right > c.right) n.scrollLeft += (r.left < c.left ? r.left - c.left : r.right - c.right);
        aggiorna();
      }
      $(document).on('shown.bs.tab', 'a[data-bs-toggle=tab]', function (e) {
        if (e.target.closest('.navbar')) inVista();
      });
      $(window).on('resize', aggiorna);
      $(function () { var n = barra(); if (n) n.addEventListener('scroll', aggiorna, {passive: true}); setTimeout(inVista, 300); });
    })();
    // Il contenuto delle pagine è la zona principale: i lettori di schermo vi saltano dal menu.
    $(function () { $('.tab-content').first().attr('role', 'main'); });
    // Finestra stretta: la barra in alto diventa un menu a tendina, che
    // Bootstrap lascia aperto dopo aver scelto la scheda, e copre mezza
    // pagina. Si chiude da solo quando si sceglie una scheda; non quando
    // si tocca English o Scuro, che si possono voler cambiare insieme.
    $(document).on('click', '.navbar-collapse.show [data-bs-toggle=tab]', function() {
      var m = document.querySelector('.navbar-collapse.show');
      if (m && window.bootstrap) bootstrap.Collapse.getOrCreateInstance(m).hide();
    });
    // Conferma che i valori di un caricamento sono arrivati: parte dopo
    // gli aggiornamenti dei campi dello stesso messaggio, quindi quando
    // il server la riceve ha già in mano i valori nuovi (vedi Salva).
    // Porta in vista un riquadro (dopo il disegno: l'esito appare con lo stesso messaggio).
    Shiny.addCustomMessageHandler('scorri_a', function(id) {
      setTimeout(function() {
        var e = document.getElementById(id);
        if (e) e.scrollIntoView({behavior: 'smooth', block: 'center'});
      }, 150);
    });
    Shiny.addCustomMessageHandler('caricato', function(n) {
      Shiny.setInputValue('caricato', n, {priority: 'event'});
    });
    // Il primo caricamento (il disegno del modulo) lo conferma il browser.
    // Non con un tempo: Shiny inserisce un uiOutput in modo asincrono
    // (prima carica le librerie dei suoi campi), e un setTimeout scattava
    // a campi non ancora creati (provato il 18/09). Si aspetta che i campi
    // del modulo siano collegati ('shiny:bound') e che il server abbia
    // finito di elaborarne i valori ('shiny:idle'): solo allora.
    var moduloDisegnato = false;
    $(document).on('shiny:value', function(e) {
      if (e.name !== 'modulo') return;
      // Il modulo si disegna UNA volta sola. Un secondo disegno non lo chiede mai
      // il codice: lo provoca Shiny quando si cambia tema (Scuro/Chiaro), perché
      // un renderUI dipende dal tema. Il DOM nuovo avrebbe i campi a etichette
      // (persone, luoghi) senza voci né valori, mentre il server crede di avergliele
      // già mandate: la scheda si apriva con titolo, persone e luoghi vuoti e
      // «Modifiche da salvare» acceso, e Salva la scriveva così (trovato il 5/10).
      // Si scarta il disegno: il modulo che c'è resta com'è, con ciò che l'utente ha scritto.
      if (moduloDisegnato) { e.preventDefault(); return; }
      moduloDisegnato = true;
      var collegato = false;
      $(document).on('shiny:bound.primocaricamento', '#modulo', function() { collegato = true; });
      $(document).on('shiny:idle.primocaricamento', function() {
        if (!collegato) return;
        $(document).off('.primocaricamento');
        Shiny.setInputValue('caricato', 1, {priority: 'event'});
      });
    });
    // Modifiche non salvate: pallino sul pulsante Salva, e il browser
    // chiede conferma se si chiude o si ricarica la pagina.
    // Le due scritte di un input_task_button (pronto e occupato) dipendono dalla modalità dell'import. Nodi di testo nuovi: il traduttore li vede.
    Shiny.addCustomMessageHandler('etichetta_task', function(m) {
      var b = document.getElementById(m.id); if (!b) return;
      var pronto = b.querySelector('[slot=ready]'), occ = b.querySelector('[slot=busy]');
      if (pronto) pronto.textContent = m.pronto;
      if (occ) {
        var n = occ.lastChild; while (n && !(n.nodeType === 3 && n.nodeValue.trim())) n = n.previousSibling;
        if (n) occ.removeChild(n);
        occ.appendChild(document.createTextNode(' ' + m.occupato));
      }
    });
    Shiny.addCustomMessageHandler('modifiche', function(m) {
      var presenti = !!(m && m.presenti), nuova = !!(m && m.nuova);
      window.tramaModifiche = presenti;
      $('.stato-salvataggio').each(function() {
        this.textContent = presenti ? 'Modifiche da salvare' : (nuova ? 'Scheda nuova, non ancora salvata' : 'Salvato');
        this.classList.toggle('da-salvare', presenti);
        this.classList.toggle('salvato', !presenti && !nuova);
      });
      $('.barra-fondo-scheda [data-pulito]').each(function() {
        var sporco = this.dataset.soloModifiche ? presenti : (presenti || nuova);
        var testo = this.getAttribute(sporco ? 'data-sporco' : 'data-pulito');
        if (this.textContent !== testo && !(document.documentElement.lang === 'en' && this.dataset.stato === String(sporco))) this.textContent = testo;
        this.dataset.stato = String(sporco);
      });
      ['salva'].forEach(function(id) {
        var b = document.getElementById(id);
        if (!b) return;
        b.classList.toggle('da-salvare', !!presenti);
        b.title = presenti ? 'Ci sono modifiche non salvate' : '';
      });
    });
    // «Manca: luogo» (completezza della scheda): porta al campo e lo attiva.
    window.tramaVai = function(id) {
      var el = document.getElementById(id);
      if (!el) return;
      (el.closest('.shiny-input-container') || el).scrollIntoView({block: 'center', behavior: 'smooth'});
      setTimeout(function() { if (el.selectize) el.selectize.focus(); else if (el.focus) el.focus(); }, 250);
    };
    window.addEventListener('beforeunload', function(e) {
      if (window.tramaModifiche) { e.preventDefault(); e.returnValue = ''; }
    });
  ")), barra_contesto())
