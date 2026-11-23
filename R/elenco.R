# ─────────────────────────────────────────────────────────────
# elenco.R: la scheda Fascicoli: tabella a tutta larghezza e scaffale
# Dipende da ricerca.R (completezza_core, CAMPI_RICERCA_AVANZATA)
# Una riga per fascicolo, su una riga sola; clic su una riga = apri la scheda. La ricerca è `filtra_avanzato` (R/ricerca.R).
# Selezione multipla (R/selezione.R): «Imposta un campo», «Ricomponi titolo e regesto» (anche i manuali, a richiesta) ed «Elimina».
# ─────────────────────────────────────────────────────────────

.esc <- function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x)))

# " | " di elenco_fascicoli() → separatore leggibile; vuoto → trattino.
.lista_leggibile <- function(x, sep) {
  x <- ifelse(is.na(x) | !nzchar(x), NA_character_, gsub(" | ", sep, x, fixed = TRUE))
  ifelse(is.na(x), "—", .esc(x))
}

# La segnatura dentro una busta (sotto la testa «b. 3»): «fasc. 7»; fuori dai gruppi serve intera.
.segn_breve <- function(segn) ifelse(grepl(", ", segn, fixed = TRUE), sub("^[^,]*, ", "", segn), segn)

# I reati come pillole nel loro colore (lo stesso di Scheda e grafici, R/colori.R; CSS in `.css_chip_reati`); il nome resta scritto.
.chip_reati <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || !nzchar(v)) return("\u2014")
    r <- trimws(strsplit(v, " | ", fixed = TRUE)[[1]]); r <- r[nzchar(r)]
    chip <- paste(sprintf('<span class="reato-chip" data-reato="%s">%s</span>', .esc(r), .esc(r)), collapse = " ")
    # Con le righe compatte resta il primo reato e «+N» per gli altri (il nome intero è nel title della cella).
    if (length(r) > 1) paste0(chip, sprintf(' <span class="reato-piu" aria-hidden="true">+%d</span>', length(r) - 1L)) else chip
  }, "", USE.NAMES = FALSE)
}

# Testo che non va a capo: se è troncato, il title lo mostra intero.
.cella <- function(testo_html, titolo = NULL) {
  if (is.null(titolo)) sprintf('<span class="cella-riga">%s</span>', testo_html)
  else sprintf('<span class="cella-riga" title="%s">%s</span>', titolo, testo_html)
}

.pillola_esito <- function(v) {
  v <- ifelse(is.na(v) | !nzchar(v), "n.d.", v)
  sprintf('<span class="pillola-esito esito-%s">%s</span>',
          ifelse(v == "sì", "si", ifelse(v == "no", "no", "nd")), .esc(v))
}

# La tabella di Fascicoli, già in HTML. Le colonne «k_» non si vedono: servono all'ordinamento (una segnatura si ordina per numero, non come testo).
tabella_elenco <- function(d) {
  n <- completezza_core(d)
  segn <- posizione_scritta(if ("sigla" %in% names(d)) d$sigla else NA, d$busta,
                            if ("sigla_f" %in% names(d)) d$sigla_f else NA, d$fascicolo, d$suffisso,
                            if ("suffisso_b" %in% names(d)) d$suffisso_b else "")
  nd <- if ("data_nd" %in% names(d)) !is.na(d$data_nd) & d$data_nd == 1L else rep(FALSE, nrow(d))
  anno <- ifelse(is.na(d$anno), ifelse(nd, "n.d.", "—"),
                 ifelse(!is.na(d$anno_a) & d$anno_a != d$anno,
                        paste0(d$anno, "–", d$anno_a), as.character(d$anno)))
  titolo <- ifelse(is.na(d$titolo) | !nzchar(d$titolo), NA_character_, d$titolo)
  data.frame(
    id        = d$id,
    Sel       = sprintf('<input type="checkbox" class="spunta-riga" data-id="%s" aria-label="Spunta %s">', .esc(d$id), .esc(segn)),
    k_segn    = ifelse(is.na(d$busta), 1e12, d$busta * 1e8 + .rango_suffisso(if ("suffisso_b" %in% names(d)) d$suffisso_b else "") * 1e5) +
                ifelse(is.na(d$fascicolo), 99999, d$fascicolo),
    # Lo stato si mostra solo dove manca qualcosa, accanto alla segnatura.
    Segnatura = .cella(paste0(sprintf('<span class="segn-piena">%s</span><span class="segn-breve">%s</span>', .esc(segn), .esc(.segn_breve(segn))),
                              ifelse(n >= 4L, "",
                                     sprintf(' <span class="stato-core" title="%d campi fondamentali su 4"><span class="stato-barra"><span style="width:%d%%"></span></span>%d/4</span>',
                                             n, n * 25L, n)))),
    k_stato   = n,
    Stato     = rep("", length(n)),   # colonna nascosta (app.R): resta per l'ordine e per i conti delle teste di busta
    # Le persone stanno sotto il titolo: una colonna in meno, il nome sta dove si legge il titolo.
    Titolo    = paste0(ifelse(is.na(titolo), '<span class="cella-riga text-muted">(senza titolo)</span>',
                              sprintf('<a href="#" class="link-apri"><span class="cella-riga cella-titolo" title="%s">%s</span></a>', .esc(titolo), .esc(titolo))),
                       ifelse(is.na(d$persone) | !nzchar(d$persone), "",
                              sprintf('<span class="cella-sub" title="%s">%s</span>', .esc(gsub(" | ", "; ", d$persone, fixed = TRUE)),
                                      .esc(gsub(" | ", " \u00b7 ", d$persone, fixed = TRUE)))),
                       # Sotto i 600 px le colonne Anno, Reato e Luoghi passano in una riga sotto il titolo (tema.R, `.cella-meta`).
                       sprintf('<span class="cella-meta">%s</span>',
                               paste0(anno, ifelse(is.na(d$reati) | !nzchar(d$reati), "", paste0(" \u00b7 ", .esc(gsub(" | ", ", ", d$reati, fixed = TRUE)))),
                                      ifelse(is.na(d$luoghi) | !nzchar(d$luoghi), "", paste0(" \u00b7 ", .esc(gsub(" | ", ", ", d$luoghi, fixed = TRUE))))))),
    k_anno    = ifelse(is.na(d$anno), 1e5, d$anno),
    Anno      = sprintf('<span class="cella-riga cella-anno">%s</span>', anno),   # l'intervallo («1561–1562») non va a capo, nemmeno in «Leggibile»
    Reato     = .cella(.chip_reati(d$reati), .lista_leggibile(d$reati, ", ")),
    Luoghi    = .cella(.lista_leggibile(d$luoghi, ", "), .lista_leggibile(d$luoghi, ", ")),
    Tortura   = .pillola_esito(d$tortura),
    Sentenza  = .pillola_esito(d$sentenza),
    # Il nome del gruppo «per busta» (RowGroup, app.R): lo stesso testo dello scaffale, «b. 3».
    k_busta   = ifelse(is.na(d$busta), "senza busta",
                       paste0(sigle_busta(if ("sigla" %in% names(d)) d$sigla else rep(NA_character_, nrow(d))), " ",
                              chiave_busta(d$busta, if ("suffisso_b" %in% names(d)) ifelse(is.na(d$suffisso_b), "", d$suffisso_b) else ""))),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

# DataTables in italiano, scritto qui: la traduzione ufficiale si
# scarica da un CDN, e l'app lavora senza rete.
DT_ITALIANO <- list(
  info = "_START_–_END_ di _TOTAL_", infoEmpty = "nessun fascicolo",
  infoFiltered = "", emptyTable = "Nessun fascicolo corrisponde alla ricerca.",
  zeroRecords = "Nessun fascicolo corrisponde alla ricerca.",
  lengthMenu = "_MENU_ per pagina",
  paginate = list(first = "«", last = "»", `next` = "›", previous = "‹"),
  aria = list(sortAscending = ": ordina crescente", sortDescending = ": ordina decrescente")
)

# Lo scaffale: una tessera per busta, colorata dalla completezza media
# dei 4 campi core. Clic = la tabella filtrata su quella busta.
scaffale_ui <- function(d) {
  if (!nrow(d)) return(p(class = "text-muted", "Nessun fascicolo corrisponde alla ricerca."))
  d$core <- completezza_core(d)
  # Una tessera per busta, e «232-bis» è un'altra busta di «232» (12/10).
  sb <- if ("suffisso_b" %in% names(d)) ifelse(is.na(d$suffisso_b), "", d$suffisso_b) else rep("", nrow(d))
  b  <- ifelse(is.na(d$busta), "senza", chiave_busta(d$busta, sb))
  gruppi <- split(d$core, b)
  # La sigla di ogni busta (b., aa.…): quella dei suoi fascicoli.
  sigle <- sigle_busta(if ("sigla" %in% names(d)) d$sigla else rep(NA_character_, nrow(d)))
  sigla_di <- tapply(sigle, b, function(x) x[1])
  num <- tapply(ifelse(is.na(d$busta), -1, d$busta), b, function(x) x[1])
  rango <- tapply(.rango_suffisso(sb), b, function(x) x[1])
  ordine <- order(num[names(gruppi)], rango[names(gruppi)], names(gruppi))
  gruppi <- gruppi[ordine]; chiavi <- names(gruppi)
  div(class = "scaffale",
      lapply(seq_along(gruppi), function(i) {
        k <- chiavi[i]; v <- gruppi[[i]]; senza <- identical(k, "senza")
        pct <- round(100 * mean(v) / 4)
        classe <- if (pct >= 100) "piena" else if (pct >= 60) "quasi" else "scarsa"
        tags$button(
          type = "button", class = paste("tessera-busta", classe),
          title = if (senza) "Fascicoli senza busta" else sprintf("Apri la busta %s nella tabella", k),
          onclick = if (!senza) sprintf("Shiny.setInputValue('scaffale_busta', '%s', {priority:'event'})", k),
          disabled = if (senza) NA,
          div(class = "tessera-nome", if (senza) "senza busta" else paste0(sigla_di[[k]], k)),
          div(class = "tessera-numeri", sprintf("%d fasc. · %d%%", length(v), pct)),
          div(class = "tessera-barra", span(style = sprintf("width:%d%%", pct))))
      }))
}

fascicoli_ui <- function() nav_panel(
  title = tagList(icon("folder-open"), "Fascicoli",
                  uiOutput("conta_fascicoli", inline = TRUE)),
  value = "Fascicoli",
  div(class = "pagina-fascicoli",
    # Una testata (titolo e «Nuovo fascicolo»), una barra (ricerca, vista, righe), una riga di filtri con i conti.
    # Gli errori sono un filtro («Con errori»): la fascia gialla compare solo quando lo si sceglie.
    div(class = "fasc-testa",
        h1(class = "fasc-titolo", "Fascicoli"),
        div(class = "cerca-fascicoli",
            textInput("cerca_fascicoli", NULL, width = "100%",
                      placeholder = "Cerca\u2026 anche per campo: reato:eresia anno:1620-1650 busta:9")),
        actionButton("nuovo_da_elenco", tagList(icon("plus"), "Nuovo fascicolo"), class = "btn-primary btn-compatto")),
    div(class = "fasc-filtri",
      uiOutput("chips_filtri", class = "fasc-chips"),
      # I campi della ricerca: un clic li aggiunge alla ricerca. Chiusi finché non servono (2/10).
      tags$details(class = "chip-dettagli",
        tags$summary("Campi di ricerca"),
        div(class = "chip-campi",
          lapply(CAMPI_RICERCA_AVANZATA, function(k)
            # In inglese l'etichetta e la parola inserita sono quelle inglesi
            # (CAMPI_RICERCA_EN, ricerca.R: la ricerca le capisce entrambe).
            tags$button(type = "button", class = "chip-campo", `data-campo` = k,
                        `data-campo-en` = names(CAMPI_RICERCA_EN)[match(k, CAMPI_RICERCA_EN)],
                        span(class = "solo-it", paste0(k, ":")),
                        span(class = "solo-en", paste0(names(CAMPI_RICERCA_EN)[match(k, CAMPI_RICERCA_EN)], ":")))),
          span(class = "small text-muted ms-1", "valori con spazi fra virgolette: persona:\"Russo, Giovanni\""))),
      # «Coorti:» e l'elenco compaiono solo se ce n'è una; «Salva questa ricerca» solo se c'è una ricerca da salvare.
      uiOutput("coorti_elenco", inline = TRUE),
      conditionalPanel("input.cerca_fascicoli", actionButton("salva_coorte_elenco", tagList(icon("bookmark"), "Salva questa ricerca"),
                   class = "btn-pillola btn-sm")),
      span(class = "fasc-conto small text-muted", textOutput("conta_elenco", inline = TRUE)),
      conditionalPanel("output.filtri_attivi", actionButton("azzera_filtri_elenco", "Azzera ricerca e filtri", class = "btn-pillola btn-sm")),
      div(class = "fasc-vista",
        div(class = "gruppo-pillole", role = "group", `aria-label` = "Vista",
            tags$button(type = "button", id = "vista_tabella", class = "btn btn-pillola attiva",
                        onclick = "impostaVista('tabella')", icon("table"), "Tabella"),
            tags$button(type = "button", id = "vista_scaffale", class = "btn btn-pillola",
                        onclick = "impostaVista('scaffale')", icon("box-archive"), "Scaffale")),
        # Densità: compatta = una riga per fascicolo; leggibile = titoli e liste fino a due righe, con le persone sotto il titolo.
        div(class = "fasc-righe",
            span(class = "fasc-righe-et", "Righe"),
            div(class = "gruppo-pillole", role = "group", `aria-label` = "Densità della tabella",
                tags$button(type = "button", id = "densita_leggibile", class = "btn btn-pillola",
                            `aria-pressed` = "false", onclick = "impostaDensita('leggibile')",
                            title = "Titoli e liste fino a due righe, persone sotto il titolo", "Comode"),
                tags$button(type = "button", id = "densita_compatta", class = "btn btn-pillola attiva",
                            `aria-pressed` = "true", onclick = "impostaDensita('compatta')",
                            title = "Una riga per fascicolo", "Compatte"))))),
    uiOutput("banner_anomalie"),          # R/anomalie.R: gli errori trovati nelle schede, quando si sceglie «Con errori»
    uiOutput("filtro_schedatura_info"),
    uiOutput("avviso_ricerca_elenco"),
    conditionalPanel("input.vista_elenco !== 'scaffale'", selezione_ui(), DTOutput("tabella")),
    conditionalPanel("input.vista_elenco === 'scaffale'", uiOutput("scaffale")),
    tags$script(HTML("
      $(document).on('click', '.chip-campo', function() {
        var el = document.getElementById('cerca_fascicoli');
        var v = el.value.replace(/\\s+$/, '');
        var inglese = document.documentElement.lang === 'en';
        el.value = (v ? v + ' ' : '') + (inglese ? this.dataset.campoEn : this.dataset.campo) + ':';
        el.focus();
        $(el).trigger('change');
      });
      Shiny.addCustomMessageHandler('mostra_omonimi', function(v) {
        requestAnimationFrame(function() {
          var el = document.getElementById('riquadro_omonimie');
          if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'});
        });
      });
      // Tastiera nell'elenco (19/11/2026): «/» cerca, j e k (o le frecce) passano al fascicolo dopo e prima e lo aprono, «?» dice i tasti.
      document.addEventListener('keydown', function(e) {
        if (e.metaKey || e.ctrlKey || e.altKey) return;
        var pane = document.querySelector('.tab-pane[data-value=\"Fascicoli\"]');
        if (!pane || pane.offsetParent === null || e.defaultPrevented || document.querySelector('.modal.show')) return;
        if (window.innerWidth < 1280 && (e.key === 'j' || e.key === 'k' || e.key === 'ArrowDown' || e.key === 'ArrowUp') && !(e.target.closest && e.target.closest('table.tabella-fascicoli'))) return;
        var t = e.target, scrive = /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName) || t.isContentEditable || (t.closest && t.closest('.selectize-control'));
        var cerca = document.getElementById('cerca_fascicoli');
        if (t === cerca) { if (e.key === 'Escape') { cerca.blur(); } return; }
        if (scrive) return;
        var guida = document.getElementById('aiuto-tasti');
        if (e.key === 'Escape' && guida) { guida.remove(); return; }
        if (e.key === '/') { e.preventDefault(); cerca.focus(); return; }
        if (e.key === '?') {
          e.preventDefault(); if (guida) { guida.remove(); return; }
          guida = document.createElement('div'); guida.id = 'aiuto-tasti'; guida.setAttribute('role', 'dialog'); guida.setAttribute('aria-label', 'Tasti');
          guida.innerHTML = '<b>Tasti dell\\u2019elenco</b><dl><dt>/</dt><dd>cerca</dd><dt>j \\u00b7 \\u2193</dt><dd>fascicolo dopo</dd><dt>k \\u00b7 \\u2191</dt><dd>fascicolo prima</dd><dt>Esc</dt><dd>chiude</dd></dl>';
          document.body.appendChild(guida); return;
        }
        var giu = e.key === 'j' || e.key === 'ArrowDown', su = e.key === 'k' || e.key === 'ArrowUp';
        if (!giu && !su) return;
        var righe = Array.prototype.slice.call(pane.querySelectorAll('table.tabella-fascicoli tbody tr:not(.gruppo-busta)'));
        if (!righe.length) return;
        e.preventDefault();
        var i = righe.findIndex(function(r) { return r.classList.contains('riga-aperta'); });
        var n = i < 0 ? 0 : Math.max(0, Math.min(righe.length - 1, i + (giu ? 1 : -1)));
        var a = righe[n].querySelector('a.link-apri'); if (a) a.click();
        righe[n].scrollIntoView({ block: 'nearest' });
      });
      // La riga aperta nella scheda resta evidenziata nella tabella.
      window.rigaAperta = null;
      function evidenziaRigaAperta() {
        var t = $('#tabella table.dataTable');
        if (!t.length) return;
        t.DataTable().rows().every(function() {
          $(this.node()).toggleClass('riga-aperta', this.data()[0] === window.rigaAperta);
        });
      }
      window.evidenziaRigaAperta = evidenziaRigaAperta;
      Shiny.addCustomMessageHandler('riga_aperta', function(id) {
        window.rigaAperta = id; evidenziaRigaAperta();
      });
      // Tabella o scaffale: un input del browser, perché lo legga anche
      // il conditionalPanel; il server lo cambia col messaggio qui sotto.
      window.impostaVista = function(v) {
        $('#vista_tabella').toggleClass('attiva', v === 'tabella');
        $('#vista_scaffale').toggleClass('attiva', v === 'scaffale');
        Shiny.setInputValue('vista_elenco', v);
      };
      Shiny.addCustomMessageHandler('vista_elenco', function(v) { impostaVista(v); });
      // Compatta o leggibile: solo aspetto (classe sulla pagina), la tabella, l'ordine,
      // la selezione e il clic che apre la scheda non cambiano. Si ricorda nel browser.
      window.impostaDensita = function(d, salva) {
        var leggibile = d === 'leggibile';
        $('.pagina-fascicoli').toggleClass('leggibile', leggibile);
        $('#densita_compatta').toggleClass('attiva', !leggibile).attr('aria-pressed', String(!leggibile));
        $('#densita_leggibile').toggleClass('attiva', leggibile).attr('aria-pressed', String(leggibile));
        if (salva !== false) { try { localStorage.setItem('trama.densita', d); } catch (e) {} }
      };
      $(function() { var d = null; try { d = localStorage.getItem('trama.densita'); } catch (e) {}
                     if (d !== 'leggibile') impostaDensita('compatta', false); });   // dal 19/11 Compatta è la vista di partenza (più righe in una schermata); Comode resta una scelta ricordata
      Shiny.addCustomMessageHandler('solo_incompleti', function(v) {
        $('#solo_incompleti').toggleClass('attiva', !!v).attr('aria-pressed', String(!!v));
      });
    ")))
)
