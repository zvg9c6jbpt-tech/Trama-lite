# ─────────────────────────────────────────────────────────────
# modulo.R: dal profilo agli input
# Dipende da profili.R (PROFILO_*, vocabolario) e compose.R (.vuoto)
# ─────────────────────────────────────────────────────────────

library(shiny)
library(bslib)


# "dimensioni/altezza" -> "dimensioni_altezza"
.slug <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_+|_+$", "", x)
}


# Identificatore stabile dell'input.
id_input <- function(riga) {
  if ("input_id" %in% names(riga) && !is.na(riga$input_id) && nzchar(riga$input_id)) return(riga$input_id)
  if (!is.na(riga$campo) && nzchar(riga$campo) && riga$dove != "extra")
    riga$campo
  else
    paste0("x_", .slug(riga$arianna))
}


.voc <- function(campo) {
  if (is.null(campo) || is.na(campo) || !nzchar(campo)) character(0)
  else vocabolario(campo)
}


.scelte <- function(...) {
  s <- unique(unlist(list(...)))
  s <- s[!is.na(s) & nzchar(s)]
  as.character(s)
}


# Quante voci il menu mostra aperto senza digitare (non quante ne conosce: cercando le trova tutte). Con migliaia di nomi, disegnarne mille a ogni apertura è lavoro buttato.
.VOCI_A_TENDINA <- 100L


# Campi a etichette (persone, luoghi, lingue…): separatore «;», non la virgola di selectize, che spezzerebbe «Cognome, Nome».
# Uscendo dal campo si tiene ciò che si è scritto; la × toglie un'etichetta, il doppio clic la rimette nella casella (JS_PILLOLE).
.OPZIONI_ETICHETTE <- list(create = TRUE, maxOptions = .VOCI_A_TENDINA, delimiter = ";",
                           createOnBlur = TRUE, plugins = list("remove_button"),
                           placeholder = "scrivi e premi Invio")

# Luoghi citati, Data topica, Luogo di conservazione (campi `auto`): ciò che si digita ha sempre la prima lettera maiuscola («napoli» → «Napoli»).
# `maiuscolaIniziale` è un marcatore che JS_AUTOCORREZIONE riscrive; le voci già note e quelle del database non si toccano.
.OPZIONI_LUOGHI <- c(.OPZIONI_ETICHETTE, list(maiuscolaIniziale = TRUE))

# Nei menu grandi (persone, imputati, luoghi) le voci non si disegnano qui: il vocabolario arriva dal server una volta sola (.manda_voci).
# numericInput vuoto: shiny scrive value="NA" e il browser avvisa per ogni campo; qui il campo nasce senza l'attributo.
numerico <- function(inputId, label, value = NA, ...) {
  x <- numericInput(inputId, label, value = value, ...)
  if (length(value) == 1 && is.na(value)) htmltools::tagQuery(x)$find("input")$removeAttrs("value")$allTags() else x
}

# Elenco di voci (etichette) con ogni voce nel suo elemento: il traduttore confronta nodi di testo interi.
# Il separatore lo mette il CSS (`data-sep`): shiny scrive i figli di un tag su righe diverse.
elenco_span <- function(x, sep = ", ") {
  n <- length(x)
  tagList(lapply(seq_len(n), function(i)
    span(class = "voce-elenco", `data-sep` = if (i < n) sep, x[i])))
}

# Il browser completa i campi che chiamano «nome» con i nomi di persona che ha in memoria: nel campo del
# fondo, in Import, suggeriva persone (Stefano, 7/10). Archivio, fondo e serie non sono persone.
senza_suggerimenti <- function(x)
  htmltools::tagQuery(x)$find("input")$addAttrs(autocomplete = "off", `data-lpignore` = "true", `data-1p-ignore` = "true")$allTags()

campo_input <- function(ns, riga, valore = NULL, lingua = "it") {

  id  <- id_input(riga)
  lab <- riga$etichetta

  switch(riga$widget,

         numero = numerico(ns(id), lab,
                               value = if (.vuoto(valore)) NA_integer_ else as.integer(valore),
                               step = 1),

         testo_lungo = textAreaInput(ns(id), lab,
                                     value = if (.vuoto(valore)) "" else as.character(valore), rows = 5),

         scelta = {
           facoltativa <- !is.na(riga$campo) && riga$campo %in% .SCELTA_FACOLTATIVA
           scelte <- traduci_scelte(riga$campo, .scelte(.voc(riga$campo), valore), lingua)
           selectizeInput(ns(id), lab,
                          choices  = if (facoltativa) c("", scelte) else scelte,
                          selected = if (facoltativa && .vuoto(valore)) "" else valore,
                          options  = list(create = TRUE))
         },

         vocabolario = selectizeInput(ns(id), lab,
                                      choices  = traduci_scelte(riga$campo, .scelte(.voc(riga$campo), valore), lingua),
                                      selected = valore, multiple = TRUE,
                                      options  = list(create = TRUE, placeholder = "scegli o aggiungi")),
         
         tag = selectizeInput(ns(id), lab,
                              choices  = .scelte(valore), selected = valore, multiple = TRUE,
                              options  = if (identical(riga$campo, "luogo")) .OPZIONI_LUOGHI else .OPZIONI_ETICHETTE),
         
         flag = checkboxInput(ns(id), lab,
                              value = isTRUE(suppressWarnings(as.integer(valore)) == 1)),

         auto = selectizeInput(ns(id), lab,
                               choices  = c("", .scelte(valore)),
                               selected = if (.vuoto(valore)) "" else valore,
                               options  = c(list(create = TRUE, maxOptions = .VOCI_A_TENDINA,
                                                 placeholder = "scegli o digita"),
                                            # Data topica e luogo di conservazione sono luoghi: stessa regola dei Luoghi citati.
                                            if (!is.na(riga$campo) && riga$campo %in% c("luogo_produzione", "luogo_conservazione")) list(maiuscolaIniziale = TRUE))),

         # disegnata a mano da .campo_coppie() (servono più valori insieme)
         coppie = NULL,

         # Una casella per ogni termine del tesauro; JS_TERMINI mostra solo quelle sotto una classe scelta nel Soggettario.
         termini = if (nrow(TERMINI_REATO))
           checkboxGroupInput(ns(id), lab, choiceValues = TERMINI_REATO$termine, selected = valore,
                              choiceNames = lapply(seq_len(nrow(TERMINI_REATO)), function(i)
                                tags$span(class = "termine-reato", `data-classe` = TERMINI_REATO$classe[i], TERMINI_REATO$termine[i]))),

         imputati = selectizeInput(ns(id), lab,
                                   choices  = .scelte(valore), selected = valore, multiple = TRUE,
                                   options  = list(create = TRUE, maxOptions = .VOCI_A_TENDINA, delimiter = ";",
                                                   placeholder = "Nome Cognome, poi Invio")),
         
         textInput(ns(id), lab,
                   value = if (.vuoto(valore)) "" else paste(valore, collapse = " | "))
  )
}

# --- griglia delle coppie (persona_reati, reato_luoghi) -------------
# Un input con un suo binding JS: il valore è il vettore delle coppie spuntate («riga<TAB>colonna», R/attribuzione.R).
# La griglia la disegna il browser dai dati del server (data-coppie al primo disegno, poi sendInputMessage, nello stesso ordine degli altri aggiornamenti).
# Un messaggio senza `selezionati` cambia solo righe e colonne. Le spunte di una riga tolta restano in memoria, che si azzera solo aprendo un fascicolo.
griglia_coppie <- function(id, etichetta, assi, selezionati = character(0), nota = NULL) {
  dati <- list(righe = as.list(assi$righe), colonne = as.list(assi$colonne),
               selezionati = as.list(as.character(selezionati)),
               avviso = if (is.null(assi$avviso)) "" else assi$avviso)
  div(class = "form-group shiny-input-container griglia-contenitore",
      tags$label(class = "control-label", `for` = id, etichetta),
      if (!is.null(nota)) div(class = "small text-muted mb-1", nota),
      div(id = id, class = "griglia-coppie",
          `data-coppie` = as.character(jsonlite::toJSON(dati, auto_unbox = FALSE))))
}

# Vuoto dal browser = character(0), non list(): e NULL solo se la
# griglia non c'è (allora il Salva non tocca le coppie, vedi crud.R).
shiny::registerInputHandler("trama.coppie", function(x, shinysession, name) {
  if (is.null(x)) character(0) else as.character(unlist(x))
}, force = TRUE)

JS_GRIGLIA_COPPIE <- "
(function() {
  var SEP = '\\t';
  function disegna(el, righe, colonne, sel, avviso) {
    var scelti = {}; (sel || []).forEach(function(v) { scelti[v] = true; });
    el.innerHTML = '';
    var a = [].concat(avviso || [])[0];
    if (a) { var n = document.createElement('div'); n.className = 'small griglia-avviso'; n.textContent = a; el.appendChild(n); }
    if (!righe.length || !colonne.length) {
      var m = document.createElement('div');
      m.className = 'small text-muted griglia-vuota';
      m.textContent = el.getAttribute('data-vuota') || 'Compila prima le due liste: le coppie si scelgono fra quelle.';
      el.appendChild(m); return;
    }
    var t = document.createElement('table'); t.className = 'table table-sm griglia-tabella';
    var th = t.createTHead().insertRow(); th.appendChild(document.createElement('th'));
    colonne.forEach(function(c) { var h = document.createElement('th'); h.textContent = c; th.appendChild(h); });
    var tb = t.createTBody();
    righe.forEach(function(r) {
      var tr = tb.insertRow(); var h = document.createElement('th'); h.textContent = r; tr.appendChild(h);
      colonne.forEach(function(c) {
        var td = tr.insertCell(); var cb = document.createElement('input');
        cb.type = 'checkbox'; cb.className = 'form-check-input'; cb.value = r + SEP + c;
        cb.title = r + ' · ' + c; cb.checked = !!scelti[cb.value]; td.appendChild(cb);
      });
    });
    el.appendChild(t);
  }
  var b = new Shiny.InputBinding();
  $.extend(b, {
    find: function(scope) { return $(scope).find('.griglia-coppie'); },
    getType: function() { return 'trama.coppie'; },
    getValue: function(el) {
      return $(el).find('input[type=checkbox]:checked').map(function() { return this.value; }).get();
    },
    initialize: function(el) {
      var d = JSON.parse(el.getAttribute('data-coppie'));
      el._memoria = {}; (d.selezionati || []).forEach(function(v) { el._memoria[v] = true; });
      disegna(el, d.righe, d.colonne, d.selezionati, d.avviso);
    },
    subscribe: function(el, cb) {
      $(el).on('change.coppie', 'input[type=checkbox]', function() {
        if (this.checked) el._memoria[this.value] = true; else delete el._memoria[this.value];
        cb();
      });
      $(el).on('coppie:ridisegnata.coppie', function() { cb(); });
    },
    unsubscribe: function(el) { $(el).off('.coppie'); },
    receiveMessage: function(el, m) {
      if (m.selezionati !== undefined && m.selezionati !== null) {
        el._memoria = {}; m.selezionati.forEach(function(v) { el._memoria[v] = true; });
      }
      disegna(el, m.righe || [], m.colonne || [], Object.keys(el._memoria || {}), m.avviso);
      $(el).trigger('coppie:ridisegnata');
    }
  });
  Shiny.inputBindings.register(b, 'trama.coppie');
})();
"

.COPPIE_TESTI <- list(
  persona_reati = "Spunta per ogni imputato i capi di cui risponde. Un imputato senza spunte resta «non esaminato».",
  reato_luoghi  = "Spunta per ogni capo i luoghi in cui si è consumato. Un capo senza spunte resta «non esaminato».")

.campo_coppie <- function(ns, p, campo, valori = list()) {
  r <- p[!is.na(p$campo) & p$campo == campo, , drop = FALSE]
  if (nrow(r) == 0) return(NULL)
  griglia_coppie(ns(campo), r$etichetta[1], assi_coppie(campo, valori),
                 selezionati = valori[[campo]], nota = .COPPIE_TESTI[[campo]])
}

# Il messaggio che aggiorna una griglia: con `selezionati` (apertura di
# un fascicolo) sostituisce le spunte, senza (sincronizzazione) le tiene.
aggiorna_coppie <- function(session, campo, valori, selezionati = NULL) {
  a <- assi_coppie(campo, valori)
  m <- list(righe = as.list(a$righe), colonne = as.list(a$colonne),
            avviso = if (is.null(a$avviso)) "" else a$avviso)
  if (!is.null(selezionati)) m$selezionati <- as.list(as.character(selezionati))
  session$sendInputMessage(campo, m)
}


# --- assemblaggio del modulo --------------------------------------

profilo_completo <- function(profilo = "Modello", extra = character()) {
  # Extra di sola provenienza (EXTRA_NASCOSTI, R/profili.R): fuori dalla maschera.
  extra <- setdiff(extra, c(EXTRA_NASCOSTI, EXTRA_PROVENIENZA))
  if (profilo != "Generale") {
    if (!profilo %in% names(PROFILI)) stop("Profilo sconosciuto.")
    return(rbind(CAMPI_TRAMA, PROFILI[[profilo]]))
  }
  # Unione stabile: cambiare vista non ricrea gli input e non perde bozze.
  core <- rbind(CAMPI_TRAMA, PROFILO_MODELLO[PROFILO_MODELLO$dove != "extra", ])
  core$profili <- ""
  # Un extra con lo stesso nome di un campo vero è una copia di provenienza (backup Electron): non si mostra, resta nel database e Salva non lo tocca.
  extra <- extra[!(tolower(trimws(extra)) %in% tolower(trimws(c(core$etichetta, core$arianna))))]
  aggiunti <- list()
  # Le colonne extra della maschera sono quelle del MODELLO; i campi dei vecchi formati si vedono solo se la scheda li ha.
  for (nome in MODELLO) {
    for (i in which(PROFILI[[nome]]$dove == "extra")) {
      r <- PROFILI[[nome]][i, ]; chiave <- r$arianna
      if (is.null(aggiunti[[chiave]])) {
        if (r$multi) r$widget <- "tag"
        if (chiave == "Trascrizione") r$widget <- "testo_lungo"
        r$profili <- nome
        aggiunti[[chiave]] <- r
      } else aggiunti[[chiave]]$profili <- paste(aggiunti[[chiave]]$profili, nome, sep = "|")
    }
  }
  for (nome in setdiff(extra, names(aggiunti))) {
    r <- .col(nome, multi = TRUE, widget = "tag", sezione = "Altri campi conservati")
    r$profili <- ""; aggiunti[[nome]] <- r
  }
  p <- rbind(core, do.call(rbind, aggiunti))
  # ID senza collisioni anche per intestazioni che differiscono per accenti/punteggiatura.
  p$input_id <- vapply(seq_len(nrow(p)), function(i) id_input(p[i, ]), "")
  duplicati <- duplicated(p$input_id) | duplicated(p$input_id, fromLast = TRUE)
  for (i in which(duplicati))
    p$input_id[i] <- paste0("x_", paste(format(charToRaw(enc2utf8(p$arianna[i]))), collapse = ""))
  p
}


.sugg <- function(suggerimenti, campo) {
  if (is.null(campo) || is.na(campo)) return(character(0))
  v <- suggerimenti[[campo]]
  if (is.null(v)) character(0) else v
}


# Un campo solo, preso dal profilo per nome; per i pochi che si
# ridisegnano a mano invece di scorrere una sezione intera (es. titolo).
.campo_singolo <- function(ns, p, campo, valori = list(), lingua = "it") {
  r <- p[!is.na(p$campo) & p$campo == campo, , drop = FALSE]
  if (nrow(r) == 0) return(NULL)
  campo_input(ns, r[1, ], valore = valori[[campo]], lingua = lingua)
}


.campi_sezione <- function(ns, righe, valori = list(), lingua = "it") {
  if (is.null(righe) || nrow(righe) == 0) return(NULL)
  campi <- lapply(seq_len(nrow(righe)), function(i) {
    r <- righe[i, ]
    {
      campo <- campo_input(ns, r, valore = valori[[id_input(r)]], lingua = lingua)
      codici <- if ("profili" %in% names(r) && r$dove == "extra") strsplit(r$profili, "|", fixed = TRUE)[[1]] else character(0)
      codici <- codici[nzchar(codici)]
      # Un campo conservato che nessun profilo conosce (es. il record originale di un backup Electron) si vede sempre; la condizione non va lasciata a metà.
      if (length(codici)) {
        condizione <- paste0("input.profilo_visibile === 'Generale'", paste0(" || input.profilo_visibile === ",
                              vapply(codici[nzchar(codici)], jsonlite::toJSON, "", auto_unbox = TRUE), collapse = ""))
        conditionalPanel(condizione, campo, ns = ns)
      } else campo
    }
  })
  larghezza <- if (any(righe$widget == "testo_lungo")) 1 else 1/2
  # `grid-lungo`: la griglia con un testo lungo resta a una colonna, anche dentro «Altri campi».
  do.call(layout_column_wrap, c(list(width = larghezza, gap = "1rem", class = if (larghezza == 1) "grid-lungo"), campi))
}


# Un campo e il suo flag in una riga sola («esaminata» accanto a tortura e sentenza, «n.d.» accanto all'estremo remoto); etichetta corta, testo vero al passaggio del mouse.
.campo_con_flag <- function(ns, p, campo, campo_flag, valori = list(), lingua = "it",
                            etichetta_flag = "esaminata") {
  r_c <- p[!is.na(p$campo) & p$campo == campo, , drop = FALSE]
  r_f <- p[!is.na(p$campo) & p$campo == campo_flag, , drop = FALSE]
  if (nrow(r_c) == 0 || nrow(r_f) == 0) return(NULL)
  v_flag <- valori[[campo_flag]]
  div(class = "campo-flag",
      campo_input(ns, r_c[1, ], valore = valori[[campo]], lingua = lingua),
      div(class = "flag-inline", title = r_f$etichetta[1],
          checkboxInput(ns(campo_flag), etichetta_flag,
                        value = isTRUE(suppressWarnings(as.integer(v_flag)) == 1))))
}


# Tipo e connettore partono vuoti, non sulla prima voce (vedi il commento
# in modulo_ui). Una lista sola, per disegnarli e per aggiornarli.
.CON_VUOTO <- list(tipo_titolo = function() names(TIPI_TITOLO),
                   connettore  = function() c("circa", "contro", "su", "di"))
# Menu a scelta che può restare VUOTO (la tipologia si sceglie caso per caso): voce vuota in testa, senza valore non sceglie la prima.
.SCELTA_FACOLTATIVA <- c("tipologia_documentaria")
.scelte_con_vuoto <- function(campo, valore = NULL) c("", .scelte(.CON_VUOTO[[campo]](), valore))


# Un campo con le sue scorciatoie a pillola: le pillole comandano il menu che c'è già (JS_PILLOLE); valore e raccolta restano quelli del menu.
#   modo «tutte»: una pillola per voce, il menu si apre con «altro…»;  modo «rapidi»: poche voci frequenti sopra il menu.
.con_pillole <- function(campo_ui, id, modo = c("tutte", "rapidi"), rapidi = character(0)) {
  modo <- match.arg(modo)
  if (is.null(campo_ui)) return(NULL)
  div(class = paste("campo-pillole", if (modo == "tutte") "solo-pillole"),
      `data-per` = id, `data-modo` = modo,
      `data-rapidi` = as.character(jsonlite::toJSON(as.character(rapidi))),
      campo_ui)
}

# I termini del tesauro compaiono solo sotto le classi scelte nel Soggettario: un termine di una classe non scelta resta nascosto (e il salvataggio lo lascia cadere).
JS_TERMINI <- "
(function() {
  function aggiorna() {
    var box = document.querySelector('.campo-termini');
    if (!box) return;
    var sel = document.getElementById(box.getAttribute('data-per'));
    var scelte = sel ? [].concat((sel.selectize ? sel.selectize.getValue() : [].map.call(sel.selectedOptions || [], function(o) { return o.value; })) || []) : [];
    var visibili = 0;
    box.querySelectorAll('.termine-reato').forEach(function(t) {
      var voce = t.closest('.checkbox, .form-check, label') || t;
      var si = scelte.indexOf(t.getAttribute('data-classe')) >= 0;
      voce.style.display = si ? '' : 'none';
      if (si) visibili++;
    });
    box.style.display = visibili ? '' : 'none';
  }
  document.addEventListener('change', function(e) { if (e.target && e.target.id && e.target.closest && !e.target.closest('.campo-termini')) setTimeout(aggiorna, 0); }, true);
  if (window.jQuery) jQuery(document).on('shiny:value shiny:inputchanged shiny:connected', function() { setTimeout(aggiorna, 0); });
  window.addEventListener('load', function() { setTimeout(aggiorna, 300); });
  window.aggiornaTermini = aggiorna;
})();
"

JS_PILLOLE <- "
(function() {
  function disegna(box) {
    var id = box.getAttribute('data-per');
    var el = document.getElementById(id);
    if (!el || !el.selectize) return;
    var s = el.selectize, modo = box.getAttribute('data-modo');
    var scelti = [].concat(s.getValue() || []).filter(function(v) { return v !== ''; });
    var voci = modo === 'tutte'
      ? Object.keys(s.options).filter(function(v) { return v !== ''; })
      : JSON.parse(box.getAttribute('data-rapidi') || '[]');
    if (modo === 'tutte') voci.sort(function(a, b) { return (s.options[a].$order || 0) - (s.options[b].$order || 0); });
    var riga = box.querySelector('.riga-pillole');
    if (!riga) { riga = document.createElement('div'); riga.className = 'riga-pillole';
                 var lab = box.querySelector('label'); lab ? lab.after(riga) : box.prepend(riga); }
    // Il fuoco era su una pillola di questa riga: dopo il ridisegno torna
    // sulla stessa, o i tasti (1…9, frecce) finirebbero nel vuoto.
    var fuoco = riga.contains(document.activeElement) ? document.activeElement.dataset.valore : null;
    riga.innerHTML = '';
    voci.forEach(function(v) {
      var b = document.createElement('button');
      b.type = 'button'; b.className = 'pillola' + (scelti.indexOf(v) >= 0 ? ' scelta' : '');
      b.textContent = s.options[v] ? s.options[v].label : v; b.dataset.valore = v;
      riga.appendChild(b);
    });
    if (modo === 'tutte') {
      var altro = document.createElement('button');
      altro.type = 'button'; altro.className = 'pillola altro'; altro.textContent = 'Altro…';
      altro.title = 'Una voce fuori elenco: apre il menu per scriverla';
      riga.appendChild(altro);
    }
    unaFermata(riga);
    rimettiFuoco(riga, fuoco, 'valore');
  }
  function tutti() { $('.campo-pillole').each(function() { disegna(this); }); }
  window.aggiornaPillole = tutti;
  $(document).on('click', '.campo-pillole .pillola', function() {
    var box = this.closest('.campo-pillole');
    var el = document.getElementById(box.getAttribute('data-per'));
    if (!el || !el.selectize) return;
    var s = el.selectize;
    if (this.classList.contains('altro')) { box.classList.remove('solo-pillole'); s.focus(); return; }
    var v = this.dataset.valore;
    if (!s.options[v]) s.addOption({value: v, label: v});
    if (el.multiple) {
      if ([].concat(s.getValue() || []).indexOf(v) >= 0) s.removeItem(v); else s.addItem(v);
    } else s.setValue(v);
  });
  $(document).on('change', '.campo-pillole select', function() { disegna(this.closest('.campo-pillole')); });
  $(document).on('shiny:bound', function(e) {
    var box = e.target && e.target.closest && e.target.closest('.campo-pillole');
    if (box) setTimeout(function() { disegna(box); }, 0);
  });
  // Estremi cronologici (28/09). Il riquadro resta evidenziato finché
  // manca l'estremo remoto e non è spuntato «n.d.». Entrando nel campo
  // (clic o Tab) un anno vuoto si riempie con la proposta, già
  // selezionato: se scrivi, lo sostituisci. Remoto: l'anno del fascicolo
  // precedente (dal server, 'proposta_anno'). Recente: remoto + 1.
  // Scelta di Stefano (28/09): così e non come suggerimento da accettare
  // con Invio (provato e scartato lo stesso giorno). Il recente manca in
  // 269 fascicoli su 367: se la fonte dà un anno solo, va cancellato.
  var propostaRemoto = null;
  function segnaEstremi() {
    var r = document.getElementById('estremo_remoto'), nd = document.getElementById('data_nd');
    var box = r && r.closest('.riquadro-estremi'); if (!box) return;
    box.classList.toggle('da-compilare', !r.value && !(nd && nd.checked));
  }
  // Un anno proposto e non ancora toccato resta «da confermare»
  // (el._proposto): rientrandoci, anche col clic, torna tutto selezionato
  // - il clic altrimenti mette il cursore in fondo e scrivendo 1578 dopo
  // 1577 veniva 15771578 (provato il 28/09). Scrivere lo conferma.
  function seleziona(el) {
    el._seleziona = true;
    setTimeout(function() { try { el.select(); } catch (e) {} }, 0);
  }
  function proponi(el, anno) {
    if (!el) return;
    if (el.value) { if (el._proposto) seleziona(el); return; }
    if (anno === null || anno === undefined || isNaN(anno)) return;
    var nd = document.getElementById('data_nd'); if (nd && nd.checked) return;
    el.value = anno; el._proposto = true; $(el).trigger('change'); segnaEstremi();
    seleziona(el);
  }
  Shiny.addCustomMessageHandler('proposta_anno', function(a) { propostaRemoto = (a === null || a === '') ? null : +a; });
  $(document).on('focusin', '#estremo_remoto', function() { proponi(this, propostaRemoto); });
  $(document).on('focusin', '#estremo_recente', function() {
    var r = document.getElementById('estremo_remoto');
    proponi(this, r && r.value ? parseInt(r.value, 10) + 1 : null);
  });
  // Il mouseup del clic che ha dato il fuoco toglierebbe la selezione.
  $(document).on('mouseup', '#estremo_remoto, #estremo_recente', function(e) {
    if (this._seleziona) { e.preventDefault(); this._seleziona = false; }
  });
  // Su un campo numerico la selezione non sempre tiene (col clic,
  // provato il 28/09): il primo inserimento su un anno ancora proposto lo
  // svuota prima. 'beforeinput' e non 'keydown': vale anche per incollare
  // o dettare, che non passano dalla tastiera.
  $(document).on('beforeinput', '#estremo_remoto, #estremo_recente', function(e) {
    if (!this._proposto) return;
    this._proposto = false; this._seleziona = false;
    var tipo = e.originalEvent.inputType || '';
    this.value = '';
    if (tipo.indexOf('delete') === 0) { e.preventDefault(); $(this).trigger('change'); segnaEstremi(); }
  });
  // Un recente ancora proposto segue il remoto: cambiato il remoto in
  // 1580, un 1578 rimasto lì sarebbe prima dell'inizio.
  $(document).on('input change', '#estremo_remoto', function() {
    var rec = document.getElementById('estremo_recente');
    if (!rec || !rec._proposto || !this.value) return;
    var nuovo = String(parseInt(this.value, 10) + 1);
    if (rec.value !== nuovo) { rec.value = nuovo; $(rec).trigger('change'); }
  });
  // Le frecce cambiano l'anno proposto: da lì in poi è un anno scelto.
  $(document).on('keydown', '#estremo_remoto, #estremo_recente', function(e) {
    if (e.key === 'ArrowUp' || e.key === 'ArrowDown') this._proposto = false;
  });
  // Un caricamento (altro fascicolo, annulla) porta valori veri, non proposte.
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'caricato') ['estremo_remoto', 'estremo_recente'].forEach(function(id) {
      var el = document.getElementById(id); if (el) el._proposto = false;
    });
  });
  $(document).on('input change', '#estremo_remoto, #data_nd', segnaEstremi);
  $(document).on('shiny:idle', segnaEstremi);
  // Persone e imputati (19/10 sera, Stefano): Trama censisce soltanto le PERSONE NOMINATE nei fascicoli, senza distinguere imputati e altre
  // figure; la riga di pillole «Imputati» non c'è più. Il campo #imputati resta nella maschera, nascosto, solo per non perdere i dati già
  // scritti (vuole la forma estesa «Nome Cognome»: «Franzese, Caterina» -> «Caterina Franzese», come .estendi_nome() in R).
  function estendi(p) {
    var i = p.indexOf(',');
    return i > 0 ? (p.slice(i + 1).trim() + ' ' + p.slice(0, i).trim()) : p.trim();
  }
  var personePrima = null;
  // Tolta una persona (la × sul nome), esce anche dagli imputati.
  $(document).on('change', '#persona', function() {
    var ora = [].concat(this.selectize ? this.selectize.getValue() || [] : []);
    var imp = document.getElementById('imputati');
    if (personePrima && imp && imp.selectize) personePrima.forEach(function(p) {
      if (ora.indexOf(p) < 0 && [].concat(imp.selectize.getValue() || []).indexOf(estendi(p)) >= 0)
        imp.selectize.removeItem(estendi(p));
    });
    personePrima = ora;
  });
  // Un caricamento porta persone nuove: si riparte da quelle.
  $(document).on('shiny:idle', function() {
    var per = document.getElementById('persona');
    if (per && per.selectize) personePrima = [].concat(per.selectize.getValue() || []);
  });
  // I luoghi rapidi seguono il fondo (e i salvataggi): li manda il server.
  Shiny.addCustomMessageHandler('luoghi_rapidi', function(v) {
    var box = document.querySelector('.campo-pillole[data-per=\"luogo\"]'); if (!box) return;
    box.setAttribute('data-rapidi', JSON.stringify([].concat(v || []))); disegna(box);
  });
  // «Altri campi» aperto se la scheda appena caricata ne ha di compilati,
  // chiuso se no: a ogni caricamento, non mentre si scrive.
  Shiny.addCustomMessageHandler('apri_sezione', function(m) {
    var d = document.querySelector('details.' + m.classe); if (d) d.open = !!m.aperto;
  });
  Shiny.addCustomMessageHandler('apri_altri', function(aperto) {
    var d = document.querySelector('details.altri-campi'); if (d) d.open = !!aperto;
  });
  // Correggere un'etichetta (28/09): doppio clic, e il suo testo torna
  // nella casella al posto dell'etichetta, pronto da modificare; Invio (o
  // uscire dal campo) la rimette corretta. La × la toglie e basta.
  $(document).on('dblclick', '.selectize-control.multi .selectize-input > .item', function(e) {
    var sel = $(this).closest('.selectize-control').siblings('select')[0];
    var s = sel && sel.selectize; if (!s || !s.settings.create) return;
    e.preventDefault();
    var v = this.getAttribute('data-value');
    s.removeItem(v); s.focus(); s.setTextboxValue(v);
    s.$control_input.trigger('update'); s.refreshOptions(true);
  });
  // Etichette rapide: aggiungono la frase in fondo alle note, una volta.
  $(document).on('click', '.nota-rapida', function() {
    var n = document.getElementById('note'); if (!n) return;
    var t = this.dataset.testo, v = n.value.replace(/\\s+$/, '');
    if (v.indexOf(t) >= 0) return;
    n.value = v ? v + (/[.;:]$/.test(v) ? ' ' : '. ') + t + '.' : t + '.';
    $(n).trigger('change');
  });
  // Titolo e regesto «composti» (verdi) finché la loro spunta «scritto a
  // mano» è spenta; le pillole-spunta («Non consultabile») piene se accese.
  function segnaSpunte() {
    $('.campo-composto').each(function() {
      var f = document.getElementById(this.dataset.flag);
      this.classList.toggle('e-composto', !!f && !f.checked);
    });
    $('.pillola-flag').each(function() {
      var f = document.getElementById(this.dataset.flag);
      this.classList.toggle('scelta', !!f && f.checked);
    });
  }
  $(document).on('click', '.pillola-flag', function() {
    var f = document.getElementById(this.dataset.flag); if (!f) return;
    f.checked = !f.checked; $(f).trigger('change'); segnaSpunte();
  });
  $(document).on('change', 'input[type=checkbox]', segnaSpunte);
  // Scrivere a mano nel titolo o nel regesto li rende «scritti a mano» (19/10, critique §13.4): senza, la prossima ricomposizione (un imputato
  // dichiarato, una persona aggiunta) cancellava ciò che l'utente aveva appena scritto. Solo la digitazione vera (isTrusted): gli aggiornamenti
  // che arrivano dal server non toccano il flag.
  $(document).on('input', '#titolo, #regesto', function(e) {
    if (!e.originalEvent || !e.originalEvent.isTrusted) return;
    var f = document.getElementById(this.id + '_manuale');
    if (f && !f.checked) { f.checked = true; $(f).trigger('change'); segnaSpunte(); }
  });

  // Tastiera (28/09): ogni riga di pillole è UNA fermata del Tab (la
  // pillola scelta, o la prima); dentro, le frecce si spostano e i tasti
  // 1…9 scelgono la pillola n. Senza, undici pillole del soggettario
  // erano undici Tab da attraversare.
  function rimettiFuoco(riga, valore, chiave) {
    if (valore === null || valore === undefined) return;
    var b = $(riga).children('button').get().filter(function(x) { return x.dataset[chiave] === valore; })[0];
    if (b) { $(riga).children('button').each(function() { this.tabIndex = -1; }); b.tabIndex = 0; b.focus(); }
  }
  function unaFermata(riga) {
    var b = $(riga).children('button').get(); if (!b.length) return;
    var prima = b.filter(function(x) { return x.classList.contains('scelta'); })[0] || b[0];
    b.forEach(function(x) { x.tabIndex = (x === prima) ? 0 : -1; });
  }
  $(document).on('keydown', '.riga-pillole, .riga-imputati-voci', function(e) {
    var b = $(this).children('button').get(), i = b.indexOf(document.activeElement);
    if (e.key === 'ArrowRight' || e.key === 'ArrowLeft') {
      e.preventDefault();
      var j = Math.max(0, Math.min(b.length - 1, (i < 0 ? 0 : i) + (e.key === 'ArrowRight' ? 1 : -1)));
      b.forEach(function(x) { x.tabIndex = -1; }); b[j].tabIndex = 0; b[j].focus();
    } else if (/^[1-9]$/.test(e.key) && !e.ctrlKey && !e.metaKey && !e.altKey) {
      // Prima il fuoco, poi il clic: il ridisegno lo rimette sulla scelta.
      var n = parseInt(e.key, 10) - 1; if (n < b.length && !b[n].classList.contains('altro')) { e.preventDefault(); b[n].focus(); b[n].click(); }
    }
  });
  // Ctrl+Invio (o Cmd+Invio): salva e passa al prossimo.
  $(document).on('keydown', function(e) {
    if (e.key === 'Enter' && (e.ctrlKey || e.metaKey) && $('#salva_succ').is(':visible') && !document.querySelector('.modal.show, .modal.in, dialog[open]')) {
      e.preventDefault(); $('#salva_succ').trigger('click');
    }
  });

  // Dopo un caricamento o un cambio di lingua le voci del menu cambiano
  // senza un evento proprio: si ridisegna quando il server ha finito.
  $(document).on('shiny:idle', function() {
    tutti(); segnaSpunte();
    $('.riga-pillole, .riga-imputati-voci').each(function() { unaFermata(this); });
  });
})();
"

# Le colonne della Scheda: identità archivistica, persone-luoghi-reato, regesto e note. I campi si prendono dal profilo per nome;
# quelli che nessuna colonna chiama finiscono in «Altri campi», per sezione (controllo in verifiche/funzioni.R).
.CAMPI_COLLOCATI <- c(
  "archivio", "fondo", "serie", "busta", "fascicolo", "fascicolo_suffisso", "busta_suffisso", "sigla_busta", "sigla_fascicolo", "altra_segnatura",
  "tipologia_documentaria", "definizione", "tipo_titolo", "connettore", "titolo_manuale", "titolo",
  "estremo_remoto", "data_nd", "estremo_recente",
  "imputati", "imputati_esaminati", "reato_ignoto", "luogo_ignoto", "non_procedimento", "identita_imputato_dubbia", "persona", "reato", "attribuzione", "persona_reati",
  "luogo_produzione", "luogo", "attribuzione_luoghi", "reato_luoghi", "reato_termini",
  "regesto", "regesto_manuale", "note", "non_consultabile",
  "tortura", "tortura_esaminata", "sentenza", "sentenza_esaminata",
  "luogo_conservazione", "soggetto", "lingua", "supporto")

# Un riquadro della Scheda, tutti dello stesso peso; `titolo = NULL` per quelli senza intestazione, `principale` = con cornice.
.colonna <- function(titolo, etichetta = NULL, ..., principale = FALSE, classe = NULL)
  div(class = paste("colonna-scheda", if (principale) "colonna-principale", classe),
      if (!is.null(titolo))
        div(class = "colonna-titolo",
            titolo, if (!is.null(etichetta)) span(class = "etichetta-sezione", etichetta)),
      ...)

.ripiegabile <- function(titolo, etichetta = NULL, ..., aperto = FALSE, classe = NULL) {
  contenuto <- list(...)
  if (!length(Filter(Negate(is.null), contenuto))) return(NULL)
  tags$details(class = paste("ripiegabile", classe), open = if (aperto) NA,
               tags$summary(titolo, if (!is.null(etichetta)) span(class = "etichetta-sezione", etichetta)),
               contenuto)
}

.nota_campo <- function(testo) div(class = "nota-campo", testo)

# I campi di «Altri campi», con il valore che vale «vuoto» (le due attribuzioni partono da «n.d.»): servono al riepilogo dei compilati (app.R).
ALTRI_CAMPI_FISSI <- c("serie", "fascicolo_suffisso", "busta_suffisso", "altra_segnatura", "tipo_titolo", "connettore",
                       "attribuzione", "persona_reati", "attribuzione_luoghi", "reato_luoghi")
campi_altri <- function(p) {
  p <- p[p$dove != "derivato", , drop = FALSE]
  fuori <- p$dove == "extra" | is.na(p$campo) | !(p$campo %in% .CAMPI_COLLOCATI) | p$campo %in% ALTRI_CAMPI_FISSI
  fuori <- fuori & !(p$arianna %in% c(EXTRA_TIPOLOGIA_FOGLIO, EXTRA_DATA_PRECISA))   # le date intere stanno sotto gli estremi; in vista accanto alla tipologia (15/10), non in «Altri campi»
  r <- p[fuori, , drop = FALSE]
  data.frame(id = vapply(seq_len(nrow(r)), function(i) id_input(r[i, ]), ""),
             etichetta = r$etichetta, stringsAsFactors = FALSE)
}
compilato <- function(id, valore) {
  v <- as.character(unlist(valore)); v <- trimws(v[!is.na(v)])
  v <- v[nzchar(v) & !(v %in% c("FALSE", "0"))]
  if (id %in% c("attribuzione", "attribuzione_luoghi")) v <- setdiff(v, "n.d.")
  length(v) > 0
}

# Impaginazione: in testa ciò che cambia a ogni scheda, in un riepilogo ciò che è uguale per tutte (archivio, fondo, produttore, conservazione, lingua, materiale),
# in «Altri campi» ciò che quasi nessuno usa. Aree ISAD(G) nell'ordine del Tab. `rapidi`: le voci frequenti per le pillole dei luoghi (le calcola app.R).
modulo_ui <- function(ns, valori = list(), lingua = "it", p = profilo_completo(),
                      rapidi = list()) {
  p <- p[p$dove != "derivato", , drop = FALSE]

  aggiuntivi <- if ("profili" %in% names(p)) p[p$dove == "extra", , drop = FALSE] else p[0, ]
  if (nrow(aggiuntivi)) p <- p[p$dove != "extra", , drop = FALSE]
  # «Tipologia documentaria nel foglio» cambia spesso (Atti, Lettere, Decreti): sta in vista sotto la tipologia.
  tip_foglio <- aggiuntivi[aggiuntivi$arianna %in% EXTRA_TIPOLOGIA_FOGLIO, , drop = FALSE]
  aggiuntivi <- aggiuntivi[!(aggiuntivi$arianna %in% EXTRA_TIPOLOGIA_FOGLIO), , drop = FALSE]
  # Giorno e mese degli estremi: facoltativi, in un riquadro che si apre da solo se uno è compilato.
  date_prec <- aggiuntivi[aggiuntivi$arianna %in% EXTRA_DATA_PRECISA, , drop = FALSE]
  aggiuntivi <- aggiuntivi[!(aggiuntivi$arianna %in% EXTRA_DATA_PRECISA), , drop = FALSE]
  resto <- p[is.na(p$campo) | !(p$campo %in% .CAMPI_COLLOCATI), , drop = FALSE]

  campo <- function(k) .campo_singolo(ns, p, k, valori, lingua)
  riga  <- function(larghezza, ...) {
    campi <- Filter(Negate(is.null), list(...))
    if (length(campi)) do.call(layout_column_wrap, c(list(width = larghezza, gap = ".75rem"), campi))
  }
  # Una spunta del profilo comandata da una pillola (JS_PILLOLE): la
  # spunta resta nella maschera, nascosta; la pillola ne mostra lo stato.
  spunta_nascosta <- function(k) div(class = "campo-nascosto", campo(k))

  layout_columns(
    # Colonna 1 contesto ereditato, 2 dove si lavora, 3 il risultato. Il contesto sta in una striscia in testa (collocazione, busta e fascicolo, tipologia); lo spazio va a contenuto e testi, 7 + 5.
    col_widths = breakpoints(sm = 12, lg = c(12, 7, 5, 12)), gap = "1rem", class = "scheda-tre-colonne",

    .colonna(NULL, NULL, classe = "colonna-contesto",
      # Collocazione e soggetti: uguali su quasi tutti i fascicoli, si leggono nel riepilogo (output riepilogo_collocazione); i campi veri si aprono con «modifica».
      tags$details(class = "collocazione",
        tags$summary(uiOutput(ns("riepilogo_collocazione"))),
        div(class = "collocazione-campi",
          campo("archivio"), campo("fondo"), campo("soggetto"),
          campo("luogo_conservazione"),
          riga(1/2, campo("lingua"), campo("definizione")),
          campo("supporto"), riga(1/2, campo("sigla_busta"), campo("sigla_fascicolo")))),
      # (Il riquadro con la segnatura composta è uscito il 2/10: era la terza
      # volta che la collocazione compariva, dopo la testa e il riepilogo.)
      riga(1/2, campo("busta"), campo("fascicolo")),
      # «Tipologia nel foglio» resta nella striscia del contesto; la tipologia vera sta nella colonna di lavoro.
      if (nrow(tip_foglio)) div(class = "tip-foglio", .campi_sezione(ns, tip_foglio, valori, lingua))),

    .colonna("Contenuto e indicizzazione", "campi fondamentali", principale = TRUE,
      # Le date in testa, con la spunta «n.d.»: un fascicolo senza data si salva solo se l'assenza è dichiarata. Evidenziato finché manca (JS_PILLOLE, 'da-compilare').
      div(class = "riquadro-estremi",
          div(class = "sottotitolo-colonna", icon("calendar"), " Estremi cronologici"),
          # «n.d.» sotto i due anni: il Tab va dal remoto al recente e poi alla spunta (il caso raro), raggiungibile da tastiera.
          riga(1/2, campo("estremo_remoto"), campo("estremo_recente")),
          div(class = "spunta-nd", campo("data_nd")),
          if (nrow(date_prec)) tags$details(class = "data-precisa", open = if (any(vapply(date_prec$arianna, function(k) !.vuoto(valori[[k]]), TRUE))) NA else NULL,
            tags$summary("Aggiungi mese e giorno (facoltativo)"),
            div(class = "text-muted small", "Scrivi «aaaa-mm-gg» (1644-03-29) o «mese aaaa» (marzo 1644). L'anno è quello del campo qui sopra."),
            .campi_sezione(ns, date_prec, valori, lingua)),
          uiOutput(ns("proposta_anno"))),
      # La tipologia a pillole, in striscia sotto gli estremi: è la scelta che apre titolo e regesto.
      div(class = "striscia-tipologia",
          .con_pillole(campo("tipologia_documentaria"), ns("tipologia_documentaria"), "tutte")),
      # Un solo elenco di persone: tutte le nominate nel fascicolo, da cui si compongono titolo e regesto. Il campo imputati resta nel profilo, nascosto.
      campo("persona"),
      # «Persone non nominate» (ignoti) resta nel profilo, nascosta: i dati già scritti non si perdono.
      div(class = "campo-nascosto", campo("imputati_esaminati")),
      div(class = "campo-nascosto", campo("imputati")),
      .con_pillole(campo("reato"), ns("reato"), "tutte"),
      div(class = "campo-termini", `data-per` = ns("reato"), campo("reato_termini")),
      .con_pillole(campo("luogo"), ns("luogo"), "rapidi", rapidi[["luogo"]]),
      # Segnalazioni sulla fonte: identità dubbia e flag «reato ignoto», «luogo ignoto», «non è un procedimento» (non escono nell'export; si spengono da soli al Salva se un dato li contraddice).
      # Riga chiusa che dice quali sono accesi e si apre se lo è qualcuno.
      .ripiegabile(tagList("Segnalazioni sulla fonte", uiOutput(ns("riepilogo_segnalazioni"), inline = TRUE)), NULL, classe = "segnalazioni",
        campo("identita_imputato_dubbia"),
        div(class = "d-flex flex-wrap column-gap-3 mb-2", `data-flag-fonte` = "1",
            campo("reato_ignoto"), campo("luogo_ignoto"), campo("non_procedimento"))),
      # Data topica su una riga: Napoli nel 97% dei fascicoli.
      tags$details(class = "data-topica",
        tags$summary(uiOutput(ns("riepilogo_data_topica"), inline = TRUE)),
        campo("luogo_produzione"))),

    .colonna("Titolo, regesto e note", NULL, principale = TRUE,
      # Titolo e regesto composti (verdi, etichetta «composto») finché non
      # si spunta «scritto a mano» (JS_PILLOLE segna la classe).
      div(class = "campo-composto", `data-flag` = ns("titolo_manuale"), campo("titolo")),
      # «Scritto a mano»: la spunta del profilo resta nascosta; la pillola la comanda (JS_PILLOLE) e mostra lo stato.
      spunta_nascosta("titolo_manuale"),
      div(class = "riga-flag mb-1",
          # Mostra lo STATO del testo: «Testo automatico» (si ricompone dai campi) o «Scritto a mano»; il clic passa dall'uno all'altro (le due etichette le alterna il CSS).
          tags$button(type = "button", class = "pillola pillola-flag pillola-modo", `data-flag` = ns("titolo_manuale"),
                      title = "Automatico: il titolo si ricompone dai campi. Scritto a mano: non si ricompone più da solo. Clic per cambiare.",
                      span(class = "modo-auto", icon("wand-magic-sparkles"), span("Testo automatico")),
                      span(class = "modo-manuale", icon("lock"), span("Scritto a mano")))),
      # Ha senso solo su un titolo scritto a mano (import da un
      # catalogo esistente): su un titolo composto sarebbe circolare.
      conditionalPanel(
        condition = "input.titolo_manuale",
        div(class = "d-flex align-items-center gap-3 mt-1 mb-2",
            actionButton(ns("estrai"), "Estrai dal titolo →", class = "btn-outline-secondary btn-sm text-nowrap flex-shrink-0"),
            span(class = "small text-muted",
                 "Propone persone, luogo e identità dubbia; li rivedi tu.")),
        uiOutput(ns("esito_estrazione"))),
      div(class = "campo-composto", `data-flag` = ns("regesto_manuale"), campo("regesto")),
      spunta_nascosta("regesto_manuale"),
      div(class = "riga-flag align-items-center mb-2",
          tags$button(type = "button", class = "pillola pillola-flag pillola-modo", `data-flag` = ns("regesto_manuale"),
                      title = "Automatico: il regesto si ricompone dai campi. Scritto a mano: non si ricompone più da solo. Clic per cambiare.",
                      span(class = "modo-auto", icon("wand-magic-sparkles"), span("Testo automatico")),
                      span(class = "modo-manuale", icon("lock"), span("Scritto a mano"))),
          actionButton(ns("ricomponi_regesto"), "Ricomponi dai campi", class = "btn-pillola btn-sm",
                       title = "Riscrive il regesto dai campi e toglie la spunta «scritto a mano»")),
      # Tortura e sentenza: quasi sempre «n.d.»; riga chiusa che dice lo stato, aperta se c'è altro.
      .ripiegabile(tagList("Tortura e sentenza", uiOutput(ns("riepilogo_tortura_sentenza"), inline = TRUE)), NULL, classe = "tortura-sentenza-sezione",
        div(class = "tortura-sentenza",
            div(.campo_con_flag(ns, p, "tortura",  "tortura_esaminata",  valori, lingua),
                uiOutput(ns("stato_tortura"),  class = "stato-esame-box")),
            div(.campo_con_flag(ns, p, "sentenza", "sentenza_esaminata", valori, lingua),
                uiOutput(ns("stato_sentenza"), class = "stato-esame-box")))),
      div(class = "sottotitolo-colonna", "Etichette rapide"),
      # «Non consultabile» è una spunta, non testo: la pillola la accende. L'altra è la frase più scritta nelle note e si aggiunge al testo.
      div(class = "riga-pillole mb-2",
          tags$button(type = "button", class = "pillola pillola-flag", `data-flag` = ns("non_consultabile"),
                      "Non consultabile"),
          tags$button(type = "button", class = "pillola nota-rapida",
                      `data-testo` = "Pessimo stato di conservazione", "Pessimo stato di conservazione")),
      spunta_nascosta("non_consultabile"),
      campo("note")),

    # «Altri campi» a tutta larghezza sotto i due riquadri.
    .colonna(NULL, NULL, classe = "colonna-altri", principale = TRUE,
      # Mai usati sulla maggior parte delle schede: chiusi, non spariti. Il titolo dice quali sono compilati (riepilogo_altri, app.R) e la sezione si apre da sola se la scheda ne ha.
      .ripiegabile(tagList("Altri campi", uiOutput(ns("riepilogo_altri"), inline = TRUE)), NULL, classe = "altri-campi",
       # Griglia adattiva: i campi brevi affiancati, quelli lunghi e i titoli di gruppo a tutta riga.
       div(class = "altri-griglia",
        div(campo("serie"), .nota_campo("vuoto se non usata (es. ASDNa)")), campo("fascicolo_suffisso"), campo("busta_suffisso"),
        div(class = "span-tutto", campo("altra_segnatura")),
        # Tipo del titolo, solo se diverso da quello della tipologia: a mano, perché la «scelta» normale prenderebbe la prima voce.
        # Non compongono più il titolo: restano nella pagina, nascosti, perché Salva li rilegga.
        div(class = "campo-nascosto", riga(1/2,
          selectizeInput(ns("tipo_titolo"), "Tipo del titolo (se diverso)",
                         choices  = .scelte_con_vuoto("tipo_titolo", valori[["tipo_titolo"]]),
                         selected = if (.vuoto(valori[["tipo_titolo"]])) "" else valori[["tipo_titolo"]],
                         options  = list(create = TRUE, placeholder = "dalla tipologia")),
          selectizeInput(ns("connettore"), "Connettore",
                         choices  = .scelte_con_vuoto("connettore", valori[["connettore"]]),
                         selected = if (.vuoto(valori[["connettore"]])) "" else valori[["connettore"]],
                         options  = list(create = TRUE)))),
        # «Attribuzione dei capi»: per ora un memento (MOSTRA_ATTRIBUZIONE, attribuzione.R); a interruttore spento i campi restano nascosti e Salva li rilegge.
        div(class = if (isTRUE(MOSTRA_ATTRIBUZIONE)) NULL else "campo-nascosto",
          campo("attribuzione"),
          # Le griglie delle coppie, solo quando la modalità le chiama. Il
          # valore di confronto è quello vero, non l'etichetta.
          conditionalPanel("input.attribuzione == 'per persona'", ns = ns,
            .campo_coppie(ns, p, "persona_reati", valori)),
          campo("attribuzione_luoghi"),
          conditionalPanel("input.attribuzione_luoghi == 'per reato'", ns = ns,
            .campo_coppie(ns, p, "reato_luoghi", valori))),
        if (nrow(aggiuntivi)) tagList(
          div(class = "sottotitolo-colonna", "Campi del profilo"),
          div(class = "span-tutto", .campi_sezione(ns, aggiuntivi, valori, lingua))),
        if (nrow(resto))
          lapply(split(resto, factor(resto$sezione, levels = unique(resto$sezione))), function(r)
            tagList(div(class = "sottotitolo-colonna", r$sezione[1]), div(class = "span-tutto", .campi_sezione(ns, r, valori, lingua)))))))
  )
}

# --- il modulo si disegna una volta, i valori entrano dopo ---
# Ridisegnarlo a ogni apertura, Salva o cambio lingua con renderUI faceva sembrare «cambiati» i campi ricreati (e scattavano le ricomposizioni),
# cancellava il testo non salvato e riportava Salva alla prima scheda. Ora modulo_ui() si disegna una volta e i valori arrivano con aggiorna_modulo(),
# che percorre la stessa tabella del profilo di campo_input(): ogni widget ha il suo gemello di aggiornamento.

# Il valore che un widget mostra quando il dato è vuoto: quello che sceglierebbe da solo al primo disegno.
# Trappole: updateSelectizeInput(selected = NULL) non svuota (serve character(0)); una «scelta» senza valore va sulla prima voce («n.d.» per tortura e sentenza).
# --- il vocabolario dei menu grandi --------------------------------
# Le voci già nel database (persone, imputati, luoghi) si mandano una volta e poi solo per differenza (dopo un salvataggio, i nomi nuovi):
# ciò che il browser ha già lo ricorda la sessione (session$userData). Prima, aprire una scheda ASMo mandava 1.003 kB; ora 5,6 kB.
# L'ordine conta: sendCustomMessage parte subito, gli aggiornamenti dei campi a fine ciclo. Quando arriva il valore la sua voce c'è già
# (selectize scarta un valore fuori dalle opzioni), per questo si manda sempre anche il valore del fascicolo.
# Al primo caricamento il campo non esiste ancora: il gestore in app.R tiene il messaggio da parte e lo applica quando il campo si collega.
.manda_voci <- function(session, id, voci) {
  voci  <- .scelte(voci)
  gia   <- session$userData$voci_menu[[id]]
  nuove <- setdiff(voci, gia)
  if (length(nuove)) {
    session$sendCustomMessage("voci_menu", list(id = id, voci = as.list(nuove)))
    session$userData$voci_menu[[id]] <- c(gia, nuove)
  }
  invisible(length(nuove))
}


aggiorna_modulo <- function(session, valori = list(), suggerimenti = list(), lingua = "it", p = profilo_completo(), fondo = NULL) {
  p <- p[p$dove != "derivato", , drop = FALSE]

  # Un altro fondo = menu dei nomi da rifare (ogni archivio va da solo): le voci già mandate si dimenticano e il browser svuota i menu prima delle nuove.
  if (!identical(session$userData$fondo_voci, fondo %||% "")) {
    ids <- vapply(which(p$widget %in% c("tag", "imputati", "auto")), function(i) id_input(p[i, ]), "")
    session$sendCustomMessage("voci_azzera", list(ids = as.list(unique(ids))))
    session$userData$voci_menu <- NULL
    session$userData$fondo_voci <- fondo %||% ""
  }

  for (i in seq_len(nrow(p))) {
    r      <- p[i, ]
    id     <- id_input(r)
    valore <- valori[[id]]
    vuoto  <- .vuoto(valore)
    sugg   <- .sugg(suggerimenti, r$campo)

    if (!is.na(r$campo) && r$campo %in% names(.CON_VUOTO)) {
      updateSelectizeInput(session, id, choices = .scelte_con_vuoto(r$campo, valore),
                           selected = if (vuoto) "" else valore)
      next
    }

    switch(r$widget,
      coppie      = aggiorna_coppie(session, id, valori,
                                    selezionati = if (vuoto) character(0) else valore),
      termini     = updateCheckboxGroupInput(session, id, selected = if (vuoto) character(0) else as.character(valore)),
      # Vuoto = stringa vuota, non NA: shiny scrive NA come il testo «NA», e il browser
      # lo rifiutava con un avviso a ogni scheda («The specified value "NA" cannot be parsed»).
      numero      = updateNumericInput(session, id, value = if (vuoto) "" else as.integer(valore)),
      testo_lungo = updateTextAreaInput(session, id, value = if (vuoto) "" else as.character(valore)),
      scelta      = {
        scelte <- traduci_scelte(r$campo, .scelte(.voc(r$campo), valore), lingua)
        facoltativa <- !is.na(r$campo) && r$campo %in% .SCELTA_FACOLTATIVA
        updateSelectizeInput(session, id, choices = if (facoltativa) c("", scelte) else scelte,
                             selected = if (!vuoto) valore
                                        else if (facoltativa) ""
                                        else if (length(scelte)) unname(scelte[1]) else character(0))
      },
      vocabolario = updateSelectizeInput(session, id,
                      choices  = traduci_scelte(r$campo, .scelte(.voc(r$campo), valore), lingua),
                      selected = if (vuoto) character(0) else valore),
      tag         = ,
      imputati    = {
        val <- if (vuoto) character(0) else as.character(valore)
        .manda_voci(session, id, c(val, sugg))
        updateSelectizeInput(session, id, selected = val)
      },
      flag        = updateCheckboxInput(session, id,
                                        value = isTRUE(suppressWarnings(as.integer(valore)) == 1)),
      auto        = {
        val <- if (vuoto) "" else as.character(valore)
        .manda_voci(session, id, c(val, sugg))
        updateSelectizeInput(session, id, selected = val)
      },
      updateTextInput(session, id, value = if (vuoto) "" else paste(valore, collapse = " | "))
    )
  }
}


# Cambio di lingua: si rietichettano sul posto solo le scelte a vocabolario (gestore JS «rietichetta» in app.R, updateOption di selectize).
# Non con updateSelectizeInput: selectize ignora la nuova etichetta dell'opzione già selezionata. Il valore non cambia, nessuna ricomposizione scatta.
traduci_modulo <- function(session, input, lingua = "it", p = profilo_completo()) {
  p <- p[p$dove != "derivato" & p$widget %in% c("scelta", "vocabolario"), , drop = FALSE]
  for (i in seq_len(nrow(p))) {
    r <- p[i, ]
    if (!is.na(r$campo) && r$campo %in% names(.CON_VUOTO)) next
    id <- id_input(r)
    scelte <- traduci_scelte(r$campo, .scelte(.voc(r$campo), input[[id]]), lingua)
    if (!length(scelte)) next
    etichette <- if (is.null(names(scelte))) scelte else names(scelte)
    session$sendCustomMessage("rietichetta",
                              list(id = id, etichette = as.list(stats::setNames(etichette, unname(scelte)))))
  }
}


# Vero se il campo mostra ancora il valore caricato: le ricomposizioni automatiche scattano solo per un cambio dell'utente, mai per un caricamento.
# Confronto normalizzato: vuoto è NULL, NA, "" o character(0); l'ordine di una lista non conta; una spunta è TRUE o 1.
.normalizza_valore <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- trimws(as.character(x))
  sort(unique(x[!is.na(x) & nzchar(x)]))
}

come_caricato <- function(id, attuale, caricati) {
  if (is.null(caricati)) return(FALSE)
  caricato <- caricati[[id]]
  p <- profilo_completo()
  r <- p[!is.na(p$campo) & p$campo == id, , drop = FALSE]
  if (nrow(r) && r$widget[1] == "flag")
    return(isTRUE(suppressWarnings(as.integer(attuale)) == 1) ==
           isTRUE(suppressWarnings(as.integer(caricato)) == 1))
  # Un menu a scelta caricato vuoto mostra la prima voce: è «come caricato», non una scelta dell'utente (altrimenti una scheda nuova chiederebbe «Modifiche non salvate: Regesto»).
  if (nrow(r) && r$widget[1] == "scelta" && !length(.normalizza_valore(caricato)) && !(r$campo[1] %in% .SCELTA_FACOLTATIVA))
    caricato <- unname(.scelte(.voc(r$campo[1]), NULL))[1]
  identical(.normalizza_valore(attuale), .normalizza_valore(caricato))
}


# --- lettura del modulo -------------------------------------------

raccogli_modulo <- function(input, p = profilo_completo()) {
  p <- p[p$dove != "derivato", , drop = FALSE]
  
  f     <- fascicolo_vuoto()
  extra <- list()
  
  for (i in seq_len(nrow(p))) {
    r  <- p[i, ]
    id <- id_input(r)
    v  <- input[[id]]
    
    if (r$dove == "extra") {
      if (!.vuoto(v)) extra[[r$arianna]] <- as.character(v)
      next
    }
    
    if (r$dove == "fascicoli") {
      f[[r$campo]] <- if (.vuoto(v)) NA else v[[1]]
      next
    }
    
    if (r$widget == "termini") {
      # Il termine sa a quale classe appartiene (profilo): la maschera porta le coppie (classe, termine); salvando, restano solo quelle sotto una classe scelta.
      t <- as.character(v); t <- t[!is.na(t) & nzchar(t)]
      i <- match(t, TERMINI_REATO$termine)
      f$reato_termini <- data.frame(reato = TERMINI_REATO$classe[i[!is.na(i)]], termine = t[!is.na(i)], stringsAsFactors = FALSE)
      next
    }

    if (r$widget == "coppie") {
      # NULL (griglia assente) resta NULL: il Salva non tocca le coppie.
      f[[r$dove]] <- if (is.null(v)) NULL
                     else tabella_da_coppie(v, if (r$dove == "persona_reati") c("persona", "reato")
                                               else c("reato", "luogo"))
      next
    }

    if (r$dove == "imputati") {
      estesi <- as.character(v)
      estesi <- estesi[!is.na(estesi) & nzchar(estesi)]
      f$imputati <- data.frame(
        esteso      = estesi,
        indicizzato = vapply(estesi, indicizza_nome, character(1), USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      )
      next
    }
    
    # persone, luoghi, reati, produttori, lingue
    f[[r$dove]] <- if (.vuoto(v)) character(0) else as.character(v)
  }
  
  # --- sincronizzazioni previste dal modello ---
  
  # L'imputato, in forma indicizzata, è anche persona citata (solo quelli che mancano, in testa; le altre restano nel loro ordine, o Salva le riordinerebbe).
  if (nrow(f$imputati) > 0)
    f$persone <- unique(c(setdiff(f$imputati$indicizzato, f$persone), f$persone))
  
  f$reati <- normalizza(f$reati)
  f$extra <- extra
  # Quali campi extra questa maschera conosce: gli altri, che la scheda
  # può avere da un altro profilo (Modena), non vanno toccati (crud.R).
  f$campi_extra_noti <- p$arianna[p$dove == "extra"]
  f
}
# --- dal fascicolo ai valori del modulo ---------------------------

valori_da_fascicolo <- function(f, p = profilo_completo()) {
  p <- p[p$dove != "derivato", , drop = FALSE]
  
  v <- list()
  
  for (i in seq_len(nrow(p))) {
    r  <- p[i, ]
    id <- id_input(r)
    
    valore <-
      if (r$widget == "coppie")       coppie_da_tabella(f[[r$dove]])
    else if (r$widget == "termini")   as.character(f$reato_termini$termine)
    else if (r$dove == "extra")       f$extra[[r$arianna]]
    else if (r$dove == "imputati")  f$imputati$esteso
    else if (r$dove == "fascicoli") f[[r$campo]]
    else                            f[[r$dove]]
    
    v[id] <- list(valore)
  }

  v
}


# --- fascicolo nuovo ----------------------------------------------

# Ciò che una scheda nuova prende dal fascicolo di riferimento: dove sta e com'è fatto (gli stessi campi che l'import non inventa mai); mai il contenuto.
# La «Tipologia documentaria» non si eredita: si sceglie caso per caso.
EREDITATI <- c("archivio", "fondo", "serie", "sigla_busta", "sigla_fascicolo", "definizione", "supporto",
               "luogo_conservazione", "lingua")
# Contesto che una scheda VUOTA esistente riprende dalla scheda di riferimento se li ha vuoti: gli EREDITATI più soggetto produttore e luogo di produzione.
CONTESTO_EREDITABILE <- c(EREDITATI, "soggetto", "luogo_produzione")


# Valori con cui si apre il modulo di una scheda nuova: `contesto` (nomi degli input) viene dal modulo a schermo o dal fascicolo di riferimento;
# `produttore` è il soggetto produttore predefinito dichiarato a inizio lavoro.
valori_nuovo <- function(contesto = list(), busta = NA_integer_, fascicolo = NA_integer_,
                         produttore = NULL, busta_suffisso = "") {
  v <- setNames(lapply(EREDITATI, function(k) contesto[[k]]), EREDITATI)
  v <- c(v, list(busta = busta, fascicolo = fascicolo, tortura = "n.d.", sentenza = "n.d."))
  # «Fascicolo seguente» dentro la busta 232-bis resta nella 232-bis (12/10).
  if (!.vuoto(busta_suffisso)) v$busta_suffisso <- busta_suffisso
  if (!.vuoto(produttore)) {
    v$soggetto <- produttore
    v$luogo_produzione <- data_topica_con_produttori(NULL, produttore)   # Inquisizione di X -> X
  }
  v
}

# ── Autocorrezione mentre si scrive ───────────────────────
# Errori banali corretti solo su ciò che l'utente DIGITA (all'uscita dal campo o alla creazione di una voce), mai sui valori caricati: aprire e salvare resta neutro.
#  - doppia maiuscola iniziale: «MAria» -> «Maria» (le sigle con più di due maiuscole restano);
#  - apostrofo al posto dell'accento a fine parola: «perche'» -> «perché», «Nicolo'» -> «Nicolò»; restano «po'», «de'», «mo'» e le elisioni;
#  - spazi doppi e spazio prima della punteggiatura;
#  - Luoghi citati (`maiuscolaIniziale`): la prima lettera è sempre maiuscola.
JS_AUTOCORREZIONE <- "
(function(){
  var TRONCHI = ['po','mo','be','de','co','pe','ne','fe','to','va','fa','sta','da','di'];
  var GRAVE = {a:'\\u00e0', e:'\\u00e8', i:'\\u00ec', o:'\\u00f2', u:'\\u00f9',
               A:'\\u00c0', E:'\\u00c8', I:'\\u00cc', O:'\\u00d2', U:'\\u00d9'};
  function accenta(w){
    var l = w.toLowerCase();
    if (TRONCHI.indexOf(l) >= 0) return null;
    if (l === 'e') return w === 'E' ? '\\u00c8' : '\\u00e8';
    if (w.length < 2) return null;
    if (/che$/i.test(w)) return w.slice(0, -1) + (w.slice(-1) === 'E' ? '\\u00c9' : '\\u00e9');
    var u = w.slice(-1);
    return GRAVE[u] ? w.slice(0, -1) + GRAVE[u] : null;
  }
  window.autocorreggi = function(t){
    if (typeof t !== 'string' || !t) return t;
    var s = t.replace(/[ \\t]{2,}/g, ' ').replace(/ +([,;:.!?])/g, '$1');
    s = s.replace(/(^|[^\\p{L}])(\\p{Lu})(\\p{Lu})(\\p{Ll}{2,})/gu, function(m, p, a, b, c){ return p + a + b.toLowerCase() + c; });
    s = s.replace(/(\\p{L}+)['\\u2019](?=$|[\\s,;:.!?)\\]\\u00bb\"])/gu, function(m, w){ var r = accenta(w); return r === null ? m : r; });
    return s;
  };
  // La prima LETTERA maiuscola (un apostrofo o uno spazio davanti restano): solo per i campi che lo chiedono (Luoghi citati).
  window.maiuscolaIniziale = function(t){
    if (typeof t !== 'string' || !t) return t;
    var i = t.search(/\\p{L}/u);
    return i < 0 ? t : t.slice(0, i) + t.charAt(i).toUpperCase() + t.slice(i + 1);
  };
  function segnala(el, prima){
    el.classList.add('autocorretto'); el.title = 'Corretto da: ' + prima;
    setTimeout(function(){ el.classList.remove('autocorretto'); }, 1600);
  }
  // Caselle di testo: solo dopo una battitura dell'utente.
  $(document).on('input', 'input[type=text], textarea', function(){ this.dataset.digitato = '1'; });
  $(document).on('change', 'input[type=text], textarea', function(){
    if (this.dataset.digitato !== '1' || this.closest('.selectize-control')) return;
    this.dataset.digitato = '';
    var prima = this.value, dopo = window.autocorreggi(prima);
    if (dopo !== prima) { this.value = dopo; $(this).trigger('change'); segnala(this, prima); }
  });
  // Menu: la voce nuova nasce già corretta.
  function aggancia(el){
    var s = el && el.selectize;
    if (!s || !s.settings.create || s.__autocorrezione) return;
    s.__autocorrezione = true;
    s.settings.create = function(input){
      // prima la maiuscola, poi l'autocorrezione: «mAria» → «MAria» → «Maria»
      var d = {}, v = window.autocorreggi(s.settings.maiuscolaIniziale ? window.maiuscolaIniziale(String(input).replace(/^\\s+/, '')) : input);
      d[s.settings.valueField] = v; d[s.settings.labelField] = v;
      return d;
    };
  }
  $(document).on('shiny:bound', function(e){ aggancia(e.target); });
  $(function(){ setTimeout(function(){ $('select.selectized').each(function(){ aggancia(this); }); }, 1500); });
})();
"
