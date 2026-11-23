# ─────────────────────────────────────────────────────────────
# R/revisione_vista.R: la finestra «Controlla prima di importare» in tre riquadri (19/11/2026)
# A sinistra i filtri (Tutte, Automatiche, Proposte, per colonna), al centro l'elenco, a destra la riga con il prima e il dopo
# e, sotto, che cosa propone Trama Lite. Le correzioni sicure («consigliate») sono «Automatiche» e partono applicate; le altre,
# e le cose da decidere, sono «Proposte». La logica non cambia: `rev_selezione` (le celle applicate) e gli `rev_dec_<id>` (le scelte)
# restano quelli di prima; qui si disegnano soltanto, e il browser mostra una riga alla volta.
# ─────────────────────────────────────────────────────────────

.rev_breve <- function(x, n = 64L) { x <- gsub("\\s+", " ", as.character(x)); if (!nzchar(x)) "(vuoto)" else if (nchar(x) > n) paste0(substr(x, 1L, n - 1L), "…") else x }
.rev_iniziale <- function(x) { i <- toupper(substr(trimws(as.character(x)[1]), 1L, 1L)); if (is.na(i) || !nzchar(i) || !grepl("[[:alnum:]]", i)) "?" else i }
.rev_etichetta_dec <- function(d) switch(d$tipo %||% "", avviso = "difformità", omonimo = "omonimo", luoghi_ignoti = "luoghi non in lista", anno = "data", "da decidere")

# Il riquadro «Trama Lite · proposta» con il motivo, e se serve il pulsante.
# Il motivo si registra com'è (è parte del contratto e del corpus); a schermo le frasi più tecniche si dicono in chiaro.
.perche_chiaro <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(x)
  gsub("forma italiana: proposta Locale \\(Italiano\\), decidi tu \\(([^()]+?) \u2192 (.+?\\))\\)",
       "\u00ab\\1\u00bb \u00e8 un luogo straniero: Trama consiglia \u00ab\\2\u00bb (il nome locale e, fra parentesi, quello italiano). Decidi tu.", x, perl = TRUE)
}
.rev_box <- function(testo, ...) div(class = "rev-box",
  div(class = "rev-box-testa", span(class = "rev-tl", "TL"), strong("Trama Lite"), span(class = "rev-box-tipo", "proposta"), div(class = "rev-box-az", ...)),
  div(class = "rev-box-testo", .perche_chiaro(testo)))

# Nome corto della colonna per i filtri («Persone citate» invece dell'intestazione intera del foglio).
.col_breve <- function(k) {
  m <- PROFILO_MODELLO$colonne %||% PROFILO_MODELLO
  e <- tryCatch(m$etichetta[match(k, m$arianna)], error = function(e) NA_character_)
  if (length(e) != 1L || is.na(e) || !nzchar(e)) k else e
}
vista_revisione <- function(pr, schede = FALSE, a = NULL) {
  celle <- pr$celle; dec <- pr$decisioni; n <- nrow(celle)
  celle$spunta <- if ("spunta" %in% names(celle)) celle$spunta else celle$consigliata   # spuntata all'apertura (vedi revisione_import.R)
  budget <- DIFF_BUDGET_TOTALE
  dd <- lapply(seq_len(n), function(i) {
    costo <- as.numeric(length(.pezzi_diff(celle$prima[i]))) * length(.pezzi_diff(celle$dopo[i]))
    if (costo > budget) return(list(prima = htmltools::htmlEscape(as.character(celle$prima[i])), dopo = htmltools::htmlEscape(as.character(celle$dopo[i]))))
    budget <<- budget - costo
    evidenzia_diff(celle$prima[i], celle$dopo[i])
  })
  righe <- sort(unique(c(celle$riga, vapply(dec, function(d) d$riga, 1L))))
  segn_di <- function(r) { s <- c(celle$segnatura[celle$riga == r], unlist(lapply(dec, function(d) if (d$riga == r) d$segnatura)))[1]; if (is.na(s)) "" else s }

  # l'elenco
  voci_c <- lapply(seq_len(n), function(i) {
    auto <- isTRUE(celle$spunta[i]); t1 <- .tipo_correzione(celle$perche[i]); t1 <- if (length(t1)) t1[1] else "correzione"
    div(class = "rev-voce", tabindex = "0", role = "option", `data-riga` = celle$riga[i], `data-cid` = celle$id[i], `data-kind` = if (auto) "auto" else "prop", `data-col` = celle$colonna[i], `data-tipi` = paste(.tipo_correzione(celle$perche[i]), collapse = "|"),
        `data-q` = tolower(paste(celle$prima[i], celle$dopo[i], celle$colonna[i], celle$segnatura[i], celle$perche[i])),
        tags$input(type = "checkbox", class = "rev-chk form-check-input", tabindex = "-1", `aria-label` = "Includi questa correzione"),
        span(class = "rev-av", .rev_iniziale(celle$colonna[i])),
        div(class = "rev-v-corpo", div(class = "rev-v-tit", .rev_breve(celle$prima[i])), div(class = "rev-v-sub", paste0(celle$colonna[i], " · ", t1))),
        span(class = "rev-pill"))
  })
  voci_d <- lapply(dec, function(d) {
    col <- as.character(d$colonna %||% "")[1]; if (is.na(col)) col <- ""
    div(class = "rev-voce", tabindex = "0", role = "option", `data-riga` = d$riga, `data-did` = d$id, `data-kind` = "prop", `data-col` = col, `data-tipi` = .rev_etichetta_dec(d),
        `data-q` = tolower(paste(d$testo, col, d$segnatura)),
        span(class = "rev-av", .rev_iniziale(if (nzchar(col)) col else d$tipo)),
        div(class = "rev-v-corpo", div(class = "rev-v-tit", .rev_breve(d$testo)), div(class = "rev-v-sub", paste0(if (nzchar(col)) paste0(col, " · ") else "", .rev_etichetta_dec(d)))),
        span(class = "rev-pill rev-pill-dec", `data-avviso` = if (identical(d$tipo, "avviso")) "1", if (identical(d$tipo, "avviso")) "Da vedere" else "Da controllare"))
  })
  voci <- c(voci_c, voci_d)
  ordine <- order(c(celle$riga, vapply(dec, function(d) d$riga, 1L)))
  voci <- voci[ordine]

  # i filtri a sinistra
  cols <- c(celle$colonna, unlist(lapply(dec, function(d) { k <- as.character(d$colonna %||% "")[1]; if (is.na(k)) "" else k })))
  cols <- cols[!is.na(cols) & nzchar(cols)]; tc <- table(factor(cols, levels = unique(cols)))
  tipi <- c(unlist(lapply(celle$perche, .tipo_correzione)), vapply(dec, .rev_etichetta_dec, ""))
  tt <- table(factor(tipi, levels = unique(tipi))); tt <- tt[order(-as.integer(tt), names(tt))]
  n_auto <- sum(celle$spunta); n_prop <- n - n_auto + length(dec)
  filtro <- function(k, etichetta, num) tags$a(href = "#", class = "rev-f", `data-f` = k, span(class = "rev-f-e", etichetta), span(class = "rev-f-n", num))
  lateral <- div(class = "rev-lat",
    filtro("tutte", "Tutte", n + length(dec)), filtro("auto", "Sicure", n_auto), filtro("prop", "Da controllare", n_prop),
    if (length(tc)) div(class = "rev-lat-tit", "Per campo"),
    lapply(seq_along(tc), function(k) tags$a(href = "#", class = "rev-f rev-fc", `data-col` = names(tc)[k], span(class = "rev-f-e", .col_breve(names(tc)[k])), span(class = "rev-f-n", as.integer(tc[k])))),
    if (length(tt)) tags$details(class = "rev-motivi", tags$summary(class = "rev-lat-tit", sprintf("Tipo di problema (%d)", length(tt))),
      lapply(seq_along(tt), function(k) tags$a(href = "#", class = "rev-f rev-fm", `data-tipo` = names(tt)[k], title = names(tt)[k], span(class = "rev-f-e", names(tt)[k]), span(class = "rev-f-n", as.integer(tt[k]))))))

  # il dettaglio, una riga per volta
  dettagli <- lapply(righe, function(r) {
    ic <- which(celle$riga == r); idc <- Filter(function(d) d$riga == r, dec)
    div(class = "rev-det", `data-riga` = r,
      div(class = "rev-dh", span(class = "rev-dh-t", "Scheda ", strong(segn_di(r))), span(class = "rev-dh-r", paste0("riga ", r, " del foglio")),
          if (length(ic)) span(class = "rev-cnt", sprintf("%d %s", length(ic), if (length(ic) == 1L) "correzione" else "correzioni"))),
      {
        ctx <- if (!is.null(a) && !is.null(a$dati) && r - 1L >= 1L && r - 1L <= nrow(a$dati)) {
          k <- a$colonna[["Titolo"]]
          if (!is.null(k) && !is.na(k) && k %in% names(a$dati) && !("Titolo" %in% celle$colonna[ic])) as.character(a$dati[[k]][r - 1L]) else ""
        } else ""
        if (nzchar(ctx) && !is.na(ctx)) div(class = "rev-lin rev-ctx", span(class = "rev-seg", ""), span(class = "rev-col", "Titolo"), span(class = "rev-val", .rev_breve(ctx, 220L)))
      },
      lapply(ic, function(i) {
        auto <- isTRUE(celle$spunta[i])
        div(class = "rev-cella", `data-cid` = celle$id[i],
          div(class = "rev-lin rev-del", span(class = "rev-seg", "Ora"), span(class = "rev-col", celle$colonna[i]), span(class = "rev-val", htmltools::HTML(dd[[i]]$prima))),
          div(class = "rev-lin rev-add", span(class = "rev-seg", "Diventa"), span(class = "rev-col", celle$colonna[i]), span(class = "rev-val", htmltools::HTML(if (nzchar(dd[[i]]$dopo)) dd[[i]]$dopo else "<em>(vuoto)</em>"))),
          .rev_box(celle$perche[i], tags$button(type = "button", class = "rev-tog", `data-cid` = celle$id[i], "Includi")))
      }),
      lapply(idc, function(d) {
        pulsante <- if (!(d$tipo %||% "") %in% c("avviso", "luoghi_ignoti") && length(d$opzioni) && length(d$opzioni) <= 12L)
          tags$button(type = "button", class = "rev-tog rev-applica-dec", onclick = sprintf("var r=$('#rev_dec_%d input[type=radio]').filter(function(){return this.value!==''}).first(); r.prop('checked',true).trigger('change');", d$id), "Applica la proposta")
        div(class = "rev-cella rev-cella-dec", `data-did` = d$id,
            .rev_box(span(class = "rev-etic", .rev_etichetta_dec(d)), pulsante), scheda_decisione(d))
      }))
  })

  tagList(
    tags$style(htmltools::HTML("
      .modal-dialog.modal-xl { max-width:min(1500px,96vw); }
      .rev-app { display:grid; grid-template-columns:13rem minmax(17rem,24rem) 1fr; border:1px solid var(--bs-border-color); border-radius:.6rem; overflow:hidden; height:max(20rem, calc(100vh - 31rem)); background:var(--bs-body-bg); }
      .rev-app > div { overflow-y:auto; min-width:0; }
      .rev-lat { border-right:1px solid var(--bs-border-color); padding:.6rem .5rem; background:color-mix(in srgb, var(--bs-body-color) 3%, var(--bs-body-bg)); }
      .rev-lat-tit { font-size:var(--rg-text-sm); color:var(--bs-secondary-color); margin:1rem .5rem .3rem; }
      .rev-f, .rev-f:hover, .rev-f:focus { text-decoration:none !important; }
      .rev-f { display:flex; align-items:center; gap:.5rem; padding:.4rem .5rem; border-radius:.45rem; color:var(--bs-body-color); text-decoration:none; font-size:var(--rg-text-sm); }
      .rev-f:hover { background:color-mix(in srgb, var(--bs-body-color) 6%, transparent); }
      .rev-f.attivo { background:color-mix(in srgb, var(--bs-body-color) 10%, transparent); font-weight:600; }
      .rev-f-e { flex:1; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; } .rev-f-n { color:var(--bs-secondary-color); font-variant-numeric:tabular-nums; }
      .rev-elenco { border-right:1px solid var(--bs-border-color); }
      .rev-cerca { position:sticky; top:0; z-index:2; padding:.5rem; background:var(--bs-body-bg); border-bottom:1px solid var(--bs-border-color); }
      .rev-cerca input.rev-q { width:100%; border:1px solid var(--bs-border-color); border-radius:.5rem; padding:.35rem .6rem; background:var(--bs-body-bg); color:var(--bs-body-color); }
      .rev-voce { display:flex; align-items:center; gap:.65rem; padding:.65rem .75rem; border-bottom:1px solid var(--bs-border-color); cursor:pointer; }
      .rev-voce:hover { background:color-mix(in srgb, var(--bs-body-color) 4%, transparent); }
      .rev-voce.attiva { background:color-mix(in srgb, var(--bs-body-color) 10%, transparent); }
      .rev-av { flex:none; width:2rem; height:2rem; border-radius:var(--rg-raggio); display:flex; align-items:center; justify-content:center; font-weight:600; font-size:var(--rg-text-sm); color:var(--bs-body-color); background:color-mix(in srgb, var(--bs-body-color) 10%, var(--bs-body-bg)); }
      .rev-v-corpo { flex:1; min-width:0; } .rev-v-tit { font-weight:600; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; } .rev-v-sub { font-size:var(--rg-text-xs); color:var(--bs-secondary-color); overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
      .rev-pill { flex:none; font-size:var(--rg-text-xs); padding:.1rem .55rem; border-radius:.5rem; border:1px solid var(--bs-border-color); white-space:nowrap; }
      .rev-pill::before { content:'\\25CF'; margin-right:.3rem; color:var(--bs-secondary-color); }
      .rev-voce[data-kind=prop] .rev-pill, .rev-pill-dec { border-color:var(--bs-body-color); background:color-mix(in srgb, var(--bs-body-color) 8%, transparent); }
      .rev-voce[data-kind=prop] .rev-pill::before, .rev-pill-dec::before { color:var(--bs-body-color); }
      .rev-voce.applicata .rev-pill { border-color:var(--rg-verde,#2e7d32); } .rev-voce.applicata .rev-pill::before { color:var(--rg-verde,#2e7d32); }
      .rev-voce.nascosta { display:none; }
      .rev-chk, .rev-chk-tutte { flex:none; width:1.1rem; height:1.1rem; margin:0; cursor:pointer; }
      .rev-voce:not([data-cid]) .rev-chk { display:none; }
      .rev-cerca input.rev-q { margin-bottom:.4rem; }
      .rev-tutte { display:flex; align-items:center; gap:.5rem; font-size:var(--rg-text-sm); cursor:pointer; margin:0; }
      .rev-n-sel { margin-left:auto; color:var(--bs-secondary-color); }
      .rev-vuoto { padding:1.5rem; color:var(--bs-secondary-color); text-align:center; display:none; }
      .rev-dett { padding:0; }
      .rev-det { display:none; } .rev-det.attivo { display:block; }
      .rev-dh { display:flex; align-items:center; gap:.8rem; padding:.7rem 1rem; border-bottom:1px solid var(--bs-border-color); position:sticky; top:0; background:var(--bs-body-bg); z-index:2; font-size:var(--rg-text-sm); }
      .rev-dh-r { color:var(--bs-secondary-color); } .rev-cnt { margin-left:auto; font-weight:600; color:var(--bs-secondary-color); } .rev-pl { color:var(--rg-verde); } .rev-mi { color:var(--rg-pericolo); }
      .rev-cella { border-bottom:1px solid var(--bs-border-color); }
      .rev-cella.evidenza { background:color-mix(in srgb, var(--bs-body-color) 4%, transparent); }
      .rev-lin { display:flex; gap:.8rem; padding:.45rem 1rem; font-size:var(--rg-text-sm); align-items:baseline; }
      .rev-seg { flex:none; width:4rem; font-size:var(--rg-text-xs); font-weight:700; text-transform:uppercase; letter-spacing:.04em; } .rev-col { flex:none; width:11rem; font-weight:600; } .rev-val { flex:1; min-width:0; overflow-wrap:anywhere; font-family:ui-monospace,Menlo,monospace; }
      .rev-ctx { color:var(--bs-secondary-color); } .rev-voce:focus-visible, .rev-f:focus-visible, .rev-tog:focus-visible { outline:2px solid var(--bs-body-color); outline-offset:-2px; }
      .rev-kbd { font-family:ui-monospace,Menlo,monospace; font-size:var(--rg-text-xs); border:1px solid var(--bs-border-color); border-radius:.3rem; padding:0 .3rem; }
      .rev-del { background:color-mix(in srgb, var(--rg-pericolo) 12%, var(--bs-body-bg)); } .rev-add { background:color-mix(in srgb, var(--rg-verde) 12%, var(--bs-body-bg)); }
      .rev-del .rev-seg { color:var(--rg-pericolo); } .rev-add .rev-seg { color:var(--rg-verde); }
      .rev-cella.rev-spenta .rev-add { background:transparent; opacity:.55; } .rev-cella.rev-spenta .rev-del { background:color-mix(in srgb, var(--bs-body-color) 8%, var(--bs-body-bg)); }
      .rev-val del { background:color-mix(in srgb, var(--rg-pericolo) 28%, var(--bs-body-bg)); color:var(--bs-body-color); text-decoration:none; padding:0 .1em; border-radius:3px; } .rev-val ins { background:color-mix(in srgb, var(--rg-verde) 28%, var(--bs-body-bg)); color:var(--bs-body-color); text-decoration:none; padding:0 .1em; border-radius:3px; }
      .rev-box { margin:.7rem 1rem; padding:.6rem .9rem .7rem; background:color-mix(in srgb, var(--bs-body-color) 5%, var(--bs-body-bg)); border:1px solid color-mix(in srgb, var(--rg-pericolo) 40%, var(--bs-border-color)); border-radius:.3rem; }
      .rev-box-testa { display:flex; align-items:center; gap:.6rem; } .rev-box-tipo { color:var(--bs-secondary-color); font-size:var(--rg-text-sm); } .rev-box-az { margin-left:auto; }
      .rev-tl { width:2rem; height:2rem; border-radius:var(--rg-raggio); background:color-mix(in srgb, var(--bs-primary) 16%, var(--bs-body-bg)); color:var(--bs-body-color); display:flex; align-items:center; justify-content:center; font-size:var(--rg-text-xs); font-weight:700; }
      .rev-box-testo { margin:.3rem 0 0 2.6rem; }
      .rev-tog { border:1px solid var(--bs-body-color); background:var(--bs-body-color); color:var(--bs-body-bg); border-radius:.5rem; padding:.3rem 1rem; font-weight:600; }
      .rev-tog.attivo { background:transparent; color:var(--bs-body-color); } .rev-tog.attivo::before { content:'\\2713  '; }
      .rev-cella-dec .rev-decisione { margin:.2rem 1rem 1rem; }
      .rev-etic { color:var(--bs-secondary-color); }
      .rev-punto { padding:2rem; color:var(--bs-secondary-color); }
      .rev-barra { display:flex; flex-wrap:wrap; gap:.5rem; align-items:center; margin:.6rem 0 .3rem; font-size:var(--rg-text-sm); color:var(--bs-secondary-color); }
      .rev-barra .conta { margin-left:auto; } .rev-n-pre { color:var(--bs-primary); font-weight:600; }
      .rev-decisione { border:1px solid var(--bs-border-color); border-top:2px solid #e0a800; border-radius:.25rem; padding:.6rem .9rem; margin-bottom:.6rem; background:var(--bs-body-bg); }
      .rev-dec-testa { font-size:var(--rg-text-xs); color:var(--bs-secondary-color); display:flex; gap:.6rem; } .rev-dec-testo { font-weight:600; margin:.15rem 0 .3rem; }
      .rev-presel { border-left-color:var(--bs-primary) !important; } .rev-avviso { border-left-color:var(--rg-controllo) !important; } .rev-riga { font-weight:600; }
      .rev-n { color:var(--bs-secondary-color); font-size:var(--rg-text-sm); } .rev-n::before { content:' \\00B7 '; }
      .rev-badge { display:inline-block; font-size:var(--rg-text-xs); font-weight:600; color:var(--bs-primary); margin-bottom:.2rem; }
      .rev-decisione .shiny-options-group { margin-bottom:0; }
      @media (min-width:901px) {
        .rev-app { grid-template-columns:minmax(20rem,27rem) 1fr; grid-template-rows:auto minmax(0,1fr); height:max(22rem, calc(100vh - 24rem)); }
        .rev-lat { grid-column:1 / -1; display:flex; flex-wrap:wrap; align-items:center; gap:.15rem .3rem; padding:.45rem .75rem; border-right:0; border-bottom:1px solid var(--bs-border-color); max-height:6.5rem; }
        .rev-lat-tit { margin:0 .2rem 0 .9rem; padding-left:.9rem; border-left:1px solid var(--bs-border-color); }
        .rev-motivi { flex:0 0 auto; } .rev-motivi[open] { flex-basis:100%; } .rev-motivi > summary { cursor:pointer; list-style:none; } .rev-motivi > summary::after { content:' \\25BE'; } .rev-motivi[open] > summary::after { content:' \\25B4'; }
        .rev-motivi[open] > .rev-f { display:inline-flex; }
        .rev-f { padding:.2rem .6rem; gap:.35rem; } .rev-f-e { max-width:15rem; }
        .rev-v-tit { white-space:normal; display:-webkit-box; -webkit-line-clamp:2; -webkit-box-orient:vertical; }
        .rev-pill { border:0 !important; background:transparent !important; padding:0 !important; color:var(--bs-secondary-color); }
        .rev-voce.applicata .rev-pill { color:var(--rg-verde,#2e7d32); }
        .rev-det.attivo .rev-cella:not(.evidenza) { opacity:.5; transition:opacity .12s; } .rev-det.attivo .rev-cella:not(.evidenza):hover { opacity:1; }
        .rev-cella.evidenza { box-shadow:inset 0 2px 0 var(--bs-body-color); background:color-mix(in srgb, var(--bs-body-color) 3%, transparent); }
      }
      @media (max-width:900px) { .rev-app { grid-template-columns:1fr; grid-template-rows:auto minmax(7rem,12rem) minmax(10rem,1fr); height:max(24rem, calc(100vh - 25rem)); } .rev-lat { display:flex; flex-wrap:nowrap; overflow-x:auto; overflow-y:hidden; align-items:center; max-height:none; border-right:0; border-bottom:1px solid var(--bs-border-color); } .rev-lat .rev-f { width:auto; flex:none; } .rev-lat .rev-lat-tit { display:none; } }")),
    div(class = "rev-app", `data-on` = paste(celle$id[celle$spunta], collapse = ","),
      lateral,
      div(class = "rev-elenco", div(class = "rev-cerca", tags$input(type = "search", class = "rev-q", placeholder = "Cerca…", `aria-label` = "Cerca nelle correzioni"),
              tags$label(class = "rev-tutte", tags$input(type = "checkbox", class = "rev-chk-tutte form-check-input"), " Seleziona tutte le mostrate", span(class = "rev-n-sel"))),
          div(class = "rev-lista", voci), div(class = "rev-vuoto", "Niente da mostrare con questo filtro.")),
      div(class = "rev-dett", div(class = "rev-punto", "Scegli una voce a sinistra."), dettagli)),
    div(class = "rev-barra",
        tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rev-batch", `data-on` = "1", "Seleziona le mostrate"),
        tags$button(type = "button", class = "btn btn-sm btn-outline-secondary rev-batch", `data-on` = "0", "Deseleziona le mostrate"),
        {
          npre <- sum(vapply(dec, function(d) nzchar(d$predefinita %||% ""), TRUE))
          if (npre > 0) span(class = "rev-n-pre", sprintf("%d scelte preselezionate da verificare", npre))
        },
        span(class = "rev-aiuto", tags$kbd(class = "rev-kbd", "\u2191\u2193"), " scorri \u00b7 ", tags$kbd(class = "rev-kbd", "/"), " cerca \u00b7 ", tags$kbd(class = "rev-kbd", "a"), " seleziona"),
        span(class = "conta")),
    tags$script(htmltools::HTML(.REV_JS)))
}

.REV_JS <- "
(function () {
  var R = document.querySelector('.rev-app'); if (!R || R.getAttribute('data-ok')) return; R.setAttribute('data-ok', '1');
  var filtro = 'tutte', colonna = '', tipo = '', testo = '', on = {};
  (R.getAttribute('data-on') || '').split(',').forEach(function (x) { if (x) on[x] = true; });
  function q(s, r) { return Array.prototype.slice.call((r || document).querySelectorAll(s)); }
  function aggiorna() {
    q('.rev-voce[data-cid]', R).forEach(function (v) {
      var c = v.getAttribute('data-cid'), s = !!on[c], p = v.querySelector('.rev-pill'), a = v.getAttribute('data-kind') === 'auto';
      var cb = v.querySelector('.rev-chk'); if (cb) cb.checked = s;
      v.classList.toggle('applicata', s && !a); p.textContent = a ? (s ? 'Sicura' : 'Esclusa') : (s ? 'Inclusa' : 'Da controllare');
    });
    q('.rev-tog[data-cid]', R).forEach(function (b) { var s = !!on[b.getAttribute('data-cid')]; b.classList.toggle('attivo', s); b.textContent = s ? 'Inclusa' : 'Includi'; });
    q('.rev-cella[data-cid]', R).forEach(function (c) { c.classList.toggle('rev-spenta', !on[c.getAttribute('data-cid')]); });
  }
  function mostra() {
    var n = 0, k = testo.toLowerCase();
    q('.rev-voce', R).forEach(function (v) {
      var ok = (filtro === 'tutte' || v.getAttribute('data-kind') === filtro) && (!colonna || v.getAttribute('data-col') === colonna) && (!tipo || ('|' + v.getAttribute('data-tipi') + '|').indexOf('|' + tipo + '|') >= 0) && (!k || v.getAttribute('data-q').indexOf(k) >= 0);
      v.classList.toggle('nascosta', !ok); if (ok) n++;
    });
    R.querySelector('.rev-vuoto').style.display = n ? 'none' : 'block'; tutte();
    var c = document.querySelector('.rev-barra .conta'); if (c) c.textContent = n + ' mostrate';
  }
  function tutte() {
    var m = R.querySelector('.rev-chk-tutte'); if (!m) return;
    var vis = q('.rev-voce[data-cid]:not(.nascosta)', R), k = vis.filter(function (v) { return on[v.getAttribute('data-cid')]; }).length;
    m.checked = vis.length > 0 && k === vis.length; m.indeterminate = k > 0 && k < vis.length; m.disabled = !vis.length;
    var t = R.querySelector('.rev-n-sel'); if (t) t.textContent = k + ' di ' + vis.length + ' spuntate';
    var cf = document.getElementById('rev_conferma'), tot = Object.keys(on).length;
    if (cf) { var sp = cf.querySelector('.rev-conta-btn'); if (!sp) { sp = document.createElement('span'); sp.className = 'rev-conta-btn'; cf.appendChild(sp); } sp.textContent = tot ? ' (' + tot + ')' : ''; }
  }
  var ultima = null;
  function imposta(ids, s) { ids.forEach(function (c) { if (s) on[c] = true; else delete on[c]; }); aggiorna(); tutte(); invia('rev_batch', { ids: ids.map(function (c) { return parseInt(c, 10); }), on: s }); }
  function apri(v) {
    q('.rev-voce.attiva', R).forEach(function (x) { x.classList.remove('attiva'); }); v.classList.add('attiva');
    q('.rev-det', R).forEach(function (d) { d.classList.toggle('attivo', d.getAttribute('data-riga') === v.getAttribute('data-riga')); });
    var p = R.querySelector('.rev-punto'); if (p) p.style.display = 'none';
    q('.rev-cella.evidenza', R).forEach(function (x) { x.classList.remove('evidenza'); });
    var sel = v.getAttribute('data-cid') ? '.rev-cella[data-cid=\"' + v.getAttribute('data-cid') + '\"]' : '.rev-cella[data-did=\"' + v.getAttribute('data-did') + '\"]';
    var e = R.querySelector('.rev-det.attivo ' + sel); if (e) { e.classList.add('evidenza'); e.scrollIntoView({ block: 'nearest' }); }
  }
  function commuta(c) { if (on[c]) delete on[c]; else on[c] = true; aggiorna(); tutte(); invia('rev_toggle', { id: parseInt(c, 10), on: !!on[c] }); }
  function visibili() { return q('.rev-voce:not(.nascosta)', R); }
  function vai(v) { if (!v) return; v.focus(); apri(v); v.scrollIntoView({ block: 'nearest' }); }
  document.addEventListener('keydown', function (ev) {
    if (!document.body.contains(R) || ev.metaKey || ev.ctrlKey || ev.altKey) return;
    var t = ev.target, scrive = t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.tagName === 'SELECT');
    if (t === R.querySelector('.rev-q')) {
      if (ev.key === 'Escape' && t.value) { t.value = ''; testo = ''; mostra(); ev.stopPropagation(); ev.preventDefault(); }
      else if (ev.key === 'ArrowDown' || ev.key === 'Enter') { ev.preventDefault(); vai(visibili()[0]); }
      return;
    }
    if (scrive || !R.contains(t)) { if (ev.key === '/' && !scrive && document.body.contains(R)) { ev.preventDefault(); R.querySelector('.rev-q').focus(); } return; }
    var v = t.closest && t.closest('.rev-voce'), lista = visibili(), i = v ? lista.indexOf(v) : -1;
    if (ev.key === '/') { ev.preventDefault(); R.querySelector('.rev-q').focus(); }
    else if (v && (ev.key === 'ArrowDown' || ev.key === 'j')) { ev.preventDefault(); vai(lista[Math.min(i + 1, lista.length - 1)]); }
    else if (v && (ev.key === 'ArrowUp' || ev.key === 'k')) { ev.preventDefault(); if (i <= 0) R.querySelector('.rev-q').focus(); else vai(lista[i - 1]); }
    else if (v && (ev.key === 'Enter' || ev.key === ' ')) { ev.preventDefault(); apri(v); }
    else if (v && ev.key === 'a' && v.getAttribute('data-cid')) { ev.preventDefault(); commuta(v.getAttribute('data-cid')); }
  });
  function invia(nome, val) { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(nome, val, { priority: 'event' }); }
  R.addEventListener('click', function (ev) {
    var f = ev.target.closest('.rev-f');
    if (f) {
      ev.preventDefault();
      if (f.classList.contains('rev-fm')) { tipo = (tipo === f.getAttribute('data-tipo')) ? '' : f.getAttribute('data-tipo'); }
      else if (f.classList.contains('rev-fc')) { colonna = (colonna === f.getAttribute('data-col')) ? '' : f.getAttribute('data-col'); invia('rev_f_colonna', colonna); }
      else { filtro = f.getAttribute('data-f'); }
      q('.rev-f', R).forEach(function (x) { x.classList.toggle('attivo', x.classList.contains('rev-fm') ? x.getAttribute('data-tipo') === tipo : x.classList.contains('rev-fc') ? x.getAttribute('data-col') === colonna : x.getAttribute('data-f') === filtro); });
      mostra(); return;
    }
    var t = ev.target.closest('.rev-tog[data-cid]');
    if (t) { commuta(t.getAttribute('data-cid')); tutte(); return; }
    var cb = ev.target.closest('.rev-chk');
    if (cb) {
      var vc = cb.closest('.rev-voce'), c = vc.getAttribute('data-cid'), s = cb.checked, lista = q('.rev-voce[data-cid]:not(.nascosta)', R);
      if (ev.shiftKey && ultima && lista.indexOf(ultima) >= 0) {
        var a = lista.indexOf(ultima), b = lista.indexOf(vc), da = Math.min(a, b), fino = Math.max(a, b);
        imposta(lista.slice(da, fino + 1).map(function (x) { return x.getAttribute('data-cid'); }), s);
      } else imposta([c], s);
      ultima = vc; return;
    }
    var mt = ev.target.closest('.rev-chk-tutte');
    if (mt) { imposta(q('.rev-voce[data-cid]:not(.nascosta)', R).map(function (x) { return x.getAttribute('data-cid'); }), mt.checked); return; }
    var v = ev.target.closest('.rev-voce'); if (v) apri(v);
  });
  // la scelta di un caso da decidere (radio o menu) si legge anche sul badge dell'elenco
  function badgeDec(c) {
    var id = c.getAttribute('data-did'), v = R.querySelector('.rev-voce[data-did=\"' + id + '\"]'); if (!v || v.querySelector('.rev-pill').getAttribute('data-avviso')) return;
    var sel = c.querySelector('input[type=radio]:checked'), val = sel ? sel.value : (c.querySelector('select') ? c.querySelector('select').value : '');
    var scelto = !!val && !c.querySelector('input[type=checkbox]:checked') || !!c.querySelector('input[type=checkbox]:checked');
    v.classList.toggle('applicata', !!scelto); v.querySelector('.rev-pill').textContent = scelto ? 'Inclusa' : 'Da controllare';
  }
  if (window.jQuery) window.jQuery(R).on('change', '.rev-cella-dec', function () { badgeDec(this); }); else R.addEventListener('change', function (ev) { var c = ev.target.closest('.rev-cella-dec'); if (c) badgeDec(c); });
  R.querySelector('.rev-q').addEventListener('input', function () { testo = this.value; mostra(); });
  document.addEventListener('click', function (ev) {
    var b = ev.target.closest && ev.target.closest('.rev-batch'); if (!b || !document.body.contains(R)) return;
    var s = b.getAttribute('data-on') === '1', ids = [];
    q('.rev-voce[data-cid]:not(.nascosta)', R).forEach(function (v) { var c = v.getAttribute('data-cid'); ids.push(parseInt(c, 10)); if (s) on[c] = true; else delete on[c]; });
    aggiorna(); tutte(); invia('rev_batch', { ids: ids, on: s });
  });
  q('.rev-f[data-f=tutte]', R).forEach(function (x) { x.classList.add('attivo'); });
  aggiorna(); mostra(); q('.rev-cella-dec', R).forEach(badgeDec);
  var prima = R.querySelector('.rev-voce'); if (prima) apri(prima);
})();
"
