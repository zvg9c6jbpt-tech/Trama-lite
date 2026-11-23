# ─────────────────────────────────────────────────────────────
# tema.R: carta calda, accento cremisi (18/11/2026, come il mockup) e navigazione laterale
# Titillium Web è in www/fonts e funziona senza rete; i caratteri di sistema sono il ripiego.
# ─────────────────────────────────────────────────────────────
library(bslib)
PILE_TESTO <- c("Titillium Web", "Helvetica Neue", "Helvetica", "Arial", "sans-serif")
PILE_TITOLI <- PILE_TESTO

PALETTE_TEMA <- list(
  # Superfici distinte su tre livelli (pagina grigio chiaro, riquadro carta, controllo rialzato `rialzo`), bordi leggibili anche nello scuro; testo antracite.
  # Un solo accento, il grafite, per l'azione, la voce attiva, il fuoco e le tinte; il rosso del pericolo vira al cremisi per non confondersi; i colori dei reati non cambiano.
  # Contrasti: primario su carta 8,8 e su pagina 7,7; testo su carta 13,8; secondario (chiaro) su carta 6,6 e su pagina 5,8; nello scuro primario 8,9 e testo 13,6.
  # 22/11/2026: pagina e fascia grigio caldo più neutro (#EFEDEB, #E8E4E2), meno beige.
  # 18/11/2026: carta calda e cremisi #9A1732 (primario su carta 8,2, su pagina 7,2); secondario #565656 su pagina 5,9; i grigi freddi sono tolti.
  chiaro = list(pagina = "#EFEDEB", carta = "#FDFCFC", bordo = "#D9D3D3", rialzo = "#F6F4F4",
                testo = "#212529", primario = "#9A1732", secondario = "#565656", segnale = "#89631F",
                verde = "#2F6B3F", pericolo = "#B23A00", controllo = "#8C8383", fascia = "#E8E4E2", ombra = "rgba(60,30,30,.08)"),
  scuro = list(pagina = "#161214", carta = "#1F1A1C", bordo = "#463D40", rialzo = "#2A2427",
               testo = "#ECE8E8", primario = "#F0A3B2", secondario = "#B3AAAD", segnale = "#E3B964",
               verde = "#7FCB92", pericolo = "#EC6156", controllo = "#7C7275", fascia = "#322A2D", ombra = "rgba(0,0,0,.5)")
)
# 18/11/2026: nello scuro il pericolo è un rosso (#EC6156, contrasto 5,2 su carta): il vermiglione #FF9A6B stava a ΔE 8 dall'arancio di Ebraismo (collaudo «colori reati»); ora ΔE >= 31 da ogni reato e 41 dal rosa del primario.
# `segnale`: il pallino «modifiche non salvate» sul pulsante Salva (colore primario), quindi l'altro accento della palette.

.css_pila <- function(p) paste(ifelse(grepl(" ", p), sprintf('"%s"', p), p), collapse = ", ")

.luminanza <- function(hex) {
  v <- grDevices::col2rgb(hex)[, 1] / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * v)
}
.contrasto <- function(a, b) {
  l <- sort(c(.luminanza(a), .luminanza(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}
.su_primario <- function(hex) if (.contrasto(hex, "#FFFFFF") >= 4.5) "#FFFFFF" else "#111111"

# Il marchio nella barra: la tessera con la «T» della presentazione e il nome. Colori dal tema (chiaro e scuro); nessun file immagine.
MARCHIO_TRAMA <- shiny::div(
  class = "marchio-trama", title = "Home: lo stato del fondo e da dove riprendere",
  shiny::span(class = "marchio-tessera", `aria-hidden` = "true", "T"),
  shiny::span("Trama Lite")
)

# Selettori davvero presenti in questa app. Il CSS sta in `tema/*.css` (undici file per argomento, in ordine di nome: l'ordine è quello della cascata),
# non più in una stringa di 1.300 righe: si legge una volta sola, all'avvio, e va nel tema come prima.
CSS_TEMA <- local({
  f <- sort(list.files("tema", pattern = "[.]css$", full.names = TRUE))
  if (!length(f)) stop("manca la cartella tema/ con i file CSS del tema")
  paste0("\n", paste(unlist(lapply(f, readLines, encoding = "UTF-8", warn = FALSE)), collapse = "\n"), "\n")
})

tema_app <- function(modo = c("chiaro", "scuro")) {
  modo <- match.arg(modo)
  p <- PALETTE_TEMA[[modo]]
  bs_theme(version = 5, bg = p$carta, fg = p$testo,
           primary = p$primario, secondary = p$secondario,
           base_font    = do.call(font_collection, as.list(PILE_TESTO)),
           code_font    = font_collection("ui-monospace", "Menlo", "Consolas", "monospace"),
           heading_font = do.call(font_collection, as.list(PILE_TITOLI)),
           font_scale = 1.0, "border-radius" = "0.375rem", "font-size-base" = "0.875rem",
           "body-emphasis-color" = p$testo,
           # Il testo secondario è un colore pieno: il 75 % del testo su un fondo tinto (intestazioni di gruppo in Qualità) stava a 4,3.
           "body-secondary-color" = p$secondario) |>
    bs_add_rules(c(sprintf(
      ":root { --rg-pagina: %s; --rg-bordo: %s; --rg-titoli: %s; --rg-su-primario: %s; --rg-segnale: %s; --rg-verde: %s; --rg-pericolo: %s; --rg-controllo: %s; --rg-fascia: %s; --rg-ombra: %s; --rg-rialzo: %s; }",
      p$pagina, p$bordo, .css_pila(PILE_TITOLI), .su_primario(p$primario), p$segnale, p$verde, p$pericolo, p$controllo, p$fascia, p$ombra, p$rialzo),
      CSS_TEMA, .css_pillole_reati(modo), .css_chip_reati(modo)))
}

# I reati nella colonna Reato di Fascicoli: il nome in testo, con un quadratino del colore del reato (senza riquadro).
.css_chip_reati <- function(modo = "chiaro") {
  tab <- COLORI_REATI[[modo]]
  colori <- vapply(names(tab), function(n) sprintf('.reato-chip[data-reato="%s"] { --c-reato: %s; }', n, tab[[n]]), "")
  paste(c(colori,
    ".reato-chip { display: inline-flex; align-items: center; gap: .35rem; padding: 0; margin: .1rem .9rem .1rem 0; border-radius: var(--rg-raggio); font-size: var(--rg-text-sm); font-weight: 500; line-height: 1.35; white-space: nowrap; color: var(--bs-body-color); border: 0; }",
    ".reato-chip::before { content: ''; width: .55rem; height: .55rem; border-radius: 1px; background: var(--c-reato, var(--rg-controllo)); flex: none; }",
    ""), collapse = "\n")
}

# Le pillole del Soggettario nel colore del loro reato (lo stesso dei grafici, `COLORI_REATI`, figure.R), chiaro o scuro secondo il tema.
# Il nome resta scritto: la pillola non scelta ha un pallino, quella scelta lo ha pieno, il fondo tinto, il bordo nel colore e la spunta. Un reato fuori elenco resta neutro.
# Solo per il campo Soggettario (data-per «reato»).
.css_pillole_reati <- function(modo = "chiaro") {
  tab <- COLORI_REATI[[modo]]
  base <- c('.campo-pillole[data-per="reato"]', '.campo-pillole[data-per$="-reato"]')
  con <- function(resto) paste(paste0(base, resto), collapse = ", ")
  colori <- vapply(names(tab), function(n) sprintf('%s { --c-reato: %s; }', con(sprintf(' .pillola[data-valore="%s"]', n)), tab[[n]]), "")
  paste(c(colori,
    sprintf("%s { display: inline-flex; align-items: center; gap: .4rem; }", con(" .pillola:not(.altro)")),
    sprintf("%s { content: ''; width: .6rem; height: .6rem; border-radius: 50%%; background: var(--c-reato, var(--rg-controllo)); flex: none; opacity: 1; margin: 0; }", con(" .pillola:not(.altro)::before")),
    sprintf("%s { background: color-mix(in srgb, var(--c-reato, var(--bs-primary)) 14%%, var(--bs-body-bg)); border-color: color-mix(in srgb, var(--c-reato, var(--bs-primary)) 60%%, var(--rg-bordo)); }", con(" .pillola.scelta:not(.altro)")),
    sprintf("%s { content: '\\2713'; width: auto; height: auto; border-radius: 0; background: none; color: var(--c-reato, currentColor); font-size: .9em; font-weight: 700; }", con(" .pillola.scelta:not(.altro)::before")),
    ""), collapse = "\n")
}


# Un passo di un percorso guidato (Importa, Esporta): numero, titolo e contenuto. `attesa`: passo non ancora raggiungibile (solo titolo, attenuato); `facoltativo`: lo dice accanto al titolo.
passo_guidato <- function(n, titolo, ..., attesa = FALSE, facoltativo = FALSE)
  div(class = paste("passo-guidato", if (attesa) "passo-attesa"),
      span(class = "numero-passo", n),
      div(class = "passo-corpo",
          div(class = "passo-titolo", titolo, if (facoltativo) span(class = "etichetta-sezione", "facoltativo")),
          ...))


# Il titolo di una pagina: lo stesso di Fascicoli, che ha in più il pulsante «Nuovo fascicolo».
titolo_pagina <- function(testo) div(class = "fasc-testa titolo-pagina", h1(class = "fasc-titolo", testo))


# Trascinare un foglio nella zona dell'import equivale a sceglierlo: i file lasciati cadere vanno nel campo del file. Un clic nella zona apre la finestra dei file.
JS_TRASCINA <- "
(function () {
  function prepara() {
    var z = document.querySelector('.zona-trascina'); if (!z || z._pronta) return; z._pronta = true;
    var campo = function () { return z.querySelector('input[type=file]'); };
    ['dragenter', 'dragover'].forEach(function (e) { z.addEventListener(e, function (ev) { ev.preventDefault(); z.classList.add('attiva'); }); });
    z.addEventListener('dragleave', function (ev) { ev.preventDefault(); z.classList.remove('attiva'); });
    z.addEventListener('drop', function (ev) {
      ev.preventDefault(); z.classList.remove('attiva');
      var f = ev.dataTransfer && ev.dataTransfer.files; if (!f || !f.length) return;
      var c = campo(); c.files = f; c.dispatchEvent(new Event('change', { bubbles: true }));
    });
    z.addEventListener('click', function (ev) { if (ev.target.closest('button, input, label, a')) return; campo().click(); });
  }
  document.addEventListener('DOMContentLoaded', prepara); $(document).on('shiny:connected', prepara); prepara();
})();"
