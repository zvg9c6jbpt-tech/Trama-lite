# ──────────────────────────────────────────────
# verifiche/stati.R: le macchine a stati implicite dell'app, scritte una volta sola (18/11/2026)
# L'app Shiny tiene i suoi stati in combinazioni di variabili reattive (`caricato_per` + `numero_caricamento` + `dopo_salva`…): nessun
# posto dice «questa è una macchina a stati, ecco gli stati e le transizioni». `docs/stati.json` lo dice; da lì si genera `docs/STATI.md`
# (con i diagrammi) e questo collaudo controlla che:
#   1. ogni macchina sia coerente: stato iniziale dichiarato, transizioni fra stati dichiarati, ogni stato raggiungibile, nessun vicolo cieco
#      fuori dagli stati finali;
#   2. i nomi nel codice (variabili reattive, funzioni) esistano ancora nei file indicati: se uno si sposta o sparisce, cade il collaudo;
#   3. i collaudi citati esistano;
#   4. `docs/STATI.md` sia aggiornato (`TRAMA_STATI_SALVA=1 Rscript verifiche/stati.R` lo riscrive).
# Le lacune dichiarate (una transizione senza collaudo) si stampano ma non fanno cadere: sono il lavoro ancora da fare.
#     Rscript verifiche/stati.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
suppressMessages(library(jsonlite))
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
SORGENTE <- "docs/stati.json"; USCITA <- "docs/STATI.md"
M <- fromJSON(SORGENTE, simplifyVector = FALSE)$macchine
txt <- function(x) if (is.null(x)) "" else as.character(x)

# ── 1. coerenza del grafo ──
for (m in M) {
  st <- names(m$stati); tr <- m$transizioni
  da <- vapply(tr, function(t) t$da, ""); a <- vapply(tr, function(t) t$a, "")
  v(sprintf("%s: lo stato iniziale «%s» è dichiarato", m$id, m$iniziale), m$iniziale %in% st)
  v(sprintf("%s: ogni transizione va fra stati dichiarati", m$id), all(da %in% st) && all(a %in% st))
  vist <- m$iniziale; ripeti <- TRUE
  while (ripeti) { nuovi <- setdiff(a[da %in% vist], vist); ripeti <- length(nuovi) > 0; vist <- c(vist, nuovi) }
  v(sprintf("%s: ogni stato è raggiungibile dall'iniziale (manca: %s)", m$id, paste(setdiff(st, vist), collapse = ", ")), all(st %in% vist))
  finali <- st[vapply(m$stati, function(s) isTRUE(s$finale), NA)]
  senza_uscita <- setdiff(st, c(unique(da[da != a]), finali))
  v(sprintf("%s: nessun vicolo cieco fuori dagli stati finali (%s)", m$id, paste(senza_uscita, collapse = ", ")), !length(senza_uscita))
  v(sprintf("%s: ha almeno un invariante", m$id), length(m$invarianti) > 0)
}
v("gli id delle macchine sono unici", !anyDuplicated(vapply(M, function(m) m$id, "")))

# ── 2. i nomi esistono ancora nel codice ──
for (m in M) for (x in m$variabili) {
  ok <- file.exists(x$file) && any(grepl(sprintf("(^|[^A-Za-z0-9_.])%s[ \t]*(<-|=)", gsub("[.]", "[.]", x$nome)), readLines(x$file, warn = FALSE, encoding = "UTF-8")))
  v(sprintf("%s: «%s» è definito in %s", m$id, x$nome, x$file), ok)
}

# ── 3. i collaudi citati esistono ──
for (m in M) for (k in unlist(m$collaudi)) v(sprintf("%s: il collaudo %s esiste", m$id, k), file.exists(file.path("verifiche", k)))

# ── 4. docs/STATI.md è aggiornato ──
md <- function() {
  o <- c("# Le macchine a stati dell'app", "",
         "*Generato da `docs/stati.json` con `TRAMA_STATI_SALVA=1 Rscript verifiche/stati.R`: non si modifica a mano. Si cambia il JSON, si rigenera.*", "",
         "Un'app Shiny non ha un posto dove dire «questo è uno stato»: gli stati vivono in combinazioni di variabili reattive. Qui sono scritti una volta sola,",
         "con gli stati, le transizioni, le promesse che non devono rompersi (gli **invarianti**) e il collaudo che le protegge. `verifiche/stati.R` controlla che",
         "i nomi nel codice esistano ancora e che il grafo sia coerente.", "",
         "Prima di cambiare una di queste parti dell'app: leggi la macchina, scrivi quale transizione cambia, tieni veri gli invarianti.", "",
         "Lo stesso schema («azione sospesa → finestra → conferma o annulla») ricorre in Importa, Ripristino, Cancellazione e nella guardia delle modifiche: ",
         "la variabile che tiene l'azione sospesa è sempre `NULL` quando non c'è nulla in attesa, e ogni via d'uscita della finestra la rimette a `NULL`.", "",
         "| Macchina | Dove | Collaudo | Lacune |", "|---|---|---|---|")
  for (m in M) o <- c(o, sprintf("| [%s](#%s) | %s | %s | %s |", m$titolo, m$id, paste0("`", unique(vapply(m$variabili, function(x) x$file, "")), "`", collapse = ", "),
                                 if (length(m$collaudi)) paste0("`", unlist(m$collaudi), "`", collapse = ", ") else "**nessuno**", length(m$lacune)))
  for (m in M) {
    o <- c(o, "", sprintf("## %s", m$titolo), "", sprintf("<a id=\"%s\"></a>", m$id), "",
           sprintf("Variabili e funzioni che ne tengono lo stato: %s.", paste0("`", vapply(m$variabili, function(x) paste0(x$file, "::", x$nome), ""), "`", collapse = ", ")), "",
           "```mermaid", "stateDiagram-v2", sprintf("    [*] --> %s", m$iniziale))
    for (t in m$transizioni) o <- c(o, sprintf("    %s --> %s : %s", t$da, t$a, gsub("[:`«»]", "", substr(t$quando, 1, 48))))
    for (n in names(m$stati)) if (isTRUE(m$stati[[n]]$finale)) o <- c(o, sprintf("    %s --> [*]", n))
    o <- c(o, "```", "", "**Stati**", "")
    for (n in names(m$stati)) o <- c(o, sprintf("- `%s`%s: %s", n, if (isTRUE(m$stati[[n]]$finale)) " *(finale)*" else if (identical(n, m$iniziale)) " *(iniziale)*" else "", m$stati[[n]]$cosa))
    o <- c(o, "", "**Transizioni**", "", "| Da | A | Quando | Effetto |", "|---|---|---|---|")
    for (t in m$transizioni) o <- c(o, sprintf("| `%s` | `%s` | %s | %s |", t$da, t$a, t$quando, t$effetto))
    o <- c(o, "", "**Invarianti (non devono rompersi)**", "", paste0("- ", unlist(m$invarianti)))
    o <- c(o, "", "**Collaudi**: ", if (length(m$collaudi)) paste0("`", unlist(m$collaudi), "`", collapse = ", ") else "nessuno trovato.")
    if (length(m$lacune)) o <- c(o, "", "**Lacune**", "", paste0("- ", unlist(m$lacune)))
  }
  paste0(paste(o, collapse = "\n"), "\n")
}
if (nzchar(Sys.getenv("TRAMA_STATI_SALVA"))) { writeLines(sub("\n$", "", md()), USCITA, useBytes = TRUE); cat("riscritto", USCITA, "\n") }
v("docs/STATI.md è aggiornato (TRAMA_STATI_SALVA=1 per riscriverlo)", file.exists(USCITA) && identical(paste0(paste(readLines(USCITA, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "\n"), md()))

lacune <- unlist(lapply(M, function(m) if (length(m$lacune)) paste0(m$id, ": ", unlist(m$lacune))))
if (length(lacune)) { cat("\nLacune dichiarate (non fanno cadere il collaudo):\n"); cat(paste0("  - ", lacune), sep = "\n") }
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok -", length(M), "macchine a stati coerenti e agganciate al codice\n")
