# ──────────────────────────────────────────────
# verifiche/contratto_colonne.R: il contratto per colonna (22/10/2026, dal parere di Codex)
# Per ognuna delle 28 colonne del modello: che cosa si corregge da solo (A), che cosa si propone (P), che cosa non si tocca (N), esempi veri e
# come lo si verifica. Non è scritto due volte: le regole sono la tabella di `NORMALIZZAZIONE_LITE.md` §4, gli esempi i casi del corpus
# (`verifiche/corpus/normalizzazione.csv`, già provati da `corpus_normalizzazione.R`), i collaudi la mappa qui sotto.
# Il documento è `CONTRATTO_COLONNE.md`. Il collaudo controlla che sia aggiornato e completo; se cambia la tabella o il corpus:
#     TRAMA_CONTRATTO_SALVA=1 Rscript verifiche/contratto_colonne.R      (riscrive il documento)
#     Rscript verifiche/contratto_colonne.R                              atteso: ESITO: ok
# Controlli: ogni colonna ha almeno un caso nel corpus; ogni collaudo nominato esiste e nomina la colonna; il documento è quello generato.
# ──────────────────────────────────────────────
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
COL <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
# colonna → (parola con cui i collaudi la nominano, collaudi che la provano oltre al corpus)
MAPPA <- list(
  "Segnatura" = list("segnatura", c("normalizzazione_prova", "pulisci", "formati_vecchi")),
  "Altra segnatura" = list("altra segnatura", c("electron")),
  "Titolo" = list("titolo", c("normalizzazione_prova", "guida_nomi", "coerenza_fondo")),
  "Estremo remoto" = list("estremo remoto", c("date_precise", "normalizzazione_prova")),
  "Estremo recente" = list("estremo recente", c("date_precise", "normalizzazione_prova")),
  "definizione.tipologia" = list("definizione", c("funzioni", "roundtrip")),
  "Supporto (materiale)" = list("supporto", c("colonne", "funzioni")),
  "dimensioni/altezza" = list("dimensioni", c("funzioni", "modello_compilabile")),
  "dimensioni/larghezza" = list("dimensioni", c("funzioni", "modello_compilabile")),
  "tipo di numerazione" = list("numerazione", c("funzioni", "modello_compilabile")),
  "Numerazione" = list("numerazione", c("funzioni", "pulisci")),
  "epoca numerazione" = list("numerazione", c("funzioni", "modello_compilabile")),
  "consistenza.quantità" = list("consistenza", c("funzioni", "layout_xlsx")),
  "consistenza.unitàMisura" = list("consistenza", c("funzioni", "layout_xlsx")),
  "Tipologia documentaria" = list("tipologia documentaria", c("normalizzazione_prova", "formati_vecchi", "pulisci")),
  "Regesto o descrizione sintetica" = list("regesto", c("normalizzazione_prova", "guida_nomi", "coerenza_fondo")),
  "Persone citate o indicizzate" = list("persone citate", c("guida_nomi", "normalizzazione_prova", "pulisci")),
  "Luogo di conservazione" = list("luogo di conservazione", c("funzioni", "roundtrip")),
  "Luogo di produzione (data topica)" = list("luogo di produzione", c("guida_nomi", "normalizzazione_prova")),
  "Luoghi citati o indicizzati" = list("luoghi citati", c("normalizzazione_prova", "autocorrezione", "guida_nomi")),
  "Soggetto produttore" = list("soggetto produttore", c("correzioni_critica", "formati_vecchi")),
  "Soggetti citati o indicizzati" = list("soggetti citati", c("normalizzazione_prova", "guida_nomi")),
  "Soggettario" = list("soggettario", c("colonne", "pulisci", "normalizzazione_prova")),
  "Eventi" = list("eventi", c("pulisci", "layout_xlsx")),
  "Note libere" = list("note libere", c("normalizzazione_prova", "guida_nomi", "export_convenzioni")),
  "Lingua" = list("lingua", c("colonne", "pulisci", "normalizzazione_prova")),
  "Manifest IIIF" = list("manifest", c("roundtrip", "layout_xlsx")),
  "Identificativo immagine o cartella" = list("identificativo", c("roundtrip", "layout_xlsx")))
SEMPRE <- c("roundtrip", "salvataggio_neutro", "tab_reali")      # ogni colonna: l'andata e ritorno, il salvataggio che non cambia nulla, i tab veri
v("la mappa ha le 28 colonne del modello, nell'ordine", identical(names(MAPPA), COL))

# la tabella di §4: una riga per numero (o «8–14»), celle separate da «|» non preceduto da «\»
md <- readLines("NORMALIZZAZIONE_LITE.md", encoding = "UTF-8")
i0 <- grep("^## 4\\. Le colonne", md); i1 <- i0 + which(grepl("^---", md[(i0 + 1):length(md)]))[1]
righe <- md[(i0 + 1):i1]; righe <- righe[grepl("^\\| *([0-9]+|[0-9]+–[0-9]+) *\\|", righe)]
celle <- lapply(righe, function(r) { z <- trimws(strsplit(sub("^\\||\\|$", "", sub("\\s+$", "", r)), "(?<!\\\\)\\|", perl = TRUE)[[1]]); z <- gsub("\\\\\\|", "|", z); z })
numero <- vapply(celle, `[`, "", 1)
cerca <- function(n) { k <- which(vapply(celle, function(z) { a <- z[1]; if (grepl("–", a)) { r <- as.integer(strsplit(a, "–")[[1]]); n >= r[1] && n <= r[2] } else as.integer(a) == n }, TRUE)); celle[[k[1]]] }
v("la tabella di §4 copre tutte le 28 colonne", all(vapply(seq_along(COL), function(n) length(cerca(n)) >= 7L, TRUE)))
dec <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- gsub("\\n", "\n", x, fixed = TRUE)
  m <- gregexpr("\\\\u[0-9A-Fa-f]{4}", x); regmatches(x, m) <- lapply(regmatches(x, m), function(z) vapply(z, function(u) intToUtf8(strtoi(substring(u, 3), 16L)), "")); x }
corpus <- utils::read.csv2("verifiche/corpus/normalizzazione.csv", colClasses = "character", fileEncoding = "UTF-8", na.strings = character(0))
for (k in c("input", "atteso")) corpus[[k]] <- dec(corpus[[k]])
code <- function(x) { x <- gsub("\n", "↵", x, fixed = TRUE); if (!nzchar(x)) "(vuoto)" else paste0("`", gsub("`", "'", x), "`") }

out <- c("# Contratto per colonna: Trama Lite", "",
  "*Generato da `verifiche/contratto_colonne.R` (22/10/2026). Non si modifica a mano: le regole sono la tabella di `NORMALIZZAZIONE_LITE.md` §4, gli esempi i casi del corpus provati da `corpus_normalizzazione.R`.*", "",
  "**A** = si corregge da solo (e ogni correzione è tracciata: cella, prima, dopo, regola) · **P** = si propone, decide chi schedatura · **N** = non si tocca. Ogni colonna passa prima da L1 (Unicode, invisibili, spazi). Ogni regola è idempotente e neutra sul dato giusto.", "")
for (n in seq_along(COL)) {
  nome <- COL[n]; z <- cerca(n); z <- c(z, rep("", 7L - length(z)))
  cs <- corpus[corpus$colonna == nome, , drop = FALSE]
  v(sprintf("%s: almeno un caso nel corpus", nome), nrow(cs) >= 1L)
  sel <- c(head(which(cs$stato == "A"), 2L), head(which(cs$stato == "P"), 1L), head(which(cs$stato == "N"), 1L))
  esempi <- vapply(sel, function(r) sprintf("%s → %s (%s, caso %s)", code(cs$input[r]), if (identical(cs$atteso[r], "=")) "resta com'è" else code(cs$atteso[r]), cs$stato[r], cs$id[r]), "")
  col_ok <- MAPPA[[nome]]; coll <- col_ok[[2]]
  for (f in coll) v(sprintf("%s: il collaudo %s esiste e nomina la colonna", nome, f), file.exists(file.path("verifiche", paste0(f, ".R"))) &&
                      any(grepl(col_ok[[1]], readLines(file.path("verifiche", paste0(f, ".R")), encoding = "UTF-8", warn = FALSE), ignore.case = TRUE)))
  out <- c(out, sprintf("## %d. %s", n, nome), "",
    sprintf("- **Separatore:** %s", if (nzchar(z[3])) z[3] else "-"),
    sprintf("- **A (da solo):** %s", z[4]), sprintf("- **P (proposta):** %s", z[5]), sprintf("- **N (intoccabile):** %s", z[6]),
    if (nzchar(z[7])) sprintf("- **Nota:** %s", z[7]),
    sprintf("- **Esempi:** %s", if (length(esempi)) paste(esempi, collapse = " · ") else "-"),
    sprintf("- **Verifica:** corpus (casi %s) · %s · sempre: %s", paste(cs$id, collapse = ", "), paste0("`", coll, ".R`", collapse = ", "), paste0("`", SEMPRE, ".R`", collapse = ", ")), "")
}
nuovo <- paste(out[!vapply(out, is.null, TRUE)], collapse = "\n")
if (nzchar(Sys.getenv("TRAMA_CONTRATTO_SALVA"))) { writeLines(nuovo, "CONTRATTO_COLONNE.md", useBytes = TRUE); cat("riscritto: CONTRATTO_COLONNE.md\n")
} else v("CONTRATTO_COLONNE.md è aggiornato (TRAMA_CONTRATTO_SALVA=1 per riscriverlo)", file.exists("CONTRATTO_COLONNE.md") && identical(paste(readLines("CONTRATTO_COLONNE.md", encoding = "UTF-8", warn = FALSE), collapse = "\n"), nuovo))
if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
