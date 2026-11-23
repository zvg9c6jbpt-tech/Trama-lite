# ─────────────────────────────────────────────────────────────
# verifiche/coerenza_fondo.R: il rapporto di coerenza di un fondo (8/10)
#
# Un fondo fatto di migliaia di schede non si controlla a occhio scheda per scheda: questo script lo guarda tutto
# e dice dove le cose non tornano. Non cambia niente (lavora su una COPIA del database, o su un foglio che importa in
# un database temporaneo), e produce un rapporto da aprire in Positron/RStudio o in Excel.
#
#   Rscript verifiche/coerenza_fondo.R "Tribunale dell'Inquisizione di Modena"            dal database di Trama Lite
#   Rscript verifiche/coerenza_fondo.R --foglio "Tab ASMo.xlsx"                          da un foglio, prima dell'import
#   Rscript verifiche/coerenza_fondo.R "Sant'Ufficio" --profondo --uscita rapporto.xlsx  con anche la ricomposizione
#   Rscript verifiche/coerenza_fondo.R --db altro.sqlite "Fondo"                         un altro database
#   Rscript verifiche/coerenza_fondo.R --foglio f.xlsx --uscita rapporto.html            una pagina web sola, da aprire con un doppio clic (l'estensione decide)
#   Rscript verifiche/coerenza_fondo.R                                                   senza argomenti: l'autoverifica
#
# In Positron o RStudio (così le tabelle si aprono nel visualizzatore dei dati):
#
#   source("verifiche/coerenza_fondo.R")
#   r <- coerenza_fondo("Sant'Ufficio")        # o coerenza_fondo(foglio = "Tab ASMo.xlsx")
#   View(r$riepilogo); View(r$numerazione); View(r$anni) …
#
# I controlli (ognuno è una tabella del rapporto, e un foglio dell'xlsx):
#   riepilogo            tutti i controlli con i casi, «Da vedere» in cima
#   qualita              i controlli di Qualità di Trama (R/qualita.R), e «schede_da_vedere» con le schede di ognuno
#   numerazione          buchi nei numeri di fascicolo (per busta, o su tutto il fondo se la numerazione è unica) e buste assenti
#   anni                 anni isolati (nessun'altra scheda a meno di 40 anni), anno finale prima dell'iniziale, intervalli di oltre 50 anni
#   senza_anno           schede senza data (e senza «n.d.» dichiarato)
#   schede_incomplete    schede senza reato, senza luogo o senza titolo (le persone no: possono mancare per regola)
#   riempimento          le 28 colonne del modello: quante schede le hanno vuote
#   vocabolario          valori delle colonne controllate che non sono nel vocabolario (nuovi, refusi, maiuscole, varianti)
#   luoghi               luoghi citati, e se stanno nella lista dei luoghi
#   grafie_vicine        persone e luoghi scritti in modo quasi uguale (probabili doppioni)
#   titoli_ripetuti      lo stesso titolo nello stesso anno in schede diverse
#   ricomposizione       (solo con --profondo) titoli e regesti composti che Trama oggi scriverebbe diversi: Salva li cambierebbe
# ─────────────────────────────────────────────────────────────

Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny); library(DBI)
  for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)
})

# ── piccole cose ──
.etichetta_scheda <- function(d) posizione_scritta(d$sigla_busta, d$busta, d$sigla_fascicolo, d$fascicolo, d$fascicolo_suffisso, d$busta_suffisso)
.intervalli <- function(n) {          # 3, 4, 5, 9 → «3–5, 9»
  n <- sort(unique(n)); if (!length(n)) return("")
  g <- cumsum(c(1, diff(n) != 1))
  paste(tapply(n, g, function(x) if (length(x) == 1L) as.character(x) else paste0(min(x), "–", max(x))), collapse = ", ")
}
.tab <- function(...) data.frame(..., stringsAsFactors = FALSE)
.vuota_tab <- function(cols) { d <- as.data.frame(matrix(character(0), 0, length(cols))); names(d) <- cols; d }

# ── i controlli: ognuno prende le schede del fondo (`d`) e/o la connessione, e dà una tabella ──
controlla_numerazione <- function(d) {
  vuoto <- data.frame(ambito = character(0), schede = integer(0), da = integer(0), a = integer(0), mancanti = character(0), numeri_mancanti = integer(0), stringsAsFactors = FALSE)
  if (!nrow(d)) return(vuoto)
  num <- d[!is.na(d$fascicolo), , drop = FALSE]
  # Se lo stesso numero (suffisso compreso) non compare in due buste, la numerazione corre su tutto il fondo: i buchi si cercano lì.
  unica <- nrow(num) > 1 && !anyDuplicated(paste(num$fascicolo, num$fascicolo_suffisso))
  gruppi <- if (unica) list("tutto il fondo (numerazione unica)" = num)
            else split(num, ifelse(is.na(num$busta), "(senza busta)", paste0(sigla_busta(num$sigla_busta), " ", num$busta, num$busta_suffisso)))
  r <- lapply(names(gruppi), function(k) {
    n <- unique(gruppi[[k]]$fascicolo); if (length(n) < 2) return(NULL)
    manca <- setdiff(seq(min(n), max(n)), n); if (!length(manca)) return(NULL)
    .tab(ambito = k, schede = nrow(gruppi[[k]]), da = as.integer(min(n)), a = as.integer(max(n)), mancanti = .intervalli(manca), numeri_mancanti = length(manca))
  })
  r <- do.call(rbind, r); r <- if (is.null(r)) vuoto else r
  # Le buste che mancano fra la prima e l'ultima.
  b <- unique(d$busta[!is.na(d$busta)])
  if (length(b) > 1) {
    mb <- setdiff(seq(min(b), max(b)), b)
    if (length(mb)) r <- rbind(r, .tab(ambito = "buste assenti fra la prima e l'ultima", schede = 0L, da = as.integer(min(b)), a = as.integer(max(b)),
                                        mancanti = .intervalli(mb), numeri_mancanti = length(mb)))
  }
  rownames(r) <- NULL; r
}

controlla_anni <- function(d, oggi = as.integer(format(Sys.Date(), "%Y"))) {
  # «Fuori dal periodo» = isolato: con almeno 20 schede datate, un anno la cui scheda più vicina (un'altra, anche dello stesso anno)
  # dista più di 40 anni. Un 1400 fra schede del 1560–1580 salta fuori; i fondi lunghi (ASMo, 1489–1784) non scattano a ogni estremo.
  v <- d$anno; ok <- !is.na(v); isolato <- rep(NA_character_, length(v))
  if (sum(ok) >= 20) {
    ys <- sort(v[ok])
    isolato[ok] <- vapply(which(ok), function(i) {
      altri <- ys[-match(v[i], ys)]; dmin <- min(abs(altri - v[i]))
      if (dmin > 40) sprintf("isolato: la scheda datata più vicina è a %d anni", dmin) else NA_character_
    }, "")
  }
  motivo <- ifelse(!is.na(v) & (v < 1000 | v > oggi), "anno impossibile",
            ifelse(!is.na(isolato), isolato,
            ifelse(!is.na(v) & !is.na(d$anno_a) & d$anno_a < v, "anno finale prima dell'iniziale",
            ifelse(!is.na(v) & !is.na(d$anno_a) & d$anno_a - v > 50, "intervallo di più di 50 anni", NA_character_))))
  i <- which(!is.na(motivo))
  if (!length(i)) return(.vuota_tab(c("segnatura", "titolo", "anno", "anno_finale", "motivo")))
  .tab(segnatura = .etichetta_scheda(d[i, ]), titolo = d$titolo[i], anno = d$anno[i], anno_finale = d$anno_a[i], motivo = motivo[i])
}

controlla_senza_anno <- function(d) {
  i <- which(is.na(d$anno) & (is.na(d$data_nd) | d$data_nd == 0))
  if (!length(i)) return(.vuota_tab(c("segnatura", "titolo")))
  .tab(segnatura = .etichetta_scheda(d[i, ]), titolo = d$titolo[i])
}

controlla_incomplete <- function(con, d, fondo) {
  n <- function(tab) { x <- dbGetQuery(con, sprintf("SELECT x.fascicolo_id AS id, COUNT(*) AS n FROM %s x JOIN fascicoli f ON f.id = x.fascicolo_id WHERE f.fondo = ? GROUP BY x.fascicolo_id", tab), list(fondo))
                       unname(stats::setNames(x$n, x$id)[d$id]) }
  # Le persone non contano: senza cognome non c'è persona (regola di Stefano), e un registro non ne ha. Quante schede ne sono senza lo dice «riempimento».
  nr <- n("reati"); nl <- n("luoghi")
  manca <- function(v, nome) ifelse(is.na(v), nome, "")
  senza <- cbind(manca(nr, "reato"), manca(nl, "luogo"), ifelse(is.na(d$titolo) | !nzchar(trimws(d$titolo)), "titolo", ""))
  txt <- apply(senza, 1, function(x) paste(x[nzchar(x)], collapse = ", "))
  i <- which(nzchar(txt))
  if (!length(i)) return(.vuota_tab(c("segnatura", "titolo", "senza")))
  .tab(segnatura = .etichetta_scheda(d[i, ]), titolo = d$titolo[i], senza = txt[i])
}

controlla_vocabolario <- function(con, fondo) {
  r <- lapply(seq_len(nrow(CAMPI_CONTROLLATI)), function(i) {
    v <- valori_colonna(con, fondo, CAMPI_CONTROLLATI$campo[i]); v <- v[v$stato != "ok", , drop = FALSE]
    if (!nrow(v)) return(NULL)
    .tab(colonna = CAMPI_CONTROLLATI$etichetta[i], valore = v$valore, schede = v$schede, stato = unname(ETICHETTA_STATO[v$stato]), forma_giusta = v$proposta)
  })
  r <- do.call(rbind, r); if (is.null(r)) .vuota_tab(c("colonna", "valore", "schede", "stato", "forma_giusta")) else r
}

controlla_luoghi <- function(con, ids) {
  if (!length(ids)) return(.vuota_tab(c("luogo", "citazioni", "nella_lista")))
  g <- dbGetQuery(con, sprintf("SELECT luogo AS nome, COUNT(*) AS occorrenze FROM luoghi WHERE fascicolo_id IN (%s) GROUP BY luogo",
                               paste(rep("?", length(ids)), collapse = ",")), params = as.list(ids))
  if (!nrow(g)) return(.vuota_tab(c("luogo", "citazioni", "nella_lista")))
  noti <- c(leggi_lista_luoghi()$chiave, chiave_grafia(forme_locali_luoghi()$locale))
  stato <- ifelse(chiave_grafia(g$nome) %in% noti, "sì", "no")
  out <- .tab(luogo = g$nome, citazioni = g$occorrenze, nella_lista = stato)
  out[order(match(out$nella_lista, c("no", "sì")), -out$citazioni, out$luogo), , drop = FALSE]
}

controlla_grafie <- function(con, fondo) {
  una <- function(tab) { v <- grafie_vicine(con, tab, fondo); if (!nrow(v)) return(NULL); .tab(tipo = tab, forme = if (is.list(v$forme)) vapply(v$forme, paste, "", collapse = " | ") else v$forme, n = v$n) }
  r <- do.call(rbind, list(una("persone"), una("luoghi"))); if (is.null(r)) .vuota_tab(c("tipo", "forme", "n")) else r
}

controlla_titoli_ripetuti <- function(d) {
  t <- ifelse(is.na(d$titolo), "", tolower(trimws(d$titolo))); k <- paste(t, d$anno)
  # «Atti per diversi» è il titolo di tutti i fascicoli con più soggetti: ripetuto per natura, non è un doppione.
  i <- which(nzchar(t) & !grepl(" per diversi$", t) & !is.na(d$anno) & k %in% k[duplicated(k)])
  if (!length(i)) return(.vuota_tab(c("titolo", "anno", "schede", "dove")))
  d2 <- d[i, ]; lab <- .etichetta_scheda(d2); g <- split(seq_along(i), k[i])
  r <- do.call(rbind, lapply(g, function(j) .tab(titolo = d2$titolo[j[1]], anno = d2$anno[j[1]], schede = length(j), dove = paste(lab[j], collapse = " | "))))
  rownames(r) <- NULL; r[order(-r$schede, r$anno), , drop = FALSE]
}

# Il più lento: ogni scheda a composizione automatica si rilegge e si ricompone. Se Salva la cambierebbe, il titolo o il regesto
# memorizzato non è più quello che le regole di oggi scrivono (è come si scoprirono le 16 «Denunce … circa» di Napoli).
controlla_ricomposizione <- function(con, d) {
  i <- which(d$titolo_manuale == 0 | d$regesto_manuale == 0)
  r <- lapply(i, function(k) {
    f <- leggi_fascicolo(con, d$id[k]); if (is.null(f)) return(NULL)
    out <- NULL
    if (isTRUE(d$titolo_manuale[k] == 0)) { tc <- componi_titolo_scheda(f); if (!is.na(tc) && !identical(tc, f$titolo)) out <- rbind(out, .tab(campo = "titolo", memorizzato = as.character(f$titolo), ricomposto = tc)) }
    if (isTRUE(d$regesto_manuale[k] == 0)) { rc <- componi_regesto_scheda(f); if (!is.na(rc) && !identical(rc, f$regesto)) out <- rbind(out, .tab(campo = "regesto", memorizzato = as.character(f$regesto), ricomposto = rc)) }
    if (is.null(out)) NULL else cbind(segnatura = .etichetta_scheda(d[k, ]), out, stringsAsFactors = FALSE)
  })
  r <- do.call(rbind, r); if (is.null(r)) .vuota_tab(c("segnatura", "campo", "memorizzato", "ricomposto")) else r
}

# ── il rapporto ──
# `con` è un database di Trama (già aperto, anche in memoria); `fondo` il suo nome.
rapporto_fondo <- function(con, fondo, profondo = FALSE) {
  d <- dbGetQuery(con, "SELECT id, busta, busta_suffisso, fascicolo, fascicolo_suffisso, sigla_busta, sigla_fascicolo, titolo, estremo_remoto AS anno,
                               estremo_recente AS anno_a, data_nd, titolo_manuale, regesto_manuale FROM fascicoli WHERE fondo = ? ORDER BY busta, busta_suffisso, fascicolo, fascicolo_suffisso", list(fondo))
  if (!nrow(d)) stop("Il fondo «", fondo, "» non ha schede.", call. = FALSE)
  d$sigla_busta <- as.character(d$sigla_busta); d$sigla_fascicolo <- as.character(d$sigla_fascicolo)
  ids <- d$id
  q <- qualita_ordinata(qualita(con, fondo))
  # le schede di ogni controllo di Qualità «Da vedere» che sa dirle (al più 3000 per controllo)
  sv <- lapply(which(q$gruppo == "Da vedere"), function(k) {
    x <- tryCatch(qualita_fascicoli(con, q$controllo[k], fondo), error = function(e) NULL)
    if (is.null(x) || !length(x)) return(NULL)
    j <- match(head(unique(x), 3000), d$id); j <- j[!is.na(j)]
    if (!length(j)) NULL else .tab(controllo = q$controllo[k], segnatura = .etichetta_scheda(d[j, ]), titolo = d$titolo[j])
  })
  sv <- do.call(rbind, sv); if (is.null(sv)) sv <- .vuota_tab(c("controllo", "segnatura", "titolo"))
  r <- list(
    qualita = q,
    schede_da_vedere = sv,
    numerazione = controlla_numerazione(d),
    anni = controlla_anni(d),
    senza_anno = controlla_senza_anno(d),
    schede_incomplete = controlla_incomplete(con, d, fondo),
    riempimento = riempimento_colonne(con, fondo),
    vocabolario = controlla_vocabolario(con, fondo),
    luoghi = controlla_luoghi(con, ids),
    grafie_vicine = controlla_grafie(con, fondo),
    titoli_ripetuti = controlla_titoli_ripetuti(d))
  if (profondo) r$ricomposizione <- controlla_ricomposizione(con, d)
  # il riepilogo
  casi <- function(x) nrow(x)
  lu <- r$luoghi; cit <- sum(lu$citazioni); nota <- sum(lu$citazioni[lu$nella_lista == "sì"])
  ries <- rbind(
    .tab(controllo = "Buchi nella numerazione dei fascicoli", casi = sum(r$numerazione$numeri_mancanti), foglio = "numerazione"),
    .tab(controllo = "Anni sospetti", casi = casi(r$anni), foglio = "anni"),
    .tab(controllo = "Schede senza anno", casi = casi(r$senza_anno), foglio = "senza_anno"),
    .tab(controllo = "Schede incomplete (senza reato, luogo o titolo)", casi = casi(r$schede_incomplete), foglio = "schede_incomplete"),
    .tab(controllo = "Valori fuori vocabolario", casi = casi(r$vocabolario), foglio = "vocabolario"),
    .tab(controllo = "Luoghi fuori dalla lista dei luoghi", casi = sum(lu$nella_lista != "sì"), foglio = "luoghi"),
    .tab(controllo = "Grafie quasi uguali (persone e luoghi)", casi = casi(r$grafie_vicine), foglio = "grafie_vicine"),
    .tab(controllo = "Titoli ripetuti nello stesso anno", casi = casi(r$titoli_ripetuti), foglio = "titoli_ripetuti"))
  if (profondo) ries <- rbind(ries, .tab(controllo = "Titoli o regesti che si ricomporrebbero diversi", casi = casi(r$ricomposizione), foglio = "ricomposizione"))
  qr <- q[q$controllo != "Luoghi fuori vocabolario" & q$controllo != "Reati fuori vocabolario", , drop = FALSE]
  ries <- rbind(ries, .tab(controllo = paste0("Qualità di Trama: ", qr$controllo), casi = qr$casi, foglio = "qualita"))
  ries$gruppo <- ifelse(ries$casi == 0, "A posto", ifelse(grepl("^Qualità di Trama: (Senza regesto strutturato|Tortura non esaminata|Sentenza non esaminata)", ries$controllo), "Lavoro di fondo", "Da vedere"))
  ries <- ries[order(match(ries$gruppo, c("Da vedere", "Lavoro di fondo", "A posto")), -ries$casi), c("gruppo", "controllo", "casi", "foglio")]
  rownames(ries) <- NULL
  r$riepilogo <- ries
  r$sintesi <- .tab(voce = c("fondo", "schede", "periodo", "citazioni di luoghi della lista"),
                    valore = c(fondo, nrow(d), if (any(!is.na(d$anno))) paste(range(d$anno, na.rm = TRUE), collapse = "–") else "n.d.",
                               if (cit) sprintf("%.0f%% (%d su %d)", 100 * nota / cit, nota, cit) else "n.d."))
  r[c("sintesi", "riepilogo", setdiff(names(r), c("sintesi", "riepilogo")))]
}

# Un database (la COPIA di quello vero, o un foglio importato in uno temporaneo) → il rapporto di un fondo.
coerenza_fondo <- function(fondo = NULL, foglio = NULL, db = NULL, profondo = FALSE, uscita = NULL) {
  tmp <- tempfile(fileext = ".sqlite"); on.exit(unlink(tmp), add = TRUE)
  if (!is.null(foglio)) {
    con <- apri_db(tmp); on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
    
    vero <- tryCatch({ f <- tempfile(fileext = ".sqlite"); file.copy(db %||% percorso_db(), f); f }, error = function(e) NULL)
    if (!is.null(vero) && file.exists(vero)) {          # le decisioni già prese per i fondi (vocabolario, varianti) valgono anche qui
      dv <- apri_db(vero); r0 <- dbGetQuery(dv, "SELECT fondo_id, campo, valore, forma, creato FROM vocabolario_fondo"); f0 <- dbGetQuery(dv, "SELECT * FROM fondi"); dbDisconnect(dv); unlink(vero)
      if (nrow(f0)) dbAppendTable(con, "fondi", f0)
      if (nrow(r0)) dbAppendTable(con, "vocabolario_fondo", r0)
    }
    a <- leggi_arianna(foglio); e <- importa_arianna(con, a, modo = "accoda")
    cat(sprintf("Foglio: %d righe lette, %d importate, %d respinte.\n", e$letti, e$importati, nrow(e$respinti)))
    fondi <- dbGetQuery(con, "SELECT fondo, COUNT(*) AS n FROM fascicoli GROUP BY fondo ORDER BY n DESC")$fondo
    if (is.null(fondo)) fondo <- fondi[1]
  } else {
    if (is.null(fondo)) stop("Dimmi il fondo (o --foglio): vedi l'intestazione di verifiche/coerenza_fondo.R.", call. = FALSE)
    file.copy(db %||% percorso_db(), tmp, overwrite = TRUE)            # la copia: mai il database vero, e le migrazioni si fanno sulla copia
    con <- apri_db(tmp); on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)
  }
  r <- rapporto_fondo(con, fondo, profondo)
  if (!is.null(uscita)) scrivi_rapporto(r, uscita)
  invisible(r)
}

# ── il rapporto come pagina web: un file solo, si apre con un doppio clic e si mostra a chi non usa R ──
.esc <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE)
}
# nome della tabella → (titolo, spiegazione in una riga, aperta per default quando ha casi)
.SEZIONI_RAPPORTO <- list(
  numerazione       = list("Numerazione", "Numeri di fascicolo che mancano, per busta o su tutto il fondo, e buste assenti.", TRUE),
  anni              = list("Anni sospetti", "Un anno isolato (nessun'altra scheda a meno di 40 anni), l'anno finale prima dell'iniziale, un intervallo di oltre 50 anni.", TRUE),
  senza_anno        = list("Schede senza anno", "Né una data né «n.d.» dichiarato.", TRUE),
  schede_incomplete = list("Schede incomplete", "Senza reato, senza luogo o senza titolo. Le persone no: possono mancare per regola.", TRUE),
  vocabolario       = list("Valori fuori vocabolario", "Refusi, maiuscole e valori nuovi nelle colonne controllate.", TRUE),
  grafie_vicine     = list("Grafie quasi uguali", "Persone e luoghi scritti in modo quasi uguale: probabili doppioni.", TRUE),
  titoli_ripetuti   = list("Titoli ripetuti", "Lo stesso titolo, nello stesso anno, in schede diverse.", TRUE),
  ricomposizione    = list("Ricomposizione", "Titoli e regesti che Trama oggi scriverebbe diversi da come sono memorizzati: Salva li cambierebbe.", TRUE),
  luoghi            = list("Luoghi citati", "Se stanno nella lista dei luoghi o no.", FALSE),
  qualita           = list("Qualità di Trama", "I controlli della scheda Qualità dell'app, con i casi di ciascuno.", FALSE),
  schede_da_vedere  = list("Schede da vedere", "Le schede di ogni controllo di Qualità «Da vedere».", FALSE),
  riempimento       = list("Riempimento delle 28 colonne", "Quante schede hanno ancora vuota ciascuna colonna del modello.", FALSE))

.CSS_RAPPORTO <- "
:root{--sfondo:#f6f4ef;--carta:#fff;--testo:#25221e;--tenue:#6c665d;--linea:#e2ddd2;--accento:#7a4a2a;
--vedere:#9a5b00;--vedere-bg:#fdf0d8;--fondo:#2c5e8a;--fondo-bg:#e3eef8;--posto:#2f6b3a;--posto-bg:#e4f2e6}
@media (prefers-color-scheme:dark){:root{--sfondo:#1c1a17;--carta:#26231f;--testo:#ece7de;--tenue:#a39b8d;--linea:#3a352e;--accento:#d9a679;
--vedere:#f0b865;--vedere-bg:#43331a;--fondo:#8ebbe6;--fondo-bg:#1f3345;--posto:#8fd09c;--posto-bg:#1f3a25}}
*{box-sizing:border-box}
body{margin:0;background:var(--sfondo);color:var(--testo);font:16px/1.5 Helvetica,Arial,sans-serif}
main{max-width:1000px;margin:0 auto;padding:24px 16px 56px}
h1{font-size:1.6rem;margin:0 0 4px}h2{font-size:1.15rem;margin:0}
.sotto{color:var(--tenue);margin:0 0 20px}
.sintesi{display:grid;grid-template-columns:repeat(auto-fit,minmax(200px,1fr));gap:12px;margin:0 0 28px}
.sintesi div{background:var(--carta);border:1px solid var(--linea);border-radius:10px;padding:12px 14px}
.sintesi b{display:block;font-size:1.25rem}.sintesi span{color:var(--tenue);font-size:.88rem}
.tab{overflow-x:auto;background:var(--carta);border:1px solid var(--linea);border-radius:10px}
table{border-collapse:collapse;width:100%;font-size:.92rem}
th,td{text-align:left;padding:8px 12px;border-bottom:1px solid var(--linea);vertical-align:top}
th{font-weight:600;color:var(--tenue);white-space:nowrap}tr:last-child td{border-bottom:0}
td.n,th.n{text-align:right;font-variant-numeric:tabular-nums}
tr.gruppo td{background:var(--sfondo);padding:10px 12px 6px}tr.gruppo .quanti{color:var(--tenue);font-size:.85rem;margin-left:6px}
.badge{display:inline-block;padding:1px 9px;border-radius:99px;font-size:.8rem;font-weight:600;white-space:nowrap}
.b-vedere{background:var(--vedere-bg);color:var(--vedere)}.b-fondo{background:var(--fondo-bg);color:var(--fondo)}.b-posto{background:var(--posto-bg);color:var(--posto)}
details{background:var(--carta);border:1px solid var(--linea);border-radius:10px;margin:12px 0}
summary{cursor:pointer;padding:12px 14px;list-style:none;display:flex;gap:10px;align-items:baseline;flex-wrap:wrap}
summary::-webkit-details-marker{display:none}summary::before{content:'▸';color:var(--accento)}details[open]>summary::before{content:'▾'}
summary .quanti{margin-left:auto;color:var(--tenue);font-size:.88rem}
details>p{margin:0;padding:0 14px 10px;color:var(--tenue);font-size:.92rem}
details .tab{border:0;border-top:1px solid var(--linea);border-radius:0 0 10px 10px}
a{color:var(--accento)}.piede{margin-top:32px;color:var(--tenue);font-size:.85rem}
@media print{body{background:#fff}details{break-inside:avoid}}
"

.html_tabella <- function(x, massimo = 200) {
  nota <- if (nrow(x) > massimo) sprintf("<p>Prime %d righe di %d: l'elenco completo è nel file xlsx del rapporto.</p>", massimo, nrow(x)) else ""
  x <- head(x, massimo)
  num <- vapply(x, is.numeric, logical(1))
  intest <- paste0("<th>", .esc(gsub("_", " ", names(x), fixed = TRUE)), "</th>", collapse = "")
  righe <- vapply(seq_len(nrow(x)), function(i)
    paste0("<tr>", paste0("<td", ifelse(num, " class=\"n\"", ""), ">", vapply(x[i, ], function(v) .esc(v), ""), "</td>", collapse = ""), "</tr>"), "")
  paste0("<div class=\"tab\"><table><thead><tr>", intest, "</tr></thead><tbody>", paste(righe, collapse = ""), "</tbody></table></div>", nota)
}

scrivi_rapporto_html <- function(r, file) {
  fondo <- r$sintesi$valore[r$sintesi$voce == "fondo"]
  ries <- r$riepilogo
  visibili <- names(.SEZIONI_RAPPORTO)[vapply(names(.SEZIONI_RAPPORTO), function(n) is.data.frame(r[[n]]) && nrow(r[[n]]) > 0, logical(1))]
  classe <- c("Da vedere" = "b-vedere", "Lavoro di fondo" = "b-fondo", "A posto" = "b-posto")
  sint <- r$sintesi[r$sintesi$voce != "fondo", , drop = FALSE]
  schede <- paste0("<div class=\"sintesi\">", paste0("<div><b>", .esc(sint$valore), "</b><span>", .esc(sint$voce), "</span></div>", collapse = ""), "</div>")
  righe_ries <- unlist(lapply(intersect(names(classe), unique(ries$gruppo)), function(g) {
    i <- which(ries$gruppo == g)
    c(paste0("<tr class=\"gruppo\"><td colspan=\"2\"><span class=\"badge ", classe[[g]], "\">", .esc(g), "</span> <span class=\"quanti\">", length(i), if (length(i) == 1) " controllo" else " controlli", "</span></td></tr>"),
      vapply(i, function(k) {
        f <- ries$foglio[k]
        vai <- if (ries$casi[k] > 0 && f %in% visibili) paste0("<a href=\"#", f, "\">", .esc(ries$controllo[k]), "</a>") else .esc(ries$controllo[k])
        paste0("<tr><td>", vai, "</td><td class=\"n\">", ries$casi[k], "</td></tr>")
      }, ""))
  }))
  riepilogo <- paste0("<div class=\"tab\"><table><thead><tr><th>Controllo</th><th class=\"n\">Casi</th></tr></thead><tbody>", paste(righe_ries, collapse = ""), "</tbody></table></div>")
  sezioni <- vapply(visibili, function(n) {
    s <- .SEZIONI_RAPPORTO[[n]]
    paste0("<details id=\"", n, "\"", if (s[[3]]) " open" else "", "><summary><h2>", .esc(s[[1]]), "</h2><span class=\"quanti\">", nrow(r[[n]]), if (nrow(r[[n]]) == 1) " riga" else " righe",
           "</span></summary><p>", .esc(s[[2]]), "</p>", .html_tabella(r[[n]]), "</details>")
  }, "")
  a_posto <- ries$controllo[ries$casi == 0]
  html <- paste0("<!doctype html><html lang=\"it\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
                 "<title>Rapporto di coerenza</title><style>", .CSS_RAPPORTO, "</style></head><body><main>",
                 "<h1>Rapporto di coerenza</h1><p class=\"sotto\">", .esc(fondo), "</p>", schede,
                 "<h2 style=\"margin-bottom:10px\">Che cosa c'è da guardare</h2>", riepilogo,
                 "<h2 style=\"margin:28px 0 0\">I casi, controllo per controllo</h2>", paste(sezioni, collapse = ""),
                 if (length(a_posto)) paste0("<p class=\"piede\">Controlli senza casi: ", .esc(paste(a_posto, collapse = " · ")), ".</p>") else "",
                 "<p class=\"piede\">Fatto da Trama (verifiche/coerenza_fondo.R) il ", format(Sys.Date(), "%d/%m/%Y"),
                 ". Il rapporto non cambia niente: lavora su una copia o su un foglio importato in un database temporaneo.</p></main></body></html>")
  writeLines(html, file, useBytes = FALSE); invisible(file)
}

scrivi_rapporto <- function(r, file) {
  if (grepl("[.]html?$", file, ignore.case = TRUE)) return(scrivi_rapporto_html(r, file))
  wb <- openxlsx::createWorkbook(creator = "Trama")
  stile <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E8EEF8", border = "bottom")
  for (nome in names(r)) {
    x <- r[[nome]]; if (!is.data.frame(x) || (!nrow(x) && !nome %in% c("riepilogo", "sintesi"))) next
    nome_foglio <- substr(nome, 1, 31)
    openxlsx::addWorksheet(wb, nome_foglio); openxlsx::writeData(wb, nome_foglio, x, headerStyle = stile)
    openxlsx::freezePane(wb, nome_foglio, firstRow = TRUE)
    if (nrow(x)) openxlsx::addFilter(wb, nome_foglio, rows = 1, cols = seq_len(ncol(x)))
    openxlsx::setColWidths(wb, nome_foglio, cols = seq_len(ncol(x)), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE); invisible(file)
}

stampa_rapporto <- function(r) {
  cat("\n", paste(r$sintesi$voce, r$sintesi$valore, sep = ": ", collapse = " · "), "\n\n", sep = "")
  rr <- r$riepilogo
  cat(sprintf("%-16s %7s  %-78s %s\n", "gruppo", "casi", "controllo", "foglio"))
  cat(sprintf("%-16s %7d  %-78s %s\n", rr$gruppo, rr$casi, substr(rr$controllo, 1, 78), rr$foglio), sep = "")
}

# ── l'autoverifica: senza argomenti il programma si prova su un fondo inventato con difetti noti ──
autoverifica <- function() {
  esiti <- new.env(); esiti$falliti <- character(0)
  controlla <- function(nome, ok) { cat(if (isTRUE(ok)) "ok        " else "NON TORNA ", nome, "\n"); if (!isTRUE(ok)) esiti$falliti <- c(esiti$falliti, nome) }
  con <- apri_db(":memory:"); 
  nuova <- function(busta, num, anno, titolo = NULL, reati = "Eresia", luoghi = "Roma", persone = "Rossi, Mario", anno_a = NA, id = NULL) {
    f <- fascicolo_vuoto(); f$id <- id %||% sprintf("p-%d-%d", busta, num); f$archivio <- "Archivio Prova"; f$fondo <- "Prova"; f$busta <- busta; f$fascicolo <- num
    f$estremo_remoto <- anno; f$estremo_recente <- anno_a; f$titolo <- titolo %||% paste("Processo contro Persona", busta, num); f$titolo_manuale <- 1L
    f$reati <- reati; f$luoghi <- luoghi; f$persone <- persone; f$tipologia_documentaria <- "Processi"; f$lingue <- "Italiano"; salva_fascicolo(con, f)
  }
  for (n in c(1:2, 4:20)) nuova(1L, n, 1560L + n %% 20)                                       # busta 1: manca il fascicolo 3
  for (n in c(21:22, 25:26)) nuova(2L, n, 1570L)                                               # busta 2: mancano 23 e 24
  nuova(3L, 27L, 1400L, anno_a = NA)                                                           # un anno fuori periodo
  nuova(3L, 28L, 1575L, anno_a = 1570L)                                                        # anno finale prima dell'iniziale
  nuova(3L, 29L, 1575L, reati = character(0), luoghi = character(0))                           # senza reato né luogo
  nuova(3L, 30L, 1572L, titolo = "Atti circa Stesso Titolo"); nuova(3L, 31L, 1572L, titolo = "Atti circa Stesso Titolo")   # titolo ripetuto
  nuova(3L, 32L, 1573L, luoghi = c("Roma", "Posto Che Non Esiste"), persone = c("Verdi, Paolo", "Verdi,Paolo"))      # luogo fuori dalla lista, persona due volte
  nuova(3L, 33L, 1574L, reati = c("Magia."))                                                   # un refuso nel reato
  nuova(3L, 34L, NA_integer_)                                                                  # senza anno
  r <- rapporto_fondo(con, "Prova", profondo = TRUE)
  controlla("il rapporto ha le tabelle promesse", all(c("sintesi", "riepilogo", "qualita", "schede_da_vedere", "numerazione", "anni", "senza_anno", "schede_incomplete", "riempimento", "vocabolario", "luoghi", "grafie_vicine", "titoli_ripetuti", "ricomposizione") %in% names(r)))
  controlla("numerazione: manca il 3 in busta 1 e 23–24 in busta 2",
            identical(r$numerazione$mancanti[match(c("b. 1", "b. 2"), r$numerazione$ambito)], c("3", "23–24")) || any(grepl("3", r$numerazione$mancanti)) && any(grepl("23", r$numerazione$mancanti)))
  controlla("numerazione unica: i buchi si cercano su tutto il fondo", any(grepl("numerazione unica", r$numerazione$ambito)))
  controlla("anni: il 1400 è isolato, gli altri no", any(r$anni$anno == 1400 & grepl("isolato", r$anni$motivo)) && !any(grepl("isolato", r$anni$motivo[r$anni$anno != 1400])))
  controlla("anni: l'anno finale prima dell'iniziale", any(grepl("anno finale prima", r$anni$motivo)))
  controlla("senza anno: la scheda 34", nrow(r$senza_anno) == 1 && grepl("34", r$senza_anno$segnatura))
  controlla("schede incomplete: senza reato e luogo", any(grepl("reato", r$schede_incomplete$senza) & grepl("luogo", r$schede_incomplete$senza)))
  controlla("vocabolario: «Magia.» è un refuso di «Magia»", any(r$vocabolario$valore == "Magia." & r$vocabolario$forma_giusta == "Magia"))
  controlla("luoghi: «Posto Che Non Esiste» è fuori dalla lista, Roma ci sta", with(r$luoghi, nella_lista[luogo == "Posto Che Non Esiste"] == "no" && nella_lista[luogo == "Roma"] == "sì"))
  controlla("grafie vicine: Verdi, Paolo scritto in due modi", any(r$grafie_vicine$tipo == "persone" & grepl("Verdi", r$grafie_vicine$forme)))
  controlla("titoli ripetuti: lo stesso titolo nello stesso anno", nrow(r$titoli_ripetuti) == 1 && r$titoli_ripetuti$schede == 2)
  controlla("il riepilogo mette «Da vedere» in cima", r$riepilogo$gruppo[1] == "Da vedere" && all(r$riepilogo$casi[r$riepilogo$gruppo == "Da vedere"] > 0))
  controlla("la sintesi dice il fondo, le schede e il periodo", identical(r$sintesi$valore[1:2], c("Prova", as.character(nrow(dbGetQuery(con, "SELECT 1 FROM fascicoli"))))) && grepl("1400", r$sintesi$valore[3]))
  f <- tempfile(fileext = ".xlsx"); scrivi_rapporto(r, f)
  fogli <- openxlsx::getSheetNames(f)
  controlla("l'xlsx ha il riepilogo per primo e un foglio per ogni controllo con casi", fogli[1] == "sintesi" && "riepilogo" %in% fogli && "numerazione" %in% fogli && "anni" %in% fogli)
  h <- tempfile(fileext = ".html"); scrivi_rapporto(r, h); testo <- paste(readLines(h, warn = FALSE), collapse = "\n")
  controlla("la pagina web: fondo, sintesi e una sezione per ogni controllo con casi", grepl("Prova", testo, fixed = TRUE) && grepl("id=\"numerazione\"", testo, fixed = TRUE) && grepl("id=\"anni\"", testo, fixed = TRUE) && grepl("Posto Che Non Esiste", testo, fixed = TRUE))
  r2 <- r; r2$senza_anno$segnatura[1] <- "<script>alert(1)</script> & Co."; h2 <- tempfile(fileext = ".html"); scrivi_rapporto(r2, h2); t2 <- paste(readLines(h2, warn = FALSE), collapse = "\n")
  controlla("la pagina web: il testo delle schede è protetto (un tag scritto in un dato non diventa HTML)", grepl("&lt;script&gt;alert(1)&lt;/script&gt; &amp; Co.", t2, fixed = TRUE) && !grepl("<script>alert", t2, fixed = TRUE))
  controlla("la pagina web: «Da vedere» prima di «A posto»", regexpr("b-vedere", testo, fixed = TRUE) < regexpr("b-posto", testo, fixed = TRUE))
  # il fondo di esempio, per intero, dal database (copia) e dal foglio
  e <- apri_db(":memory:"); invisible(carica_esempio(e))
  fe <- dbGetQuery(e, "SELECT DISTINCT fondo FROM fascicoli")$fondo
  re <- rapporto_fondo(e, fe); dbDisconnect(e)
  controlla("l'esempio: 24 schede, e le sue due incomplete si vedono", re$sintesi$valore[2] == "24" && nrow(re$schede_incomplete) >= 2)
  # il database non si tocca: si prova su un database temporaneo, con la funzione che lavora su una copia
  dbfile <- tempfile(fileext = ".sqlite"); k <- apri_db(dbfile); invisible(carica_esempio(k)); dbDisconnect(k)
  vuoto <- tempfile(fileext = ".sqlite"); k <- apri_db(vuoto); dbDisconnect(k)
  rf <- suppressMessages(coerenza_fondo(foglio = "dati/esempio/esempio.xlsx", db = vuoto))
  controlla("dal foglio, prima dell'import: lo stesso fondo e lo stesso numero di schede", identical(rf$sintesi$valore[1:2], re$sintesi$valore[1:2]))
  prima <- unname(tools::md5sum(dbfile)); rd <- coerenza_fondo(fe, db = dbfile)
  controlla("da un database: lo stesso rapporto, e il file di partenza resta identico (si lavora su una copia)", identical(rd$sintesi$valore[1:2], re$sintesi$valore[1:2]) && identical(unname(tools::md5sum(dbfile)), prima))
  # il pacchetto verifica-dati/: la copia dell'esempio con difetti messi apposta li fa scattare tutti (si rifà con verifica-dati/genera_fondo_con_difetti.R)
  if (file.exists("verifica-dati/fondo-con-difetti.xlsx")) {
    rd2 <- suppressMessages(coerenza_fondo(foglio = "verifica-dati/fondo-con-difetti.xlsx", db = vuoto))
    controlla("verifica-dati: la numerazione ha i buchi 5 e 18–19", any(grepl("5, 18–19", rd2$numerazione$mancanti, fixed = TRUE)))
    controlla("verifica-dati: un anno isolato (1480) e un anno finale prima dell'iniziale", any(rd2$anni$anno == 1480 & grepl("isolato", rd2$anni$motivo)) && any(grepl("anno finale prima", rd2$anni$motivo)))
    controlla("verifica-dati: una scheda senza anno", nrow(rd2$senza_anno) == 1)
    controlla("verifica-dati: «Magia.» è un refuso", any(rd2$vocabolario$valore == "Magia." & rd2$vocabolario$forma_giusta == "Magia"))
    controlla("verifica-dati: un luogo fuori dalla lista", any(rd2$luoghi$luogo == "Castelvecchio dei Sassi" & rd2$luoghi$nella_lista == "no"))
    controlla("verifica-dati: la stessa persona scritta in due modi", any(rd2$grafie_vicine$tipo == "persone" & grepl("Lodì", rd2$grafie_vicine$forme)))
    controlla("verifica-dati: un titolo ripetuto nello stesso anno", nrow(rd2$titoli_ripetuti) == 1)
  }
  dbDisconnect(con)
  cat("\n"); if (length(esiti$falliti)) { cat("ESITO: NON TORNA\n"); if (!interactive()) quit(status = 1) else stop("verifiche fallite") }
  cat("ESITO: ok\n")
}

# ── da riga di comando ──
principale <- function(a) {
  if (!length(a)) return(autoverifica())
  val <- function(k) { i <- match(k, a); if (is.na(i) || i == length(a)) NULL else a[i + 1] }
  flag <- c("--foglio", "--db", "--uscita"); i <- which(a %in% flag); resto <- if (length(i)) a[-c(i, i + 1)] else a
  profondo <- "--profondo" %in% resto; resto <- setdiff(resto, "--profondo")
  fondo <- if (length(resto)) resto[1] else NULL
  uscita <- val("--uscita") %||% paste0("coerenza-", gsub("[^[:alnum:]]+", "-", fondo %||% "foglio"), ".xlsx")
  r <- coerenza_fondo(fondo, foglio = val("--foglio"), db = val("--db"), profondo = profondo, uscita = uscita)
  stampa_rapporto(r); cat("\nRapporto completo:", normalizePath(uscita), "\n")
}
if (!interactive() && sys.nframe() == 0L) principale(commandArgs(TRUE))
