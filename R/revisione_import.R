# ─────────────────────────────────────────────────────────────
# revisione_import.R: controllare e correggere PRIMA di importare
# L'import normale fa entrare un foglio com'è; qui il foglio si legge, si propongono correzioni cella per cella e cose da decidere,
# e si scrive solo dopo la conferma. Funzioni pure sui dati del foglio (`a$dati`); la finestra è in app.R.
#   proposte_import(a, con, fondo)      → list(celle, decisioni)
#   applica_proposte(a, pr, ok, scelte) → il foglio `a` con le correzioni spuntate e le decisioni prese
#   evidenzia_diff(prima, dopo)         → HTML con le parole tolte (barrate) e messe (evidenziate)
# ─────────────────────────────────────────────────────────────

# (ANNI_AMMESSI - gli anni ammessi, 1200–1899 - sta in arianna_import.R: lo usa anche la lettura dei fogli.)

# Parole tolte e messe fra due testi: separatori e parole sono pezzi a sé, si cerca la parte comune più lunga.
.pezzi_diff <- function(x) { x <- as.character(x); if (!nzchar(x)) character(0) else regmatches(x, gregexpr("[^|;, ]+|[|;, ]", x))[[1]] }
DIFF_MAX_CELLE <- 90000          # circa 300 parole contro 300 parole
DIFF_BUDGET_TOTALE <- 8e6          # somma dei costi di tutte le celle di una finestra (~2 s): oltre, le celle restanti si mostrano senza evidenziare
evidenzia_diff <- function(prima, dopo) {
  a <- .pezzi_diff(prima); b <- .pezzi_diff(dopo); n <- length(a); m <- length(b)
  # Il confronto costa il prodotto delle lunghezze: un testo molto lungo (una nota) si mostra intero, senza evidenziare.
  if (as.numeric(n) * m > DIFF_MAX_CELLE) return(list(prima = htmltools::htmlEscape(as.character(prima)), dopo = htmltools::htmlEscape(as.character(dopo))))
  L <- matrix(0L, n + 1L, m + 1L)
  for (i in rev(seq_len(n))) for (j in rev(seq_len(m))) L[i, j] <- if (a[i] == b[j]) L[i + 1L, j + 1L] + 1L else max(L[i + 1L, j], L[i, j + 1L])
  i <- 1L; j <- 1L; out_a <- character(0); out_b <- character(0)
  esc <- function(z) htmltools::htmlEscape(z)
  while (i <= n || j <= m) {
    if (i <= n && j <= m && a[i] == b[j]) { out_a <- c(out_a, esc(a[i])); out_b <- c(out_b, esc(b[j])); i <- i + 1L; j <- j + 1L }
    else if (j <= m && (i > n || L[i, j + 1L] >= L[i + 1L, j])) { out_b <- c(out_b, sprintf("<ins>%s</ins>", esc(b[j]))); j <- j + 1L }
    else { out_a <- c(out_a, sprintf("<del>%s</del>", esc(a[i]))); i <- i + 1L }
  }
  list(prima = paste(out_a, collapse = ""), dopo = paste(out_b, collapse = ""))
}

# Colonne a vocabolario chiuso corrette in anticipo, con il nome della colonna del foglio e come si dividono le voci.
.COLONNE_VOCABOLARIO <- function(a) {
  cc <- data.frame(campo = c("reato", "lingua", "supporto", "definizione"), sep = c("|", ";|,", NA, NA), stringsAsFactors = FALSE)
  cc$colonna <- vapply(cc$campo, function(k) {
    nome <- PROFILO_MODELLO$arianna[!is.na(PROFILO_MODELLO$campo) & PROFILO_MODELLO$campo == k][1]
    x <- if (is.na(nome)) NULL else unname(a$colonna[nome]); if (is.null(x) || !length(x) || is.na(x) || !x %in% names(a$dati)) NA_character_ else x
  }, "")
  cc[!is.na(cc$colonna), , drop = FALSE]
}

# ── Controlli sulle segnature ─────────────────────────────
# Sequenza: nel foglio busta e fascicolo crescono (il 14 non viene prima del 12). Si segnala, non si riordina.
.segnature_fuori_sequenza <- function(segnature, righe = seq_along(segnature)) {
  out <- list(); massimo <- list()
  for (k in seq_along(segnature)) {
    s <- tryCatch(analizza_segnatura(segnature[k]), error = function(e) NULL)
    if (is.null(s) || is.na(s$busta) || is.na(s$fascicolo)) next
    # «m.1, inv.» e «m.1, non inv.» sono due serie: la qualifica dopo la virgola sta nel gruppo, il resto del suffisso («-bis») nell'ordine.
    qual <- sub("^[^,]*", "", s$busta_suffisso %||% ""); base_b <- sub(",.*$", "", s$busta_suffisso %||% "")
    gruppo <- paste(s$archivio, s$fondo, s$serie, qual, sep = "\r")
    chiave <- c(s$busta, .rango_suffisso(base_b), s$fascicolo, .rango_suffisso(s$fascicolo_suffisso %||% ""))
    m <- massimo[[gruppo]]
    if (!is.null(m)) {
      d <- which(chiave != m$chiave)[1]
      if (!is.na(d) && chiave[d] < m$chiave[d]) {
        out[[length(out) + 1L]] <- data.frame(riga = righe[k] + 1L,
          testo = sprintf("fuori sequenza: b. %s fasc. %s viene dopo b. %s fasc. %s", s$busta, s$fascicolo, m$busta, m$fascicolo), stringsAsFactors = FALSE)
        next
      }
    }
    massimo[[gruppo]] <- list(chiave = chiave, busta = s$busta, fascicolo = s$fascicolo)
  }
  if (length(out)) do.call(rbind, out) else data.frame(riga = integer(0), testo = character(0))
}

# Il tratto dopo «per» somiglia a un elenco di reati? Una classe è una o due parole, senza articoli, preposizioni o apostrofi; due parole maiuscole sono una persona.
# Un regesto in prosa («per la divisione dei beni») non è a formula e non si confronta col vocabolario.
.PAROLE_FUNZIONALI <- c("di", "del", "dello", "della", "dei", "degli", "delle", "al", "allo", "alla", "ai", "agli", "alle", "nel", "nello", "nella", "nei", "negli",
                        "nelle", "sul", "sulla", "sui", "sugli", "sulle", "da", "dal", "dalla", "dai", "il", "lo", "la", "i", "gli", "le", "un", "una", "con", "su", "in",
                        "tra", "fra", "che", "a", "o", "come", "dopo", "contro")
.sembra_un_reato <- function(x) {
  pezzi <- trimws(strsplit(as.character(x), "\\s+(?:per|e|ed)\\s+", perl = TRUE)[[1]]); pezzi <- pezzi[nzchar(pezzi)]
  length(pezzi) > 0L && all(vapply(pezzi, function(p) {
    if (grepl("['\u2019]", p)) return(FALSE)
    w <- strsplit(p, "\\s+")[[1]]
    length(w) <= 2L && !any(tolower(w) %in% .PAROLE_FUNZIONALI) && !(length(w) == 2L && all(grepl("^[[:upper:]]", w)))
  }, TRUE))
}

# I reati nominati in un regesto o titolo composto: solo le classi del vocabolario. Restituisce le parti del tratto, ciascuna con la sua classe (esatta, vicina o nessuna).
.reati_nel_testo <- function(testo, classi) {
  if (.vuoto(testo)) return(NULL)
  t <- trimws(sub("[.]\\s*$", "", as.character(testo)))
  if (grepl("[\n:]", t)) return(NULL)                                   # strutturato dei cataloghi: non si tocca
  coda <- if (grepl(" per diversi per ", t, ignore.case = TRUE)) sub("^.* per diversi per ", "", t, ignore.case = TRUE)
          else if (grepl(" per .+? (contro|circa) ", t, ignore.case = TRUE)) sub("^.*? per (.+?) (contro|circa) .*$", "\\1", t, ignore.case = TRUE, perl = TRUE)
          else if (grepl(" per ", t, ignore.case = TRUE)) sub("^.*? per ", "", t, ignore.case = TRUE, perl = TRUE) else return(NULL)
  if (!nzchar(coda) || grepl("^(diversi|ignot)", coda, ignore.case = TRUE)) return(NULL)
  parti <- character(0)
  for (seg in trimws(strsplit(coda, ";", fixed = TRUE)[[1]])) {
    # «e», e «ed» solo davanti a un reato che comincia per «e» («Eresia ed Ebraismo»): sono congiunzioni, non parte della classe.
    p <- strsplit(seg, "\\s+(?:e|ed(?=\\s+[eE]))\\s+", perl = TRUE)[[1]]; i <- 1L
    while (i <= length(p)) {
      ok <- FALSE
      for (j in length(p):i) { cand <- paste(p[i:j], collapse = " e "); if (tolower(cand) %in% classi) { parti <- c(parti, cand); i <- j + 1L; ok <- TRUE; break } }
      if (!ok) { parti <- c(parti, p[i]); i <- i + 1L }
    }
  }
  r <- data.frame(parte = parti, classe = vapply(parti, function(x) {
    k <- tolower(x); if (k %in% classi) return(k)
    d <- drop(utils::adist(k, classi)); v <- which(d <= 2L & nchar(k) >= 5L)
    if (length(v) == 1L) classi[v] else NA_character_ }, ""), stringsAsFactors = FALSE, row.names = NULL)
  nc <- r$parte[is.na(r$classe)]
  if (length(nc) && !all(vapply(nc, .sembra_un_reato, TRUE))) return(NULL)    # prosa, non una formula: niente confronto col vocabolario
  attr(r, "coda") <- coda       # il tratto dei reati com'è scritto: serve a riscriverlo
  r
}

# Il soggettario di una riga contro i reati che dice il suo regesto («Processo per eresia contro…»): per «Pulisci un foglio», che non ha la finestra di revisione.
# Si confronta solo quando il regesto è una formula con classi del vocabolario e ogni voce del soggettario è una classe (o un refuso vicino): il resto lo dicono altri controlli.
# Restituisce riga e testo; non cambia niente.
avvisi_reati <- function(a) {
  a$colonna <- .con_nomi_modello(a$colonna, a$profilo)
  d <- a$dati; out <- data.frame(riga = integer(0), testo = character(0), stringsAsFactors = FALSE)
  c_reg <- unname(a$colonna["Regesto o descrizione sintetica"]); c_sog <- unname(a$colonna["Soggettario"]); c_seg <- unname(a$colonna["Segnatura"])
  if (any(is.na(c(c_reg, c_sog))) || !all(c(c_reg, c_sog) %in% names(d))) return(out)
  classi <- tolower(unique(vocabolario("reato")))
  classe_di <- function(x) { k <- tolower(trimws(x)); if (k %in% classi) return(k); dd <- drop(utils::adist(k, classi)); v <- which(dd <= 2L & nchar(k) >= 5L); if (length(v) == 1L) classi[v] else NA_character_ }
  for (i in seq_len(nrow(d))) {
    pr <- .reati_nel_testo(d[[c_reg]][i], classi); if (is.null(pr) || !nrow(pr) || anyNA(pr$classe)) next
    dal_regesto <- sort(unique(pr$classe))
    sog <- as.character(d[[c_sog]][i]); voci <- if (.vuoto(sog)) character(0) else trimws(unlist(strsplit(sog, "[|;]")))
    voci <- voci[nzchar(voci)]
    nel_sog <- vapply(voci, classe_di, ""); if (anyNA(nel_sog)) next
    nel_sog <- sort(unique(nel_sog))
    if (identical(nel_sog, dal_regesto)) next
    s <- if (!is.na(c_seg)) as.character(d[[c_seg]][i]) else ""
    dove <- paste0("riga ", i + 1L, if (nzchar(s) && !is.na(s)) paste0(" (", trimws(sub("^.*,", "", s)), ")") else "")
    out[nrow(out) + 1L, ] <- list(i + 1L, sprintf("%s: il regesto parla di %s, il soggettario ha %s", dove,
                                                   paste(sprintf("«%s»", dal_regesto), collapse = " e "),
                                                   if (length(nel_sog)) paste(sprintf("«%s»", nel_sog), collapse = " e ") else "niente"))
  }
  out
}

proposte_import <- function(a, con = NULL, fondo = NULL, luoghi_ignoti = TRUE) {
  a$colonna <- .con_nomi_modello(a$colonna, a$profilo)
  d0 <- a$dati
  cz <- concordanza_righe(a)
  d1 <- cz$dati
  motivi <- if (nrow(cz$registro)) split(cz$registro$perche, paste(cz$registro$riga, cz$registro$colonna, sep = "\r")) else list()
  seg <- a$colonna[["Segnatura"]]
  segn <- function(i) if (is.null(seg) || is.na(seg)) "" else sub("^.*?,\\s*(b\\.)", "\\1", as.character(d0[[seg]][i]), perl = TRUE)
  dopo <- d1; celle <- list(); decisioni <- cz$decisioni
  aggiungi <- function(i, colonna, perche) {
    k <- paste(i + 1L, colonna, sep = "\r"); prima <- as.character(d0[[colonna]][i]); nuovo <- as.character(dopo[[colonna]][i])
    celle[[length(celle) + 1L]] <<- data.frame(riga = i + 1L, segnatura = segn(i), colonna = colonna, prima = ifelse(is.na(prima), "", prima),
                                               dopo = ifelse(is.na(nuovo), "", nuovo),
                                               perche = paste(unique(c(motivi[[k]], perche)), collapse = " · "), stringsAsFactors = FALSE)
  }
  # 2. Refusi dei vocabolari chiusi (reati, lingue, supporto, definizione): correzioni sicure e valori nuovi da decidere.
  regole <- if (!is.null(con) && !.vuoto(fondo)) tryCatch(regole_fondo(con, fondo), error = function(e) NULL) else NULL
  if (is.null(regole)) regole <- .con_equivalenze(NULL)
  extra_perche <- list(); nuovi_valori <- list()
  for (r in seq_len(nrow(cc <- .COLONNE_VOCABOLARIO(a)))) {
    col <- cc$colonna[r]; campo <- cc$campo[r]; sep <- cc$sep[r]
    celle_v <- as.character(dopo[[col]])
    voci <- unique(trimws(unlist(lapply(celle_v[!is.na(celle_v) & nzchar(celle_v)], function(x) if (is.na(sep)) x else strsplit(x, paste0("[", sep, "]"))[[1]]))))
    voci <- voci[nzchar(voci)]
    if (!length(voci)) next
    rg <- if (is.null(regole)) NULL else regole[regole$campo == campo, , drop = FALSE]
    cl <- classifica_valori(voci, vocabolario(campo), if (is.null(rg)) character(0) else rg$valore[is.na(rg$forma)],
                            if (is.null(rg)) character(0) else stats::setNames(rg$forma[!is.na(rg$forma)], rg$valore[!is.na(rg$forma)]))
    # Per lingua, supporto e definizione le maiuscole sono una convenzione del foglio (minuscole): non sono errori; per i reati («magia» → «Magia») sì.
    stati <- if (identical(campo, "reato")) c("vicino", "maiuscole", "variante") else c("vicino", "variante")
    mappa <- stats::setNames(cl$proposta[cl$stato %in% stati], cl$valore[cl$stato %in% stati])
    # nel foglio le lingue si scrivono minuscole («latino»): la proposta si mostra come nel foglio
    if (identical(campo, "lingua")) mappa <- stats::setNames(tolower(unname(mappa)), names(mappa))
    if (identical(campo, "supporto") || identical(campo, "definizione")) mappa <- stats::setNames(tolower(unname(mappa)), names(mappa))
    for (i in which(!is.na(celle_v) & nzchar(celle_v))) {
      pezzi <- if (is.na(sep)) celle_v[i] else strsplit(celle_v[i], paste0("[", sep, "]"))[[1]]
      t <- trimws(pezzi); m <- t %in% names(mappa)
      if (any(m)) {
        pezzi[m] <- unname(mappa[t[m]])
        # Un termine che rientra in una classe già scritta nella cella (Amministrazione|Giurisdizione) non la raddoppia: l'uscita la scrive una volta, e la promessa deve essere quella.
        if (identical(campo, "reato")) pezzi <- pezzi[!duplicated(trimws(pezzi))]
        dopo[[col]][i] <- if (is.na(sep)) pezzi else paste(trimws(pezzi), collapse = if (identical(campo, "lingua")) "; " else "|")
        eq <- EQUIVALENZE[[campo]]; globali <- m & tolower(t) %in% tolower(names(eq))
        del_tesauro <- globali & !(tolower(t) %in% tolower(names(EQUIVALENZE_ESPLICITE[[campo]])))
        extra_perche[[paste(i + 1L, col, sep = "\r")]] <- paste(c(
          if (any(!globali & m)) sprintf("refuso nel vocabolario (%s)", cc$campo[r]),
          if (any(globali & !del_tesauro)) sprintf("equivalenza della Guida al Tab: %s", paste(sprintf("«%s» rientra in «%s»", t[globali & !del_tesauro], mappa[t[globali & !del_tesauro]]), collapse = ", ")),
          if (any(del_tesauro)) sprintf("termine del tesauro, resta registrato sotto la classe: %s", paste(sprintf("«%s» rientra in «%s»", t[del_tesauro], mappa[t[del_tesauro]]), collapse = ", "))), collapse = " · ")
      }
    }
    nu <- cl$valore[cl$stato == "nuovo"]
    for (v in nu) {
      righe <- which(vapply(as.character(dopo[[col]]), function(x) !is.na(x) && v %in% trimws(strsplit(x, if (is.na(sep)) "\u0001" else paste0("[", sep, "]"))[[1]]), TRUE))
      decisioni[[length(decisioni) + 1L]] <- list(riga = righe[1] + 1L, segnatura = sprintf("%d righe", length(righe)),
        testo = sprintf("%s: «%s» non è nel vocabolario", c(reato = "Soggettario", lingua = "Lingua", supporto = "Supporto", definizione = "Definizione")[[campo]], v),
        opzioni = unique(vocabolario(campo)), bersagli = data.frame(riga = righe + 1L, colonna = col, da = v, stringsAsFactors = FALSE), tipo = "vocabolario")
    }
  }
  # 3. anni: un numero che non è un anno, o un estremo recente prima del remoto
  c_rem <- unname(a$colonna["Estremo remoto"]); c_rec <- unname(a$colonna["Estremo recente"])
  # Una data intera («1625-01-09», «marzo 1644») vale come il suo anno, se è nell'intervallo.
  anno_ok <- function(x) { d <- if (is.na(x)) NULL else .data_o_anno(x); !is.null(d) && d$anno >= ANNI_AMMESSI[1] && d$anno <= ANNI_AMMESSI[2] }
  for (c_an in na.omit(c(c_rem, c_rec))) if (c_an %in% names(dopo)) for (i in seq_len(nrow(dopo))) {
    v <- trimws(as.character(dopo[[c_an]][i])); if (is.na(v) || !nzchar(v) || tolower(v) %in% c("n.d.", "nd") || .datazione_secolo(v)) next
    if (!anno_ok(v)) {
      ref <- if (identical(c_an, c_rec) && !is.na(c_rem)) trimws(as.character(dopo[[c_rem]][i])) else NA_character_
      cand <- unique(c(if (nchar(v) == 5L) vapply(seq_len(5L), function(k) paste0(substr(v, 1, k - 1L), substr(v, k + 1L, 5L)), "") else character(0)))
      cand <- cand[vapply(cand, anno_ok, TRUE)]
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i), testo = sprintf("«%s» %s (%s)", v, if (grepl("^[0-9]{4}$", v)) sprintf("è fuori dall'intervallo %d\u2013%d", ANNI_AMMESSI[1], ANNI_AMMESSI[2]) else "non è un anno",
                        if (identical(c_an, c_rec)) "estremo recente" else "estremo remoto"),
        opzioni = cand, bersagli = data.frame(colonna = c_an, da = v, stringsAsFactors = FALSE), tipo = "anno", libero = TRUE,
        nota = "Una data non risolta non si inserisce: se non scrivi un anno valido qui, la cella resta vuota.")
    }
  }
  # 3b. L'estremo recente non viene prima del remoto: si decide quale dei due è sbagliato.
  if (!is.na(c_rem) && !is.na(c_rec) && all(c(c_rem, c_rec) %in% names(dopo))) for (i in seq_len(nrow(dopo))) {
    r1 <- trimws(as.character(dopo[[c_rem]][i])); r2 <- trimws(as.character(dopo[[c_rec]][i]))
    if (is.na(r1) || is.na(r2) || !anno_ok(r1) || !anno_ok(r2) || !.data_dopo(.data_o_anno(r1), .data_o_anno(r2))) next
    decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
      testo = sprintf("l'estremo recente (%s) viene prima del remoto (%s)", r2, r1),
      opzioni = stats::setNames(c("scambia", "solo_remoto", "solo_recente"),
                                c(sprintf("Scambiali: %s \u2013 %s", r2, r1), sprintf("Tieni solo %s (togli l'estremo recente)", r1), sprintf("Tieni solo %s (togli l'altro)", r2))),
      bersagli = data.frame(colonna = c(c_rem, c_rec), da = c(r1, r2), stringsAsFactors = FALSE), tipo = "ordine_anni")
  }
  # 3b-bis. Un produttore che è una frase e non un nome («Decreti e atti del S.Uffizio sul magnetismo animale…»): si propone il produttore e la frase va in «Note libere», così non si perde nulla.
  c_prod <- unname(a$colonna["Soggetto produttore"]); c_note <- unname(a$colonna["Note libere"])
  if (!is.na(c_prod) && c_prod %in% names(dopo)) for (i in which(!is.na(dopo[[c_prod]]))) {
    frase <- trimws(as.character(dopo[[c_prod]][i])); prop <- produttore_descrizione(frase); if (is.null(prop)) next
    if (is.na(prop) || is.na(c_note) || !c_note %in% names(dopo)) {
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i), tipo = "avviso", opzioni = character(0), bersagli = NULL,
        testo = sprintf("il Soggetto produttore «%s» è una frase, non il nome di un ente: scrivi chi ha prodotto le carte%s", frase, if (is.na(c_note)) " (il foglio non ha «Note libere» dove tenere la frase)" else ""))
      next
    }
    decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i), tipo = "produttore_descrizione", proposto = prop, col_produttore = c_prod, col_note = c_note,
      testo = sprintf("il Soggetto produttore è una frase: «%s»", frase),
      opzioni = stats::setNames(c("sposta", "lascia"), c(sprintf("Produttore «%s» e la frase in «Note libere»", prop), "Lascia com'è")),
      bersagli = data.frame(colonna = c(c_prod, c_note), da = c(frase, as.character(dopo[[c_note]][i])), stringsAsFactors = FALSE),
      nota = "Proposta: il produttore è il Sant'Uffizio; la frase originale non si perde, va nelle Note libere", predefinita = "sposta")
  }
  # 3c. Persone senza cognome certo («Nicola da Isernia»): non vanno fra le persone citate; si propone di toglierle.
  c_per <- unname(a$colonna["Persone citate o indicizzate"])
  if (!is.na(c_per) && c_per %in% names(dopo)) for (i in which(!is.na(dopo[[c_per]]) & nzchar(dopo[[c_per]]))) {
    voci <- .nomi_nella_cella(dopo[[c_per]][i]); if (!length(voci)) next
    senza <- senza_cognome(.estendi_nome(vapply(voci, .nome_persona_import, "", USE.NAMES = FALSE)))      # «Isernia da, Nicola» è la forma del foglio
    if (!any(senza)) next
    dopo[[c_per]][i] <- if (all(senza)) NA_character_ else paste(voci[!senza], collapse = "|")
    k <- paste(i + 1L, c_per, sep = "\r")
    extra_perche[[k]] <- paste(unique(c(extra_perche[[k]], sprintf("senza cognome certo: non va fra le persone citate (%s) · decidi tu: non è spuntata", paste(voci[senza], collapse = ", ")))), collapse = " · ")
  }
  # 3c2. Persone: i duplicati esatti si segnalano, non si decidono da soli (D4). Il database tiene un nome una volta per scheda: due voci identiche si fondono,
  # e si dice che potrebbero essere omonimi da distinguere.
  if (!is.na(c_per) && c_per %in% names(dopo)) for (i in which(!is.na(dopo[[c_per]]) & nzchar(dopo[[c_per]]))) {
    voci <- trimws(strsplit(as.character(dopo[[c_per]][i]), "|", fixed = TRUE)[[1]]); voci <- voci[nzchar(voci)]
    # Una scelta per nome ripetuto: lasciando com'è si fondono; scrivendo una forma, il primo diventa quella («Rossi, Paolo (padre)»).
    for (dp in unique(voci[duplicated(voci)]))
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
        testo = sprintf("duplicato esatto fra le persone: «%s» compare due volte", dp),
        opzioni = character(0), libero = TRUE, lascia = "Tieni una voce sola (l'altra si toglie)",
        bersagli = data.frame(colonna = c_per, da = dp, stringsAsFactors = FALSE),
        nota = "Il database tiene una voce sola per scheda. Se sono due omonimi, scrivi la forma che distingue il primo (es. «Rossi, Paolo (padre)»); lasciando com'è, restano una voce sola.")
  }
  # 3c2a. Una persona senza virgola in una cella dove le altre hanno «Cognome, Nome»: quasi sempre manca la virgola («Carli Maurizio»).
  # L'import la girerebbe come «Nome Cognome»: è una scelta sul significato, si decide; proposto «Cognome, Nome».
  if (!is.na(c_per) && c_per %in% names(dopo)) for (i in which(!is.na(dopo[[c_per]]) & nzchar(dopo[[c_per]]))) {
    voci <- trimws(strsplit(as.character(dopo[[c_per]][i]), "|", fixed = TRUE)[[1]]); voci <- unique(voci[nzchar(voci)])
    # Solo due parole, entrambe un nome proprio nudo: «Anna di Mosca», «Conte di Carpegna», «N. Flavia» sono altra cosa.
    senza <- voci[grepl("^[[:upper:]][[:alpha:]'\u2019-]+\\s+[[:upper:]][[:alpha:]'\u2019-]+$", voci)]
    if (!length(senza) || !any(grepl(",", voci, fixed = TRUE))) next
    for (dp in senza) {
      parole <- strsplit(dp, "\\s+")[[1]]
      prima <- paste0(parole[1], ", ", paste(parole[-1], collapse = " ")); dopo_ <- indicizza_nome(dp)
      if (identical(prima, dopo_)) next
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
        testo = sprintf("«%s» non ha la virgola, le altre persone della cella sì: quale è il cognome?", dp),
        opzioni = stats::setNames(c(prima, dopo_), c(sprintf("Il cognome è «%s»: %s", parole[1], prima), sprintf("Il cognome è «%s»: %s", parole[length(parole)], dopo_))),
        predefinita = prima, bersagli = data.frame(colonna = c_per, da = dp, stringsAsFactors = FALSE),
        nota = "Senza una scelta il programma lo girerebbe da sé come «Nome Cognome».")
    }
  }
  # 3c2c. Persone che passano in silenzio e non dovrebbero (audit 18/11): il cognome tutto minuscolo («rossi, mario»), il nome tutto maiuscolo con una qualifica
  # fra parentesi («CHIARAMONTI, BARNABA (GREGORIO)»: senza parentesi lo si corregge da sé), il nome senza virgola con particella o numerale («Savoia Carlo Emanuele I di»).
  # Nessuna delle tre si indovina: si chiede, e lasciando com'è la cella non cambia.
  if (!is.na(c_per) && c_per %in% names(dopo)) {
    particelle <- "^(di|de|del|della|dei|degli|delle|da|dal|dalla|dai|d'|van|von|der|den|la|le|lo|y|el|al|ibn|bin)$"
    # Si guarda la cella com'è scritta (dopo L1), non quella già toccata dalle altre proposte: una cella con una proposta di altro genere non deve nascondere questo dubbio.
    orig_per <- if (c_per %in% names(a$dati)) a$dati[[c_per]] else dopo[[c_per]]
    for (i in which(!is.na(orig_per) & nzchar(orig_per))) {
      voci <- trimws(strsplit(as.character(orig_per[i]), "|", fixed = TRUE)[[1]]); voci <- unique(voci[nzchar(voci)])
      # Una voce già corretta da sola da un'altra regola (stesso numero di voci, forma cambiata) non si chiede due volte; se invece una proposta toglie voci, si guarda l'originale.
      vd <- if (is.na(dopo[[c_per]][i])) character(0) else trimws(strsplit(as.character(dopo[[c_per]][i]), "|", fixed = TRUE)[[1]])
      if (length(vd[nzchar(vd)]) >= length(voci)) voci <- intersect(voci, vd)
      for (dp in voci) {
        fuori <- trimws(gsub("\\([^()]*\\)", "", dp)); prima_parola <- sub("[ ,].*$", "", fuori)
        motivo <- NULL; forma <- NULL
        if (grepl(",", fuori, fixed = TRUE) && grepl("^[[:lower:]]", fuori) && !grepl(particelle, prima_parola) && !grepl("['\u2019]", prima_parola)) {
          motivo <- "il cognome è tutto minuscolo"
          forma <- gsub("(^|[ ,(])([[:lower:]])", "\\1\\U\\2", dp, perl = TRUE)
        } else if (grepl("\\(", dp) && grepl("[[:upper:]]{3,}", fuori) && !grepl("[[:lower:]]", fuori)) {
          motivo <- "il nome è tutto maiuscolo"
          forma <- gsub("(?<![\\p{L}'\u2019])(\\p{L})(\\p{L}+)", "\\1\\L\\2", dp, perl = TRUE)
          forma <- gsub("(^|[ ,(])(\\p{Ll})", "\\1\\U\\2", forma, perl = TRUE)
          forma <- gsub("\\b(Di|De|Da|Del|Della)\\b", "\\L\\1", forma, perl = TRUE)
        }
        if (is.null(forma) && grepl("(?<![[:alpha:]'\u2019])[[:upper:]]{2,}[[:lower:]]+", fuori, perl = TRUE) && !grepl("[[:lower:]]\\.[[:upper:]]", fuori)) {
          motivo <- "una parola ha le maiuscole in mezzo"
          forma <- gsub("(?<![[:alpha:]'\u2019])([[:upper:]])([[:upper:]]+)(?=[[:lower:]])", "\\1\\L\\2", dp, perl = TRUE)
        }
        if (!is.null(forma) && !identical(forma, dp)) {
          decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
            testo = sprintf("«%s»: %s. Corretto in «%s»?", dp, motivo, forma),
            opzioni = stats::setNames(forma, sprintf("Sì: %s", forma)),
            bersagli = data.frame(colonna = c_per, da = dp, stringsAsFactors = FALSE),
            nota = "Senza una scelta resta com'è.")
        } else if (is.null(forma) && !grepl("[,()]", dp) && length(strsplit(dp, "\\s+")[[1]]) >= 3 && grepl("^[[:upper:]]", dp)) {
          parole <- strsplit(dp, "\\s+")[[1]]
          if (tolower(parole[length(parole)]) %in% c("di", "de", "del", "della", "da", "dal", "van", "von") || grepl("\\s[IVXL]+(\\s|$)", dp))
            decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
              testo = sprintf("«%s» non ha la virgola e il cognome non è certo (particella in fondo o numerale): scrivi «Cognome, Nome» oppure lascia com'è", dp),
              opzioni = character(0), libero = TRUE, lascia = "Lascia com'è",
              bersagli = data.frame(colonna = c_per, da = dp, stringsAsFactors = FALSE),
              nota = "Il programma non lo gira da sé: non sa dove finisce il cognome.")
        }
      }
    }
  }
  # 3c2b. Luoghi, soggetti (reati), lingue e produttori: il database tiene una voce una volta per scheda, un doppione si fonderebbe in silenzio (L4).
  # Si dice riga per riga e si può scrivere una forma diversa per la prima delle due; lasciando com'è, ne resta una.
  for (campo in c("luogo", "reato", "lingua", "soggetto")) {
    nome <- PROFILO_MODELLO$arianna[!is.na(PROFILO_MODELLO$campo) & PROFILO_MODELLO$campo == campo & PROFILO_MODELLO$multi]
    if (identical(campo, "luogo")) nome <- unique(c(nome, "Luogo di produzione (data topica)"))      # anche i luoghi di produzione: «Roma|Roma» passava in silenzio (audit 18/11)
    for (n in nome) {
      c_l <- unname(a$colonna[n]); if (is.null(c_l) || is.na(c_l) || !c_l %in% names(dopo)) next
      sep <- if (identical(campo, "lingua")) "[;|,]" else "[|;]"
      for (i in which(!is.na(dopo[[c_l]]) & nzchar(dopo[[c_l]]))) {
        voci <- trimws(strsplit(as.character(dopo[[c_l]][i]), sep)[[1]]); voci <- voci[nzchar(voci)]
        voci_c <- if (identical(campo, "lingua")) tolower(voci) else voci
        for (dp in unique(voci[duplicated(voci_c)]))
          decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
            testo = sprintf("duplicato esatto in «%s»: «%s» compare due volte", n, dp),
            opzioni = character(0), libero = TRUE, lascia = "Tieni una voce sola (l'altra si toglie)",
            bersagli = data.frame(colonna = c_l, da = dp, stringsAsFactors = FALSE),
            nota = "Il database tiene una voce sola per scheda. Lasciando com'è, ne resta una; se la seconda dice altro, scrivi la forma giusta per la prima.")
      }
    }
  }
  # 3c3. «Soggetti citati»: un nome di persona («Cognome, Nome») è un errore, si propone lo spostamento fra le persone (D12); non si sposta da soli.
  c_sog <- unname(a$colonna["Soggetti citati o indicizzati"])
  if (!is.na(c_sog) && c_sog %in% names(dopo)) for (i in which(!is.na(dopo[[c_sog]]) & nzchar(dopo[[c_sog]]))) {
    voci <- trimws(strsplit(as.character(dopo[[c_sog]][i]), "|", fixed = TRUE)[[1]]); voci <- voci[nzchar(voci)]
    pers <- voci[grepl("^[[:upper:]][[:alpha:]'.-]+(?: [[:alpha:]'.-]+)*, [[:upper:]][[:alpha:]'. -]+$", voci, perl = TRUE)]
    if (length(pers)) decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i),
      testo = sprintf("persona fra i soggetti: «%s» è un nome di persona, va in «Persone citate o indicizzate» (non lo sposto io)", paste(pers, collapse = "», «")),
      opzioni = character(0), bersagli = NULL, tipo = "avviso")
  }
  # 3d. Sequenza delle segnature: si segnala, non si riordina
  if (!is.null(seg) && !is.na(seg) && seg %in% names(d0)) {
    fs <- .segnature_fuori_sequenza(as.character(d0[[seg]]), righe = seq_len(nrow(d0)))
    if (nrow(fs)) {
      mostra <- head(seq_len(nrow(fs)), 10L)
      for (k in mostra) decisioni[[length(decisioni) + 1L]] <- list(riga = fs$riga[k], segnatura = segn(fs$riga[k] - 1L), testo = fs$testo[k],
                                                                   opzioni = character(0), bersagli = NULL, tipo = "avviso")
      if (nrow(fs) > 10L) decisioni[[length(decisioni) + 1L]] <- list(riga = fs$riga[11], segnatura = sprintf("%d righe", nrow(fs) - 10L),
        testo = sprintf("e altre %d segnature fuori sequenza", nrow(fs) - 10L), opzioni = character(0), bersagli = NULL, tipo = "avviso")
    }
  }
  # 3e. Reati nominati nel regesto: solo le classi del vocabolario (refuso vicino: correzione; altro: avviso).
  # Il Titolo è testo libero (D16): solo controlli formali, mai il vocabolario dei reati.
  classi <- tolower(unique(vocabolario("reato")))
  for (nomecol in c("Regesto o descrizione sintetica")) {
    cc_ <- unname(a$colonna[nomecol]); if (is.na(cc_) || !cc_ %in% names(dopo)) next
    for (i in which(!is.na(dopo[[cc_]]) & nzchar(dopo[[cc_]]))) {
      # «Processo per contro X»: fra «per» e «contro» manca il reato; «contro X» è la persona, non una classe del vocabolario.
      t0 <- trimws(as.character(dopo[[cc_]][i]))
      if (!grepl("[\n:]", t0) && grepl("(^|\\s)per\\s+(contro|circa)\\s", t0, ignore.case = TRUE)) {
        mm <- regmatches(t0, regexec("per\\s+(contro|circa)\\s+(.*)$", t0, ignore.case = TRUE))[[1]]
        decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i), opzioni = character(0), bersagli = NULL, tipo = "avviso",
          testo = sprintf("%s: manca il reato dopo «per»: «%s %s» è la persona, non un reato", nomecol, tolower(mm[2]), sub("[.]\\s*$", "", mm[3])))
        next
      }
      pr_ <- .reati_nel_testo(dopo[[cc_]][i], classi); if (is.null(pr_) || !nrow(pr_)) next
      if (anyNA(pr_$classe)) {
        decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i), opzioni = character(0), bersagli = NULL, tipo = "avviso",
          testo = {
            nc_ <- pr_$parte[is.na(pr_$classe)]
            # Un secondo «per» nel tratto dei reati («Atti per eresia per diversi») è l'ordine delle parti, non un reato sconosciuto.
            if (any(grepl("\\sper\\s", nc_, ignore.case = TRUE)))
              sprintf("%s: «%s» ha due «per»: i reati vengono dopo un solo «per» (per più persone: «Atti per diversi per <reati>»)", nomecol, trimws(as.character(dopo[[cc_]][i])))
            else sprintf("%s: «%s» non è una classe del vocabolario dei reati", nomecol, paste(nc_, collapse = "», «")) })
        next
      }
      da <- pr_$parte[tolower(pr_$parte) != pr_$classe]; if (!length(da)) next
      nuovo <- as.character(dopo[[cc_]][i])
      for (q in which(tolower(pr_$parte) != pr_$classe)) nuovo <- sub(pr_$parte[q], pr_$classe[q], nuovo, fixed = TRUE)
      dopo[[cc_]][i] <- nuovo
      k <- paste(i + 1L, cc_, sep = "\r"); extra_perche[[k]] <- paste(unique(c(extra_perche[[k]], "refuso nel nome di un reato (vocabolario)")), collapse = " · ")
    }
  }
  # 3f. Regesto e Soggettario devono dire gli stessi reati: se collidono decide chi guarda.
  c_reg <- unname(a$colonna["Regesto o descrizione sintetica"]); c_sg <- unname(a$colonna["Soggettario"])
  if (!is.na(c_reg) && !is.na(c_sg) && all(c(c_reg, c_sg) %in% names(dopo))) {
    nome_classe <- function(k) { v <- unique(vocabolario("reato")); v[match(k, tolower(v))] }
    for (i in which(!is.na(dopo[[c_reg]]) & nzchar(dopo[[c_reg]]))) {
      pr_ <- .reati_nel_testo(dopo[[c_reg]][i], classi); if (is.null(pr_) || !nrow(pr_) || anyNA(pr_$classe)) next
      rg <- unique(pr_$classe)
      sg <- if (is.na(dopo[[c_sg]][i]) || !nzchar(dopo[[c_sg]][i])) character(0) else unique(tolower(trimws(strsplit(dopo[[c_sg]][i], "|", fixed = TRUE)[[1]])))
      if (setequal(rg, sg)) next
      testo <- sprintf("il Regesto dice «%s» e il Soggettario %s", paste(nome_classe(rg), collapse = "|"),
                       if (length(sg)) sprintf("«%s»", paste(sapply(sg, function(k) { n <- nome_classe(k); if (is.na(n)) k else n }), collapse = "|")) else "è vuoto")
      opz <- c("soggettario" = "soggettario")
      names(opz) <- sprintf("Scrivi nel Soggettario i reati del Regesto: %s", paste(nome_classe(rg), collapse = "|"))
      nuova_coda <- NULL
      if (length(sg) && !anyNA(match(sg, classi))) {
        nuova_coda <- .elenca_reati(sg)
        opz2 <- c("regesto" = "regesto"); names(opz2) <- sprintf("Riscrivi i reati nel Regesto dal Soggettario: %s", nuova_coda)
        opz <- c(opz, opz2)
      }
      decisioni[[length(decisioni) + 1L]] <- list(riga = i + 1L, segnatura = segn(i), testo = testo, nota = if (!is.null(nuova_coda)) "Proposta: vale il Soggettario; controlla e, se vuoi, cambia scelta" else NULL, opzioni = opz,
        bersagli = data.frame(colonna = c(c_reg, c_sg), da = c(as.character(dopo[[c_reg]][i]), as.character(dopo[[c_sg]][i])), stringsAsFactors = FALSE),
        tipo = "reati", col_regesto = c_reg, col_soggettario = c_sg, coda = attr(pr_, "coda"), nuova_coda = nuova_coda,
        predefinita = if (!is.null(nuova_coda)) "regesto" else "",       # Stefano, 16/10: di default vince il Soggettario, ma lo schedatore verifica
        classi_regesto = nome_classe(rg))
    }
  }
  # 3g. Luoghi stranieri nella lingua del paese (guida al Tab): «Londra» → «London», «Siviglia» → «Sevilla».
  forme <- forme_locali_luoghi()
  for (nomecol in c("Luoghi citati o indicizzati", "Luogo di produzione (data topica)")) {
    cc_ <- unname(a$colonna[nomecol]); if (is.na(cc_) || !cc_ %in% names(dopo) || !nrow(forme)) next
    for (i in which(!is.na(dopo[[cc_]]) & nzchar(dopo[[cc_]]))) {
      voci <- .nomi_nella_cella(dopo[[cc_]][i]); if (!length(voci)) next
      fl <- luogo_forma_locale(voci, forme); nuove <- fl$nuova; if (all(is.na(nuove))) next
      dopo[[cc_]][i] <- paste(ifelse(is.na(nuove), voci, nuove), collapse = "|")
      k <- paste(i + 1L, cc_, sep = "\r")
      # Automatica solo la forma locale nuda o già con parentesi; la sola forma italiana («Londra») è una proposta, spenta (D7, D8).
      extra_perche[[k]] <- paste(unique(c(extra_perche[[k]], sprintf("%s (%s)", if (any(!fl$sicura, na.rm = TRUE)) "forma italiana: proposta Locale (Italiano), decidi tu" else "nella lingua del paese",
                                                                    paste(sprintf("%s → %s", voci[!is.na(nuove)], nuove[!is.na(nuove)]), collapse = ", ")))), collapse = " · ")
    }
  }
  # 3h. «S.» si scioglie sempre in concordanza.R (0g). Resta un dubbio: «S.» dopo una parola maiuscola che non nomina un luogo («Mario S. Rossi»):
  # la cella resta com'è e qui si propone lo scioglimento, mai spuntato di partenza.
  for (nomecol in c("Persone citate o indicizzate", "Soggetti citati o indicizzati", "Titolo", "Regesto o descrizione sintetica")) {
    cc_ <- unname(a$colonna[nomecol]); if (is.na(cc_) || !cc_ %in% names(dopo)) next
    for (i in which(!is.na(dopo[[cc_]]) & grepl("S\\.|Sta\\.|Sto\\.", dopo[[cc_]]))) {
      v0 <- as.character(dopo[[cc_]][i]); v1 <- sciogli_santi(v0, dubbi = TRUE); if (identical(v1, v0)) next
      dopo[[cc_]][i] <- v1; k <- paste(i + 1L, cc_, sep = "\r")
      extra_perche[[k]] <- paste(unique(c(extra_perche[[k]], "«S.» da sciogliere? può essere un'iniziale o un santo che il dizionario non conosce: decidi tu")), collapse = " · ")
    }
  }
  lista_s <- tryCatch(leggi_lista_luoghi(), error = function(e) NULL)
  # 3i. Luoghi non nella lista: un caso dubbio, non blocca l'import; una scheda sola con una spunta per luogo, nessuna di partenza; gli spuntati entrano nella lista personale.
  if (luoghi_ignoti && !is.null(lista_s) && nrow(lista_s)) {   # solo all'import vero: non per le schede già salvate né per «Pulisci» (la lista non c'entra)
    gia <- unlist(lapply(decisioni, function(d) if (is.data.frame(d$bersagli)) d$bersagli$da else character(0)))
    ignoti <- list()
    chiavi_lista <- unique(c(chiave_grafia(lista_s$nome), lista_s$chiave))   # una volta sola: ricalcolarle per ogni nome costava il 70% della revisione
    for (nomecol in c("Luoghi citati o indicizzati", "Luogo di produzione (data topica)")) {
      cc_ <- unname(a$colonna[nomecol]); if (is.na(cc_) || !cc_ %in% names(dopo)) next
      for (i in which(!is.na(dopo[[cc_]]) & nzchar(dopo[[cc_]]))) {
        for (x in .nomi_nella_cella(dopo[[cc_]][i])) {
          if (grepl("[()]", x)) next                                                # mai ciò che sta fra parentesi: salta la voce, non la cella (audit 18/11)
          if (!nzchar(chiave_grafia(x)) || x %in% gia || grepl("[!,;\r\n]", x) || chiave_grafia(x) %in% chiavi_lista) next      # anche la chiave di una grafia diversa («Berlin»): «Aggiungi» la scarterebbe, già presente
          ignoti[[x]] <- c(ignoti[[x]], i)
        }
      }
    }
    if (length(ignoti)) {
      n_righe <- vapply(ignoti, function(r) length(unique(r)), 1L)
      reg <- regioni_luoghi(); cap <- reg$capoluogo[match(chiave_grafia(names(ignoti)), reg$chiave)]      # una regione nota: entra con le coordinate del capoluogo
      eti <- stats::setNames(names(ignoti), sprintf("%s (%d %s)%s", names(ignoti), n_righe, ifelse(n_righe == 1L, "riga", "righe"), ifelse(is.na(cap), "", sprintf(" · regione: coordinate di %s", cap))))
      decisioni[[length(decisioni) + 1L]] <- list(riga = min(unlist(ignoti)) + 1L, segnatura = if (length(ignoti) == 1L) "1 luogo" else sprintf("%d luoghi", length(ignoti)),
        testo = "Luoghi che non sono nella lista dei luoghi (possono essere luoghi nuovi o refusi). Spunta «Aggiungi» per quelli da mettere nella lista, oppure «Elimina» per toglierli dalla scheda: senza spunte restano come sono.",
        opzioni = eti, n_righe = unname(n_righe), bersagli = data.frame(colonna = unname(a$colonna["Luoghi citati o indicizzati"]), da = names(ignoti), stringsAsFactors = FALSE),
        tipo = "luoghi_ignoti", luoghi = names(ignoti))
    }
  }
  # 3j. Sospetti refusi nel testo libero (Titolo e Regesto): lessico del corpus (foglio + database); solo proposta, mai spuntata.
  corpus_ <- c(unlist(lapply(c("Titolo", "Regesto o descrizione sintetica"), function(n) { cc_ <- unname(a$colonna[n]); if (!is.na(cc_) && cc_ %in% names(dopo)) as.character(dopo[[cc_]]) })),
               if (!is.null(con)) tryCatch(unlist(DBI::dbGetQuery(con, "SELECT titolo, regesto FROM fascicoli"), use.names = FALSE), error = function(e) NULL))
  lessico_ <- .lessico_corpus(corpus_)
  if (any(lessico_ >= REFUSI_FREQUENTE)) for (nomecol in c("Titolo", "Regesto o descrizione sintetica")) {
    cc_ <- unname(a$colonna[nomecol]); if (is.na(cc_) || !cc_ %in% names(dopo)) next
    for (i in which(!is.na(dopo[[cc_]]) & nzchar(dopo[[cc_]]))) {
      sosp_ <- .sospetti_refusi(as.character(dopo[[cc_]][i]), lessico_); if (!nrow(sosp_)) next
      dopo[[cc_]][i] <- .applica_refusi(as.character(dopo[[cc_]][i]), sosp_)
      k <- paste(i + 1L, cc_, sep = "\r")
      extra_perche[[k]] <- paste(unique(c(extra_perche[[k]], sprintf("refuso sospetto (lessico del corpus), decidi tu: %s", paste(sprintf("%s → %s", sosp_$parola, sosp_$proposta), collapse = ", ")))), collapse = " · ")
    }
  }
  # 3k. Segnatura con spazi doppi o ai bordi: correzione proposta con anteprima. Se due righe diventerebbero la stessa segnatura la correzione NON è spuntata e lo dice.
  # Una segnatura già nel database la respinge l'import («già occupata»); le altre forme sbagliate («;», virgola senza spazio) restano respinte.
  if (!is.null(seg) && !is.na(seg) && seg %in% names(dopo)) {
    v_ <- as.character(dopo[[seg]]); pulita_ <- trimws(gsub("[ \t\u00a0]+", " ", v_))
    cambia_ <- which(!is.na(v_) & nzchar(trimws(v_)) & v_ != pulita_)
    if (length(cambia_)) {
      # La collisione è quella del database (stesso fondo, busta, fascicolo e suffissi, comunque scritti). Se una riga non dice archivio e fondo si conta come possibile collisione: si spegne la spunta.
      an_ <- lapply(pulita_, function(x) if (is.na(x) || !nzchar(x)) NULL else tryCatch(analizza_segnatura(x), error = function(e) NULL))
      num_ <- vapply(an_, function(z) if (is.null(z)) NA_character_ else paste(z$busta, z$busta_suffisso %||% "", z$fascicolo, z$fascicolo_suffisso %||% "", z$serie %||% "", sep = "\r"), "")
      luogo_ <- vapply(an_, function(z) if (is.null(z)) NA_character_ else tolower(paste(z$archivio %||% "", z$fondo %||% "", sep = "\r")), "")
      stesso_ <- function(i, j) {
        if (!is.na(num_[i]) && !is.na(num_[j]) && num_[i] == num_[j]) {
          li <- luogo_[i]; lj <- luogo_[j]; vuoto_ <- "\r"
          li == lj || li == vuoto_ || lj == vuoto_ || startsWith(li, "\r") || startsWith(lj, "\r")
        } else identical(tolower(pulita_[i]), tolower(pulita_[j]))
      }
      for (i in cambia_) {
        altre_ <- setdiff(which(vapply(seq_along(pulita_), function(j) !is.na(pulita_[j]) && stesso_(i, j), TRUE)), i)
        dopo[[seg]][i] <- pulita_[i]
        k <- paste(i + 1L, seg, sep = "\r")
        extra_perche[[k]] <- paste(unique(c(extra_perche[[k]], if (length(altre_))
          sprintf("spazi nella segnatura · collisione: diventerebbe uguale a quella della riga %s: decidi tu", paste(altre_ + 1L, collapse = ", "))
          else "spazi doppi o ai bordi nella segnatura")), collapse = " · ")
      }
    }
  }
  # 4. le celle cambiate: da concordanza e da vocabolari, una riga per cella
  for (cc2 in names(dopo)) {
    vecchie <- as.character(d0[[cc2]]); nuove <- as.character(dopo[[cc2]])
    for (i in which(!(is.na(vecchie) & is.na(nuove)) & (is.na(vecchie) != is.na(nuove) | vecchie != nuove))) aggiungi(i, cc2, extra_perche[[paste(i + 1L, cc2, sep = "\r")]])
  }
  celle <- if (length(celle)) do.call(rbind, celle) else data.frame(riga = integer(0), segnatura = character(0), colonna = character(0), prima = character(0), dopo = character(0), perche = character(0), stringsAsFactors = FALSE)
  celle <- celle[order(celle$riga, celle$colonna), , drop = FALSE]; rownames(celle) <- NULL
  # Consigliata = spuntata all'apertura. Togliere una persona senza cognome certo cancella un dato: la decide chi guarda (non spuntata).
  celle$consigliata <- !grepl("senza cognome certo|da sciogliere|forma italiana: proposta|refuso sospetto|collisione: diventerebbe", celle$perche)
  # `spunta` (23/11/2026) = spuntata all'apertura della FINESTRA: come `consigliata`, ma un refuso di vocabolario («Maggia» per «Magia») è una lettura, non una
  # regola tipografica: lo decide chi guarda. `consigliata` resta il contratto di «Pulisci e scarica» e del corpus.
  celle$spunta <- celle$consigliata & !grepl("refuso nel vocabolario|refuso nel nome di un reato", celle$perche)
  celle$id <- seq_len(nrow(celle))
  if (length(decisioni)) decisioni <- decisioni[order(vapply(decisioni, function(d) as.integer(d$riga), 1L))]   # in ordine di riga del foglio
  for (k in seq_along(decisioni)) decisioni[[k]]$id <- k
  list(celle = celle, decisioni = decisioni, dati_corretti = dopo,
       totale = nrow(celle) + length(decisioni))
}

# Gli id delle celle spuntate all'apertura della finestra (`spunta`; senza quella colonna, le consigliate).
celle_spuntate <- function(celle) celle$id[if ("spunta" %in% names(celle)) celle$spunta else celle$consigliata]

# Applica: le correzioni spuntate (id) e le decisioni prese (`scelte`: nome = id della decisione, valore = forma scelta; vuoto o assente = lasciare com'è).
applica_proposte <- function(a, pr, ok = pr$celle$id, scelte = list()) {
  a$colonna <- .con_nomi_modello(a$colonna, a$profilo)
  d <- a$dati; originale <- a$dati
  c1 <- pr$celle[pr$celle$id %in% ok, , drop = FALSE]
  for (r in seq_len(nrow(c1))) d[[c1$colonna[r]]][c1$riga[r] - 1L] <- if (nzchar(c1$dopo[r])) c1$dopo[r] else NA_character_
  da_aggiungere <- character(0); tolti <- NULL; rimasti <- NULL; omonimi <- NULL; omonimi_testi <- NULL
  prese <- NULL        # le celle cambiate da una decisione presa in finestra: entrano nel registro come decisioni accettate, non come «mai annunciate»
  confronta_decisione <- function(prec) {
    if (is.null(prec)) return(invisible(NULL))
    mod <- names(d)[!vapply(names(d), function(cc) identical(prec$d[[cc]], d[[cc]]), TRUE)]
    for (cc in mod) for (i in which(!mapply(identical, as.character(prec$d[[cc]]), as.character(d[[cc]])))) {
      prese <<- rbind(prese, data.frame(id = NA_character_, riga = i + 1L, segnatura = NA_character_, colonna = cc,
        prima = if (is.na(prec$d[[cc]][i])) "" else as.character(prec$d[[cc]][i]), dopo = if (is.na(d[[cc]][i])) "" else as.character(d[[cc]][i]),
        perche = sprintf("decisione presa nella finestra (%s)", prec$dec$tipo %||% "nomi"), consigliata = TRUE, spunta = TRUE, accettata = TRUE, stringsAsFactors = FALSE))
    }
  }
  prec <- NULL
  for (dec in pr$decisioni) {
    confronta_decisione(prec); prec <- NULL
    s <- scelte[[as.character(dec$id)]]; if (is.null(s) || !nzchar(s)) next
    prec <- list(d = d, dec = dec)
    if (identical(dec$tipo, "omonimo")) {
      # La cella resta com'è: la scelta (con identificativo e coordinate) va solo nella traccia.
      k <- match(s, dec$info$scelta)
      if (!is.na(k)) omonimi <- rbind(omonimi, data.frame(Riga = dec$riga, Segnatura = dec$segnatura %||% "", Colonna = dec$colonna %||% "", Luogo = dec$info$nome[k], Scelta = s,
                                                          Wikidata = dec$info$wikidata[k], Coordinate = dec$info$coordinate[k], stringsAsFactors = FALSE))
      # Il caso scelto non è più «da decidere»: il testo serve a toglierlo dalla lista (`.senza_omonimi_scelti`).
      if (!is.na(k)) omonimi_testi <- rbind(omonimi_testi, data.frame(riga = dec$riga, testo = dec$testo, stringsAsFactors = FALSE))
      prec <- NULL; next
    }
    if (identical(dec$tipo, "luoghi_ignoti")) {
      # D9: «Aggiungi alla lista» = il nome puro; «Elimina» = il nome col prefisso \002 (il luogo esce dalla scheda). Un luogo spuntato in entrambe non si tocca.
      voci_s <- strsplit(s, "\001", fixed = TRUE)[[1]]
      el <- intersect(sub("^\002", "", voci_s[startsWith(voci_s, "\002")]), dec$luoghi)
      ag <- setdiff(intersect(voci_s[!startsWith(voci_s, "\002")], dec$luoghi), el)
      da_aggiungere <- c(da_aggiungere, ag)
      el <- setdiff(el, voci_s[!startsWith(voci_s, "\002")])
      for (col in unname(a$colonna[c("Luoghi citati o indicizzati", "Luogo di produzione (data topica)")])) {
        if (is.na(col) || !col %in% names(d) || !length(el)) next
        nome_col <- names(a$colonna)[match(col, a$colonna)]
        for (i in which(!is.na(d[[col]]) & nzchar(d[[col]]))) {
          if (grepl("[()]", d[[col]][i])) next
          voci <- .nomi_nella_cella(d[[col]][i]); resto <- voci[!voci %in% el]
          if (length(resto) < length(voci)) {
            prima <- d[[col]][i]; d[[col]][i] <- if (length(resto)) paste(resto, collapse = "|") else NA_character_
            tolti <- rbind(tolti, data.frame(Riga = i + 1L, Colonna = nome_col, Prima = prima, Dopo = if (is.na(d[[col]][i])) "" else d[[col]][i], stringsAsFactors = FALSE))   # la traccia (contratto, punto 4)
          } else if (any(trimws(strsplit(d[[col]][i], "[|;,]")[[1]]) %in% el)) {
            # Il luogo è nella cella ma la cella ha ancora un separatore da correggere: non si indovina, si segnala.
            rimasti <- rbind(rimasti, data.frame(Riga = i + 1L, Colonna = nome_col, Cella = d[[col]][i], stringsAsFactors = FALSE))
          }
        }
      }
      next
    }
    b <- dec$bersagli
    for (q in seq_len(nrow(b))) {
      i <- (if ("riga" %in% names(b)) b$riga[q] else dec$riga) - 1L; col <- b$colonna[q]
      x <- as.character(d[[col]][i]); if (is.na(x)) next
      if (identical(dec$tipo, "reati")) {                  # una scelta per riga: o i reati del Regesto nel Soggettario, o il contrario
        if (q > 1L) next
        if (s == "soggettario") d[[dec$col_soggettario]][i] <- paste(dec$classi_regesto, collapse = "|")
        else if (s == "regesto" && !is.null(dec$nuova_coda)) d[[dec$col_regesto]][i] <- sub(dec$coda, dec$nuova_coda, as.character(d[[dec$col_regesto]][i]), fixed = TRUE)
        next
      }
      if (identical(dec$tipo, "produttore_descrizione")) {  # il produttore e le note insieme, una volta sola per decisione
        if (q > 1L) next
        if (s == "sposta") {
          nota <- as.character(d[[dec$col_note]][i]); frase <- sprintf("Produttore indicato nel foglio: %s", x)
          d[[dec$col_note]][i] <- if (is.na(nota) || !nzchar(trimws(nota))) frase else paste(nota, frase, sep = " · ")
          d[[dec$col_produttore]][i] <- dec$proposto
        }
        next
      }
      if (identical(dec$tipo, "ordine_anni")) {            # le due colonne insieme, una volta sola per decisione
        if (q > 1L) next
        c1 <- b$colonna[1]; c2 <- b$colonna[2]; r1 <- d[[c1]][i]; r2 <- d[[c2]][i]
        if (s == "scambia") { d[[c1]][i] <- r2; d[[c2]][i] <- r1 }
        else if (s == "solo_remoto") d[[c2]][i] <- NA_character_
        else if (s == "solo_recente") { d[[c1]][i] <- r2; d[[c2]][i] <- NA_character_ }
        next
      }
      es <- !is.null(dec$tipo) && dec$tipo %in% c("anno", "vocabolario")
      voci <- if (es && identical(dec$tipo, "anno")) x else .nomi_nella_cella(x)
      if (es && identical(dec$tipo, "anno")) { d[[col]][i] <- s; next }
      # Il pezzo si cerca prima fra le voci intere («|», «;»), così una voce già decisa («Rossi, Mario (padre)») non si spezza; poi con le virgole come separatore.
      if (!b$da[q] %in% voci) voci <- .nomi_nella_cella(x, con_virgole = TRUE)
      j <- match(b$da[q], voci)
      if (is.na(j)) next
      nuova <- if (!is.null(dec$tipo) && identical(dec$tipo, "vocabolario")) s else if (grepl(",", b$da[q], fixed = TRUE) && !grepl(",", s, fixed = TRUE)) indicizza_nome(s) else s      # già «Cognome, Nome»: non si rigira
      voci[j] <- nuova
      d[[col]][i] <- paste(voci, collapse = "|")
    }
  }
  confronta_decisione(prec)
  attr(a, "celle_cambiate") <- sum(vapply(names(d), function(cc) sum(!mapply(identical, as.character(d[[cc]]), as.character(a$dati[[cc]]))), 0L))
  # Il registro delle modifiche parte dal foglio com'era e dalle proposte che chi guardava aveva davanti.
  attr(a, "dati_originali") <- attr(a, "dati_originali") %||% originale
  viste <- pr$celle; if (nrow(viste)) viste$accettata <- viste$id %in% ok else viste$accettata <- logical(0)
  if (!is.null(prese)) viste <- rbind(viste, prese[, names(viste), drop = FALSE])
  attr(a, "proposte_viste") <- viste
  a$dati <- d; attr(a, "luoghi_da_aggiungere") <- da_aggiungere; attr(a, "luoghi_tolti") <- tolti; attr(a, "luoghi_rimasti") <- rimasti; attr(a, "omonimi_scelti") <- omonimi; attr(a, "omonimi_scelti_testi") <- omonimi_testi; a
}

# ── la finestra «Controlla prima di importare» ───────────────────
# Una finestra in tre mosse: che cosa ha trovato Trama; le correzioni, una per cella, con prima e dopo evidenziati e la spunta per toglierle;
# le cose da decidere, una scheda per ciascuna. Sotto: importare con le correzioni, importare com'è, annullare.
# ── Scegliere per gruppi: per colonna, per scheda (busta e fascicolo) o per tipo di correzione ──
# Il «tipo» è il motivo senza i dettagli («senza cognome certo», «nella lingua del paese»…); una cella può averne più d'uno.
.tipo_correzione <- function(perche) {
  z <- trimws(strsplit(as.character(perche), " \u00b7 ", fixed = TRUE)[[1]])
  z <- trimws(sub("\\s*\\(.*$", "", sub(":.*$", "", z))); z[nzchar(z)]
}

scheda_decisione <- function(dec) {
  id <- dec$id
  if (identical(dec$tipo, "avviso"))                 # una difformità da guardare (non c'è niente da scegliere)
    return(div(class = "rev-decisione rev-avviso",
               div(class = "rev-dec-testa", span(class = "rev-riga", sprintf("riga %d", dec$riga)), span(class = "rev-segn", dec$segnatura)),
               div(class = "rev-dec-testo", dec$testo)))
  if (identical(dec$tipo, "luoghi_ignoti"))          # tutti i luoghi ignoti in una scheda: una spunta per luogo, nessuna di partenza
    return(div(class = "rev-decisione",
               div(class = "rev-dec-testa", span(class = "rev-riga", sprintf("da riga %d", dec$riga)), span(class = "rev-segn", dec$segnatura)),
               div(class = "rev-dec-testo", dec$testo),
               div(class = "mb-1",
                   tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", onclick = sprintf("$('#rev_dec_%d input').prop('checked', true).trigger('change')", id), "Spunta tutti"), " ",
                   tags$button(type = "button", class = "btn btn-sm btn-outline-secondary", onclick = sprintf("$('#rev_dec_%1$d input, #rev_dec_del_%1$d input').prop('checked', false).trigger('change')", id), "Togli tutte")),
               div(style = "max-height:14rem; overflow-y:auto;", class = "rev-luoghi-ignoti", `data-dec` = id,
                 div(class = "fw-semibold small mt-1", "Aggiungi alla lista"),
                 checkboxGroupInput(sprintf("rev_dec_%d", id), NULL, selected = character(0), choiceValues = unname(dec$opzioni),
                   choiceNames = lapply(seq_along(dec$luoghi), function(q) tagList(span(class = "no-trad", dec$luoghi[q]), span(class = "rev-n", if (dec$n_righe[q] == 1L) "1 riga" else sprintf("%d righe", dec$n_righe[q]))))),
                 div(class = "fw-semibold small mt-1", "Elimina dalla scheda"),
                 checkboxGroupInput(sprintf("rev_dec_del_%d", id), NULL, selected = character(0), choiceValues = unname(dec$opzioni),
                   choiceNames = lapply(seq_along(dec$luoghi), function(q) span(class = "no-trad", dec$luoghi[q]))),
                 # Un luogo o si aggiunge o si elimina: spuntare un'azione toglie l'altra.
                 tags$script(HTML(sprintf("$('#rev_dec_%1$d input, #rev_dec_del_%1$d input').on('change', function() { if (!this.checked) return; var v = this.value; var altro = $(this).closest('[id^=rev_dec_]').attr('id') === 'rev_dec_%1$d' ? '#rev_dec_del_%1$d' : '#rev_dec_%1$d'; $(altro).find('input').filter(function() { return this.value === v; }).prop('checked', false).trigger('change'); });", id))))))
  presel <- nzchar(dec$predefinita %||% "")
  scelte <- c(stats::setNames("", if (identical(dec$tipo, "anno")) "Non inserire la data" else (dec$lascia %||% "Lascia com'è")),
              if (!is.null(names(dec$opzioni))) dec$opzioni else stats::setNames(dec$opzioni, dec$opzioni))
  div(class = paste("rev-decisione", if (presel) "rev-presel"),
      div(class = "rev-dec-testa", span(class = "rev-riga", sprintf("riga %d", dec$riga)), span(class = "rev-segn", dec$segnatura)),
      if (presel) div(class = "rev-badge", "Preselezionata: da verificare"),
      div(class = "rev-dec-testo", dec$testo),
      if (!is.null(dec$nota)) div(class = "small text-muted mb-1", dec$nota),
      if (length(dec$opzioni) > 12L) selectInput(sprintf("rev_dec_%d", id), NULL, choices = scelte, selected = dec$predefinita %||% "", width = "100%")
      else radioButtons(sprintf("rev_dec_%d", id), NULL, choices = scelte, selected = dec$predefinita %||% "", inline = TRUE),
      if (isTRUE(dec$libero)) textInput(sprintf("rev_dec_libero_%d", id), NULL, placeholder = "oppure scrivi tu l'anno", width = "14rem"))
}

modale_revisione <- function(pr, pulisci = FALSE, schede = FALSE, modo_testo = NULL, a = NULL) {
  modalDialog(
    title = if (schede) "Controlla le schede del fondo" else if (pulisci) "Controlla prima di pulire" else "Controlla prima di importare", size = "xl", easyClose = FALSE,
    if (!is.null(modo_testo)) div(class = "small text-muted mb-2", "Modalità: ", strong(modo_testo), " \u00b7 ", actionLink("rev_cambia_modalita", "cambia")),
    p(class = "text-muted mb-2", if (schede) "Trama ha trovato possibili errori nelle schede già salvate di questo fondo. Per ora non ha cambiato niente: scorri le voci a sinistra, togli la spunta alle correzioni che non vuoi e decidi i casi dubbi. Solo quando premi «Correggi le schede» le schede cambiano, e prima Trama fa una copia di sicurezza del database." else if (pulisci) "Trama ha letto il foglio e ha trovato qualcosa da sistemare. Il foglio pulito non è ancora stato fatto (e il database non si tocca): guarda le correzioni, togli la spunta a quelle che non vuoi, decidi i casi dubbi, poi pulisci." else "Trama ha letto il foglio e ha trovato qualcosa da sistemare. Niente è stato scritto: guarda le correzioni, togli la spunta a quelle che non vuoi, decidi i casi dubbi, poi importa."),
    vista_revisione(pr, schede = schede, a = a),
    footer = if (schede) tagList(
      actionButton("rev_annulla", "Non ora", class = "btn-outline-secondary"),
      actionButton("rev_conferma", "Correggi le schede", class = "btn-primary")) else tagList(
      actionButton("rev_annulla", "Annulla", class = "btn-outline-secondary"),
      if (!pulisci) actionButton("rev_scarica_pulito", "Scarica pulito, senza importare", class = "btn-outline-secondary"),
      actionButton("rev_come_sta", if (pulisci) "Pulisci com'è, senza correggere" else "Importa com'è, senza correggere", class = "btn-outline-primary"),
      actionButton("rev_conferma", if (pulisci) "Pulisci con le correzioni scelte" else "Importa con le correzioni scelte", class = "btn-primary")))
}

# I casi «da decidere» (riga, testo) senza gli omonimi già scelti nella finestra (`attr(a, "omonimi_scelti_testi")`): una scelta fatta sta in «Luoghi scelti», non anche fra i dubbi.
.senza_omonimi_scelti <- function(dd, a) {
  t <- attr(a, "omonimi_scelti_testi")
  if (is.null(t) || !nrow(t) || is.null(dd) || !nrow(dd)) return(dd)
  dd[!mapply(function(r, x) any(r == t$riga & endsWith(x, t$testo)), dd$riga, dd$testo), , drop = FALSE]
}
