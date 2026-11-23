# ──────────────────────────────────────────────
# R/registro_modifiche.R: il registro di ciò che cambia fra il foglio originale e il foglio pulito
# Nasce dal CONFRONTO cella per cella fra l'originale e il pulito: ogni differenza ha riga, segnatura, colonna, prima, dopo e regola.
# La regola viene dalle proposte che l'utente ha visto (`proposte_import`); una differenza che nessuna proposta annunciava è «non dichiarata»:
# una trasformazione dell'export che l'anteprima non mostrava, da portare in anteprima.
# ──────────────────────────────────────────────

# La chiave con cui si ritrova una riga dopo la pulizia (l'ordine può cambiare): busta e fascicolo con i suffissi.
.chiave_riga_segnatura <- function(x) {
  vapply(as.character(x), function(s) {
    if (.vuoto(s)) return(NA_character_)
    r <- .scomponi_segnatura(s)
    paste(c(r$busta, r$busta_suffisso, r$fascicolo, r$fascicolo_suffisso), collapse = "|")      # senza archivio e fondo: chi pulisce può scriverli (13/10)
  }, "", USE.NAMES = FALSE)
}

# Le convenzioni FISSE di uscita (NORMALIZZAZIONE_LITE.md), contate in anteprima da `correzioni_import`: una differenza che è una di queste ha il suo nome, altrimenti NA.
# `scritti`: archivio e fondo scritti a mano da chi pulisce.
.convenzione_uscita <- function(colonna, prima, dopo, scritti = character(0)) {
  prima <- if (is.na(prima)) "" else as.character(prima); dopo <- if (is.na(dopo)) "" else as.character(dopo)
  spazi <- function(x) gsub("[[:space:]]+", " ", trimws(x))
  if (identical(spazi(prima), dopo)) return("spazi ai bordi e doppi (L1)")
  if (colonna %in% c("Regesto o descrizione sintetica", "Note libere") && 
      dopo %in% c(.regesto_export(sub("[[:space:]]+$", "", prima)), .regesto_export(sub("[[:space:]]+$", "", gsub("[ \t\u00a0]+", " ", prima))), .regesto_export(spazi(prima)))) return("punto finale in uscita (convenzione del foglio, L3)")
  if (colonna == "Tipologia documentaria" && !nzchar(prima) && identical(dopo, "Atti")) return("tipologia documentaria vuota: esce «Atti» (decisione del 2/10)")
  if (colonna == "Luogo di conservazione" && length(scritti) && nzchar(scritti[1]) && identical(dopo, scritti[1])) return("archivio scritto da chi pulisce: è anche il luogo di conservazione (13/10)")
  if (colonna == "Luogo di conservazione" && nzchar(prima) && grepl(tolower(prima), tolower(dopo), fixed = TRUE)) return("luogo di conservazione: la città dell'archivio diventa l'ente (12/10)")
  if (colonna == "Segnatura") {
    k <- function(x) gsub("busta", "b", gsub("fasc", "f", gsub("[[:space:].,]+", "", tolower(x))))      # «f.1» e «fasc. 1»: la stessa segnatura nella forma del modello
    if (identical(k(prima), k(dopo))) return("segnatura nella forma «sigla N» (spazio dopo la sigla)")
    if (length(scritti) && all(vapply(scritti[nzchar(scritti)], function(z) grepl(tolower(z), tolower(dopo), fixed = TRUE), TRUE))) return("archivio e fondo scritti da chi pulisce (13/10)")
  }
  if (colonna == "Persone citate o indicizzate") {
    a <- trimws(strsplit(prima, "|", fixed = TRUE)[[1]]); b <- trimws(strsplit(dopo, "|", fixed = TRUE)[[1]])
    uscita <- function(x) vapply(x, function(v) if (grepl(",", v, fixed = TRUE)) .nome_persona_export(.nome_persona_import(v)) else v, "", USE.NAMES = FALSE)
    if (length(a) == length(b) && identical(uscita(a), b)) return("particella dopo il nome, come nella guida al Tab")
    ua <- unique(a); if (length(ua) < length(a) && identical(uscita(ua), b)) return("duplicato esatto: il database tiene un nome una volta per scheda (D4)")
  }
  if (colonna == "Luoghi citati o indicizzati" || colonna == "Luogo di produzione (data topica)") {
    a <- trimws(strsplit(prima, "|", fixed = TRUE)[[1]]); b <- trimws(strsplit(dopo, "|", fixed = TRUE)[[1]])
    if (length(unique(a)) < length(a) && identical(unique(a), b)) return("duplicato esatto: il database tiene un luogo una volta per scheda (D4)")
  }
  NA_character_
}

# `originale`: i dati del foglio prima di ogni correzione (a$dati) con la mappa `colonna` (modello → colonna del foglio); `pulito`: il foglio pulito (28 colonne, una riga per scheda entrata);
# `viste`: le proposte che l'utente ha avuto davanti (riga, colonna, perche, accettata). Una riga per cella cambiata: Riga, Segnatura, Colonna, Prima, Dopo, Regola, Dichiarata:
#   «sì» = proposta vista e accettata · «convenzione» = convenzione fissa di uscita · «no» = proposta rifiutata applicata comunque, o trasformazione mai annunciata.
# `righe`: il numero di riga nel foglio di ogni riga di `originale` (se è un sottoinsieme: le righe sorgente di UN fondo, `riga_fondo` dell'import).
registro_modifiche <- function(originale, colonna, pulito, viste = NULL, scritti = character(0), righe = seq_len(nrow(originale)) + 1L) {
  vuoto <- data.frame(Riga = integer(0), Segnatura = character(0), Colonna = character(0), Prima = character(0), Dopo = character(0),
                      Regola = character(0), Dichiarata = character(0), stringsAsFactors = FALSE)
  cs <- unname(colonna["Segnatura"]); if (is.na(cs) || !cs %in% names(originale) || !"Segnatura" %in% names(pulito)) return(vuoto)
  ko <- .chiave_riga_segnatura(originale[[cs]]); kp <- .chiave_riga_segnatura(pulito[["Segnatura"]])
  # la stessa chiave più volte (duplicati): si appaiano nell'ordine
  no <- stats::ave(seq_along(ko), ko, FUN = seq_along); np <- stats::ave(seq_along(kp), kp, FUN = seq_along)
  idx <- match(paste(ko, no), paste(kp, np)); idx[is.na(ko)] <- NA
  nota <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
  out <- list(); da_proposta <- logical(0)
  for (nm in names(colonna)) {
    cc <- unname(colonna[nm]); if (is.na(cc) || !cc %in% names(originale) || !nm %in% names(pulito)) next
    o <- nota(originale[[cc]]); pp <- nota(pulito[[nm]])[idx]
    for (i in which(!is.na(idx) & o != pp)) {
      k <- if (!is.null(viste) && nrow(viste)) which(viste$riga == righe[i] & viste$colonna == cc) else integer(0)
      acc <- length(k) > 0L && all(viste$accettata[k])
      # accettata: l'uscita deve essere ciò che la proposta prometteva; il resto è una convenzione o no
      promesso <- if (acc) nota(viste$dopo[k[length(k)]]) else o[i]
      ok <- acc && identical(promesso, pp[i])
      conv <- if (ok) NA_character_ else .convenzione_uscita(nm, promesso, pp[i], scritti)
      out[[length(out) + 1L]] <- data.frame(Riga = righe[i], Segnatura = as.character(originale[[cs]])[i], Colonna = nm, Prima = o[i], Dopo = pp[i],
        Regola = if (ok) paste(unique(viste$perche[k]), collapse = " · ")
                 else if (!is.na(conv)) (if (acc) paste0(paste(unique(viste$perche[k]), collapse = " · "), " + ", conv) else conv)
                 else if (length(k)) paste0("proposta rifiutata, ma l'uscita la applica comunque: ", paste(unique(viste$perche[k]), collapse = " · "))
                 else "trasformazione di uscita mai annunciata in anteprima",
        Dichiarata = if (ok) "sì" else if (!is.na(conv)) "convenzione" else "no", stringsAsFactors = FALSE)
      da_proposta <- c(da_proposta, acc)
    }
  }
  if (!length(out)) return(vuoto)
  r <- do.call(rbind, out); o_ <- order(r$Riga, r$Colonna); r <- r[o_, , drop = FALSE]
  attr(r, "da_proposta") <- da_proposta[o_]          # la cella era una correzione accettata in finestra (anche se l'uscita ha aggiunto una convenzione)
  r
}
