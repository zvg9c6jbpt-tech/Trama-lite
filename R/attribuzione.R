# ─────────────────────────────────────────────────────────────
# attribuzione.R: le coppie persona/reato e reato/luogo della maschera
# Dipende da crud.R (leggi_fascicolo), profili.R (normalizza), compose.R (indicizza_nome)
# Interruttore dell'interfaccia: «attribuzione dei capi» per ora è solo un memento. I campi restano nel database, nel profilo e nei backup;
# FALSE li toglie dalla Scheda (inputs nascosti, così Salva non li azzera) e dai controlli di Qualità. TRUE li rimette.
MOSTRA_ATTRIBUZIONE <- FALSE

# Il problema: `reati` sta sul fascicolo, non sulla persona. Con un solo imputato il legame è implicito; da due in su appiccicare i reati a ciascuno è un'assunzione, non un dato
# (sui dati veri del vecchio Regestum solo 353 su 1.314 attribuzioni mostrate come fatti erano certe).
# persona_reati/reato_luoghi si scrivono dalla maschera: con attribuzione 'per persona' la griglia imputati × capi, con attribuzione_luoghi 'per reato' la griglia capi × luoghi.
# Le coppie restano salvate se si torna a 'collettiva' o 'n.d.': sono inerti, contano solo quando la modalità le chiama.
# ─────────────────────────────────────────────────────────────


# --- le coppie come testo -----------------------------------------
# Nella maschera una coppia è una stringa "persona<TAB>reato" (o "reato<TAB>luogo"): un input Shiny porta vettori di testo, non tabelle.
.SEP_COPPIA <- "\t"

coppie_da_tabella <- function(d) {
  if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) return(character(0))
  paste(d[[1]], d[[2]], sep = .SEP_COPPIA)
}

tabella_da_coppie <- function(v, colonne) {
  v <- as.character(v)
  v <- v[!is.na(v) & grepl(.SEP_COPPIA, v, fixed = TRUE)]
  parti <- strsplit(v, .SEP_COPPIA, fixed = TRUE)
  d <- data.frame(vapply(parti, `[`, "", 1), vapply(parti, `[`, "", 2), stringsAsFactors = FALSE)
  setNames(unique(d), colonne)
}


# Le righe e le colonne di ciascuna griglia, dai valori della maschera (nomi = id degli input), con le stesse regole del salvataggio: le persone citate e i capi normalizzati.
assi_coppie <- function(campo, valori) {
  pulisci <- function(x) { x <- trimws(as.character(unlist(x))); unique(x[!is.na(x) & nzchar(x)]) }
  reati <- pulisci(normalizza(pulisci(valori$reato)))
  if (campo == "persona_reati") {
    righe <- if (isTRUE(as.integer(valori$imputati_esaminati) == 1L)) character(0) else pulisci(valori$persona)
    return(list(righe = righe, colonne = reati, avviso = NULL))
  }
  list(righe = reati, colonne = pulisci(valori$luogo))
}

# Al salvataggio restano solo le coppie che il fascicolo può avere:
# una dichiarazione non inventa imputati, capi o luoghi che non ci sono.
.coppie_valide <- function(d, righe, colonne) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- d[d[[1]] %in% righe & d[[2]] %in% colonne, , drop = FALSE]
  if (nrow(d) == 0) NULL else unique(d)
}

# I termini del tesauro dei reati di una scheda: (classe, termine) ammessi solo se la scheda ha la classe e il profilo mette il termine sotto di essa.
.termini_validi <- function(d, reati) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  ammessi <- paste(TERMINI_REATO$classe, TERMINI_REATO$termine)
  d <- d[d[[1]] %in% reati & paste(d[[1]], d[[2]]) %in% ammessi, c("reato", "termine"), drop = FALSE]
  if (nrow(d) == 0) NULL else unique(d)
}

# Le persone di cui si parla nel fascicolo: tutte le nominate, senza distinguere imputati e altre figure (il nome è storico; la tabella `imputati` non entra più).
# Chi ha dichiarato di aver guardato la fonte senza trovarvi nessuno (`imputati_esaminati`) non ne ha.
persone_imputate <- function(f) unique(trimws(.imputati_scheda(f)))
