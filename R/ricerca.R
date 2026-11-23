# ─────────────────────────────────────────────────────────────
# ricerca.R: ricerca avanzata e coorti salvate
# Dipende da crud.R (.ora), qualita.R (.senza_accenti)
# La query combina filtri `campo:valore` (in AND) e testo libero: "reato:eresia anno:1620-1650 tortura:sì busta:9 stregoneria",
# vettoriale sul data.frame di elenco_fascicoli(). Una coorte è la query salvata con un nome: non tocca mai i fascicoli.
# ─────────────────────────────────────────────────────────────

CAMPI_RICERCA_AVANZATA <- c("reato", "anno", "busta", "fascicolo", "tortura",
                            "sentenza", "luogo", "persona", "titolo", "completo", "esame")

# Le stesse parole chiave in inglese, valide in qualunque lingua sia l'interfaccia.
CAMPI_RICERCA_EN <- c(offence = "reato", offense = "reato", year = "anno", box = "busta", casefile = "fascicolo",
                      torture = "tortura", sentence = "sentenza", place = "luogo", person = "persona",
                      title = "titolo", complete = "completo", check = "esame")
# «offence:heresy»: il valore inglese di un reato torna alla voce italiana
# con cui è scritto nel database (VOCABOLARI_EN, i18n.R).
.reato_da_inglese <- function(valore) {
  if (!exists("VOCABOLARI_EN")) return(valore)
  v <- VOCABOLARI_EN[["reato"]]; en <- .senza_accenti(tolower(unname(v)))
  i <- which(startsWith(en, valore))
  if (length(i) == 1 || valore %in% en) .senza_accenti(tolower(names(v)[if (valore %in% en) match(valore, en) else i])) else valore
}

# "1620" -> c(1620,1620); "1620-1650" -> c(1620,1650); NULL se non si legge.
.parse_intervallo_ricerca <- function(v) {
  m <- regmatches(v, regexec("^([0-9]+)(?:-([0-9]+))?$", v))[[1]]
  if (length(m) == 0) return(NULL)
  da <- as.integer(m[2])
  a  <- if (nzchar(m[3])) as.integer(m[3]) else da
  if (da <= a) c(da, a) else c(a, da)
}

.parse_sino_nd_ricerca <- function(v) {
  n <- .senza_accenti(tolower(v))
  if (n %in% c("si", "yes")) return("sì")
  if (n == "no") return("no")
  if (n %in% c("nd", "n.d.", "n.d", "undocumented", "n/s", "ns", "notstated", "not-stated")) return("n.d.")
  NULL
}

# Un campo con più valori arriva concatenato " | " da elenco_fascicoli()
# (reati/luoghi/persone): vero se il valore cercato è in almeno uno.
.contiene_concat <- function(campo, valore) {
  campo <- ifelse(is.na(campo), "", campo)
  vapply(strsplit(campo, " \\| "), function(v)
    any(grepl(valore, .senza_accenti(tolower(v)), fixed = TRUE)), logical(1))
}

# I 4 campi core della catalogazione (persone o titolo, anno, luoghi, reato): tortura e sentenza non contano, spesso non reperibili nella fonte.
.completo_riga <- function(d) completezza_core(d) == 4L

# Quanti dei 4 campi core sono compilati, da 0 a 4: lo "Stato" della
# tabella Fascicoli e la completezza per busta dello scaffale.
completezza_core <- function(d) {
  m <- campi_core(d)
  as.integer(m[[1]]) + as.integer(m[[2]]) + as.integer(m[[3]]) + as.integer(m[[4]])
}

# I quattro campi fondamentali, uno per colonna (TRUE = compilato); servono a completezza_core e all'elenco delle schede incomplete (R/avanzamento.R).
campi_core <- function(d) {
  pieno <- function(x) nzchar(ifelse(is.na(x), "", x))
  # Flag sulla fonte: «reato ignoto» e «luogo ignoto» dicono che la fonte è stata guardata; «non è un procedimento» toglie reato e luogo dal conto; «data non disponibile» (n.d.) dichiara l'anno assente dalla fonte, quindi chiude il campo anno.
  fl <- function(n) if (n %in% names(d)) !is.na(d[[n]]) & d[[n]] == 1L else rep(FALSE, nrow(d))
  np <- fl("non_procedimento")
  # «sec. XV», «prima metà del sec. XV»…: la scheda è datata, non per anno; non è una lacuna e l'anno non si inventa (`schede_solo_secolo` le conta).
  secolo <- if ("anno_non_risolto" %in% names(d)) is.na(d$anno) & vapply(d$anno_non_risolto, .datazione_secolo, TRUE, USE.NAMES = FALSE) else rep(FALSE, nrow(d))
  data.frame(persone_o_titolo = pieno(d$persone) | pieno(d$titolo), anno = !is.na(d$anno) | secolo | fl("data_nd"),
             luogo = pieno(d$luoghi) | fl("luogo_ignoto") | np, reato = pieno(d$reati) | fl("reato_ignoto") | np)
}

# Tortura o sentenza ancora da esaminare: `n.d.` senza la spunta «esaminata»; un sì o un no vale come esaminato. Serve a «Verifiche della fonte» e a `esame:da-fare`.
esame_da_fare <- function(d) {
  nd <- function(v) is.na(v) | !tolower(trimws(as.character(v))) %in% c("sì", "si", "no")
  fl <- function(x) !is.na(x) & x == 1
  (nd(d$tortura) & !fl(d$tortura_esaminata)) | (nd(d$sentenza) & !fl(d$sentenza_esaminata))
}

# Maschera logica per un token «campo:valore», o NULL se il campo non è riconosciuto o il valore non si legge (il token torna testo libero).
.predicato_campo <- function(d, campo, valore_grezzo) {
  valore <- .senza_accenti(tolower(trimws(valore_grezzo)))
  if (!nzchar(valore)) return(NULL)

  if (campo %in% c("anno", "busta", "fascicolo")) {
    iv <- .parse_intervallo_ricerca(valore)
    if (is.null(iv)) return(NULL)
    colonna <- d[[campo]]
    return(!is.na(colonna) & colonna >= iv[1] & colonna <= iv[2])
  }
  if (campo %in% c("tortura", "sentenza")) {
    v <- .parse_sino_nd_ricerca(valore)
    if (is.null(v)) return(NULL)
    colonna <- d[[campo]]
    return(!is.na(colonna) & colonna == v)
  }

  if (identical(campo, "esame")) {
    da_fare <- esame_da_fare(d)
    if (valore %in% c("da-fare", "dafare", "da_fare", "pending", "todo")) return(da_fare)
    if (valore %in% c("fatto", "fatti", "done")) return(!da_fare)
    return(NULL)
  }

  switch(campo,
    # La classe della Guida o un termine del tesauro sotto di lei (reato:bigamia trova le schede in «Crimini sessuali» che hanno quel termine).
    reato     = .contiene_concat(d$reati,   valore) | .contiene_concat(d$reati, .reato_da_inglese(valore)) |
                (if (is.null(d$termini)) FALSE else .contiene_concat(d$termini, valore)),
    luogo     = .contiene_concat(d$luoghi,  valore),
    persona   = .contiene_concat(d$persone, valore),
    titolo    = grepl(valore, .senza_accenti(tolower(ifelse(is.na(d$titolo), "", d$titolo))), fixed = TRUE),
    completo  = if (valore %in% c("si", "yes")) .completo_riga(d)
                else if (valore == "no") !.completo_riga(d)
                else NULL,
    NULL
  )
}

# Il testo che resta tolti i «campo:valore» riconosciuti: un'unica sottostringa su titolo + reati + luoghi + persone + segnatura (non un AND fra parole).
.testo_libero_maschera <- function(d, testo) {
  if (!nzchar(trimws(testo))) return(rep(TRUE, nrow(d)))
  q <- .senza_accenti(tolower(trimws(testo)))
  unito <- tolower(paste(
    ifelse(is.na(d$titolo),  "", d$titolo),
    ifelse(is.na(d$reati),   "", d$reati),
    if (is.null(d$termini)) "" else ifelse(is.na(d$termini), "", d$termini),
    ifelse(is.na(d$luoghi),  "", d$luoghi),
    ifelse(is.na(d$persone), "", d$persone),
    sprintf("b.%s f.%s", d$busta, d$fascicolo)
  ))
  grepl(q, .senza_accenti(unito), fixed = TRUE)
}

# Spezza la query sugli spazi tenendo insieme ciò che sta fra virgolette (persona:"Russo, Giovanni"); le virgolette si tolgono, una rimasta aperta vale fino in fondo.
.token_ricerca <- function(query) {
  q <- trimws(query)
  if (lengths(regmatches(q, gregexpr('"', q, fixed = TRUE))) %% 2L == 1L) q <- paste0(q, '"')
  tk <- regmatches(q, gregexpr('(?:[^\\s"]+|"[^"]*")+', q, perl = TRUE))[[1]]
  tk <- gsub('"', "", tk, fixed = TRUE)
  tk[nzchar(tk)]
}

# Che cosa non si è capito della query: un messaggio per ogni `campo:valore` con campo giusto e valore illeggibile (senza, «anno:abc» darebbe zero risultati in silenzio).
avvisi_ricerca <- function(query) {
  if (is.null(query) || !nzchar(trimws(query))) return(character(0))
  out <- character(0)
  for (t in .token_ricerca(query)) {
    sep <- regexpr(":", t, fixed = TRUE); if (sep <= 1) next
    campo <- .senza_accenti(tolower(substr(t, 1, sep - 1)))
    if (campo %in% names(CAMPI_RICERCA_EN)) campo <- unname(CAMPI_RICERCA_EN[campo])
    valore <- .senza_accenti(tolower(trimws(substr(t, sep + 1, nchar(t)))))
    if (!campo %in% CAMPI_RICERCA_AVANZATA || !nzchar(valore)) next
    if (campo %in% c("anno", "busta", "fascicolo") && is.null(.parse_intervallo_ricerca(valore)))
      out <- c(out, sprintf("%s: atteso un numero o un intervallo (1560 oppure 1560-1565)", campo))
    else if (campo == "esame" && is.null(.predicato_campo(data.frame(tortura = "n.d.", sentenza = "n.d.", tortura_esaminata = 0L, sentenza_esaminata = 0L), "esame", valore)))
      out <- c(out, "esame: atteso da-fare oppure fatto")
    else if (campo %in% c("tortura", "sentenza") && is.null(.parse_sino_nd_ricerca(valore)))
      out <- c(out, sprintf("%s: atteso sì, no oppure n.d.", campo))
    else if (campo == "completo" && !valore %in% c("si", "yes", "no"))
      out <- c(out, "completo: atteso sì oppure no")
  }
  unique(out)
}

# Applica la query avanzata a un data.frame gia' filtrato dai controlli
# strutturati (buste/anni/reato): query vuota -> nessun cambiamento.
filtra_avanzato <- function(d, query) {
  if (is.null(query) || !nzchar(trimws(query)) || nrow(d) == 0) return(d)

  token   <- .token_ricerca(query)
  maschere <- list()
  residuo  <- character(0)

  for (t in token) {
    sep <- regexpr(":", t, fixed = TRUE)
    riconosciuto <- FALSE
    if (sep > 1) {
      campo  <- .senza_accenti(tolower(substr(t, 1, sep - 1)))
      if (campo %in% names(CAMPI_RICERCA_EN)) campo <- unname(CAMPI_RICERCA_EN[campo])
      valore <- substr(t, sep + 1, nchar(t))
      if (campo %in% CAMPI_RICERCA_AVANZATA && !nzchar(trimws(valore))) {
        # «reato:» ancora da completare (lo scrive un clic sull'etichetta): non filtra, invece di cercare «reato:» come testo.
        riconosciuto <- TRUE
      } else if (campo %in% CAMPI_RICERCA_AVANZATA) {
        m <- .predicato_campo(d, campo, valore)
        if (!is.null(m)) { maschere[[length(maschere) + 1]] <- m; riconosciuto <- TRUE }
      }
    }
    if (!riconosciuto) residuo <- c(residuo, t)
  }

  maschera <- if (length(maschere)) Reduce(`&`, maschere) else rep(TRUE, nrow(d))
  maschera <- maschera & .testo_libero_maschera(d, paste(residuo, collapse = " "))
  d[which(maschera), , drop = FALSE]
}

# --- coorti: la query salvata con un nome ---------------------------

elenco_coorti <- function(con) dbGetQuery(con, "SELECT id, nome, query FROM coorti ORDER BY nome COLLATE NOCASE")

# Upsert per nome: salvare due volte con lo stesso nome sovrascrive la
# query, non duplica la coorte.
salva_coorte <- function(con, nome, query) {
  pulito <- trimws(nome)
  if (!nzchar(pulito) || !nzchar(trimws(query))) return(invisible(NULL))
  esistente <- dbGetQuery(con, "SELECT id FROM coorti WHERE nome = ?", list(pulito))
  if (nrow(esistente))
    dbExecute(con, "UPDATE coorti SET query = ? WHERE id = ?", list(query, esistente$id[1]))
  else
    dbExecute(con, "INSERT INTO coorti (nome, query, creato) VALUES (?, ?, ?)", list(pulito, query, .ora()))
  invisible(NULL)
}

elimina_coorte <- function(con, id) {
  dbExecute(con, "DELETE FROM coorti WHERE id = ?", list(id))
  invisible(NULL)
}
