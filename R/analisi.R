# ─────────────────────────────────────────────────────────────
# analisi.R: lo stato di una scheda a parole
# Dipende da ricerca.R (completezza_core)
# ─────────────────────────────────────────────────────────────

# Quali dei quattro campi fondamentali mancano in una scheda, con il campo a cui portare il clic (gli stessi di completezza_core: persone o titolo, anno, luogo, reato).
# Compilato non vuol dire verificato sulla fonte.
core_mancanti <- function(chi, anno, dove, reato) {
  tutti <- data.frame(campo = c("persona", "estremo_remoto", "luogo", "reato"),
                      etichetta = c("Persone o titolo", "Anno", "Luogo", "Reato"),
                      pieno = c(isTRUE(chi), isTRUE(anno), isTRUE(dove), isTRUE(reato)),
                      stringsAsFactors = FALSE)
  tutti[!tutti$pieno, c("campo", "etichetta"), drop = FALSE]
}


# I quattro stati di tortura e sentenza in una scheda: il menu (n.d./sì/no) e la spunta «esaminata» letti insieme; non cambia nessun valore.
# Un sì o un no vale come esaminato anche a spunta spenta (l'import non la accende, Salva sì). `n.d.` senza spunta = nessuno ha guardato la fonte; con la spunta = guardata, e tace.
# Il simbolo accompagna il nome, non lo sostituisce: lo stato si legge anche senza colore.
stato_esame <- function(valore, esaminata) {
  v <- tolower(trimws(as.character(if (length(valore)) valore[1] else NA)))
  e <- isTRUE(suppressWarnings(as.integer(esaminata)) == 1L) || isTRUE(as.logical(esaminata))
  if (v %in% c("sì", "si")) return(list(codice = "si",  simbolo = "\u25CF", testo = "Sì"))
  if (identical(v, "no"))   return(list(codice = "no",  simbolo = "\u2715", testo = "No"))
  if (e) list(codice = "muta", simbolo = "\u25D0", testo = "Esaminata: la fonte tace")
  else   list(codice = "da_esaminare", simbolo = "\u25CB", testo = "Da esaminare")
}
