# ─────────────────────────────────────────────────────────────
# errori_import.R: la finestra «Errori e discordanze» dell'import
# Quando un import (o «Pulisci e scarica») dà errori o cose da decidere, un pulsante apre UNA finestra: righe respinte (col motivo), nomi e luoghi che non concordano,
# valori da decidere, correzioni già fatte; lo stesso elenco si scarica in un .xlsx. `elenco_errori_import()` è pura: prende l'esito e restituisce i fogli, vuoti compresi.
# ─────────────────────────────────────────────────────────────
.vuoto_df <- function(...) as.data.frame(stats::setNames(replicate(length(c(...)), character(0), simplify = FALSE), c(...)), stringsAsFactors = FALSE)

elenco_errori_import <- function(esito = NULL, pulito = NULL) {
  resp <- if (!is.null(pulito)) pulito$respinti else if (!is.null(esito)) esito$respinti else NULL
  respinte <- if (is.null(resp) || !nrow(resp)) .vuoto_df("Riga", "Segnatura", "Motivo")
              else stats::setNames(as.data.frame(resp[, 1:3], stringsAsFactors = FALSE), c("Riga", "Segnatura", "Motivo"))
  # Da decidere: valori di una colonna e, per i nomi e i luoghi, i casi della riga
  dec <- if (!is.null(pulito) && !is.null(pulito$da_decidere) && nrow(pulito$da_decidere))
           stats::setNames(as.data.frame(pulito$da_decidere[, 1:3], stringsAsFactors = FALSE), c("Colonna", "Valore", "Schede"))
         else .vuoto_df("Colonna", "Valore", "Schede")
  if (!is.null(esito) && !is.null(esito$da_decidere_nomi) && nrow(esito$da_decidere_nomi))
    dec <- rbind(dec, data.frame(Colonna = "Nomi o luoghi della riga", Valore = esito$da_decidere_nomi$testo, Schede = "1", stringsAsFactors = FALSE))
  # Luoghi con «Elimina»: quelli tolti stanno in una scheda a parte; quelli non toccati (la cella aveva ancora un separatore da correggere) vanno fra i casi da decidere.
  tolti <- if (!is.null(esito) && !is.null(esito$luoghi_tolti) && nrow(esito$luoghi_tolti)) esito$luoghi_tolti else .vuoto_df("Riga", "Colonna", "Prima", "Dopo")
  if (!is.null(esito) && !is.null(esito$luoghi_rimasti) && nrow(esito$luoghi_rimasti))
    dec <- rbind(dec, data.frame(Colonna = esito$luoghi_rimasti$Colonna,
                                 Valore = sprintf("riga %d: «%s» non toccata, il luogo da eliminare è ancora lì", esito$luoghi_rimasti$Riga, esito$luoghi_rimasti$Cella),
                                 Schede = "1", stringsAsFactors = FALSE))
  # Discordanze: ciò che «Pulisci e scarica» corregge (nell'import normale si mostra, non si applica)
  disc <- if (!is.null(pulito) && !is.null(pulito$correzioni) && nrow(pulito$correzioni)) {
            r <- pulito$correzioni[grepl("^riga \\d+: ", pulito$correzioni$regola), , drop = FALSE]
            if (nrow(r)) data.frame(Riga = sub("^riga (\\d+): .*$", "\\1", r$regola), Colonna = r$colonna,
                                    Correzione = sub("^riga \\d+: ", "", r$regola), stringsAsFactors = FALSE)
            else .vuoto_df("Riga", "Colonna", "Correzione")
          } else if (!is.null(esito) && !is.null(esito$discordanze) && nrow(esito$discordanze))
            data.frame(Riga = as.character(esito$discordanze$riga), Colonna = esito$discordanze$colonna,
                       Correzione = sprintf("«%s» → «%s» (%s)", esito$discordanze$prima, esito$discordanze$dopo, esito$discordanze$perche), stringsAsFactors = FALSE)
          else .vuoto_df("Riga", "Colonna", "Correzione")
  scelti <- if (!is.null(esito) && !is.null(esito$omonimi_scelti) && nrow(esito$omonimi_scelti))
              esito$omonimi_scelti[, c("Riga", "Colonna", "Luogo", "Scelta", "Wikidata", "Coordinate")] else .vuoto_df("Riga", "Colonna", "Luogo", "Scelta", "Wikidata", "Coordinate")
  list(respinte = respinte, da_decidere = dec, discordanze = disc, luoghi_tolti = tolti, luoghi_scelti = scelti,
       totale = nrow(respinte) + nrow(dec) + nrow(disc) + nrow(tolti),
       da_correggere = nrow(respinte) + nrow(dec))     # ciò che resta in sospeso: le discordanze si correggono con «Pulisci e scarica»
}

# Il foglio .xlsx con l'elenco, per lavorarci fuori.
scrivi_errori_xlsx <- function(elenco, percorso) {
  wb <- openxlsx::createWorkbook()
  agg <- function(nome, d, larg) { openxlsx::addWorksheet(wb, nome); openxlsx::writeData(wb, nome, if (nrow(d)) d else d[0, , drop = FALSE])
                                   openxlsx::setColWidths(wb, nome, cols = seq_along(larg), widths = larg) }
  agg("Respinte", elenco$respinte, c(8, 70, 80)); agg("Da decidere", elenco$da_decidere, c(32, 90, 9)); agg("Discordanze", elenco$discordanze, c(8, 34, 100))
  if (nrow(elenco$luoghi_tolti)) agg("Luoghi tolti", elenco$luoghi_tolti, c(8, 34, 60, 60))
  if (nrow(elenco$luoghi_scelti)) agg("Luoghi scelti", elenco$luoghi_scelti, c(8, 34, 24, 50, 12, 22))
  openxlsx::saveWorkbook(wb, percorso, overwrite = TRUE); .togli_relazioni_pendenti(percorso); invisible(percorso)
}
