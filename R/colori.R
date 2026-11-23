# ─────────────────────────────────────────────────────────────
# R/colori.R: i colori dei reati
# Li usano le pillole del Soggettario nella Scheda (R/tema.R, `.css_pillole_reati`). Non dipende da ggplot2.
# ─────────────────────────────────────────────────────────────

# Un colore per reato, sempre lo stesso in ogni pillola. Toni terrosi nella palette pergamena/bosco, in scuro le stesse tinte schiarite.
# Il nome del reato resta sempre scritto: il colore aiuta a ritrovarlo, non è l'unico modo di leggerlo. Un reato fuori vocabolario prende un colore di riserva, stabile per nome.
COLORI_REATI <- list(
  # Le tinte sono ritoccate (stessa famiglia, al massimo 14° di tinta e 9 di luminosità) perché con protanopia e deuteranopia alcune coppie diventavano indistinguibili:
  # ora la coppia più vicina sta a ΔE 15 con la vista normale e a circa 10 con i due daltonismi (matrici di Machado).
  # Eresia e Bestemmia escono dai colori di stato (rosso di errore, ambra degli avvisi): fucsia e oro, ΔE ≥ 19 da ogni altro colore.
  # Niente marroni né blu: rosso, viola, ambra, verde, grigio, arancio, oliva, rosa, sabbia, grafite e verde acqua.
  chiaro = c("Eresia" = "#983D9F", "Magia" = "#5C1E60", "Bestemmia e miscredenza" = "#A7AA22",
             "Sacramenti" = "#536A3A", "Giurisdizione" = "#5C564F", "Ebraismo" = "#EC7F4C",
             "Censura" = "#5C8F15", "Crimini sessuali" = "#BE4D70", "Santità" = "#9C916D",
             "Clero" = "#2B2B29", "Islam" = "#17877B"),
  scuro  = c("Eresia" = "#E964B7", "Magia" = "#CFA3E6", "Bestemmia e miscredenza" = "#BFD408",
             "Sacramenti" = "#A8BE8D", "Giurisdizione" = "#BFC1C2", "Ebraismo" = "#EF9B61",
             "Censura" = "#98CE47", "Crimini sessuali" = "#D77C98", "Santità" = "#DBD2AC",
             "Clero" = "#F3E4DF", "Islam" = "#97DCE2"))
.RISERVA_REATI <- list(chiaro = c("#6D5A2E", "#48606E", "#7A4A64", "#556B52"),
                       scuro  = c("#C9B27A", "#9FB6C4", "#CFA0B9", "#A9C2A4"))
colori_reati <- function(reati, modo = "chiaro") {
  reati <- as.character(reati); tab <- COLORI_REATI[[modo]]; ris <- .RISERVA_REATI[[modo]]
  i <- match(tolower(reati), tolower(names(tab)))
  out <- unname(tab[i])
  fuori <- is.na(i)
  out[fuori] <- ris[1L + vapply(reati[fuori], function(r) sum(utf8ToInt(r)), numeric(1)) %% length(ris)]
  stats::setNames(out, reati)
}
