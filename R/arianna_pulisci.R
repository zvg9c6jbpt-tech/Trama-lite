# ─────────────────────────────────────────────────────────────
# R/arianna_pulisci.R: «Pulisci un foglio»: il foglio esce nel modello senza toccare il database
# Dipende da: arianna.R, arianna_import.R, arianna_export.R, concordanza.R e revisione_import.R (avvisi_date, avvisi_reati).
# ─────────────────────────────────────────────────────────────

# ── «Pulisci un foglio» ───────────────────────
# Il foglio entra in un database TEMPORANEO con le regole dell'import e ne esce nel modello; `da` serve solo a prendere decisioni e attribuzione.
# Ha solo le 28 colonne e i refusi corretti (ogni correzione è nel foglio «Correzioni»); ciò che non assomiglia a niente va in «Da decidere».
# Altri fogli: «Correzioni», «Da decidere», «Respinte» (con il motivo). Un file per fondo (zip se più d'uno).
pulisci_foglio <- function(a, da = NULL, nome = "foglio", archivio = NULL, fondo = NULL, serie = NULL, revisionato = FALSE) {
  archivio <- if (is.null(archivio)) NULL else .carattere_pulito(archivio)      # apostrofo dritto anche nei nomi scritti a mano (D2)
  fondo <- if (is.null(fondo)) NULL else .carattere_pulito(fondo); serie <- if (is.null(serie)) NULL else .carattere_pulito(serie)
  tmp <- tempfile(fileext = ".sqlite"); con <- apri_db(tmp)
  on.exit({ try(DBI::dbDisconnect(con), silent = TRUE); unlink(tmp) }, add = TRUE)
  if (!is.null(da)) {
    fo <- DBI::dbGetQuery(da, "SELECT id, archivio, denominazione, serie, profilo, creato, chiave, dichiarato FROM fondi")
    if (nrow(fo)) DBI::dbAppendTable(con, "fondi", fo)          # stessi id: le decisioni del vocabolario si riferiscono a loro
    r <- DBI::dbGetQuery(da, "SELECT fondo_id, campo, valore, forma, creato FROM vocabolario_fondo")
    if (nrow(r)) DBI::dbAppendTable(con, "vocabolario_fondo", r)
    for (k in c("attribuzione_schedatore", "attribuzione_progetto")) {
      v <- leggi_impostazione(da, k); if (!is.null(v)) scrivi_impostazione(con, k, v)
    }
  }
  # Il registro delle modifiche confronta il foglio com'era col pulito: da `applica_proposte` se le correzioni le ha scelte la finestra, altrimenti si ricavano qui.
  originale <- attr(a, "dati_originali") %||% a$dati
  viste <- attr(a, "proposte_viste")
  if (is.null(viste)) {
    viste <- tryCatch(proposte_import(a, da, NULL, luoghi_ignoti = FALSE)$celle, error = function(e) NULL)
    if (!is.null(viste) && nrow(viste)) viste$accettata <- viste$consigliata
  }
  mappa_colonne <- .con_nomi_modello(a$colonna, a$profilo)
  # Nomi e luoghi si correggono sul foglio PRIMA di importare; ogni cella toccata è una riga di «Correzioni», il resto va in «Da decidere».
  correzioni <- correzioni_import(a, con, concordanza = FALSE)
  # `revisionato`: le correzioni le ha già scelte la finestra, non si rifanno (una tolta dalla spunta tornerebbe).
  cz <- concordanza_righe(a, avvisi_luoghi = !revisionato); if (!revisionato) a$dati <- cz$dati
  if (!revisionato && nrow(cz$registro)) correzioni <- rbind(correzioni, data.frame(
    regola = sprintf("riga %d: «%s» → «%s» (%s)", cz$registro$riga, cz$registro$prima, cz$registro$dopo, cz$registro$perche),
    colonna = cz$registro$colonna, celle = 1L, stringsAsFactors = FALSE))
  # Una segnatura con un nome solo non dice archivio e fondo: chi pulisce può scriverli, valgono per tutte le righe.
  esito <- if (!.vuoto(archivio) && !.vuoto(fondo)) importa_nuovo_archivio(con, a, archivio, fondo, serie)
           else importa_arianna(con, a, modo = "accoda")
  fondi <- DBI::dbGetQuery(con, "SELECT f.id FROM fondi f WHERE f.id IN (SELECT fondo_id FROM fascicoli) ORDER BY f.denominazione")$id        # gli id; per i file serve il nome
  if (!length(fondi)) stop("Nessuna riga è entrata, quindi non c'è niente da scaricare: guarda le righe respinte.", call. = FALSE)
  # I refusi: si correggono nel database temporaneo, prima di esportare.
  corrette <- data.frame(regola = character(0), colonna = character(0), celle = integer(0), stringsAsFactors = FALSE)
  da_decidere <- stats::setNames(rep(list(data.frame(Colonna = character(0), Valore = character(0), Schede = integer(0), stringsAsFactors = FALSE)), length(fondi)), fondi)
  for (f in fondi) for (k in CAMPI_CONTROLLATI$campo) {
    v <- valori_colonna(con, f, k); et <- CAMPI_CONTROLLATI$etichetta[CAMPI_CONTROLLATI$campo == k]
    s <- v[v$stato %in% c("vicino", "maiuscole") & !is.na(v$proposta) & nzchar(v$proposta), , drop = FALSE]
    if (revisionato && k == "reato") s <- s[0, , drop = FALSE]                    # i refusi dei reati li ha visti la finestra
    if (revisionato && k %in% c("lingua", "supporto", "definizione")) s <- s[s$stato != "vicino", , drop = FALSE]
    if (nrow(s)) {
      sostituisci_valori(con, f, k, stats::setNames(s$proposta, s$valore))
      corrette <- rbind(corrette, data.frame(regola = sprintf("refuso corretto: «%s» → «%s»", s$valore, s$proposta), colonna = et,
                                             celle = as.integer(s$schede), stringsAsFactors = FALSE))
    }
    n <- v[v$stato == "nuovo", , drop = FALSE]
    if (nrow(n)) da_decidere[[f]] <- rbind(da_decidere[[f]], data.frame(Colonna = et, Valore = n$valore, Schede = as.integer(n$schede), stringsAsFactors = FALSE))
  }
  dd_nomi <- .senza_omonimi_scelti(cz$da_decidere, a)       # un omonimo scelto in finestra sta in «Luoghi scelti», non anche qui
  if (nrow(dd_nomi)) for (f in fondi) da_decidere[[f]] <- rbind(da_decidere[[f]],
    data.frame(Colonna = "Nomi o luoghi della riga", Valore = dd_nomi$testo, Schede = 1L, stringsAsFactors = FALSE))
  # Le date illeggibili (il foglio le conserva com'erano): con la finestra di revisione le ha già viste chi ha scelto.
  ar <- if (revisionato) NULL else avvisi_reati(a)
  if (!is.null(ar) && nrow(ar)) for (f in fondi) da_decidere[[f]] <- rbind(da_decidere[[f]],
    data.frame(Colonna = "Reati della riga", Valore = ar$testo, Schede = 1L, stringsAsFactors = FALSE))
  ad <- if (revisionato) NULL else avvisi_date(a)
  if (!is.null(ad) && nrow(ad)) for (f in fondi) da_decidere[[f]] <- rbind(da_decidere[[f]],
    data.frame(Colonna = "Date della riga", Valore = ad$testo, Schede = 1L, stringsAsFactors = FALSE))
  if (nrow(corrette)) correzioni <- rbind(correzioni, corrette)
  # Le colonne del modello che il foglio non ha escono vuote: il foglio pulito ha sempre le 28.
  if (length(a$mancanti)) correzioni <- rbind(correzioni, data.frame(regola = "colonna del modello assente nel foglio: aggiunta, vuota",
                                                                     colonna = a$mancanti, celle = as.integer(esito$importati), stringsAsFactors = FALSE))
  base <- gsub("[^[:alnum:]_.-]+", "-", sub("\\.[A-Za-z0-9]+$", "", nome))
  cartella <- tempfile("pulito-"); dir.create(cartella)
  file_di <- function(f) file.path(cartella, paste0(base, "-pulito", if (length(fondi) > 1) paste0("-", gsub("[^[:alnum:]]+", "-", nome_fondo(con, f))), ".xlsx"))
  conti <- function(d, vuoto) if (nrow(d)) d else vuoto
  registri <- list()
  for (f in fondi) {
    p <- file_di(f); esporta_arianna(con, p, fondo = f)
    # Righe sorgente di QUESTO fondo: due fondi con la stessa «b. 1, fasc. 1» non si confondono.
    rg <- esito$riga_fondo; rg <- if (is.null(rg)) seq_len(nrow(originale)) + 1L else rg$riga[rg$fondo == f]
    reg <- tryCatch(registro_modifiche(originale[rg - 1L, , drop = FALSE], mappa_colonne, as.data.frame(readxl::read_excel(p, sheet = 1, col_types = "text"), check.names = FALSE),
                                       viste, scritti = c(archivio, fondo), righe = rg), error = function(e) NULL)
    registri[[length(registri) + 1L]] <- reg
    corr_f <- correzioni
    acc_ <- if (!is.null(reg) && nrow(reg)) attr(reg, "da_proposta") else logical(0)
    if (any(acc_)) {      # le correzioni accettate in finestra: una riga per regola e colonna
      s_ <- reg[acc_, ]; k_ <- unique(s_[, c("Regola", "Colonna")])
      corr_f <- rbind(corr_f, data.frame(regola = k_$Regola, colonna = k_$Colonna, celle = vapply(seq_len(nrow(k_)), function(i) sum(s_$Regola == k_$Regola[i] & s_$Colonna == k_$Colonna[i]), 1L), stringsAsFactors = FALSE))
    }
    wb <- openxlsx::loadWorkbook(p)
    openxlsx::addWorksheet(wb, "Correzioni")
    openxlsx::writeData(wb, "Correzioni", conti(stats::setNames(corr_f, c("Regola", "Colonna", "Celle")),
                                                data.frame(Regola = "Nessuna correzione", Colonna = "", Celle = "")))
    openxlsx::addWorksheet(wb, "Registro modifiche")
    openxlsx::writeData(wb, "Registro modifiche", conti(reg %||% data.frame(), data.frame(Riga = "", Segnatura = "Nessuna modifica", Colonna = "", Prima = "", Dopo = "", Regola = "", Dichiarata = "")))
    openxlsx::setColWidths(wb, "Registro modifiche", cols = 1:7, widths = c(7, 40, 28, 45, 45, 55, 12))
    openxlsx::addWorksheet(wb, "Da decidere")
    openxlsx::writeData(wb, "Da decidere", conti(da_decidere[[f]], data.frame(Colonna = "Nessun valore da decidere", Valore = "", Schede = "")))
    openxlsx::setColWidths(wb, "Da decidere", cols = 1:3, widths = c(30, 60, 8))
    # Gli omonimi scelti nella finestra: la cella non cambia, la scelta (con identificativo e coordinate) sta qui.
    om_ <- attr(a, "omonimi_scelti")
    if (!is.null(om_) && nrow(om_) && any(om_$Riga %in% rg)) {
      openxlsx::addWorksheet(wb, "Luoghi scelti"); openxlsx::writeData(wb, "Luoghi scelti", om_[om_$Riga %in% rg, , drop = FALSE])
      openxlsx::setColWidths(wb, "Luoghi scelti", cols = 1:7, widths = c(7, 24, 28, 20, 45, 12, 22))
    }
    openxlsx::addWorksheet(wb, "Respinte")
    openxlsx::writeData(wb, "Respinte", conti(stats::setNames(esito$respinti, c("Riga", "Segnatura", "Motivo")),
                                              data.frame(Riga = "Nessuna riga respinta", Segnatura = "", Motivo = "")))
    openxlsx::setColWidths(wb, "Correzioni", cols = 1:3, widths = c(70, 40, 8))
    openxlsx::setColWidths(wb, "Respinte", cols = 1:3, widths = c(8, 70, 70))
    openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
    .togli_relazioni_pendenti(p)
  }
  files <- vapply(fondi, file_di, "", USE.NAMES = FALSE)
  if (length(files) == 1L) { percorso <- files }
  else { percorso <- file.path(cartella, paste0(base, "-pulito.zip")); zip::zipr(percorso, files = basename(files), root = cartella) }
  list(percorso = percorso, nome = basename(percorso), fondi = fondi, letti = esito$letti, importati = esito$importati,
       respinti = esito$respinti, correzioni = correzioni, registro = if (length(registri)) do.call(rbind, registri) else NULL, da_decidere = do.call(rbind, unname(da_decidere)))
}

