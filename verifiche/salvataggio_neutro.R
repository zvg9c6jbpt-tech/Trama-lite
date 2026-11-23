# ─────────────────────────────────────────────────────────────
# verifiche/salvataggio_neutro.R: la maschera non deve cambiare i dati
#
# Ogni scheda del database fa il giro della maschera senza essere
# toccata: valori_da_fascicolo() -> raccogli_modulo() -> salva_fascicolo().
# Poi gli export xlsx e CSV devono essere identici a quelli di partenza.
# Il 14/09 questo giro ha trovato che Salva cancellava 250 titoli.
#
# Lavora su una COPIA temporanea di regestum.sqlite: il database vero
# non viene né aperto in scrittura né toccato.
#
# Uso, dalla cartella del progetto:
#     Rscript verifiche/salvataggio_neutro.R
# Atteso: 0 celle diverse in ogni formato. Se no esce con stato 1
# (con source() in RStudio si ferma con un errore).
# ─────────────────────────────────────────────────────────────

suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
source("verifiche/aiuti.R")

copia <- database_di_prova()
con <- apri_db(copia)
profilo_modulo <- profilo_completo("Generale", dbGetQuery(con, "SELECT DISTINCT campo FROM campi_extra")$campo)
on.exit(dbDisconnect(con), add = TRUE)


# Un export e' sempre di UN fondo solo (invariante): si esporta ogni fondo del database e si
# incollano i fogli, che hanno le stesse intestazioni.
fondi <- dbGetQuery(con, "SELECT DISTINCT fondo_id FROM fascicoli ORDER BY fondo, fondo_id")$fondo_id  # per id: due archivi possono avere un fondo con lo stesso nome
export <- function(formato) {
  do.call(rbind, lapply(fondi, function(fo) {
    p <- tempfile(fileext = paste0(".", formato))
    if (formato == "xlsx") esporta_arianna(con, p, fondo = fo)
    else                   esporta_csv(con, p, fondo = fo)
    d <- as.matrix(leggi_tabella(p))
    d[is.na(d)] <- ""
    d
  }))
}

prima <- list(xlsx = export("xlsx"), csv = export("csv"))

ids <- dbGetQuery(con, "SELECT id FROM fascicoli")$id
# I campi che decidono se Salva può svuotare un regesto (regola del 12/10), fotografati PRIMA di risalvare:
# l'atteso si giudica da questi dati e mai dalla forma del testo.
foto <- dbGetQuery(con, "SELECT id, fondo, busta, busta_suffisso, fascicolo, fascicolo_suffisso, regesto, regesto_manuale,
                                imputati_esaminati, reato_ignoto, luogo_ignoto, non_procedimento, tipologia_documentaria,
                                (SELECT COUNT(*) FROM reati r WHERE r.fascicolo_id = f.id) AS n_reati FROM fascicoli f")
# «Regola del regesto vuoto» applicata ai dati della fotografia: vero = Salva DEVE svuotare questo regesto.
da_svuotare <- function(r) {
  vuoto <- function(x) is.na(x) | !nzchar(trimws(as.character(x)))
  # «Non è un procedimento»: il regesto composto non esiste. Altrimenti: niente reato, nessuna
  # dichiarazione di aver guardato la fonte (esaminati, reato ignoto, luogo ignoto) e non «Atti per diversi».
  !vuoto(r$regesto) & r$regesto_manuale == 0L &
    (r$non_procedimento == 1L |
     (r$imputati_esaminati == 0L & r$reato_ignoto == 0L & r$luogo_ignoto == 0L & r$n_reati == 0L &
      !(trimws(r$tipologia_documentaria) %in% TIPOLOGIA_PER_DIVERSI)))
}
# Il caso opposto: un regesto VUOTO (non a mano) che ora ha i dati per essere composto (un reato aggiunto, un flag
# «ignoto») Salva lo compone: è il comportamento voluto. Il testo atteso lo dà la stessa funzione dell'app, calcolata
# sulla scheda com'era PRIMA di risalvare.
attesi_composti <- local({
  v <- foto$id[(is.na(foto$regesto) | !nzchar(trimws(foto$regesto))) & foto$regesto_manuale == 0L]
  t <- vapply(v, function(i) { x <- componi_regesto_scheda(leggi_fascicolo(con, i)); if (is.na(x)) NA_character_ else x }, "")
  t[!is.na(t)]
})
# La scheda di una segnatura del foglio: busta, fascicolo e suffissi dalla segnatura, il fondo come uno dei suoi pezzi
# (analizza_segnatura non lo restituisce quando c'è una serie: «Archivio, Fondo, Serie 1, b. 1, fasc. 2»).
trova_scheda <- function(segnatura) {
  p <- analizza_segnatura(segnatura); pezzi <- strsplit(segnatura, ", ", fixed = TRUE)[[1]]
  which(foto$busta == p$busta & foto$busta_suffisso == p$busta_suffisso & foto$fascicolo == p$fascicolo &
        foto$fascicolo_suffisso == p$fascicolo_suffisso & foto$fondo %in% pezzi)
}
for (id in ids) {
  f    <- raccogli_modulo(valori_da_fascicolo(leggi_fascicolo(con, id), profilo_modulo), profilo_modulo)
  f$id <- id
  salva_fascicolo(con, f)
}

diverse <- sapply(c("xlsx", "csv"), function(formato) {
  a <- prima[[formato]]; b <- export(formato)
  if (!identical(dim(a), dim(b)) || !identical(colnames(a), colnames(b))) return(NA_integer_)
  w <- which(a != b, arr.ind = TRUE)
  # Cambiamento ATTESO (regola del 12/10): Salva svuota un regesto solo se, secondo i dati della scheda PRIMA del
  # salvataggio, non ha reati, non è scritto a mano, non ha «imputati esaminati» e non è
  # «Atti per diversi». Ogni cella è giudicata dalla sua scheda (trovata dalla segnatura): una perdita che la regola
  # non spiega, o un regesto che si svuota senza esserne un caso, fa cadere il collaudo.
  if (nrow(w)) {
    col_regesto <- grepl("^Regesto", colnames(a)[w[, 2]])
    atteso <- rep(FALSE, nrow(w)); id_atteso <- rep(NA_character_, nrow(w))
    for (i in which(col_regesto & b[w] == "" & a[w] != "")) {
      k <- trova_scheda(a[w[i, 1], "Segnatura"])
      if (length(k) == 1L && da_svuotare(foto[k, ])) { atteso[i] <- TRUE; id_atteso[i] <- foto$id[k] }
    }
    if (any(atteso)) {
      cat(sprintf("%s: %d regesti che la regola del 12/10 fa svuotare (senza reato né luogo, non a mano, non esaminati):\n", formato, sum(atteso)))
      for (i in which(atteso)) cat("   ", id_atteso[i], "·", sub("^.*, (b\\.[^,]+, f\\.[^,]+)$", "\\1", a[w[i, 1], "Segnatura"]), "·", trimws(a[w[i, 1], w[i, 2]]), "\n")
    }
    # E la composizione attesa di un regesto vuoto: stessa scheda (dalla segnatura), testo uguale a quello dell'app
    # (nel foglio il regesto esce col punto finale).
    for (i in which(col_regesto & a[w] == "" & b[w] != "" & !atteso)) {
      k <- trova_scheda(a[w[i, 1], "Segnatura"])
      if (length(k) == 1L && !is.na(attesi_composti[foto$id[k]]) &&
          identical(sub("[.]$", "", trimws(unname(b[w[i, 1], w[i, 2]]))), sub("[.]$", "", trimws(unname(attesi_composti[[foto$id[k]]]))))) {
        atteso[i] <- TRUE; id_atteso[i] <- foto$id[k]; cat(sprintf("%s: regesto vuoto composto da Salva (voluto): %s · %s\n", formato, foto$id[k], trimws(unname(b[w[i, 1], w[i, 2]]))))
      }
    }
    if (any(atteso)) w <- w[!atteso, , drop = FALSE]
    # E all'inverso: un regesto che la regola vuole svuotato e che Salva ha lasciato com'è non è una cella diversa,
    # ma dice che la regola non agisce (qui si conta soltanto, il test unitario è in funzioni.R).
  }
  if (nrow(w)) {
    t <- table(colnames(a)[w[, 2]])
    cat(sprintf("%s: celle cambiate per colonna: %s\n", formato,
                paste(names(t), t, sep = " = ", collapse = " · ")))
  }
  nrow(w)
})

cat(sprintf("schede risalvate: %d · celle diverse: xlsx %s, CSV %s\n",
            length(ids), diverse[["xlsx"]], diverse[["csv"]]))
ok <- all(!is.na(diverse) & diverse == 0)
cat(if (ok) "ESITO: ok\n" else "ESITO: NON TORNA\n")
if (!ok) {
  if (interactive()) stop("il salvataggio neutro NON TORNA") else quit(status = 1)
}
