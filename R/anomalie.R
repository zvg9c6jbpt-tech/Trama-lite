# ──────────────────────────────────────────────
# R/anomalie.R: gli errori già nel database, segnalati in Fascicoli
# Stessi controlli dell'import (`proposte_import`, concordanza.R + revisione_import.R) sul fondo a schermo, letto come foglio del modello;
# le correzioni scelte nella STESSA finestra si riscrivono nelle schede, dopo una copia di sicurezza.
#   anomalie_fondo(con, fondo)                  → list(fondo, a, pr, ids, schede)
#   applica_anomalie_db(con, an, ok, scelte)    → quante schede e celle sono cambiate
# ──────────────────────────────────────────────

anomalie_fondo <- function(con, fondo) {
  t <- tabella_arianna(con, fondo = fondo)
  a <- list(dati = t, colonna = stats::setNames(names(t), names(t)), profilo = MODELLO)
  pr <- proposte_import(a, con, fondo, luoghi_ignoti = FALSE)
  # Un omonimo non è un errore e qui la scelta non si può salvare (le schede tengono solo il nome): non entra nel conteggio né nella finestra, altrimenti il banner non si spegnerebbe mai.
  pr$decisioni <- Filter(function(d) !identical(d$tipo, "omonimo"), pr$decisioni)
  pr$totale <- nrow(pr$celle) + length(pr$decisioni)
  list(fondo = fondo, a = a, pr = pr, ids = attr(t, "ids"), schede = nrow(t))
}

# Quante schede hanno almeno una correzione o una decisione.
schede_con_anomalie <- function(an) {
  if (is.null(an)) return(0L)
  length(unique(c(an$pr$celle$riga, vapply(an$pr$decisioni, function(d) d$riga, 1L))))
}

# Una chiave che cambia quando le schede del fondo cambiano: dice se il controllo è ancora buono.
chiave_fondo <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  r <- DBI::dbGetQuery(con, "SELECT COUNT(*) n, MAX(modificato) m FROM fascicoli WHERE fondo_id = ?", list(fondo))
  paste(r$n, r$m)
}

# Una correzione è «di forma» se prima e dopo differiscono solo per maiuscole e spazi; altrimenti cambia il contenuto e va guardata con più attenzione.
# La punteggiatura NON è forma: «Maria;Nicola» → «Maria|Nicola» fa due persone da una, «Rossi Maria» → «Rossi, Maria» cambia la lettura del nome.
correzioni_di_forma <- function(celle) {
  if (!nrow(celle)) return(logical(0))
  norm <- function(x) gsub("[[:space:]]+", "", tolower(x))
  norm(celle$prima) == norm(celle$dopo)
}

# Il banner in cima a Fascicoli. `vecchio` = il fondo è cambiato dopo l'ultimo controllo (nei fondi grandi si rifà a richiesta).
banner_anomalie_ui <- function(an, vecchio = FALSE) {
  n <- if (is.null(an)) 0L else nrow(an$pr$celle); m <- if (is.null(an)) 0L else length(an$pr$decisioni)
  if (n + m == 0L) return(NULL)
  s <- schede_con_anomalie(an)
  div(class = "alert alert-warning banner-anomalie d-flex flex-wrap align-items-center gap-2", role = "status",
      icon("triangle-exclamation"),
      span(strong(sprintf("%d possibili errori in %d schede", n + m, s)),
           sprintf(" di questo fondo: %s%s. ",
                   if (n) {
                     f <- sum(correzioni_di_forma(an$pr$celle))
                     sprintf("%s%s%s", if (f) sprintf("%d di forma (maiuscole, spazi)", f) else "",
                             if (f && f < n) " e " else "",
                             if (f < n) sprintf("%d di contenuto (refusi in nomi, reati, luoghi)", n - f) else "")
                   } else "",
                   if (m) sprintf("%s%d da decidere", if (n) " e " else "", m) else ""),
           if (vecchio) "Il fondo è cambiato dopo l'ultimo controllo."),
      actionButton("apri_anomalie", "Controlla e correggi…", class = "btn-sm btn-primary"),
      if (vecchio) actionButton("ricontrolla_anomalie", "Ricontrolla", class = "btn-sm btn-outline-secondary"))
}

# Riscrive nelle schede ciò che la finestra ha confermato: per ogni riga cambiata si legge la scheda vera e si applicano solo le DIFFERENZE fra la riga com'era e quella corretta,
# lette con le regole dell'import (un campo a più valori perde il vecchio e prende il nuovo nello stesso posto). Titolo e regesto si riscrivono come sono, senza ricomporli.
applica_anomalie_db <- function(con, an, ok, scelte = list()) {
  a2 <- applica_proposte(an$a, an$pr, ok = ok, scelte = scelte)
  d1 <- an$a$dati; d2 <- a2$dati
  cambiate <- which(vapply(seq_len(nrow(d1)), function(i) !identical(as.character(unlist(d1[i, ])), as.character(unlist(d2[i, ]))), TRUE))
  p <- profilo_di_lettura(an$a$profilo)
  campi_semplici <- c("titolo", "regesto", "supporto", "definizione", "note", "luogo_conservazione", "luogo_produzione", "altra_segnatura")
  n <- 0L
  .atomico(con, for (i in cambiate) {
    b  <- normalizza_valori_fascicolo(con, fascicolo_da_riga(d1[i, ], p, an$a$colonna))     # le stesse regole dell'import: la forma
    c2 <- normalizza_valori_fascicolo(con, fascicolo_da_riga(d2[i, ], p, an$a$colonna))     # salvata, non quella del foglio
    f <- leggi_fascicolo(con, an$ids[i]); if (is.null(f)) next
    cambio <- FALSE
    # Gli «altri campi» (per esempio Soggetti citati o indicizzati) stanno in `extra`: si riscrive solo la voce cambiata, e solo se la scheda ha ancora il valore letto dal controllo.
    for (nm in names(c2$extra)) {
      if (identical(b$extra[[nm]], c2$extra[[nm]])) next
      if (is.null(f$extra) || !identical(as.character(f$extra[[nm]]), as.character(b$extra[[nm]]))) next
      f$extra[[nm]] <- c2$extra[[nm]]; cambio <- TRUE
    }
    for (k in intersect(names(b), names(c2))) {
      if (k %in% c("extra", "campi_extra_noti", "id", "creato", "modificato")) next
      if (identical(b[[k]], c2[[k]])) next
      if (identical(k, "estremo_recente") && is.na(c2[[k]]) && !is.na(b[[k]])) {
        # «estremo recente uguale al remoto»: si toglie l'anno e, se c'è, la data precisa che lo accompagna
        f$estremo_recente <- NA_integer_; f$extra[[EXTRA_DATA_PRECISA[["estremo_recente"]]]] <- NULL; cambio <- TRUE
      } else if (k %in% c("persone", "luoghi", "reati", "lingue", "produttori")) {
        rinomina <- list()
        v <- as.character(unlist(f[[k]])); vecchi <- setdiff(b[[k]], c2[[k]]); nuovi <- setdiff(c2[[k]], b[[k]])
        for (j in seq_along(vecchi)) {
          # Le persone nel database stanno come le ha scritte la fonte («Violo, Giovanni» o «Giovanni Violo»): si riconoscono in ogni forma.
          # La correzione accettata si scrive come la propone la finestra, altrimenti la proposta tornerebbe a ogni controllo.
          w <- if (identical(k, "persone")) which(vapply(v, function(x) identical(.estendi_nome(x), .estendi_nome(vecchi[j])), TRUE))[1] else match(vecchi[j], v)
          if (is.na(w)) next
          if (j <= length(nuovi)) { if (identical(k, "persone")) rinomina[[v[w]]] <- nuovi[j]; v[w] <- nuovi[j] }
          else v[w] <- NA_character_
        }
        # Le coppie persona–reato seguono il nome nuovo: restano solo quelle di persone che la scheda ha ancora (.coppie_valide).
        if (identical(k, "persone") && length(rinomina) && NROW(f$persona_reati)) {
          dp <- as.character(f$persona_reati$persona); i_r <- dp %in% names(rinomina); dp[i_r] <- unlist(rinomina[dp[i_r]], use.names = FALSE)
          f$persona_reati$persona <- dp
        }
        v <- c(v[!is.na(v)], if (length(nuovi) > length(vecchi)) nuovi[-seq_along(vecchi)])
        f[[k]] <- unique(v); cambio <- TRUE
      } else if (k %in% campi_semplici) { f[[k]] <- c2[[k]]; cambio <- TRUE }
    }
    if (cambio) {
      # Cambia SOLO ciò che la finestra ha confermato: titolo e regesto non si ricompongono per effetto collaterale. Si salva con i due flag «a mano» e poi si rimettono com'erano.
      fl <- list(t = f$titolo_manuale, r = f$regesto_manuale)
      f$titolo_manuale <- 1L; f$regesto_manuale <- 1L
      salva_fascicolo(con, f)
      DBI::dbExecute(con, "UPDATE fascicoli SET titolo_manuale = ?, regesto_manuale = ? WHERE id = ?",
                     list(as.integer(fl$t %||% 0L), as.integer(fl$r %||% 0L), f$id))
      n <- n + 1L
    }
  })
  registra("INFO", "anomalie corrette", sprintf("fondo con %d schede: %d schede riscritte", an$schede, n))
  list(schede = n, celle = sum(vapply(names(d1), function(cc) sum(!mapply(identical, as.character(d1[[cc]]), as.character(d2[[cc]]))), 0L)))
}

# D30 nella Scheda (avviso al salvataggio): i due estremi sono la stessa data? Restituisce il testo di quella data (l'anno, o «aaaa-mm-gg») oppure NULL.
# Un anno e una data dentro quell'anno non sono la stessa data.
estremi_uguali <- function(f) {
  r1 <- suppressWarnings(as.integer(f$estremo_remoto)); r2 <- suppressWarnings(as.integer(f$estremo_recente))
  if (length(r1) != 1L || length(r2) != 1L || is.na(r1) || is.na(r2) || r1 != r2) return(NULL)
  p1 <- trimws(as.character(unlist(f$extra[[EXTRA_DATA_PRECISA[["estremo_remoto"]]]]))[1])
  p2 <- trimws(as.character(unlist(f$extra[[EXTRA_DATA_PRECISA[["estremo_recente"]]]]))[1])
  p1 <- if (is.na(p1)) "" else p1; p2 <- if (is.na(p2)) "" else p2
  if (!identical(p1, p2)) return(NULL)
  if (nzchar(p1)) p1 else as.character(r1)
}
