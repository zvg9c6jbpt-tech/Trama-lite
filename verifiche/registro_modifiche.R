# ──────────────────────────────────────────────
# verifiche/registro_modifiche.R: ogni modifica fra il foglio originale e il pulito è nel registro (22/10/2026, parere di Codex)
# Il percorso intero: scelta nell'anteprima → file esportato → registro. Dopo la scelta, l'uscita non deve cambiare altro in silenzio.
#     Rscript verifiche/registro_modifiche.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
COL <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
foglio <- function(...) {
  v_ <- list(...); riga <- stats::setNames(as.list(rep(NA_character_, length(COL))), COL)
  riga[["Segnatura"]] <- "Archivio di Prova, Fondo Prova, b. 1, fasc. 1"; riga[["Titolo"]] <- "Atti"
  for (k in names(v_)) riga[[k]] <- v_[[k]]
  f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(as.data.frame(riga, check.names = FALSE, stringsAsFactors = FALSE), f); leggi_arianna(f)
}
pulisci_con <- function(a, accetta = "consigliate") {
  pr <- proposte_import(a, NULL, NULL, luoghi_ignoti = FALSE)
  ok <- if (identical(accetta, "consigliate")) pr$celle$id[pr$celle$consigliata] else integer(0)
  a1 <- applica_proposte(a, pr, ok = ok, scelte = list())
  list(p = pulisci_foglio(a1, nome = "prova.xlsx", revisionato = TRUE), pr = pr)
}
foglio_di <- function(p, nome) as.data.frame(readxl::read_excel(p$percorso, sheet = nome, col_types = "text"), check.names = FALSE)

a <- foglio("Luoghi citati o indicizzati" = "S. Severo|Torino|Torino", "Persone citate o indicizzate" = "Anna di Mosca|Rossi, Mario",
            "Regesto o descrizione sintetica" = "Atti di prova")
r <- pulisci_con(a); p <- r$p; reg <- p$registro; pul <- foglio_di(p, 1)
v("l'uscita ha le correzioni accettate: «S. Severo» → «San Severo»", grepl("^San Severo", pul[["Luoghi citati o indicizzati"]]))
v("il nome senza virgola resta com'è scritto («Anna di Mosca» non diventa «Mosca, Anna di»)", grepl("^Anna di Mosca\\|", pul[["Persone citate o indicizzate"]]))
v("il registro ha la correzione accettata, dichiarata «sì», con segnatura, prima e dopo", any(reg$Colonna == "Luoghi citati o indicizzati" & reg$Dichiarata != "no" & grepl("«S.» sciolto", reg$Regola) & grepl("S\\. Severo", reg$Prima) & grepl("San Severo", reg$Dopo) & nzchar(reg$Segnatura)))
v("il duplicato di luogo tolto dal database è nel registro, accanto alla correzione, come convenzione dichiarata (D4)", any(grepl("duplicato esatto", reg$Regola) & reg$Dichiarata == "convenzione"))
v("il punto finale del regesto è una convenzione dichiarata", any(reg$Colonna == "Regesto o descrizione sintetica" & reg$Dichiarata == "convenzione" & grepl("punto finale", reg$Regola)))
v("nessuna modifica è «non dichiarata»", !any(reg$Dichiarata == "no"))
cor <- foglio_di(p, "Correzioni")
v("il foglio «Correzioni» riporta le correzioni accettate in finestra (non solo i refusi)", any(grepl("S\\.", cor$Regola) & grepl("Luoghi", cor$Colonna)))
rf <- foglio_di(p, "Registro modifiche")
v("il foglio «Registro modifiche» è il registro intero (colonne e righe)", identical(names(rf), c("Riga", "Segnatura", "Colonna", "Prima", "Dopo", "Regola", "Dichiarata")) && nrow(rf) == nrow(reg))
cp <- correzioni_import(a)
v("l'anteprima conta le convenzioni di uscita (punto finale; tipologia vuota)", any(grepl("punto finale aggiunto in uscita", cp$regola)) && any(grepl("tipologia documentaria vuota", cp$regola)))

# Deselezionare vale fino all'export: una correzione non accettata non compare nel pulito e non è nel registro
r0 <- pulisci_con(a, accetta = "nessuna"); pul0 <- foglio_di(r0$p, 1)
v("se la correzione non è accettata, «S. Severo» resta com'è nel file pulito", grepl("^S\\. Severo", pul0[["Luoghi citati o indicizzati"]]))
v("…e il registro non la riporta come modifica del luogo", !any(r0$p$registro$Colonna == "Luoghi citati o indicizzati" & grepl("San Severo", r0$p$registro$Dopo)))
# il nome con la qualifica (il caso di Trinity) non si riordina nel file pulito
a2 <- foglio("Persone citate o indicizzate" = "Giacomo Armando (cavaliere dell'Ordine di Malta)")
pul2 <- foglio_di(pulisci_con(a2)$p, 1)
v("«Giacomo Armando (cavaliere…)» non viene riordinato dall'uscita", identical(pul2[["Persone citate o indicizzate"]], "Giacomo Armando (cavaliere dell'Ordine di Malta)"))
# il registro di un foglio già a posto è vuoto (neutro sul corretto)
a3 <- foglio("Luoghi citati o indicizzati" = "San Severo|Torino", "Regesto o descrizione sintetica" = "Atti di prova.", "Persone citate o indicizzate" = "Rossi, Mario", "Tipologia documentaria" = "Atti")
v("un foglio già pulito ha un registro vuoto", nrow(pulisci_con(a3)$p$registro) == 0L)

# Decisione presa in finestra (estremi cronologici scambiati): è una decisione accettata, non una trasformazione «mai annunciata»
a2 <- foglio("Estremo remoto" = "1650", "Estremo recente" = "1640", "Regesto o descrizione sintetica" = "Atti di prova.")
pr2 <- proposte_import(a2, NULL, NULL, luoghi_ignoti = FALSE)
dec2 <- Filter(function(x) identical(x$tipo, "ordine_anni"), pr2$decisioni)
v("la finestra chiede come ordinare gli estremi 1650–1640", length(dec2) == 1L)
if (length(dec2)) {
  a2b <- applica_proposte(a2, pr2, ok = pr2$celle$id[pr2$celle$consigliata], scelte = stats::setNames(list("scambia"), as.character(dec2[[1]]$id)))
  p2 <- pulisci_foglio(a2b, nome = "dec.xlsx", revisionato = TRUE); r2 <- p2$registro; u2 <- foglio_di(p2, 1)
  v("lo scambio arriva nel file (remoto 1640, recente 1650)", identical(u2[["Estremo remoto"]], "1640") && identical(u2[["Estremo recente"]], "1650"))
  v("il registro lo dice: decisione presa nella finestra, dichiarata «sì», e nessun «no»", any(grepl("decisione presa nella finestra", r2$Regola) & r2$Dichiarata == "sì" & r2$Colonna == "Estremo remoto") && !any(r2$Dichiarata == "no"))
}

# Due fondi nello stesso foglio con la stessa «b. 1, fasc. 1»: ogni riga resta legata alla propria riga sorgente
COLS <- COL; riga2 <- function(seg, tit, reg) { x <- stats::setNames(as.list(rep(NA_character_, length(COLS))), COLS); x[["Segnatura"]] <- seg; x[["Titolo"]] <- tit; x[["Regesto o descrizione sintetica"]] <- reg; as.data.frame(x, check.names = FALSE, stringsAsFactors = FALSE) }
f3 <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(rbind(riga2("Archivio A, Fondo A, b. 1, fasc. 1", "Uno", "Alfa."), riga2("Archivio B, Fondo B, b. 1, fasc. 1", "Due", "Beta."),
                            riga2("Archivio B, Fondo B, b. 1, fasc. 2", "Tre", "Gamma.")), f3)
a3 <- leggi_arianna(f3); p3 <- pulisci_foglio(a3, nome = "mf.xlsx"); r3 <- p3$registro
v("due fondi: nessuna cella attribuita all'altro fondo (Titolo e Regesto invariati)", !any(r3$Colonna %in% c("Titolo", "Segnatura")) && !any(r3$Dichiarata == "no"))
v("due fondi: il regesto del fondo B è «Beta.» alla riga 3, mai «Alfa.»→«Beta.»", !any(r3$Prima == "Alfa." & r3$Dopo == "Beta."))
v("due fondi: ogni riga del registro porta il numero della propria riga sorgente (2, 3, 4)", setequal(unique(r3$Riga), c(2L, 3L, 4L)))

# Due decisioni sulla stessa cella: il nome scritto a mano con la virgola non si spezza
a4 <- foglio("Persone citate o indicizzate" = "Carli, Paolo|Carli Maurizio|Rossi, Mario|Rossi, Mario")
pr4 <- proposte_import(a4, NULL, NULL, luoghi_ignoti = FALSE)
sc4 <- list(); for (d_ in pr4$decisioni) sc4[[as.character(d_$id)]] <- if (grepl("duplicato", d_$testo)) "Rossi, Mario (padre)" else "Carli, Maurizio"
a4b <- applica_proposte(a4, pr4, ok = integer(0), scelte = sc4)
v("due decisioni sulla stessa cella: «Rossi, Mario (padre)» resta una voce sola", identical(a4b$dati[["Persone citate o indicizzate"]], "Carli, Paolo|Carli, Maurizio|Rossi, Mario (padre)|Rossi, Mario"))

# Archivio e fondo scritti nella finestra «Pulisci» (13/10, collaudato 23/10/2026): ogni riga cambiata è nel registro e dichiarata; il foglio pulito porta
# archivio e fondo scritti; la segnatura di prima resta leggibile nella colonna «Prima». Due casi: segnatura con un nome solo, e già completa ma diversa.
foglio_segn <- function(segn, extra = list()) {
  riga <- stats::setNames(as.list(rep(NA_character_, length(COL))), COL)
  d <- do.call(rbind, lapply(segn, function(x) { r <- riga; r[["Segnatura"]] <- x; r[["Titolo"]] <- "Atti"; for (k in names(extra)) r[[k]] <- extra[[k]]; as.data.frame(r, check.names = FALSE, stringsAsFactors = FALSE) }))
  f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, f); leggi_arianna(f)
}
for (caso in list(list("nome solo", c("Tribunale X, b. 1, fasc. 1", "Tribunale X, b. 1, fasc. 2")),
                  list("segnatura completa ma diversa", c("Archivio Z, Fondo Y, b. 1, fasc. 1", "Archivio Z, Fondo Y, b. 1, fasc. 2")))) {
  pw <- pulisci_foglio(foglio_segn(caso[[2]]), nome = "w.xlsx", archivio = "Archivio A", fondo = "Fondo B", revisionato = TRUE)
  uw <- foglio_di(pw, 1); rw <- pw$registro; sg <- rw[rw$Colonna == "Segnatura", , drop = FALSE]
  v(sprintf("[%s] il foglio pulito porta archivio e fondo scritti, nel modello", caso[[1]]), identical(uw$Segnatura, c("Archivio A, Fondo B, b. 1, fasc. 1", "Archivio A, Fondo B, b. 1, fasc. 2")) && ncol(uw) == length(COL))
  v(sprintf("[%s] una riga di registro per ogni segnatura riscritta, con la segnatura di prima in «Prima»", caso[[1]]), nrow(sg) == 2L && identical(sg$Prima, caso[[2]]) && identical(sg$Dopo, uw$Segnatura))
  v(sprintf("[%s] tutte le modifiche sono dichiarate (nessuna «no»)", caso[[1]]), !any(rw$Dichiarata == "no") && all(sg$Dichiarata == "convenzione") && all(grepl("archivio e fondo scritti da chi pulisce", sg$Regola)))
  v(sprintf("[%s] «Luogo di conservazione» = archivio scritto, dichiarato", caso[[1]]), identical(uw[["Luogo di conservazione"]], c("Archivio A", "Archivio A")) && all(rw$Dichiarata[rw$Colonna == "Luogo di conservazione"] == "convenzione"))
  v(sprintf("[%s] nessuna riga è sparita: stesse righe, respinte vuote", caso[[1]]), identical(pw$letti, 2L) && identical(pw$importati, 2L) && !nrow(pw$respinti))
}
# senza archivio e fondo scritti la segnatura con un nome solo è respinta e il motivo lo dice (non entra, non si inventa)
pn <- tryCatch(pulisci_foglio(foglio_segn("Tribunale X, b. 1, fasc. 1"), nome = "n.xlsx", revisionato = TRUE), error = function(e) conditionMessage(e))
v("senza archivio e fondo scritti, una segnatura con un nome solo non entra e non si indovina (errore: nessuna riga entrata)", is.character(pn) && grepl("Nessuna riga è entrata", pn))

# Segnatura con spazi doppi (Archiginnasio, 23/10/2026): una correzione proposta con anteprima; se due righe diventerebbero la stessa segnatura non è
# spuntata e lo dice; applicata o no, l'import non fonde mai due schede.
sp <- foglio_segn(c("Archiginnasio, Fondo A, b. 1, fasc. 1", "Archiginnasio,  Fondo A,  b. 1, fasc. 2", "Archiginnasio, Fondo A, b.  2, fasc. 1"))
prs <- proposte_import(sp, NULL, NULL, luoghi_ignoti = FALSE); cs <- prs$celle[prs$celle$colonna == "Segnatura", , drop = FALSE]
v("spazi doppi nella segnatura: una proposta per riga, con prima e dopo, spuntata", nrow(cs) == 2L && all(cs$consigliata) && all(grepl("  ", cs$prima)) && !any(grepl("  ", cs$dopo)))
as_ <- applica_proposte(sp, prs, ok = prs$celle$id[prs$celle$consigliata], scelte = list()); dbs <- apri_db(":memory:"); es <- importa_arianna(dbs, as_, "accoda")
v("applicata la proposta, tutte e tre le righe entrano", es$importati == 3L && !nrow(es$respinti))
es0 <- importa_arianna(apri_db(":memory:"), sp, "accoda")
v("rifiutata la proposta, le due righe con il doppio spazio restano respinte con il motivo di prima", es0$importati == 1L && nrow(es0$respinti) == 2L && all(grepl("doppio spazio non ammesso", es0$respinti$motivo)))
pc <- proposte_import(foglio_segn(c("Archiginnasio, Fondo A, b. 1, fasc. 1", "Archiginnasio,  Fondo A, b. 1, fasc. 1")), NULL, NULL, luoghi_ignoti = FALSE)
cc <- pc$celle[pc$celle$colonna == "Segnatura", , drop = FALSE]
v("collisione: la correzione che renderebbe uguali due segnature non è spuntata e nomina l'altra riga", nrow(cc) == 1L && !cc$consigliata && grepl("collisione", cc$perche) && grepl("riga 2", cc$perche))
ac <- applica_proposte(foglio_segn(c("Archiginnasio, Fondo A, b. 1, fasc. 1", "Archiginnasio,  Fondo A, b. 1, fasc. 1")), pc, ok = pc$celle$id, scelte = list())
ec <- importa_arianna(apri_db(":memory:"), ac, "accoda")
v("…e se la si accetta lo stesso l'import non fonde: la seconda è respinta come «già occupata»", ec$importati == 1L && nrow(ec$respinti) == 1L && grepl("già occupata", ec$respinti$motivo))
pn <- proposte_import(foglio_segn("Archiginnasio, Fondo A; b. 1, fasc. 1"), NULL, NULL, luoghi_ignoti = FALSE)
v("un «;» al posto della virgola non è una correzione proposta: la riga resta respinta", !any(pn$celle$colonna == "Segnatura") && importa_arianna(apri_db(":memory:"), foglio_segn("Archiginnasio, Fondo A; b. 1, fasc. 1"), "accoda")$importati == 0L)

# Collisione letta come il database (23/10/2026, dalla critica): «b.  1, f. 1» e «b. 1, fasc. 1» sono la stessa segnatura anche se il testo è diverso.
sc <- foglio_segn(c("Archivio A, Fondo B, b. 1, fasc. 1", "Archivio A, Fondo B, b.  1, f. 1", "Archivio A, Fondo B, b. 2, fasc. 1"))
pcs <- proposte_import(sc, NULL, NULL, luoghi_ignoti = FALSE); ccs <- pcs$celle[pcs$celle$colonna == "Segnatura", , drop = FALSE]
v("collisione per numeri («f. 1» e «fasc. 1»): la correzione non è spuntata", nrow(ccs) == 1L && !ccs$consigliata && grepl("riga 2", ccs$perche))
# «nuovo archivio»: un fondo entra tutto o niente; se due righe sono la stessa segnatura l'errore è in italiano, non quello di SQLite.
sn <- foglio_segn(c("b. 1, fasc. 1", "b. 1, f. 1", "b. 2, fasc. 1")); dn <- apri_db(":memory:")
en <- try(importa_nuovo_archivio(dn, sn, "Archivio A", "Fondo B", NULL), silent = TRUE); mn <- as.character(en)
v("nuovo archivio: due righe con la stessa segnatura fermano il fondo, e l'errore è in italiano", inherits(en, "try-error") && grepl("già occupata", mn) && !grepl("UNIQUE", mn) && nrow(elenco_fondi(dn)) == 0L)
# Senza cognome (titolo + nome) non è una persona da censire: anche l'export dal database lo lascia com'è (Stefano, 23/10).
dbf <- apri_db(":memory:"); invisible(importa_arianna(dbf, foglio_segn("Archivio A, Fondo B, b. 1, fasc. 1", list("Persone citate o indicizzate" = "fra Paolo|Rossi, Mario")), "accoda"))
xf <- tempfile(fileext = ".xlsx"); esporta_arianna(dbf, xf, fondo = elenco_fondi(dbf)$id[1])
v("export dal database: «fra Paolo» resta com'è, non «Paolo, fra»", identical(as.data.frame(readxl::read_excel(xf, col_types = "text"), check.names = FALSE)[["Persone citate o indicizzate"]], "fra Paolo|Rossi, Mario"))

# Il rifiuto di un nome sopravvive al salvataggio, alla chiusura e alla riapertura del database, fino al file esportato (scelta A, 23/10/2026).
giro_nomi <- function(accetta, persone) {
  an <- foglio("Persone citate o indicizzate" = persone, "Regesto o descrizione sintetica" = "Atti di prova.")
  pn <- proposte_import(an, NULL, NULL, luoghi_ignoti = FALSE)
  a1 <- applica_proposte(an, pn, ok = if (accetta) pn$celle$id[pn$celle$consigliata] else integer(0), scelte = list())
  dbp <- tempfile(fileext = ".sqlite"); cn <- apri_db(dbp); invisible(importa_arianna(cn, a1, "accoda")); dbDisconnect(cn)
  cn <- apri_db(dbp); on.exit(dbDisconnect(cn), add = TRUE); xn <- tempfile(fileext = ".xlsx"); esporta_arianna(cn, xn)
  as.data.frame(readxl::read_excel(xn, col_types = "text"), check.names = FALSE)[["Persone citate o indicizzate"]]
}
v("nome senza virgola, correzione rifiutata: dopo salvataggio e riapertura l'export lo lascia com'è scritto", identical(giro_nomi(FALSE, "Mario Rossi|Anna Bianchi|fra Paolo"), "Mario Rossi|Anna Bianchi|fra Paolo"))
v("nome senza virgola, correzione accettata: il database tiene «Rossi, Mario» e l'export lo scrive così", identical(giro_nomi(TRUE, "Mario Rossi|Anna Bianchi"), "Rossi, Mario|Bianchi, Anna"))
v("«fra Paolo» solo nelle Persone: la proposta di toglierlo non è spuntata, e con la spunta di partenza resta com'è", identical(giro_nomi(TRUE, "fra Paolo"), "fra Paolo"))

# Estremo recente uguale al remoto (24/10): è una correzione visibile, dichiarata nel registro, che nel file pulito toglie il recente
ae <- foglio("Estremo remoto" = "1544", "Estremo recente" = "1544", "Regesto o descrizione sintetica" = "Atti di prova.")
re_ <- pulisci_con(ae); pe <- foglio_di(re_$p, 1)
v("estremi uguali: la proposta c'è, spuntata", any(grepl("estremo recente uguale al remoto", re_$pr$celle$perche) & re_$pr$celle$consigliata))
v("estremi uguali: nel file pulito il recente è vuoto e il remoto resta", is.na(pe[["Estremo recente"]]) && identical(pe[["Estremo remoto"]], "1544"))
v("estremi uguali: il registro lo dichiara «sì», con prima «1544»", any(re_$p$registro$Colonna == "Estremo recente" & re_$p$registro$Dichiarata == "sì" & re_$p$registro$Prima == "1544") && !any(re_$p$registro$Dichiarata == "no"))
r0_ <- pulisci_con(ae, accetta = "nessuna"); p0_ <- foglio_di(r0_$p, 1)
v("estremi uguali: se rifiuti la proposta, il recente resta com'è", identical(p0_[["Estremo recente"]], "1544"))

# Maiuscola dopo l'apostrofo (24/10, D31): «De'cefali» -> «De'Cefali», correzione visibile, dichiarata, rifiutabile
ap <- foglio("Persone citate o indicizzate" = "De'cefali, Aliprando", "Regesto o descrizione sintetica" = "Atti di prova.")
ra_ <- pulisci_con(ap); pa_ <- foglio_di(ra_$p, 1)
v("apostrofo: la proposta c'è, spuntata", any(grepl("De'cefali", ra_$pr$celle$prima) & ra_$pr$celle$consigliata))
v("apostrofo: nel file pulito la lettera dopo l'apostrofo è maiuscola", identical(pa_[["Persone citate o indicizzate"]], "De'Cefali, Aliprando"))
v("apostrofo: il registro lo dichiara «sì»", any(ra_$p$registro$Prima == "De'cefali, Aliprando" & ra_$p$registro$Dichiarata == "sì") && !any(ra_$p$registro$Dichiarata == "no"))
rr_ <- pulisci_con(ap, accetta = "nessuna"); v("apostrofo: se rifiuti, resta com'è", identical(foglio_di(rr_$p, 1)[["Persone citate o indicizzate"]], "De'cefali, Aliprando"))

if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
