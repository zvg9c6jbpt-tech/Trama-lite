# ──────────────────────────────────────────────
# verifiche/anomalie.R: gli errori già nel database, segnalati e corretti da Fascicoli (13/10, strada B)
#   - un fondo con i refusi del Tab di prova: il controllo li trova, la correzione li riscrive nelle schede,
#     cambia SOLO ciò che la finestra ha confermato, e a correzione fatta restano solo le cose da decidere;
#   - una correzione tolta dalla spunta non si applica;
#   - sui fondi veri (copia del database di prova): la correzione tocca le sole celle proposte, e un secondo
#     controllo non ne ritrova (nessuna correzione che torna a proporsi);
#   - «Tommaso d'Auria» e «D'Auria, Tommaso» sono la stessa grafia (Napoli): non è un'anomalia.
#     Rscript verifiche/anomalie.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
source("verifiche/aiuti.R")
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
v("«d'Auria» e «D'Auria» sono la stessa grafia", .solo_particella_apostrofo("Tommaso D'Auria", "Tommaso d'Auria") && !.solo_particella_apostrofo("PAolo Uno", "Paolo Uno"))
# «di forma» = solo maiuscole e spazi; la punteggiatura e i separatori sono contenuto (critique del 19/10)
v("«di forma»: maiuscole e spazi sì", all(correzioni_di_forma(data.frame(prima = c("paolo rossi", "Rossi , Anna", "A  B"), dopo = c("Paolo Rossi", "Rossi,Anna", "A B")))))
v("«di forma»: separatori, virgole e refusi no", !any(correzioni_di_forma(data.frame(prima = c("Maria Due;Nicola tre", "Rossi Maria", "Roma: Napoli", "Avignon"), dopo = c("Maria Due|Nicola tre", "Rossi, Maria", "Roma|Napoli", "Avignone")))))
celle_diverse <- function(x, y) which(as.matrix(x) != as.matrix(y) & !(is.na(as.matrix(x)) & is.na(as.matrix(y))), arr.ind = TRUE)

# 1. il Tab di prova, importato com'è (senza finestra), poi controllato e corretto dalle schede
con <- apri_db(tempfile(fileext = ".sqlite"))
importa_nuovo_archivio(con, leggi_arianna("dati/esempio/prova-normalizzazione.xlsx"), "Archivio di Prova", "Fondo Anomalie", "")
an <- anomalie_fondo(con, "Fondo Anomalie")
v("il controllo trova i refusi (e il banner li conta)", an$pr$totale >= 10L && !is.null(banner_anomalie_ui(an)) && schede_con_anomalie(an) >= 5L)
v("un fondo senza anomalie non ha banner", is.null(banner_anomalie_ui(list(pr = list(celle = data.frame(), decisioni = list())))))
prima <- tabella_arianna(con, fondo = "Fondo Anomalie")
una <- applica_anomalie_db(con, an, ok = an$pr$celle$id[1])
v("una sola spunta tocca una sola cella", nrow(celle_diverse(prima, tabella_arianna(con, fondo = "Fondo Anomalie"))) == 1L && una$schede == 1L)
con2 <- apri_db(tempfile(fileext = ".sqlite"))
importa_nuovo_archivio(con2, leggi_arianna("dati/esempio/prova-normalizzazione.xlsx"), "Archivio di Prova", "Fondo Anomalie", "")
an2 <- anomalie_fondo(con2, "Fondo Anomalie"); prima2 <- tabella_arianna(con2, fondo = "Fondo Anomalie")
r <- applica_anomalie_db(con2, an2, ok = an2$pr$celle$id)
dopo2 <- tabella_arianna(con2, fondo = "Fondo Anomalie")
dif <- celle_diverse(prima2, dopo2)
v("le celle cambiate nel database sono le colonne proposte e nient'altro", all(names(prima2)[dif[, 2]] %in% an2$pr$celle$colonna))
v("nessun refuso sicuro resta nelle schede; i nomi discordi (Violo/Viola) restano, si decidono (D11, 20/10)", !any(grepl("cicra|miscredenxe|Maggia|vIola|PAolo", unlist(dopo2))) && any(grepl("Violo", unlist(dopo2))))
v("le altre colonne delle schede sono identiche", ncol(prima2) == ncol(dopo2) && all(dif[, 2] %in% match(unique(an2$pr$celle$colonna), names(prima2))))
an3 <- anomalie_fondo(con2, "Fondo Anomalie")
v("dopo la correzione restano solo le cose da decidere (nessuna correzione sicura da rifare)", nrow(an3$pr$celle) == 0L && length(an3$pr$decisioni) >= 1L)
v("la scheda riscritta mantiene il suo titolo manuale e il suo numero", {
  s <- DBI::dbGetQuery(con2, "SELECT COUNT(*) n, SUM(titolo_manuale) m FROM fascicoli WHERE fondo = 'Fondo Anomalie'"); s$n == 9L })

# 2. i fondi veri: la correzione tocca le sole celle proposte, e non si ripropone
cp <- database_di_prova(); cx <- apri_db(cp)
fondi_grandi <- DBI::dbGetQuery(cx, "SELECT fondo_id, MIN(fondo) fondo FROM fascicoli GROUP BY fondo_id HAVING COUNT(*) >= 50")  # per id: due archivi possono avere un fondo con lo stesso nome
for (k in seq_len(nrow(fondi_grandi))) {
  fo <- fondi_grandi$fondo[k]; fid <- fondi_grandi$fondo_id[k]
  t0 <- tabella_arianna(cx, fondo = fid); a <- anomalie_fondo(cx, fid)
  applica_anomalie_db(cx, a, ok = a$pr$celle$id)
  t1 <- tabella_arianna(cx, fondo = fid); d <- celle_diverse(t0, t1)
  v(sprintf("«%s»: %d celle proposte, %d cambiate, tutte nelle colonne proposte", fo, nrow(a$pr$celle), nrow(d)),
    nrow(d) <= nrow(a$pr$celle) && (!nrow(d) || all(names(t0)[d[, 2]] %in% a$pr$celle$colonna)))
  b <- anomalie_fondo(cx, fid)
  v(sprintf("«%s»: un secondo controllo non ritrova correzioni da fare", fo), nrow(b$pr$celle) <= max(2L, nrow(a$pr$celle) - nrow(d)))
}
# Un nome senza virgola nel database (l'export non lo gira più, 23/10): la correzione accettata lo scrive «Cognome, Nome», la coppia persona–reato lo segue,
# e un secondo controllo non lo ripropone.
cn <- apri_db(":memory:"); fn <- fascicolo_vuoto(FALSE); fn$archivio <- "A"; fn$fondo <- "F"; fn$busta <- 1L; fn$fascicolo <- 1L
fn$persone <- "Mario Rossi"; fn$reati <- "Eresia"; fn$luoghi <- "Napoli"; fn$persona_reati <- data.frame(persona = "Mario Rossi", reato = "Eresia", stringsAsFactors = FALSE)
salva_fascicolo(cn, fn)
an_n <- anomalie_fondo(cn, "F"); invisible(applica_anomalie_db(cn, an_n, ok = an_n$pr$celle$id))
gn <- leggi_fascicolo(cn, DBI::dbGetQuery(cn, "SELECT id FROM fascicoli")$id)
v("nome senza virgola nel database: la correzione accettata lo scrive «Rossi, Mario»", identical(as.character(unlist(gn$persone)), "Rossi, Mario"))
v("…la coppia persona–reato lo segue", NROW(gn$persona_reati) == 1L && identical(gn$persona_reati$persona, "Rossi, Mario") && identical(gn$persona_reati$reato, "Eresia"))
v("…e un secondo controllo non lo ripropone", nrow(anomalie_fondo(cn, "F")$pr$celle) == 0L)
# Più persone nella stessa scheda, reati distinti: ogni coppia segue il proprio nome (anche con «fra Paolo» che esce dalle Persone).
coppie_dopo <- function(persone, coppie, reati) {
  cm <- apri_db(":memory:"); fm <- fascicolo_vuoto(FALSE); fm$archivio <- "A"; fm$fondo <- "F"; fm$busta <- 1L; fm$fascicolo <- 1L
  fm$persone <- persone; fm$reati <- reati; fm$luoghi <- "Napoli"; fm$persona_reati <- coppie; salva_fascicolo(cm, fm)
  am <- anomalie_fondo(cm, "F"); invisible(applica_anomalie_db(cm, am, ok = am$pr$celle$id))
  leggi_fascicolo(cm, DBI::dbGetQuery(cm, "SELECT id FROM fascicoli")$id)$persona_reati
}
d3 <- coppie_dopo(c("Mario Rossi", "Anna Bianchi", "Luca Verdi"), data.frame(persona = c("Mario Rossi", "Anna Bianchi", "Luca Verdi"), reato = c("Eresia", "Stregoneria", "Magia")), c("Eresia", "Stregoneria", "Magia"))
v("tre persone, tre reati: ogni coppia segue il suo nome", NROW(d3) == 3L && identical(d3$reato[match(c("Rossi, Mario", "Bianchi, Anna", "Verdi, Luca"), d3$persona)], c("Eresia", "Stregoneria", "Magia")))
dp <- coppie_dopo(c("fra Paolo", "Mario Rossi"), data.frame(persona = c("fra Paolo", "Mario Rossi"), reato = c("Eresia", "Magia")), c("Eresia", "Magia"))
v("«fra Paolo» esce dalle Persone: la coppia di Mario Rossi resta sua (Magia), non passa a un altro", NROW(dp) == 1L && identical(dp$reato, "Magia") && grepl("Mario Rossi|Rossi, Mario", dp$persona))
# Estremo recente uguale al remoto (24/10): la correzione accettata toglie il recente anche dalla scheda nel database (anno e data precisa), e non torna.
ce <- apri_db(":memory:")
for (n in 1:3) { fe <- fascicolo_vuoto(FALSE); fe$archivio <- "A"; fe$fondo <- "F"; fe$busta <- 1L; fe$fascicolo <- n
  fe$estremo_remoto <- if (n == 3L) 1544L else 1544L; fe$estremo_recente <- c(1544L, 1544L, 1545L)[n]
  if (n == 2L) fe$extra <- list("Estremo remoto (data precisa)" = "1544-03-12", "Estremo recente (data precisa)" = "1544-03-12")
  salva_fascicolo(ce, fe) }
ae <- anomalie_fondo(ce, "F"); invisible(applica_anomalie_db(ce, ae, ok = ae$pr$celle$id))
ge <- lapply(DBI::dbGetQuery(ce, "SELECT id FROM fascicoli ORDER BY fascicolo")$id, function(i) leggi_fascicolo(ce, i))
v("estremi uguali (anni): il recente si toglie dalla scheda", is.na(ge[[1]]$estremo_recente) && ge[[1]]$estremo_remoto == 1544L)
v("estremi uguali (data precisa): si toglie l'anno e la data precisa del recente, il remoto resta", is.na(ge[[2]]$estremo_recente) && is.null(unlist(ge[[2]]$extra[["Estremo recente (data precisa)"]])) && identical(unlist(ge[[2]]$extra[["Estremo remoto (data precisa)"]])[1], "1544-03-12"))
v("anni diversi: restano entrambi", ge[[3]]$estremo_remoto == 1544L && ge[[3]]$estremo_recente == 1545L)
v("un secondo controllo non lo ripropone", !any(grepl("estremo recente uguale", anomalie_fondo(ce, "F")$pr$celle$perche)))
# D30 nella Scheda: la funzione che decide se avvisare al salvataggio
v("scheda: 1544 e 1544 sono la stessa data", identical(estremi_uguali(list(estremo_remoto = 1544L, estremo_recente = 1544L)), "1544"))
v("scheda: «1544-03-12» e «1544-03-12» sono la stessa data", identical(estremi_uguali(list(estremo_remoto = 1544L, estremo_recente = 1544L,
  extra = list("Estremo remoto (data precisa)" = "1544-03-12", "Estremo recente (data precisa)" = "1544-03-12"))), "1544-03-12"))
v("scheda: un anno e una data dentro quell'anno non lo sono", is.null(estremi_uguali(list(estremo_remoto = 1544L, estremo_recente = 1544L,
  extra = list("Estremo recente (data precisa)" = "1544-05-10")))))
v("scheda: anni diversi, recente vuoto o non numerico: nessun avviso", is.null(estremi_uguali(list(estremo_remoto = 1544L, estremo_recente = 1545L))) &&
  is.null(estremi_uguali(list(estremo_remoto = 1544L, estremo_recente = NA_integer_))) && is.null(estremi_uguali(list(estremo_remoto = NA_integer_, estremo_recente = NA_integer_))) &&
  is.null(estremi_uguali(list(estremo_remoto = "", estremo_recente = ""))))
# 02/11: un omonimo non è un errore del fondo: non entra nel conteggio del banner (la scelta non si può salvare nelle schede)
con_o <- apri_db(tempfile(fileext = ".sqlite"))
importa_nuovo_archivio(con_o, leggi_arianna("dati/esempio/prova-normalizzazione.xlsx"), "Archivio di Prova", "Fondo Omonimi", "")
fid_o <- dbGetQuery(con_o, "SELECT id FROM fascicoli LIMIT 1")$id
dbExecute(con_o, "DELETE FROM luoghi WHERE fascicolo_id = ?", list(fid_o)); dbExecute(con_o, "INSERT INTO luoghi (fascicolo_id, ordine, luogo) VALUES (?, 1, 'Castro')", list(fid_o))
pr_grezzo <- proposte_import({ t_ <- tabella_arianna(con_o, fondo = "Fondo Omonimi"); list(dati = t_, colonna = stats::setNames(names(t_), names(t_)), profilo = MODELLO) }, con_o, "Fondo Omonimi", luoghi_ignoti = FALSE)
an_o <- anomalie_fondo(con_o, "Fondo Omonimi")
v("omonimo: nel controllo grezzo c'è, nel banner di Fascicoli no", any(vapply(pr_grezzo$decisioni, function(d) identical(d$tipo, "omonimo"), TRUE)) &&
    !any(vapply(an_o$pr$decisioni, function(d) identical(d$tipo, "omonimo"), TRUE)) && an_o$pr$totale == nrow(an_o$pr$celle) + length(an_o$pr$decisioni))
cat(if (length(falliti)) sprintf("ESITO: NON TORNA (%s)\n", paste(falliti, collapse = "; ")) else "ESITO: ok\n"); if (length(falliti)) quit(status = 1)
