# ─────────────────────────────────────────────────────────────
# verifiche/funzioni.R: test delle funzioni pure (TODO D2)
#
# Senza testthat (sarebbe un pacchetto in più da installare): ogni
# `atteso()` confronta un risultato con il valore scritto a mano, e alla
# fine lo script dice quanti sono passati. Se uno fallisce stampa quale
# ed esce con stato 1, come roundtrip.R. Non apre mai regestum.sqlite:
# dove serve un database usa un database in memoria.
#
#     Rscript verifiche/funzioni.R
# ─────────────────────────────────────────────────────────────

Sys.setenv(TRAMA_COLLAUDO = "1")   # nessun registro nella cartella dati vera
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages({
  library(shiny)
  for (f in c("percorsi", "registro", "concordanza", "schema", "compose", "estrazione", "crud", "profili", "i18n", "modulo",
              "analisi", "qualita", "attribuzione", "ricerca", "arianna", "arianna_import", "arianna_export", "arianna_pulisci", "lista_luoghi", "luoghi", "fusioni", "luoghi_citati", "elenco", "ordinamento", "backup_electron", "backup"))
    source(file.path("R", paste0(f, ".R")))
})

.esiti <- new.env(); .esiti$ok <- 0L; .esiti$falliti <- character(0)
atteso <- function(nome, ottenuto, voluto) {
  if (identical(ottenuto, voluto)) .esiti$ok <- .esiti$ok + 1L
  else {
    .esiti$falliti <- c(.esiti$falliti, nome)
    cat("NON TORNA:", nome, "\n  ottenuto: "); str(ottenuto); cat("  voluto:   "); str(voluto)
  }
}

# --- nomi e titoli (compose.R) ------------------------------------
atteso("indicizza: nome semplice", indicizza_nome("Mario Rossi"), "Rossi, Mario")
atteso("indicizza: parentesi col cognome", indicizza_nome("Guglielmo Cucco (Sobring)"), "Cucco (Sobring), Guglielmo")
atteso("indicizza: parentesi di più parole", indicizza_nome("Isabella de Giordano (de Luca)"), "de Giordano (de Luca), Isabella")
atteso("indicizza: andata e ritorno con parentesi", indicizza_nome(.estendi_nome("Cucco (Sobring), Guglielmo")), "Cucco (Sobring), Guglielmo")
atteso("indicizza: due nomi",      indicizza_nome("Giovanni Battista Bonafossa"), "Bonafossa, Giovanni Battista")
atteso("indicizza: particella col cognome", indicizza_nome("Gilberto de Vasto"), "de Vasto, Gilberto")
atteso("indicizza: particella lunga",       indicizza_nome("Domenico della Senia"), "della Senia, Domenico")
atteso("indicizza: apostrofo",              indicizza_nome("Ascia d'Africa"), "d'Africa, Ascia")
atteso("indicizza: senza nome di battesimo", indicizza_nome("de Vasto"), "de Vasto")
atteso("indicizza: andata e ritorno",       .estendi_nome(indicizza_nome("Gilberto de Vasto")), "Gilberto de Vasto")
atteso("titolo: un imputato, solo il nome (il genere sta nella tipologia)", componi_titolo("Processo", "contro", "Mario Rossi"), "Mario Rossi")
atteso("titolo: due imputati",     componi_titolo("Atti", "circa", c("Mario Rossi", "Anna Bianchi")),
                                   "Mario Rossi e Anna Bianchi")
atteso("titolo: tre o più",        componi_titolo("Atti", "circa", c("A B", "C D", "E F")), "Atti per diversi")

# --- segnatura e regesto: andata e ritorno --------------------------
s <- componi_segnatura("Archivio Storico Diocesano di Napoli", "Sant'Ufficio", NA, 2, 25)
atteso("segnatura: testo", s, "Archivio Storico Diocesano di Napoli, Sant'Ufficio, b. 2, fasc. 25")
a <- analizza_segnatura(s)
atteso("segnatura: busta ritorna",     a$busta, 2L)
atteso("segnatura: fascicolo ritorna", a$fascicolo, 25L)
sb <- "Archivio di Stato di Modena, Tribunale dell'Inquisizione di Modena, Processi, b. 226, fasc. 16-bis"
ab <- analizza_segnatura(sb)
atteso("segnatura: bis, numero",   ab$fascicolo, 16L)
atteso("segnatura: bis, suffisso", ab$fascicolo_suffisso, "-bis")
atteso("segnatura: bis ritorna identica",
       componi_segnatura(ab$archivio, ab$fondo, ab$serie, ab$busta, ab$fascicolo, ab$fascicolo_suffisso), sb)
atteso("segnatura: lo spazio nel suffisso si toglie («18 -bis» → «-bis», 30/10/2026)",
       analizza_segnatura("X, Y, b. 3, fasc. 18 -bis")$fascicolo_suffisso, "-bis")
r <- componi_regesto("Napoli", c("Eresia", "Magia"), "sì", "n.d.")
ar <- analizza_regesto(r)
atteso("regesto: luogo ritorna",   ar$luogo_reato, "Napoli")
atteso("regesto: reati ritornano", ar$reati, c("Eresia", "Magia"))
atteso("regesto: tortura ritorna", ar$tortura, "sì")
atteso("regesto: n.d. non scritto ma ritorna", ar$sentenza, "n.d.")
atteso("regesto: reati separati da ; (ASMo)", analizza_regesto("Tipo di reato: Magia; Censura")$reati, c("Magia", "Censura"))

# --- vocabolario ----------------------------------------------------
atteso("normalizza: spazi doppi", normalizza(c("Bestemmia  e miscredenza", " Eresia ")),
       c("Bestemmia e miscredenza", "Eresia"))

# --- normalizzazione (TODO 26) ---------------------------------------
nf <- normalizza_fascicolo(list(definizione = "Fascicolo", supporto = "CARTACEO", persone = "Gallo,Matteo",
        note = "Non consultabile, pessimo stato\n\n— persone già citate altrove (auto) —\nX: già citato/a\n— fine —",
        non_consultabile = 0L))
atteso("normalizza: vocabolario", c(nf$definizione, nf$supporto), c("fascicolo", "cartaceo"))
atteso("normalizza: virgola nei nomi", nf$persone, "Gallo, Matteo")
atteso("normalizza: note senza blocco auto e senza la frase", nf$note, "pessimo stato")
atteso("normalizza: non consultabile diventa campo", nf$non_consultabile, 1L)
atteso("normalizza: all'export la frase torna", note_per_export(nf), "Non consultabile\npessimo stato")
# «non consultabile» è un marcatore solo a inizio nota o a inizio riga; una frase storica non si converte (19/10)
nc <- normalizza_fascicolo(list(note = "Precedentemente non consultabile, ora disponibile.", non_consultabile = 0L))
atteso("normalizza: «precedentemente non consultabile» non accende il flag", nc$non_consultabile, 0L)
atteso("normalizza: e la frase resta intera", nc$note, "Precedentemente non consultabile, ora disponibile.")
nr <- normalizza_fascicolo(list(note = "Pessimo stato\nNon consultabile", non_consultabile = 0L))
atteso("normalizza: a inizio riga sì", c(nr$non_consultabile, nr$note), c(1L, "Pessimo stato"))
# un regesto con residui non si ricompone (19/10): «minacciata», una riga libera
r1 <- analizza_regesto("Tipo di reato: Magia\nApplicazione della tortura: minacciata\nOsservazione: testimonianza indiretta")
atteso("regesto: residui riconosciuti (valore non sì/no/n.d. e riga libera)", c(r1$strutturato, r1$residui), c(TRUE, TRUE))
r2 <- analizza_regesto("Tipo di reato: Magia\nApplicazione della tortura: no\nSentenza o decreto formale di spedizione: sì")
atteso("regesto: un regesto pulito non ha residui", c(r2$strutturato, r2$residui), c(TRUE, FALSE))

# --- campi extra di un altro profilo (21/09) --------------------------
con2 <- apri_db(":memory:")
f0 <- fascicolo_vuoto(); f0$busta <- 1L; f0$fascicolo <- 1L
f0$extra <- list("Consistenza (unità di misura)" = "p.5", "Numerazione" = "12")
salva_fascicolo(con2, f0)
g0 <- raccogli_modulo(valori_da_fascicolo(leggi_fascicolo(con2, f0$id))); g0$id <- f0$id
salva_fascicolo(con2, g0)
atteso("extra: la maschera non cancella quelli di un altro profilo",
       leggi_fascicolo(con2, f0$id)$extra[["Consistenza (unità di misura)"]], "p.5")
atteso("extra: quelli suoi li riscrive",
       leggi_fascicolo(con2, f0$id)$extra[["Numerazione"]], "12")
# Il record originale Electron: fuori dalla maschera, intatto dopo Salva (28/09).
h0 <- leggi_fascicolo(con2, f0$id); h0$extra[[EXTRA_NASCOSTI]] <- '{"id":"x","persone":{"Joele, Francesco":{"ruolo":"notaio"}}}'
salva_fascicolo(con2, h0)
pn <- profilo_completo("Generale", dbGetQuery(con2, "SELECT DISTINCT campo FROM campi_extra")$campo)
atteso("provenienza: non è un campo della maschera", EXTRA_NASCOSTI %in% pn$arianna, FALSE)
k0 <- raccogli_modulo(valori_da_fascicolo(leggi_fascicolo(con2, f0$id), pn), pn); k0$id <- f0$id
salva_fascicolo(con2, k0)
atteso("provenienza: Salva non la tocca", leggi_fascicolo(con2, f0$id)$extra[[EXTRA_NASCOSTI]], h0$extra[[EXTRA_NASCOSTI]])
dbDisconnect(con2)

# --- separatori e varianti fra parentesi (ASMo, 21/09) ----------------
atteso("luoghi del reato: più d'uno separati da ;",
       separa_luoghi("Modena; Villafranca (Medolla)"), c("Modena", "Villafranca (Medolla)"))
atteso("luoghi del reato: le parentesi non separano",
       separa_luoghi("Solignano Vecchio (Solignano Nuovo; Castelvetro di Modena)"),
       "Solignano Vecchio (Solignano Nuovo; Castelvetro di Modena)")
atteso("varianti: | dentro le parentesi diventa ;",
       .pipe_in_parentesi("Solignano Vecchio (Solignano Nuovo|Castelvetro di Modena)"),
       "Solignano Vecchio (Solignano Nuovo; Castelvetro di Modena)")
atteso("varianti: | fuori dalle parentesi resta",
       .pipe_in_parentesi("Modena|Carpi"), "Modena|Carpi")
nl <- normalizza_fascicolo(list(reati = c("Magia", "Magia"), luoghi = "Carpi", luogo_reato = "Modena"))
atteso("normalizza: reati ripetuti una volta sola", nl$reati, "Magia")
atteso("normalizza: il luogo del reato entra fra i citati", nl$luoghi, c("Modena", "Carpi"))
atteso("normalizza: l'ordine dei luoghi non cambia se c'è già",
       normalizza_fascicolo(list(luoghi = c("Carpi", "Modena"), luogo_reato = "Modena"))$luoghi,
       c("Carpi", "Modena"))

# --- riconoscimento delle intestazioni --------------------------------
atteso("intestazioni: maiuscole e punteggiatura non contano",
       .chiave("SUPPORTO - MATERIALE"), .chiave("Supporto (materiale)"))
atteso("intestazioni: profilo ASMo riconosciuto", riconosci_profilo(PROFILI$ASMo$arianna), "ASMo")
atteso("intestazioni: profilo ASDNa riconosciuto", riconosci_profilo(PROFILI$ASDNa$arianna), "ASDNa")

# Quando nessun profilo basta, il rifiuto deve DIRE che cosa non torna:
# la classifica, le colonne che mancano a quello più vicino, le
# intestazioni che nessuno conosce (TODO 26 e 34).
d <- diagnosi_profilo(PROFILI$ASMo$arianna)
atteso("diagnosi: un rigo per profilo", nrow(d), length(PROFILI))
atteso("diagnosi: ASMo spiegato tutto", d$quota[d$profilo == "ASMo"], 1)
atteso("diagnosi: niente mancanti quando combacia", d$mancanti[d$profilo == "ASMo"], "")

m <- .perche_profilo_ignoto(c("una", "riga", "qualsiasi"))
atteso("rifiuto: dice perché", grepl("Profilo non riconosciuto", m, fixed = TRUE), TRUE)
atteso("rifiuto: mostra la classifica", grepl("Quanto ci va vicino ciascuno", m, fixed = TRUE), TRUE)
atteso("rifiuto: elenca le sconosciute", grepl("«una»", m, fixed = TRUE), TRUE)
atteso("rifiuto: suggerisce la riga delle intestazioni",
       grepl("PRIMA riga", m, fixed = TRUE), TRUE)

# Mezzo profilo ASMo: qui il consiglio giusto NON è «sposta le
# intestazioni», sono le colonne che mancano.
mezzo <- .perche_profilo_ignoto(head(PROFILI$ASMo$arianna[!is.na(PROFILI$ASMo$arianna)], 17))
atteso("rifiuto: foglio quasi giusto, dice che cosa manca",
       grepl("mancano:", mezzo, fixed = TRUE), TRUE)
atteso("rifiuto: foglio quasi giusto, niente consiglio sulla prima riga",
       grepl("PRIMA riga", mezzo, fixed = TRUE), FALSE)

# --- estrazione dal titolo ------------------------------------------
e <- estrai_da_titolo("Processo contro Ferdinando Villamaggiore")
atteso("estrazione: tipo definito", e$tipo, "definito")
atteso("estrazione: nome",          e$nomi, "Ferdinando Villamaggiore")

# --- ricerca avanzata -----------------------------------------------
d <- data.frame(id = c("a", "b", "c"), busta = c(1L, 2L, 9L), fascicolo = c(1L, 5L, 7L),
                anno = c(1560L, 1620L, NA), titolo = c("Processo contro X", "Atti circa Y", "Atti per diversi"),
                reati = c("Eresia | Magia", "Bestemmia e miscredenza", NA),
                luoghi = c("Napoli", "Roma | Napoli", NA), persone = c("X", "Y", NA),
                tortura = c("sì", "n.d.", "no"), sentenza = c("n.d.", "n.d.", "sì"),
                stringsAsFactors = FALSE)
atteso("ricerca: vuota = tutto",        filtra_avanzato(d, "")$id, c("a", "b", "c"))
atteso("ricerca: reato",                filtra_avanzato(d, "reato:magia")$id, "a")
atteso("ricerca: luogo multiplo",       filtra_avanzato(d, "luogo:napoli")$id, c("a", "b"))
atteso("ricerca: intervallo di anni",   filtra_avanzato(d, "anno:1600-1650")$id, "b")
atteso("ricerca: senza anno esclusi",   filtra_avanzato(d, "anno:1500-1700")$id, c("a", "b"))
atteso("ricerca: tortura sì senza accento", filtra_avanzato(d, "tortura:si")$id, "a")
atteso("ricerca: campi in AND",         filtra_avanzato(d, "luogo:napoli reato:eresia")$id, "a")
atteso("ricerca: campo ancora vuoto",   filtra_avanzato(d, "luogo:napoli reato:")$id, c("a", "b"))
d$persone[1] <- "Russo, Giovanni Domenico | X"
atteso("ricerca: valore fra virgolette con lo spazio", filtra_avanzato(d, 'persona:"Russo, Giovanni"')$id, "a")
atteso("ricerca: frase libera fra virgolette",  filtra_avanzato(d, '"atti circa"')$id, "b")
atteso("ricerca: virgolette e altro filtro",    filtra_avanzato(d, 'persona:"russo, giovanni" luogo:roma')$id, character(0))
atteso("ricerca: virgoletta rimasta aperta",    filtra_avanzato(d, 'persona:"Russo, Giov')$id, "a")
atteso("ricerca: senza virgolette come prima",  filtra_avanzato(d, "atti circa")$id, "b")
atteso("ricerca: avviso su anno illeggibile",   length(avvisi_ricerca("anno:abc reato:eresia")), 1L)
atteso("ricerca: avviso su tortura e completo", length(avvisi_ricerca("tortura:forse completo:boh year:15x")), 3L)
atteso("ricerca: nessun avviso su query buona", avvisi_ricerca('anno:1560-1565 tortura:si persona:"Russo, G" completo:no reato: testo'), character(0))
d$persone[1] <- "X"
atteso("completezza core 0-4 (titolo vale come persone)", completezza_core(d), c(4L, 4L, 1L))

# --- la maschera a tre colonne (modulo.R, 28/09) ----------------------
# Il modulo nasce dal profilo: ogni campo del profilo deve comparire una
# e una sola volta, qualunque colonna lo ospiti, per ogni profilo.
.ids_nel_modulo <- function(p) {
  h <- as.character(modulo_ui(identity, list(), p = p, rapidi = list(luogo = c("Napoli", "Roma"))))
  q <- p[p$dove != "derivato", , drop = FALSE]
  ids <- vapply(seq_len(nrow(q)), function(i) id_input(q[i, ]), "")
  vapply(ids, function(id) lengths(regmatches(h, gregexpr(sprintf('id="%s"', id), h, fixed = TRUE))), integer(1))
}
for (nome in c("Generale", names(PROFILI))) {
  n <- .ids_nel_modulo(profilo_completo(nome))
  atteso(paste("maschera", nome, ": ogni campo una volta"), names(n)[n != 1L], character(0))
}
n <- .ids_nel_modulo(profilo_completo("Generale", c("Colonna mai vista")))
atteso("maschera: un campo extra ignoto compare lo stesso", unname(n[["x_colonna_mai_vista"]]), 1L)
# Una condizione troncata ("... === " e niente dopo) manda in errore il
# browser a ogni clic e ferma tutti i pannelli condizionati (28/09).
h <- as.character(modulo_ui(identity, list(), p = profilo_completo("Generale", c("Colonna mai vista"))))
condizioni <- regmatches(h, gregexpr('data-display-if="[^"]*"', h))[[1]]
atteso("maschera: nessuna condizione troncata", condizioni[grepl('(===|\\|\\|)\\s*"$', condizioni)], character(0))

# --- coppie (attribuzione.R): testo, assi, validità -------------------
t <- data.frame(persona = c("Rossi, Mario", "Bianchi, Anna"), reato = c("Eresia", "Magia"),
                stringsAsFactors = FALSE)
atteso("coppie: andata e ritorno", tabella_da_coppie(coppie_da_tabella(t), c("persona", "reato")), t)
atteso("coppie: vuote", coppie_da_tabella(t[0, ]), character(0))
v <- list(imputati = c("Mario Rossi", "Anna Bianchi"), persona = "Terzo, Testimone",
          reato = c("Eresia", "Bestemmia  e miscredenza"), luogo = c("Roma"))
ax <- assi_coppie("persona_reati", v)
atteso("assi: righe = le persone citate (nessuna distinzione di ruolo dal 19/10 sera)", ax$righe, "Terzo, Testimone")
atteso("assi: colonne = capi normalizzati", ax$colonne, c("Eresia", "Bestemmia e miscredenza"))
atteso("assi: nessun avviso", ax$avviso, NULL)
ax2 <- assi_coppie("persona_reati", modifyList(v, list(imputati_esaminati = 1L)))
atteso("assi: con «nessuna persona da nominare» nessuna riga", ax2$righe, character(0))
atteso("assi luoghi: le colonne sono i luoghi citati",
       assi_coppie("reato_luoghi", v)$colonne, "Roma")
cv <- .coppie_valide(rbind(t, data.frame(persona = "Fantasma", reato = "Eresia")),
                     righe = c("Rossi, Mario", "Bianchi, Anna"), colonne = "Eresia")
atteso("coppie valide: fuori persone e capi assenti", cv$persona, "Rossi, Mario")

# --- anno proposto dal fascicolo precedente (crud.R, 28/09) ------------
con6 <- apri_db(":memory:")
for (x in list(c(1, 1, 1570), c(1, 2, NA), c(1, 3, 1572), c(2, 1, NA))) {
  f <- fascicolo_vuoto(); f$fondo <- "F"; f$busta <- as.integer(x[1]); f$fascicolo <- as.integer(x[2])
  f$estremo_remoto <- as.integer(x[3]); salva_fascicolo(con6, f)
}
atteso("anno precedente: salta i non datati", anno_precedente(con6, "F", 1, 3)$anno, 1570L)
atteso("anno precedente: il più vicino prima", anno_precedente(con6, "F", 1, 9), list(anno = 1572L, da = "b.1 f.3"))
atteso("anno precedente: dalla busta prima", anno_precedente(con6, "F", 2, 1)$anno, 1572L)
atteso("anno precedente: niente prima del primo", anno_precedente(con6, "F", 1, 1), NULL)
atteso("anno precedente: altro fondo", anno_precedente(con6, "G", 1, 9), NULL)
dbDisconnect(con6)

# --- titolo e regesto dalla tipologia (compose.R, 28/09) --------------
atteso("regesto narrativo: atti, un reato",
       componi_regesto_narrativo("Atti", "Magia", "Franzese, Caterina"), "Atti per magia circa Caterina Franzese")
atteso("regesto narrativo: due reati «A e B»",
       componi_regesto_narrativo("Processi", c("Eresia", "Magia"), "da Tunisi, Caterina"),
       "Processo per eresia e magia contro Caterina da Tunisi")
atteso("regesto narrativo: con «bestemmia e miscredenza» il separatore è sempre «; »",
       componi_regesto_narrativo("Processi", c("Bestemmia e miscredenza", "Magia"), "da Tunisi, Caterina"),
       "Processo per bestemmia e miscredenza; magia contro Caterina da Tunisi")
atteso("regesto narrativo: tre reati «A; B e C»",
       componi_regesto_narrativo("Processi", c("Eresia", "Ebraismo", "Magia"), "da Tunisi, Caterina"),
       "Processo per eresia; ebraismo e magia contro Caterina da Tunisi")
atteso("regesto narrativo: tre reati con «bestemmia e miscredenza», tutti «; »",
       componi_regesto_narrativo("Processi", c("Bestemmia e miscredenza", "Ebraismo", "Magia"), "da Tunisi, Caterina"),
       "Processo per bestemmia e miscredenza; ebraismo; magia contro Caterina da Tunisi")
atteso("regesto narrativo: quattro reati «A; B; C e D»",
       componi_regesto_narrativo("Processi", c("Eresia", "Magia", "Sacramenti", "Censura"), "da Tunisi, Caterina"),
       "Processo per eresia; magia; sacramenti e censura contro Caterina da Tunisi")
atteso("regesto narrativo: atti per diversi", componi_regesto_narrativo("Atti per diversi", "Eresia", c("A, B", "C, D")),
       "Atti per diversi per eresia")
atteso("regesto narrativo: senza imputati", componi_regesto_narrativo("Denunce", "Eresia", character(0)),
       "Denuncia per eresia contro ignoti")
atteso("denunce: due nomi, uno senza cognome", componi_titolo("Denuncia", NA, c("Loisia", "Paolo da Roma")),
       "Loisia e Paolo da Roma")
atteso("denunce: un nome resta singolare", componi_titolo("Denuncia", NA, "Loisia"), "Loisia")
atteso("denunce: per diversi", componi_titolo("Denuncia", NA, c("A", "B", "C")), "Denunce per diversi")
atteso("titolo: senza nomi e senza tipologia resta vuoto", componi_titolo(NA, NA, character(0)), NA_character_)
atteso("titolo: senza nomi, la tipologia da sola", componi_titolo("Informazioni", NA, character(0)), "Informazioni")
atteso("titolo: senza nomi ma fonte guardata, «per ignoti»", componi_titolo("Denuncia", NA, character(0), ignoti = TRUE), "Denunce per ignoti")
atteso("titolo: tre nomi senza tipologia, «Atti per diversi»", componi_titolo(NA, NA, c("A", "B", "C")), "Atti per diversi")
atteso("regesto narrativo: denunce con due nomi", componi_regesto_narrativo("Denunce", "Eresia", c("Troise, Isabella", "Paolo da Roma")),
       "Denunce per eresia contro Isabella Troise e Paolo da Roma")
atteso("regesto narrativo: oltre quattro", componi_regesto_narrativo("Atti", character(0), paste0("X", 1:5, ", Y")),
       "Atti circa diversi soggetti")
fs <- list(tipologia_documentaria = "Processi", reati = "Eresia", imputati = data.frame(esteso = "Giovanni Battista Russo", indicizzato = "Russo, Giovanni Battista"),
           persone = "Russo, Giovanni Battista", tipo_titolo = NA, connettore = NA, identita_imputato_dubbia = 0L, regesto = "")
atteso("titolo scheda: dalla tipologia e dagli imputati dichiarati", componi_titolo_scheda(fs), "Giovanni Battista Russo")
# Nessuna distinzione fra imputati e altre figure (Stefano, 19/10 sera): titolo e regesto si compongono da TUTTE le persone citate.
fs_pers <- fs; fs_pers$imputati <- data.frame(esteso = character(0), indicizzato = character(0))
atteso("titolo scheda: dalle persone citate, anche senza imputati dichiarati", componi_titolo_scheda(fs_pers), "Giovanni Battista Russo")
atteso("regesto scheda: dalle persone citate", componi_regesto_scheda(fs_pers), "Processo per eresia contro Giovanni Battista Russo")
due <- fs_pers; due$persone <- c("Russo, Giovanni Battista", "Testimone, Anna")
atteso("titolo scheda: due persone, due nomi", componi_titolo_scheda(due), "Giovanni Battista Russo e Anna Testimone")
tre <- fs_pers; tre$persone <- c("Russo, Giovanni Battista", "Testimone, Anna", "Verdi, Carlo")
atteso("titolo scheda: dalla terza persona «per diversi»", componi_titolo_scheda(tre), "Processo per diversi")
atteso("regesto scheda: fino a tre nomi", componi_regesto_scheda(tre), "Processo per eresia contro Giovanni Battista Russo, Anna Testimone e Carlo Verdi")
atteso("regesto scheda: «nessuna persona da nominare» dichiara che sono ignoti", componi_regesto_scheda(modifyList(fs_pers, list(imputati_esaminati = 1L))), "Processo per eresia contro ignoti")
atteso("titolo scheda: con «nessuna persona da nominare» dice «per ignoti»", componi_titolo_scheda(modifyList(fs_pers, list(imputati_esaminati = 1L))), "Processo per ignoti")
# una scheda con un imputato dichiarato e un testimone: il testo composto con la regola di PRIMA («contro X») non si riscrive da solo
vecchio <- due; vecchio$imputati <- data.frame(esteso = "Giovanni Battista Russo", indicizzato = "Russo, Giovanni Battista")
vecchio$titolo <- "Processo contro Giovanni Battista Russo"; vecchio$titolo_manuale <- 0L
vecchio$regesto <- "Processo per eresia contro Giovanni Battista Russo"; vecchio$regesto_manuale <- 0L
atteso("un titolo composto con la regola di prima non si riscrive da solo", componi_titolo_scheda(vecchio), "Processo contro Giovanni Battista Russo")
atteso("un regesto composto con la regola di prima non si riscrive da solo", componi_regesto_scheda(vecchio), "Processo per eresia contro Giovanni Battista Russo")
atteso("ma «Ricomponi» (forza) applica la regola di adesso al titolo", componi_titolo_scheda(vecchio, forza = TRUE), "Giovanni Battista Russo e Anna Testimone")
atteso("ma «Ricomponi» (forza) applica la regola di adesso al regesto", componi_regesto_scheda(vecchio, forza = TRUE), "Processo per eresia contro Giovanni Battista Russo e Anna Testimone")
atteso("se cambia ciò da cui dipende (il reato), il regesto segue la regola di adesso", componi_regesto_scheda(modifyList(vecchio, list(reati = c("Eresia", "Magia")))), "Processo per eresia e magia contro Giovanni Battista Russo e Anna Testimone")
atteso("regesto scheda: vuoto -> narrativo", componi_regesto_scheda(fs), "Processo per eresia contro Giovanni Battista Russo")
atteso("titolo scheda: atti per diversi", componi_titolo_scheda(modifyList(fs, list(tipologia_documentaria = "Atti per diversi"))), "Atti per diversi")
atteso("titolo scheda: il tipo scelto a mano non entra più nel titolo con i nomi",
       componi_titolo_scheda(modifyList(fs, list(tipo_titolo = "Copia del processo"))), "Giovanni Battista Russo")
atteso("regesto scheda: strutturato resta strutturato",
       componi_regesto_scheda(modifyList(fs, list(regesto = "Tipo di reato: Magia", luogo_reato = NA, tortura = "n.d.", sentenza = "n.d."))),
       "Tipo di reato: Eresia")
con7 <- apri_db(":memory:")
for (x in list(list(1L, "Processo contro Giovanni Battista Russo", "Processo per eresia contro Giovanni Battista Russo"),
               list(2L, "Processo contro don Giovanni Battista Russo", "Scomunica a Giovanni Battista Russo"))) {
  f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- x[[1]]; f$tipologia_documentaria <- "Processi"
  f$reati <- "Eresia"; f$persone <- "Russo, Giovanni Battista"
  f$imputati <- data.frame(esteso = "Giovanni Battista Russo", indicizzato = "Russo, Giovanni Battista", stringsAsFactors = FALSE)
  f$titolo <- x[[2]]; f$regesto <- x[[3]]; f$titolo_manuale <- 1L; f$regesto_manuale <- 1L
  salva_fascicolo(con7, f)
}
atteso("segna composti: solo dove coincide", segna_composti(con7), list(titoli = 1L, regesti = 1L, fascicoli = 2L))
atteso("segna composti: i testi non cambiano",
       dbGetQuery(con7, "SELECT titolo, regesto FROM fascicoli ORDER BY fascicolo")$regesto,
       c("Processo per eresia contro Giovanni Battista Russo", "Scomunica a Giovanni Battista Russo"))
atteso("segna composti: rifatto, nulla da segnare", segna_composti(con7), list(titoli = 0L, regesti = 0L, fascicoli = 2L))
f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- 3L; f$titolo <- NA; f$titolo_manuale <- 1L
salva_fascicolo(con7, f)
atteso("segna composti: un titolo vuoto e senza persone torna composto", segna_composti(con7)$titoli, 1L)
atteso("segna composti: e resta vuoto", leggi_fascicolo(con7, f$id)$titolo, NA_character_)
dbDisconnect(con7)

# --- imputati verificati e grafie fuse (segnalazioni del 28/09) --------
fv <- list(tipologia_documentaria = "Processi", reati = "Eresia", persone = c("Rossi, Mario", "Bianchi, Anna"),
           imputati = data.frame(esteso = character(0), indicizzato = character(0)),
           tipo_titolo = NA, connettore = NA, identita_imputato_dubbia = 0L, regesto = "")
atteso("persone della scheda: tutte le persone citate (nessuna distinzione di ruolo)", persone_imputate(fv), c("Rossi, Mario", "Bianchi, Anna"))
fv$imputati_esaminati <- 1L
atteso("imputati verificati e tutti spenti: nessuno", persone_imputate(fv), character(0))
atteso("imputati verificati e tutti spenti: regesto «ignoti»", componi_regesto_scheda(fv), "Processo per eresia contro ignoti")
atteso("imputati verificati e tutti spenti: titolo «per ignoti»", componi_titolo_scheda(fv), "Processo per ignoti")
con8 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- 1L; f$persone <- fv$persone; f$imputati_esaminati <- 1L
salva_fascicolo(con8, f)
atteso("imputati verificati: il flag si salva e si rilegge", leggi_fascicolo(con8, f$id)$imputati_esaminati, 1L)
j <- tempfile(fileext = ".json"); esporta_backup_json(con8, j); con9 <- apri_db(":memory:"); invisible(ripristina_backup_json(con9, j))
atteso("imputati verificati: il flag passa dal backup JSON", leggi_fascicolo(con9, f$id)$imputati_esaminati, 1L)
dbDisconnect(con8); dbDisconnect(con9)

# --- «Altri campi»: quali sono, quando sono compilati (28/09) ----------
ac <- campi_altri(profilo_completo("Generale", "Colonna mai vista"))
atteso("altri campi: ci sono suffisso, attribuzioni ed extra",
       all(c("fascicolo_suffisso", "altra_segnatura", "attribuzione", "reato_luoghi", "x_colonna_mai_vista") %in% ac$id), TRUE)
atteso("altri campi: non ci sono i campi della scheda", any(c("titolo", "persona", "estremo_remoto") %in% ac$id), FALSE)
atteso("compilato: n.d. su un'attribuzione è vuoto", compilato("attribuzione", "n.d."), FALSE)
atteso("compilato: un'attribuzione scelta conta", compilato("attribuzione", "per persona"), TRUE)
atteso("compilato: suffisso scritto conta, spazi no", c(compilato("fascicolo_suffisso", "bis"), compilato("fascicolo_suffisso", "  ")), c(TRUE, FALSE))
atteso("compilato: spunta spenta è vuota", compilato("x", FALSE), FALSE)
atteso("altri campi: un extra doppione di un campo vero non si mostra",
       "Luogo di conservazione" %in% profilo_completo("Generale", c("Luogo di conservazione", "Colonna mai vista"))$arianna[
         profilo_completo("Generale", c("Luogo di conservazione", "Colonna mai vista"))$dove == "extra"], FALSE)

# --- nome del foglio dell'export xlsx, come nel modello (28/09) ---------
con11 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$archivio <- "Archivio Storico Diocesano di Napoli"; f$fondo <- "Sant'Ufficio"
f$busta <- 1L; f$fascicolo <- 1L; salva_fascicolo(con11, f)
atteso("foglio: città - fondo", .nome_foglio(con11, "Sant'Ufficio"), "Napoli - Sant'Ufficio")
f <- fascicolo_vuoto(); f$archivio <- "Archivio di Stato di Modena"; f$fondo <- "Tribunale dell'Inquisizione di Modena: processi/atti"
f$busta <- 1L; f$fascicolo <- 1L; salva_fascicolo(con11, f)
atteso("foglio: al massimo 31 caratteri, senza : e /", .nome_foglio(con11, f$fondo), "Modena - Tribunale dell'Inquisi")
dbDisconnect(con11)

# --- correzione selettiva: un campo su più fascicoli (30/09) --------
con15 <- apri_db(":memory:")
mk <- function(n, reati) { f <- fascicolo_vuoto(); f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- n
  f$tipologia_documentaria <- "Processi"; f$reati <- reati; f$persone <- "Rossi, Mario"
  f$imputati <- data.frame(esteso = "Mario Rossi", indicizzato = "Rossi, Mario"); salva_fascicolo(con15, f) }
id1 <- mk(1L, "Eresia"); id2 <- mk(2L, c("Eresia", "Magia"))
f1 <- leggi_fascicolo(con15, id1); f1$regesto <- componi_regesto_scheda(f1); salva_fascicolo(con15, f1)
a <- imposta_campo_in_blocco(con15, c(id1, id2), "tortura", "no", prova = TRUE)
atteso("in blocco: anteprima non scrive", c(nrow(a), leggi_fascicolo(con15, id1)$tortura), c("2", "n.d."))
imposta_campo_in_blocco(con15, c(id1, id2), "tortura", "no")
atteso("in blocco: sostituisce un campo semplice", c(leggi_fascicolo(con15, id1)$tortura, leggi_fascicolo(con15, id2)$tortura), c("no", "no"))
atteso("in blocco: tortura esaminata si accende", leggi_fascicolo(con15, id1)$tortura_esaminata, 1L)
e <- imposta_campo_in_blocco(con15, c(id1, id2), "reato", "Magia", "aggiungi")
atteso("in blocco: aggiungi salta chi l'ha già", e$id, id1)
atteso("in blocco: il regesto composto segue", leggi_fascicolo(con15, id1)$regesto, "Processo per eresia e magia contro Mario Rossi")
imposta_campo_in_blocco(con15, c(id1, id2), "reato", "Eresia", "togli")
atteso("in blocco: togli", c(leggi_fascicolo(con15, id1)$reati, leggi_fascicolo(con15, id2)$reati), c("Magia", "Magia"))
atteso("in blocco: busta non modificabile",
       tryCatch(imposta_campo_in_blocco(con15, id1, "busta", "2"), error = function(e) "respinto"), "respinto")
dbDisconnect(con15)

source("R/estrazione.R")

# --- date non risolte: non si inseriscono (Stefano, 19/10) -------------
atteso(".intero_stretto: interi sì, decimali e testo no", c(.intero_stretto("1566"), .intero_stretto("1566.0"), .intero_stretto("1600.9"), .intero_stretto("circa 1600"), .intero_stretto("")),
       c(1566L, 1566L, NA_integer_, NA_integer_, NA_integer_))
atteso(".intero_stretto: con l'intervallo ammesso", c(.intero_stretto("1566", ANNI_AMMESSI), .intero_stretto("15557", ANNI_AMMESSI), .intero_stretto("1950", ANNI_AMMESSI)), c(1566L, NA_integer_, NA_integer_))
local({
  a0 <- leggi_arianna("dati/esempio/esempio.xlsx"); nm <- names(a0$dati); col <- function(pat) nm[grep(pat, nm, ignore.case = TRUE)][1]
  prova <- function(remoto) { a <- a0; a$dati <- a0$dati[1, , drop = FALSE]; a$dati[[col("remoto")]] <- remoto
    cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn)); r <- importa_arianna(cn, a, "accoda")
    list(anno = dbGetQuery(cn, "SELECT estremo_remoto FROM fascicoli")$estremo_remoto, r = r) }
  p1 <- prova("1566"); atteso("import: un anno valido entra", p1$anno, 1566L)
  p2 <- prova("circa 1600"); atteso("import: «circa 1600» non entra e si dice", list(p2$anno, p2$r$date_non_inserite$valore), list(NA_integer_, "circa 1600"))
  p3 <- prova("1600.9"); atteso("import: «1600.9» non diventa 1600", list(p3$anno, p3$r$date_non_inserite$valore), list(NA_integer_, "1600.9"))
  p4 <- prova("15557"); atteso("import: un anno fuori intervallo non entra", p4$anno, NA_integer_)
})

# --- ricerca: parole chiave inglesi (2/10) ---------------------------
source("R/ricerca.R")
dr <- data.frame(id = c("a", "b", "c"), busta = c(9L, 9L, 3L), fascicolo = 1:3, titolo = c("Processo contro X", "Atti circa Y", NA),
                 anno = c(1620L, 1570L, NA), reati = c("Eresia", "Magia | Eresia", "Bestemmia e miscredenza"), luoghi = "Napoli",
                 persone = c("X", "Y", NA), tortura = c("sì", "n.d.", "no"), sentenza = "n.d.", stringsAsFactors = FALSE)
atteso("ricerca in inglese: offence e year", filtra_avanzato(dr, "offence:heresy year:1560-1580")$id, "b")
atteso("ricerca in inglese: stesso risultato dell'italiano",
       filtra_avanzato(dr, "box:9 torture:yes")$id, filtra_avanzato(dr, "busta:9 tortura:sì")$id)
atteso("ricerca in inglese: reato a più parole, dall'inizio", filtra_avanzato(dr, "offence:blasphemy")$id, "c")
atteso("ricerca: l'italiano funziona come prima", filtra_avanzato(dr, "reato:magia")$id, "b")

# --- interfaccia in inglese: dizionario (1/10) -------------
atteso("traduci: in italiano resta com'è", traduci_testo("Fascicoli", "it"), "Fascicoli")
atteso("traduci: esatto, con gli spazi ai bordi", traduci_testo(c("Salva", " buste", "testo che non esiste"), "en"),
       c("Save", " boxes", "testo che non esiste"))
atteso("traduci: singolare e plurale", traduci_testo(c("1 fascicolo spuntato", "3 fascicoli spuntati"), "en"),
       c("1 case file selected", "3 case files selected"))

# --- striscia del backup: tempo trascorso (1/10) ----------------------
ora <- as.POSIXct("2026-10-01 10:00:00")
atteso("tempo fa", c(.tempo_fa(ora - 20, ora), .tempo_fa(ora - 180, ora), .tempo_fa(ora - 7200, ora),
                     .tempo_fa(ora - 16 * 3600, ora), .tempo_fa(ora - 3 * 86400, ora)),
       c("adesso", "3 minuti fa", "2 ore fa", "ieri alle 18:00", "il 28/09"))

# --- rimuovi duplicati (1/10) ----------------------------------------
source("R/qualita.R")
con19 <- apri_db(":memory:")
mk <- function(fondo, n, reati = character(0)) { f <- fascicolo_vuoto(); f$fondo <- fondo; f$busta <- 1L; f$fascicolo <- n
  f$reati <- reati; salva_fascicolo(con19, f) }
a1 <- mk("Sant'Ufficio", 1L, "Eresia"); a2 <- mk("Sant'Uffizio", 1L); mk("Sant'Ufficio", 2L)
mk("Tribunale Ecclesiastico", 1L, c("Magia", "Eresia"))
dd <- duplicati_fascicoli(con19)
atteso("duplicati: solo la grafia vicina del fondo, non un altro fondo", sort(dd$id), sort(c(a1, a2)))
atteso("duplicati: si tiene la più compilata", dd$id[dd$tieni], a1)
atteso("duplicati: rimossa la copia, restano gli altri", c(rimuovi_duplicati(con19, dd$id[!dd$tieni]), nrow(dbGetQuery(con19, "SELECT 1 FROM fascicoli")),
                                                           nrow(duplicati_fascicoli(con19))), c(1L, 3L, 0L))
# «b. 232» e «b. 232-bis» sono fascicoli diversi, non copie (19/10: lo strumento li metteva nello stesso gruppo e proponeva di eliminarne uno).
# Si provano: bis e ter della busta, suffisso del fascicolo, stesse posizioni in un altro fondo (la copia autentica è nel collaudo qui sopra).
mk2 <- function(fondo, busta, sfx_b, fasc, sfx_f = "") { f <- fascicolo_vuoto(); f$fondo <- fondo; f$busta <- busta; f$busta_suffisso <- sfx_b
  f$fascicolo <- fasc; f$fascicolo_suffisso <- sfx_f; f$titolo <- sprintf("%s b%s%s f%s%s", fondo, busta, sfx_b, fasc, sfx_f); f$titolo_manuale <- 1L
  f$persone <- "Rossi, Antonio"; f$reati <- "Eresia"; salva_fascicolo(con19, f); f$id }
distinti <- c(mk2("Fondo bis", 232L, "", 1L), mk2("Fondo bis", 232L, "-bis", 1L), mk2("Fondo bis", 232L, "-ter", 1L),
              mk2("Fondo bis", 232L, "", 2L), mk2("Fondo bis", 232L, "", 2L, "-bis"), mk2("Fondo bis", 232L, "-bis", 2L),
              mk2("Fondo bis", 232L, "-bis", 2L, "-bis"), mk2("Fondo ter", 232L, "", 1L), mk2("Fondo ter", 232L, "-bis", 1L))
atteso("duplicati: busta, busta-bis, busta-ter, suffissi del fascicolo e altri fondi non sono duplicati", nrow(duplicati_fascicoli(con19)), 0L)
# l'ordine dell'export: busta, suffisso della busta (bis, ter), fascicolo, suffisso del fascicolo
ordine <- .ids_in_ordine_di_segnatura(con19, "Fondo bis")
tit <- dbGetQuery(con19, sprintf("SELECT id, titolo FROM fascicoli WHERE id IN (%s)", paste0("'", ordine, "'", collapse = ",")))$titolo
tit <- tit[match(ordine, dbGetQuery(con19, sprintf("SELECT id FROM fascicoli WHERE id IN (%s)", paste0("'", ordine, "'", collapse = ",")))$id)]
atteso("ordine di segnatura: 232/1, 232/2, 232/2-bis, 232-bis/1, 232-bis/2, 232-bis/2-bis, 232-ter/1", tit,
       c("Fondo bis b232 f1", "Fondo bis b232 f2", "Fondo bis b232 f2-bis", "Fondo bis b232-bis f1", "Fondo bis b232-bis f2", "Fondo bis b232-bis f2-bis", "Fondo bis b232-ter f1"))
# eliminazione reale: si toglie SOLO la copia autentica (stessa busta-bis, fondo scritto «Fondo terr»), i fascicoli distinti restano tutti
mk2("Fondo terr", 232L, "-bis", 1L)
n_prima <- nrow(dbGetQuery(con19, "SELECT 1 FROM fascicoli"))
atteso("duplicati: la copia autentica con busta-bis è riconosciuta", nrow(duplicati_fascicoli(con19)), 2L)
atteso("duplicati: si elimina una sola scheda e tutti gli altri distinti restano (qualunque delle due copie si tenga)",
       c(rimuovi_duplicati(con19, local({ x <- duplicati_fascicoli(con19); x$id[!x$tieni] })), nrow(dbGetQuery(con19, "SELECT 1 FROM fascicoli")),
         sum(distinti[-9] %in% dbGetQuery(con19, "SELECT id FROM fascicoli")$id)), c(1L, n_prima - 1L, length(distinti) - 1L))
# Il fondo ha un id interno (19/10/2026, R/fondi.R): la stessa denominazione in due archivi sono DUE fondi, anche con la stessa segnatura
ma <- function(arch, fondo, n, busta = 1L) { f <- fascicolo_vuoto(); f$archivio <- arch; f$fondo <- fondo; f$busta <- busta; f$fascicolo <- n; f$titolo <- "t"; f$titolo_manuale <- 1L; salva_fascicolo(con19, f) }
ma("Archivio Uno", "Fondo Gemello", 1L)
atteso("fondi: la stessa scheda nello stesso archivio (anche con spazi e maiuscole diverse) resta nello stesso fondo",
       { try(ma(" archivio  UNO ", "Fondo Gemello", 2L), silent = TRUE); nrow(dbGetQuery(con19, "SELECT id FROM fondi WHERE denominazione = 'Fondo Gemello'")) }, 1L)
ma("Archivio Due", "Fondo Gemello", 1L)          # stessa denominazione, stessa busta e stesso fascicolo, ALTRO archivio: non è un conflitto
atteso("fondi: due archivi con la stessa denominazione di fondo sono due fondi, con la stessa segnatura",
       c(nrow(dbGetQuery(con19, "SELECT id FROM fondi WHERE denominazione = 'Fondo Gemello'")),
         dbGetQuery(con19, "SELECT COUNT(*) n FROM fascicoli WHERE fondo = 'Fondo Gemello' AND busta = 1 AND fascicolo = 1")$n), c(2L, 2L))
atteso("fondi: una denominazione che sta in due archivi non si risolve da sola (serve dire quale)",
       inherits(try(.fid(con19, "Fondo Gemello"), silent = TRUE), "try-error"), TRUE)
id_uno <- fondo_id_da(con19, "Archivio Uno", "Fondo Gemello"); id_due <- fondo_id_da(con19, "Archivio Due", "Fondo Gemello")
atteso("fondi: gli id sono diversi e ciascuno vede solo le sue schede",
       c(id_uno != id_due, nrow(elenco_fascicoli(con19, id_uno)), nrow(elenco_fascicoli(con19, id_due))), c(TRUE, 2L, 1L))
atteso("fondi: il piano di busta è del fondo, non del nome",
       { salva_piano(con19, id_uno, 1L, 1L, 5L); salva_piano(con19, id_due, 1L, 1L, 9L)
         c(dbGetQuery(con19, "SELECT ultimo FROM piani_busta WHERE fondo_id = ?", list(id_uno))$ultimo, dbGetQuery(con19, "SELECT ultimo FROM piani_busta WHERE fondo_id = ?", list(id_due))$ultimo) }, c(5L, 9L))
atteso("fondi: prossimo numero per fondo", c(prossimo_numero(con19, id_uno, 1L), prossimo_numero(con19, id_due, 1L)), c(3L, 2L))
atteso("fondi: archivio_del_fondo dice di quale archivio", c(archivio_del_fondo(con19, id_uno), archivio_del_fondo(con19, id_due)), c("Archivio Uno", "Archivio Due"))
atteso("fondi: crea_archivio rifiuta (archivio, denominazione) già esistenti, ma accetta lo stesso nome in un archivio nuovo",
       c(inherits(try(crea_archivio(con19, "archivio uno", "fondo gemello"), silent = TRUE), "try-error"),
         !inherits(try(crea_archivio(con19, "Archivio Tre", "Fondo Gemello"), silent = TRUE), "try-error")), c(TRUE, TRUE))
dbDisconnect(con19)

# --- colori dei reati (30/09) ----------------------------------------
source("R/colori.R")
atteso("colori reati: fissi e indipendenti dall'ordine",
       unname(colori_reati(c("Magia", "Eresia"))), unname(rev(colori_reati(c("Eresia", "Magia")))))
atteso("colori reati: tutti diversi, in chiaro e in scuro",
       c(anyDuplicated(COLORI_REATI$chiaro), anyDuplicated(COLORI_REATI$scuro)), c(0L, 0L))
# 29/10: nessun reato ha il colore di un segnale di stato (rosso di errore, ambra di avviso): era Eresia = rosso di errore, identico.
suppressMessages(source("R/tema.R"))
.de <- function(a, b) sqrt(sum((grDevices::convertColor(t(grDevices::col2rgb(a) / 255), "sRGB", "Lab") - grDevices::convertColor(t(grDevices::col2rgb(b) / 255), "sRGB", "Lab"))^2))
for (m in c("chiaro", "scuro")) {
  stato <- unlist(PALETTE_TEMA[[m]][c("pericolo", "segnale")])
  vicino <- vapply(COLORI_REATI[[m]], function(c) min(vapply(stato, function(s) .de(c, s), 1)), 1)
  atteso(sprintf("colori reati (%s): lontani dal rosso di errore e dall'ambra di avviso (ΔE >= 15)", m), names(vicino)[vicino < 15], character(0))
}
atteso("colori reati: fuori vocabolario, colore di riserva stabile",
       colori_reati("Stregoneria")[[1]] == colori_reati(c("Eresia", "Stregoneria"))[["Stregoneria"]], TRUE)

# 01/11: «data non disponibile» (data_nd = 1) chiude il campo anno nel conto dei quattro campi fondamentali.
atteso("completezza: data_nd conta come anno dichiarato assente",
       completezza_core(data.frame(persone = NA, titolo = "T", anno = NA_integer_, luoghi = "Roma", reati = "Eresia", data_nd = c(0L, 1L))), c(3L, 4L))

# --- ricomponi ed elimina le spuntate (30/09) -----------------------
con18 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- 1L; f$tipologia_documentaria <- "Processi"
f$reati <- "Eresia"; f$imputati <- data.frame(esteso = "Mario Rossi", indicizzato = "Rossi, Mario"); f$persone <- "Rossi, Mario"
f$titolo <- "Titolo scritto a mano"; f$titolo_manuale <- 1L; f$regesto <- NA; id18 <- salva_fascicolo(con18, f)
r <- ricomponi_in_blocco(con18, id18)
g <- leggi_fascicolo(con18, id18)
atteso("ricomponi: il manuale resta, il vuoto si compone", c(g$titolo, g$regesto),
       c("Titolo scritto a mano", "Processo per eresia contro Mario Rossi"))
ricomponi_in_blocco(con18, id18, anche_manuali = TRUE)
g <- leggi_fascicolo(con18, id18)
atteso("ricomponi: anche i manuali, tornano composti", c(g$titolo, g$titolo_manuale), c("Mario Rossi", "0"))
# Ricomposizione in blocco (20/10): i testi scritti a mano non si toccano di default, titoli e regesti si includono separatamente,
# l'anteprima (prova) non scrive, un risultato vuoto non sostituisce un testo presente.
conr <- apri_db(":memory:")
nuova <- function(b, titolo, regesto, tipologia = "Processi") {
  f <- fascicolo_vuoto(); f$fondo <- "F"; f$busta <- as.integer(b); f$fascicolo <- 1L; f$tipologia_documentaria <- tipologia
  f$reati <- "Eresia"; f$persone <- "Rossi, Mario"; f$imputati <- data.frame(esteso = "Mario Rossi", indicizzato = "Rossi, Mario")
  f$titolo <- titolo; f$titolo_manuale <- 1L; f$regesto <- regesto; f$regesto_manuale <- 1L; salva_fascicolo(conr, f) }
idm <- nuova(1, "Titolo d'inventario", "Regesto d'inventario")
p0 <- ricomponi_in_blocco(conr, idm, prova = TRUE, anche_titoli = TRUE, anche_regesti = TRUE)
g <- leggi_fascicolo(conr, idm)
atteso("ricomponi in blocco: la prova non scrive", c(g$titolo, g$regesto), c("Titolo d'inventario", "Regesto d'inventario"))
atteso("ricomponi in blocco: la prova dice quali testi scritti a mano sostituirebbe", p0$protetti, "titolo e regesto")
atteso("ricomponi in blocco: di default i protetti restano", nrow(ricomponi_in_blocco(conr, idm, prova = TRUE)), 0L)
ricomponi_in_blocco(conr, idm, anche_titoli = TRUE)
g <- leggi_fascicolo(conr, idm)
atteso("ricomponi in blocco: solo i titoli, il regesto scritto a mano resta",
       c(g$titolo, g$regesto, g$titolo_manuale, g$regesto_manuale), c("Mario Rossi", "Regesto d'inventario", "0", "1"))
idr <- nuova(2, "Titolo d'inventario", "Regesto d'inventario")
ricomponi_in_blocco(conr, idr, anche_regesti = TRUE)
g <- leggi_fascicolo(conr, idr)
atteso("ricomponi in blocco: solo i regesti, il titolo scritto a mano resta",
       c(g$titolo, g$regesto, g$titolo_manuale, g$regesto_manuale), c("Titolo d'inventario", "Processo per eresia contro Mario Rossi", "1", "0"))
ids <- nuova(3, "Stadera, Bernardina", "Atti d'inventario", tipologia = NA)
ids_f <- leggi_fascicolo(conr, ids); ids_f$persone <- character(0); ids_f$imputati <- ids_f$imputati[0, , drop = FALSE]
salva_fascicolo(conr, ids_f); ids_f <- leggi_fascicolo(conr, ids)      # senza persone e senza tipologia il titolo composto è vuoto
atteso("ricomponi in blocco: il composer qui dà un risultato vuoto", .vuoto(componi_titolo_scheda(ids_f, forza = TRUE)), TRUE)
ricomponi_in_blocco(conr, ids, anche_titoli = TRUE, anche_regesti = TRUE)
g <- leggi_fascicolo(conr, ids)
atteso("ricomponi in blocco: un risultato vuoto non sostituisce il titolo scritto a mano", c(g$titolo, g$titolo_manuale), c("Stadera, Bernardina", "1"))
dbDisconnect(conr)
atteso("elimina: con le tabelle figlie", c(elimina_fascicoli(con18, id18), nrow(dbGetQuery(con18, "SELECT 1 FROM reati")),
                                          nrow(dbGetQuery(con18, "SELECT 1 FROM imputati"))), c(1L, 0L, 0L))
dbDisconnect(con18)

# --- note rapide da archivio (30/09) --------------------------------
con17 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$archivio <- "ASDNa"; f$fondo <- "F"; f$busta <- 3L; f$fascicolo <- 3L
f$tipologia_documentaria <- "Processi"; f$estremo_remoto <- 1620L; salva_fascicolo(con17, f)
testo <- "3 | Mario Rossi | 1623 | eresia, Magia | sì | no | controllare data\n4 | | 1631 | Censura | n.d. | si\n5 | Anna Bianchi | 1618 | Sacramenti | forse | no"
a <- applica_note_rapide(con17, "F", 3, testo, prova = TRUE)
atteso("note rapide: azioni", a$azione, c("aggiorna", "nuova scheda", "errore"))
atteso("note rapide: anteprima non scrive", nrow(dbGetQuery(con17, "SELECT 1 FROM fascicoli")), 1L)
atteso("note rapide: conflitto sull'anno segnalato", grepl("resta «1620»", a$conflitti[1]), TRUE)
applica_note_rapide(con17, "F", 3, testo)
f3 <- leggi_fascicolo(con17, dbGetQuery(con17, "SELECT id FROM fascicoli WHERE fascicolo = 3")$id)
atteso("note rapide: riempie senza sovrascrivere",
       list(f3$estremo_remoto, f3$reati, f3$tortura, f3$sentenza, f3$persone, f3$note),
       list(1620L, c("Eresia", "Magia"), "sì", "no", "Rossi, Mario", "controllare data"))
atteso("note rapide: titolo composto", f3$titolo, "Mario Rossi")
f4 <- leggi_fascicolo(con17, dbGetQuery(con17, "SELECT id FROM fascicoli WHERE fascicolo = 4")$id)
atteso("note rapide: scheda nuova", list(f4$archivio, f4$estremo_remoto, f4$sentenza), list("ASDNa", 1631L, "sì"))
dbDisconnect(con17)

# --- genera fascicoli vuoti (30/09) ---------------------------------
con16 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$archivio <- "Archivio Storico Diocesano di Napoli"; f$fondo <- "Sant'Ufficio"
f$busta <- 29L; f$fascicolo <- 329L; f$reati <- "Eresia"; f$lingue <- "Latino"; f$note <- "da non copiare"; salva_fascicolo(con16, f)
g <- genera_fascicoli_vuoti(con16, "Sant'Ufficio", 29, 329, 332, produttore = "Inquisizione di Napoli")
atteso("vuoti: crea i mancanti, salta i presenti", list(g$creati, g$saltati), list(3L, 329L))
v <- leggi_fascicolo(con16, g$ids[1])
atteso("vuoti: eredita dove sta", c(v$archivio, v$lingue, v$produttori, v$luogo_produzione),
       c("Archivio Storico Diocesano di Napoli", "Latino", "Inquisizione di Napoli", "Napoli"))
atteso("vuoti: mai che cosa contiene", list(length(v$reati), v$note, v$tortura), list(0L, NA_character_, "n.d."))
atteso("vuoti: rifatto non crea doppioni", genera_fascicoli_vuoti(con16, "Sant'Ufficio", 29, 329, 332)$creati, 0L)
atteso("vuoti: numeri al contrario respinti",
       tryCatch(genera_fascicoli_vuoti(con16, "Sant'Ufficio", 29, 5, 2), error = function(e) "respinto"), "respinto")
dbDisconnect(con16)

# --- contesto delle schede vuote e tipologia che non si eredita (13/10, Stefano) ---
atteso("EREDITATI: la tipologia documentaria non si eredita", "tipologia_documentaria" %in% EREDITATI, FALSE)
atteso("EREDITATI: dove sta e com'è fatto sì", all(c("luogo_conservazione", "lingua", "definizione", "supporto") %in% EREDITATI), TRUE)
atteso("scheda nuova: parte senza tipologia", is.null(valori_nuovo(list(tipologia_documentaria = "Processi", fondo = "F"))$tipologia_documentaria), TRUE)
con18 <- apri_db(tempfile(fileext = ".sqlite"))
invisible(crea_archivio(con18, 'Archivio di Prova', 'Prova', NULL))
g18 <- genera_fascicoli_vuoti(con18, "Prova", 1, 1, 3)           # fondo nuovo: schede vuote PRIMA della prima compilata
v18 <- lapply(g18$ids, function(i) leggi_fascicolo(con18, i))
atteso("vuote: senza contenuto", vapply(v18, scheda_senza_contenuto, TRUE), c(TRUE, TRUE, TRUE))
p1 <- v18[[1]]; p1$luogo_conservazione <- "Archivio di Prova"; p1$lingue <- "Latino"; p1$produttori <- "Inquisizione di Napoli"
p1$luogo_produzione <- "Napoli"; p1$tipologia_documentaria <- "Processi"
p1$persone <- "Rossi, Mario"; p1$reati <- "Eresia"; p1$estremo_remoto <- 1600L
invisible(salva_fascicolo(con18, p1))
atteso("compilata: ha contenuto", scheda_senza_contenuto(leggi_fascicolo(con18, g18$ids[1])), FALSE)
r18 <- contesto_di_riferimento(con18, v18[[2]])
atteso("riferimento: la scheda compilata", r18$id, g18$ids[1])
atteso("riferimento: non è la scheda stessa", is.null(contesto_di_riferimento(con18, leggi_fascicolo(con18, g18$ids[1]))) || contesto_di_riferimento(con18, leggi_fascicolo(con18, g18$ids[1]))$id != g18$ids[1], TRUE)
rv <- valori_da_fascicolo(r18, profilo_completo("Generale"))[CONTESTO_EREDITABILE]
atteso("riferimento: i valori del contesto, senza la tipologia", list(rv$luogo_conservazione, rv$lingua, rv$soggetto, rv$luogo_produzione, "tipologia_documentaria" %in% names(rv)),
       list("Archivio di Prova", "Latino", "Inquisizione di Napoli", "Napoli", FALSE))
g18b <- genera_fascicoli_vuoti(con18, "Prova", 2, 1, 2)          # una busta nuova, dopo la prima compilata: eredita subito
vb <- leggi_fascicolo(con18, g18b$ids[1])
atteso("vuoti dopo la prima: contesto sì, tipologia no", list(vb$luogo_conservazione, vb$lingue, vb$produttori, vb$luogo_produzione, vb$tipologia_documentaria),
       list("Archivio di Prova", "Latino", "Inquisizione di Napoli", "Napoli", NA_character_))
dbDisconnect(con18)

# --- data topica dal soggetto produttore (30/09) ---------------------
atteso("data topica: Inquisizione di Capua -> Capua", citta_da_produttore("Inquisizione di Capua"), "Capua")
atteso("data topica: più produttori", citta_da_produttore(c("Inquisizione di Napoli", "Inquisizione di Roma", "Vescovo")),
       c("Napoli", "Roma"))
atteso("data topica: aggiunge senza togliere", data_topica_con_produttori("Napoli|Aversa", "Inquisizione di Capua"),
       "Capua|Napoli|Aversa")
atteso("data topica: già presente, invariata", data_topica_con_produttori("Napoli", "Inquisizione di Napoli"), "Napoli")
atteso("data topica: scheda nuova", valori_nuovo(list(), produttore = "Inquisizione di Capua")$luogo_produzione, "Capua")

# --- senza cognome non è una persona citata (29/09) -------------------
atteso("senza cognome: riconosciuti",
       senza_cognome(c("Loisia", "Paolo da Roma", "don Isidoro da Firenze", "da Tunisi, Caterina",
                       "Da Bagnoli, Crisostomo", "Caterina da Tunisi")), rep(TRUE, 6))
atteso("senza cognome: persone vere",
       senza_cognome(c("Franzese, Caterina", "de Vasto, Gilberto", "Maria Russo da Napoli", "di Gaeta, Camillo",
                       "dell'Abbate Giovanni Tommaso", "Caterina Franzese", "", NA)), rep(FALSE, 8))
atteso("senza cognome: nel titolo resta per esteso",
       componi_titolo("Denuncia", NA, .estendi_nome("da Tunisi, Caterina")), "Caterina da Tunisi")
con14 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$fondo <- "F"; f$busta <- 1L; f$fascicolo <- 1L
f$persone <- c("da Tunisi, Caterina", "Franzese, Caterina"); salva_fascicolo(con14, f)
atteso("senza cognome: resta nella scheda", sort(unlist(leggi_fascicolo(con14, dbGetQuery(con14, "SELECT id FROM fascicoli")$id)$persone)),
       c("da Tunisi, Caterina", "Franzese, Caterina"))
dbDisconnect(con14)

# --- menu a scelta caricato vuoto (29/09) ----------------------------
atteso("come caricato: scelta vuota = prima voce mostrata",
       come_caricato("definizione", "fascicolo", list(fondo = "x")), TRUE)
atteso("come caricato: scelta vuota, altra voce = modificato",
       come_caricato("definizione", "registro", list(fondo = "x")), FALSE)
# la tipologia documentaria parte VUOTA (13/10): vuota = com'era caricata, scegliere «Atti» è una modifica
atteso("tipologia: vuota caricata, vuota ora = non modificata", come_caricato("tipologia_documentaria", "", list(fondo = "x")), TRUE)
atteso("tipologia: vuota caricata, «Atti» scelta = modificata", come_caricato("tipologia_documentaria", "Atti", list(fondo = "x")), FALSE)
atteso("come caricato: tortura vuota = n.d.", come_caricato("tortura", "n.d.", list()), TRUE)

# --- nuovo archivio (Home, 29/09) ------------------------------------
con12 <- apri_db(":memory:")
f <- fascicolo_vuoto(); f$archivio <- "Archivio Storico Diocesano di Napoli"; f$fondo <- "Sant'Ufficio"
f$busta <- 1L; f$fascicolo <- 1L; salva_fascicolo(con12, f)
crea_archivio(con12, " Archivio di Stato di Modena ", "Inquisizione", "Processi", "ASMo")
atteso("archivio: il fondo vuoto compare con 0 schede", as.integer(elenco_fondi(con12)$n), c(1L, 0L))
atteso("archivio: la scheda nuova eredita archivio, fondo e serie", contesto_archivio(con12, "Inquisizione"),
       list(archivio = "Archivio di Stato di Modena", fondo = "Inquisizione", serie = "Processi"))
atteso("archivio: un solo profilo, il modello (2/10)", profilo_del_fondo(con12, "Inquisizione"), "Modello")
atteso("archivio: fondo già esistente respinto",
       tryCatch(crea_archivio(con12, "Archivio Storico Diocesano di Napoli", "sant'ufficio"), error = function(e) "respinto"), "respinto")
atteso("archivio: senza nome respinto", tryCatch(crea_archivio(con12, "", "Y"), error = function(e) "respinto"), "respinto")
tmp <- tempfile(fileext = ".json"); esporta_backup_json(con12, tmp)
con13 <- apri_db(":memory:"); .ripristina_dati_json(con13, leggi_backup_json(tmp), "sostituisci"); unlink(tmp)
atteso("archivio: passa dal backup JSON", dbGetQuery(con13, "SELECT denominazione, profilo FROM fondi WHERE dichiarato = 1")$profilo, "ASMo")
dbDisconnect(con12); dbDisconnect(con13)

# --- «Tipologia documentaria» nel foglio: sempre e solo «Atti» (2/10) ---
local({
  c0 <- apri_db(":memory:")
  for (k in seq_along(c("Processi", "Informazioni", "Atti per diversi", NA))) {
    f <- fascicolo_vuoto(); f$archivio <- "Archivio Storico Diocesano di Napoli"; f$fondo <- "Sant'Ufficio"
    f$busta <- 1L; f$fascicolo <- k; f$tipologia_documentaria <- c("Processi", "Informazioni", "Atti per diversi", NA)[k]
    salva_fascicolo(c0, f)
  }
  for (pn in names(PROFILI)) {
    d <- tabella_arianna(c0, pn)
    atteso(paste("export", pn, ": Tipologia documentaria sempre «Atti»"), unique(d[["Tipologia documentaria"]]), "Atti")
  }
  atteso("il tipo del titolo resta nel database", sort(dbGetQuery(c0, "SELECT tipologia_documentaria t FROM fascicoli WHERE t IS NOT NULL")$t),
         c("Atti per diversi", "Informazioni", "Processi"))

  # «…a meno che io non lo modifichi» (Stefano, 5/10): il campo apposito cambia
  # ciò che esce nel foglio, e solo quello; il giro del foglio lo conserva.
  g <- leggi_fascicolo(c0, dbGetQuery(c0, "SELECT id FROM fascicoli WHERE fascicolo = 1")$id)
  atteso("tipologia nel foglio: senza modifica è «Atti»", tipologia_foglio(g), "Atti")
  g$extra[[EXTRA_TIPOLOGIA_FOGLIO]] <- "Registri"; salva_fascicolo(c0, g)
  d <- tabella_arianna(c0, fondo = "Sant'Ufficio")
  atteso("tipologia nel foglio: quella scritta a mano esce, le altre restano «Atti»",
         as.character(d[["Tipologia documentaria"]][order(as.integer(sub(".*f\\.", "", d$Segnatura)))]), c("Registri", "Atti", "Atti", "Atti"))
  atteso("tipologia nel foglio: il tipo del titolo non cambia", leggi_fascicolo(c0, g$id)$tipologia_documentaria, "Processi")
  for (est in c("xlsx", "csv")) {
    file <- tempfile(fileext = paste0(".", est))
    if (est == "xlsx") esporta_arianna(c0, file, fondo = "Sant'Ufficio") else esporta_csv(c0, file, fondo = "Sant'Ufficio")
    c1 <- apri_db(":memory:"); importa_arianna(c1, leggi_arianna(file))
    d1 <- tabella_arianna(c1, fondo = "Sant'Ufficio")
    # Un foglio che arriva non cambia la tipologia del foglio di uscita: è sempre «Atti» (5/10).
    atteso(paste("tipologia nel foglio: un foglio che arriva non la cambia (", est, ")"), unique(as.character(d1[["Tipologia documentaria"]])), "Atti")
    dbDisconnect(c1)
  }
  # Un vecchio export scriveva nella colonna il tipo del titolo: reimportato non deve tornare nel foglio.
  riga <- as.list(d[1, ]); riga[["Tipologia documentaria"]] <- "Processi"
  col <- stats::setNames(as.list(names(d)), PROFILO_MODELLO$arianna)
  h <- fascicolo_da_riga(riga, PROFILO_MODELLO, col)
  atteso("un tipo del titolo nella colonna non diventa una modifica a mano", c(tipologia_foglio(h), h$tipologia_documentaria), c("Atti", "Processi"))
  g$extra[[EXTRA_TIPOLOGIA_FOGLIO]] <- NULL; g$campi_extra_noti <- EXTRA_TIPOLOGIA_FOGLIO; salva_fascicolo(c0, g)
  atteso("tipologia nel foglio: svuotato il campo torna «Atti»", unique(as.character(tabella_arianna(c0, fondo = "Sant'Ufficio")[["Tipologia documentaria"]])), "Atti")
  dbDisconnect(c0)
})

# --- Atti è sempre «circa», Denuncia è sempre «contro» (2/10) ----------
atteso("titolo: il connettore a mano non conta più", componi_titolo("Atti", "su", "Paolo Boccalupo"), "Paolo Boccalupo")
atteso("titolo: due nomi, solo i nomi", componi_titolo("Denuncia", NA, c("A B", "C D")), "A B e C D")
atteso("regesto: Denuncia contro", componi_regesto_narrativo("Denunce", "Eresia", "Solito, Matteo de"), "Denuncia per eresia contro Matteo de Solito")
atteso("regesto: Atti circa", componi_regesto_narrativo("Atti", "Magia", "Boccalupo, Paolo"), "Atti per magia circa Paolo Boccalupo")
atteso("regesto: Informazioni circa", componi_regesto_narrativo("Informazioni", "Eresia", "Rossi, Anna"), "Informazioni per eresia circa Anna Rossi")

# --- elenco dei nomi nel regesto composto (2/10) ----------------------
atteso("regesto: due imputati, «e»", componi_regesto_narrativo("Processi", "Eresia", c("Rossi, Anna", "Bianchi, Paolo")),
       "Processo per eresia contro Anna Rossi e Paolo Bianchi")
atteso("regesto: tre imputati, virgole e una sola «e»", componi_regesto_narrativo("Processi", "Eresia", c("Rossi, Anna", "Bianchi, Paolo", "Verdi, Luca")),
       "Processo per eresia contro Anna Rossi, Paolo Bianchi e Luca Verdi")
atteso("regesto: tre imputati si elencano", componi_regesto_narrativo("Atti", "Eresia", c("A, B", "C, D", "E, F")),
       "Atti per eresia circa B A, D C e F E")
atteso("regesto: quattro imputati sono «diversi soggetti» (al massimo tre nomi nel regesto)", componi_regesto_narrativo("Atti", "Eresia", c("A, B", "C, D", "E, F", "G, H")),
       "Atti per eresia circa diversi soggetti")
atteso(".regesto_uguale: «; » di prima = «e» di adesso", .regesto_uguale("Processo per eresia e magia contro X.", "Processo per eresia; magia contro X."), TRUE)
atteso(".regesto_uguale: «bestemmia e miscredenza» resta una voce", .regesto_uguale("Atti per diversi per bestemmia e miscredenza; ebraismo e magia.", "Atti per diversi per bestemmia e miscredenza; ebraismo; magia."), TRUE)
atteso(".regesto_uguale: quattro nomi di prima = «diversi soggetti» di adesso", .regesto_uguale("Atti per eresia circa diversi soggetti.", "Atti per eresia circa A, B, C e D."), TRUE)
atteso(".regesto_uguale: un reato diverso NON è uguale", .regesto_uguale("Processo per eresia contro X.", "Processo per magia contro X."), FALSE)
atteso(".regesto_uguale: un nome diverso NON è uguale", .regesto_uguale("Processo per eresia contro Y.", "Processo per eresia contro X."), FALSE)

# --- un nome scritto a mano: maiuscole e ordine (2/10) ---------------
atteso("nome: minuscolo e in ordine naturale", normalizza_persona("paolo gargiulo"), "Gargiulo, Paolo")
atteso("nome: due nomi di battesimo",        normalizza_persona("giovanni battista russo"), "Russo, Giovanni Battista")
atteso("nome: particella col cognome",       normalizza_persona("gilberto de vasto"), "de Vasto, Gilberto")
atteso("nome: apostrofo",                    normalizza_persona("caterina d'africa"), "d'Africa, Caterina")
atteso("nome: con la virgola resta l'ordine", normalizza_persona("gargiulo, paolo"), "Gargiulo, Paolo")
atteso("nome: virgola senza spazio",         normalizza_persona("gargiulo,paolo"), "Gargiulo, Paolo")
atteso("nome: titolo minuscolo",             normalizza_persona("don paolo boccalupo"), "Boccalupo, don Paolo")
atteso("nome: solo il nome non si gira",     normalizza_persona("loisia"), "Loisia")
atteso("nome: nome e provenienza non si gira", normalizza_persona("paolo da roma"), "Paolo da Roma")
atteso("nome: soprannome non si gira",       normalizza_persona("domenico detto il mancino"), "Domenico detto il Mancino")
atteso("nome: parentesi col cognome",        normalizza_persona("guglielmo cucco (sobring)"), "Cucco (Sobring), Guglielmo")
atteso("nome: già giusto resta com'è",       normalizza_persona(c("Russo, Giovanni Domenico", "de Vasto, Gilberto", "McDonald, John")),
       c("Russo, Giovanni Domenico", "de Vasto, Gilberto", "McDonald, John"))
atteso("nome: maiuscole scritte non si toccano", normalizza_persona("Paolo GARGIULO"), "GARGIULO, Paolo")
atteso("nome: vuoto e NA",                   normalizza_persona(c("", NA)), c("", NA))

# --- tabella dei fascicoli con zero righe ---------------------------
# Una ricerca senza risultati (2/10) faceva comparire un errore di R al posto
# della tabella vuota: una colonna costante non si ricicla su zero righe.
local({
  c0 <- apri_db(":memory:"); f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- 1L; salva_fascicolo(c0, f)
  d <- elenco_fascicoli(c0)
  atteso("tabella_elenco: una riga", nrow(tabella_elenco(d)), 1L)
  atteso("tabella_elenco: zero righe senza errore", tryCatch(nrow(tabella_elenco(d[0, , drop = FALSE])), error = function(e) conditionMessage(e)), 0L)
  dbDisconnect(c0)
})

# --- nessun nome definito in due file di R/ ------------------------
# runApp carica R/ in ordine alfabetico: di due funzioni con lo stesso nome
# vince l'ultima, in silenzio. Il 2/10 `.nel_periodo` di stati.R ha coperto
# quella di figure.R e due grafici della Dashboard davano errore.
local({
  nomi <- unlist(lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), function(f) {
    e <- parse(f, keep.source = FALSE)
    n <- vapply(e, function(x) if (is.call(x) && is.name(x[[1]]) && as.character(x[[1]]) %in% c("<-", "=") && is.name(x[[2]]))
                                 as.character(x[[2]]) else NA_character_, "")
    setNames(n[!is.na(n)], rep(basename(f), sum(!is.na(n))))
  }))
  per_file <- tapply(names(nomi), nomi, function(f) length(unique(f)))
  doppi <- names(per_file)[per_file > 1]
  atteso(paste("nomi definiti in più file di R/:", paste(doppi, collapse = ", ")), length(doppi), 0L)
})

# --- esito --------------------------------------------------------
# --- regesto vuoto senza reato, anche con persone (12/10, 535 schede di ASMo) ---
local({
  f <- fascicolo_vuoto(con_default = FALSE); f$tipologia_documentaria <- "Processi"
  atteso("regesto: senza persone né reati resta vuoto", componi_regesto_scheda(f), NA_character_)
  f$persone <- "Paffi, Camillo"
  atteso("regesto: con una persona ma senza reato né luogo resta vuoto (Paffi)", componi_regesto_scheda(f), NA_character_)
  f$persone <- c("Tassi, Giovanna", "Roveri, Giovanni"); f$tipologia_documentaria <- "Atti"
  atteso("regesto: più persone, senza reato né luogo, restano vuoto", componi_regesto_scheda(f), NA_character_)
  g <- fascicolo_vuoto(con_default = FALSE); g$tipologia_documentaria <- "Processi"; g$reati <- "Magia"
  atteso("regesto: con un reato si compone, impersonale (nessuno ha dichiarato imputati)", componi_regesto_scheda(g), "Processo per magia")
  g$persone <- "Rossi, Mario"
  atteso("regesto: reato e persona citata", componi_regesto_scheda(g), "Processo per magia contro Mario Rossi")
  l <- fascicolo_vuoto(con_default = FALSE); l$tipologia_documentaria <- "Processi"; l$persone <- "Rossi, Mario"; l$luoghi <- "Modena"
  atteso("regesto: i luoghi citati da soli non lo compongono (26/10: niente più «luogo del reato»)", componi_regesto_scheda(l), NA_character_)
  h <- fascicolo_vuoto(con_default = FALSE); h$tipologia_documentaria <- "Processi"; h$imputati_esaminati <- 1L
  atteso("regesto: «imputati esaminati» e nessuno = «ignoti» resta un fatto", componi_regesto_scheda(h), "Processo contro ignoti")
  k <- fascicolo_vuoto(con_default = FALSE); k$tipologia_documentaria <- TIPOLOGIA_PER_DIVERSI
  atteso("regesto: «Atti per diversi» è già una descrizione", componi_regesto_scheda(k), TIPOLOGIA_PER_DIVERSI)
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn), add = TRUE)
  v <- fascicolo_vuoto(con_default = FALSE); v$id <- "v1"; v$archivio <- "A"; v$fondo <- "M"; v$busta <- 1L; v$fascicolo <- 1L
  v$tipologia_documentaria <- "Processi"; v$persone <- "Paffi, Camillo"
  salva_fascicolo(cn, v)
  atteso("regesto: Salva su Paffi non scrive il regesto e tiene la persona",
         c(is.na(dbGetQuery(cn, "SELECT regesto FROM fascicoli WHERE id='v1'")$regesto), dbGetQuery(cn, "SELECT persona FROM persone WHERE fascicolo_id='v1'")$persona), c(TRUE, "Paffi, Camillo"))
})

# --- i tre flag sulla fonte: reato_ignoto, luogo_ignoto, non_procedimento (12/10) ---
local({
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn), add = TRUE)
  nuova <- function(id, n, ...) { f <- fascicolo_vuoto(con_default = FALSE); f$id <- id; f$archivio <- "A"; f$fondo <- "M"; f$busta <- 1L; f$fascicolo <- n
    f$tipologia_documentaria <- "Atti"; a <- list(...); for (k in names(a)) f[[k]] <- a[[k]]; salva_fascicolo(cn, f); leggi_fascicolo(cn, id) }
  vuota <- fascicolo_vuoto(con_default = FALSE)
  atteso("flag fonte: nascono a zero", c(vuota$reato_ignoto, vuota$luogo_ignoto, vuota$non_procedimento), c(0L, 0L, 0L))
  a <- nuova("a", 1L, persone = "Montagnano, Giulio", reato_ignoto = 1L)
  atteso("flag fonte: reato ignoto compone la formula di Stefano", a$regesto, "Atti circa Giulio Montagnano, ignoto il reato contestato")
  atteso("flag fonte: il flag si salva e si rilegge", c(a$reato_ignoto, a$luogo_ignoto, a$non_procedimento), c(1L, 0L, 0L))
  b <- nuova("a", 1L, persone = "Montagnano, Giulio", reato_ignoto = 1L)
  atteso("flag fonte: Salva ripetuto è idempotente", c(b$regesto, b$reato_ignoto, b$modificato == a$modificato || TRUE), c(a$regesto, 1L, TRUE))
  s0 <- nuova("s0", 9L, reato_ignoto = 1L)
  atteso("flag fonte: reato ignoto senza persone non compone «circa ignoti»", c(s0$reato_ignoto, is.na(s0$regesto)), c(1L, TRUE))
  l <- nuova("l", 2L, persone = "Rossi, Mario", luogo_ignoto = 1L)
  atteso("flag fonte: luogo ignoto compone la sua formula", l$regesto, "Atti circa Mario Rossi, ignoto il luogo")
  rl <- nuova("rl", 3L, persone = "Rossi, Mario", reato_ignoto = 1L, luogo_ignoto = 1L)
  atteso("flag fonte: reato e luogo ignoti insieme", rl$regesto, "Atti circa Mario Rossi, ignoto il reato contestato, ignoto il luogo")
  # spegnimento automatico quando un dato li contraddice
  r <- nuova("r", 4L, persone = "Rossi, Mario", reato_ignoto = 1L, reati = "Magia")
  atteso("flag fonte: un reato spegne «reato ignoto»", c(r$reato_ignoto, r$regesto), c(0L, "Atti per magia circa Mario Rossi"))
  g <- nuova("g", 5L, persone = "Rossi, Mario", luogo_ignoto = 1L, luoghi = "Modena", reati = "Magia")
  atteso("flag fonte: un luogo citato spegne «luogo ignoto»", g$luogo_ignoto, 0L)
  # se poi si toglie il reato, il flag NON si riaccende da solo: vale la regola (regesto vuoto)
  r2 <- nuova("r", 4L, persone = "Rossi, Mario", reati = character(0))
  atteso("flag fonte: tolto il reato il flag non si riaccende e il regesto resta vuoto", c(r2$reato_ignoto, is.na(r2$regesto)), c(0L, TRUE))
  # non è un procedimento: esclude gli altri due, nessun regesto composto
  n <- nuova("n", 6L, persone = "Rossi, Mario", non_procedimento = 1L, reato_ignoto = 1L, luogo_ignoto = 1L)
  atteso("flag fonte: «non è un procedimento» spegne gli altri due e non compone", c(n$non_procedimento, n$reato_ignoto, n$luogo_ignoto, is.na(n$regesto)), c(1L, 0L, 0L, TRUE))
  # regesto scritto a mano: il flag non lo tocca né lo aggira
  m <- nuova("m", 7L, persone = "Rossi, Mario", regesto = "Scritto a mano.", regesto_manuale = 1L, reato_ignoto = 1L, non_procedimento = 0L)
  atteso("flag fonte: il regesto a mano resta com'è", m$regesto, "Scritto a mano.")
  m2 <- nuova("m", 7L, persone = "Rossi, Mario", regesto = "Scritto a mano.", regesto_manuale = 1L, non_procedimento = 1L)
  atteso("flag fonte: nemmeno «non è un procedimento» tocca un regesto a mano", m2$regesto, "Scritto a mano.")
  # Annulla rimette anche i flag
  nuova("u", 8L, persone = "Rossi, Mario", reato_ignoto = 1L)
  f <- leggi_fascicolo(cn, "u"); f$reato_ignoto <- 0L; f$non_procedimento <- 1L; salva_fascicolo(cn, f, storia = TRUE)
  atteso("flag fonte: la modifica si salva", c(leggi_fascicolo(cn, "u")$reato_ignoto, leggi_fascicolo(cn, "u")$non_procedimento), c(0L, 1L))
  annulla_ultima_modifica(cn, "u")
  atteso("flag fonte: Annulla rimette i flag di prima", c(leggi_fascicolo(cn, "u")$reato_ignoto, leggi_fascicolo(cn, "u")$non_procedimento), c(1L, 0L))
  # backup JSON: i flag fanno il giro
  j <- tempfile(fileext = ".json"); esporta_backup_json(cn, j); c2 <- apri_db(":memory:"); invisible(ripristina_backup_json(c2, j))
  atteso("flag fonte: fanno il giro del backup JSON", dbGetQuery(c2, "SELECT id, reato_ignoto, luogo_ignoto, non_procedimento FROM fascicoli ORDER BY id"),
         dbGetQuery(cn, "SELECT id, reato_ignoto, luogo_ignoto, non_procedimento FROM fascicoli ORDER BY id"))
  dbDisconnect(c2)
  # un backup vecchio (senza i flag) si legge con zeri
  vb <- jsonlite::read_json(j); for (i in seq_along(vb$fascicoli)) vb$fascicoli[[i]][c("reato_ignoto", "luogo_ignoto", "non_procedimento")] <- NULL
  jv <- tempfile(fileext = ".json"); jsonlite::write_json(vb, jv, auto_unbox = TRUE, null = "null", na = "null"); c3 <- apri_db(":memory:"); invisible(ripristina_backup_json(c3, jv))
  atteso("flag fonte: un backup di prima del 12/10 si legge con i flag a zero", sum(dbGetQuery(c3, "SELECT SUM(reato_ignoto + luogo_ignoto + non_procedimento) AS n FROM fascicoli")$n), 0L)
  dbDisconnect(c3)
  # export: nessuna colonna in più, i flag non escono
  tb <- tabella_arianna(cn, fondo = "M")
  atteso("flag fonte: l'export resta di 28 colonne e non nomina i flag", c(ncol(tb), any(grepl("ignoto|procedimento", names(tb), ignore.case = TRUE))), c(28L, 0L))
  atteso("flag fonte: nessuna cella dell'export contiene i flag", any(grepl("reato_ignoto|luogo_ignoto|non_procedimento", unlist(tb))), FALSE)
  # Qualità: «Senza tipo di reato» non conta chi è ignoto o non procedimento
  q <- qualita(cn, "M"); sr <- q$casi[q$controllo == "Senza tipo di reato"]
  atteso("flag fonte: Qualità non conta reato ignoto e non procedimento fra i «senza reato»", sr, nrow(dbGetQuery(cn, "SELECT 1 FROM fascicoli f WHERE NOT EXISTS (SELECT 1 FROM reati r WHERE r.fascicolo_id=f.id) AND reato_ignoto=0 AND non_procedimento=0")))
  # completezza: i flag contano come «guardato»
  d <- data.frame(persone = "X", titolo = "T", anno = 1600L, luoghi = NA_character_, reati = NA_character_, reato_ignoto = 1L, luogo_ignoto = 0L, non_procedimento = 0L)
  atteso("flag fonte: completezza_core conta «reato ignoto»", completezza_core(d), 3L)
  d$non_procedimento <- 1L
  atteso("flag fonte: «non è un procedimento» toglie reato e luogo dal conto", completezza_core(d), 4L)
  # lavoro in blocco: sono impostabili
  atteso("flag fonte: «Imposta un campo» li offre", all(c("reato_ignoto", "luogo_ignoto", "non_procedimento") %in% CAMPI_IN_BLOCCO()$campo), TRUE)
})

# --- luogo di conservazione e soggetto produttore: enti, per fondo; soggetto conservatore eliminato (12/10) ---
local({
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn), add = TRUE)
  mk <- function(id, archivio, fondo, n, produttore = NULL, conserv = NA_character_, ...) {
    f <- fascicolo_vuoto(con_default = FALSE); f$id <- id; f$archivio <- archivio; f$fondo <- fondo; f$busta <- 1L; f$fascicolo <- n
    f$luogo_conservazione <- conserv; if (!is.null(produttore)) f$produttori <- produttore; salva_fascicolo(cn, f) }
  mk("n1", "Archivio Storico Diocesano di Napoli", "Sant'Ufficio", 1L, "Inquisizione di Napoli", "Archivio Storico Diocesano di Napoli")
  mk("b1", "Archivio Arcivescovile di Bologna", "Sant'Officio", 1L, "Inquisizione di Bologna", "Bologna")
  mk("z1", "Archivio di Prova", "Fondo Nuovo", 1L)
  sn <- suggerimenti_da_db(cn, "Sant'Ufficio")
  atteso("suggerimenti: luogo di conservazione = solo enti del fondo, mai città", sn$luogo_conservazione, "Archivio Storico Diocesano di Napoli")
  atteso("suggerimenti: soggetto produttore = solo quelli del fondo", sn$soggetto, "Inquisizione di Napoli")
  sb <- suggerimenti_da_db(cn, "Sant'Officio")
  atteso("suggerimenti: Bologna non vede Napoli e «Bologna» (città) non è un ente", c(sb$luogo_conservazione, sb$soggetto), c("Archivio Arcivescovile di Bologna", "Inquisizione di Bologna"))
  sz <- suggerimenti_da_db(cn, "Fondo Nuovo")
  atteso("suggerimenti: un fondo nuovo non ha produttori da altri fondi; l'ente è il suo archivio", list(sz$soggetto, sz$luogo_conservazione), list(character(0), "Archivio di Prova"))
  atteso("suggerimenti: le città non entrano mai nel luogo di conservazione", any(c("Napoli", "Bologna") %in% suggerimenti_da_db(cn)$luogo_conservazione), FALSE)
  atteso("produttore di partenza: quello dell'ultima scheda DEL FONDO", c(produttore_del_fondo(cn, "Sant'Ufficio"), produttore_del_fondo(cn, "Sant'Officio")), c("Inquisizione di Napoli", "Inquisizione di Bologna"))
  atteso("produttore di partenza: un fondo nuovo parte vuoto, mai Napoli", is.null(produttore_del_fondo(cn, "Fondo Nuovo")), TRUE)
  mk("z2", "Archivio di Prova", "Fondo Nuovo", 2L, "Tribunale di Prova")
  atteso("produttore di partenza: la prima scheda lo fissa per le successive di quel fondo, solo di quel fondo", c(produttore_del_fondo(cn, "Fondo Nuovo"), produttore_del_fondo(cn, "Sant'Ufficio")), c("Tribunale di Prova", "Inquisizione di Napoli"))
  # «Soggetto conservatore» non c'è più
  atteso("soggetto conservatore: non è nella scheda vuota né nel profilo della maschera", c("soggetto_conservatore" %in% names(fascicolo_vuoto()), "soggetto_conservatore" %in% profilo_completo("Generale")$campo), c(FALSE, FALSE))
  atteso("soggetto conservatore: una scheda nuova non riceve più il default di Modena", is.na(dbGetQuery(cn, "SELECT soggetto_conservatore AS v FROM fascicoli WHERE id = 'z1'")$v), TRUE)
  # il soggetto produttore: «Tribunale dell'Inquisizione di X» = «Inquisizione di X»
  atteso("produttore: la forma lunga si assorbe", .produttore_canonico(c("Tribunale dell'Inquisizione di Napoli", "Inquisizione di Modena", "Tribunale dell\u2019Inquisizione di Reggio Emilia", "Tribunale di Prova")),
         c("Inquisizione di Napoli", "Inquisizione di Modena", "Inquisizione di Reggio Emilia", "Tribunale di Prova"))
  atteso("produttore: le due forme della stessa scheda diventano una", .produttore_canonico(c("Tribunale dell'Inquisizione di Napoli", "Inquisizione di Napoli")), "Inquisizione di Napoli")
  mk("t1", "Archivio di Stato di Modena", "Fondo T", 1L, c("Tribunale dell'Inquisizione di Modena", "Inquisizione di Modena"))
  atteso("produttore: Salva scrive la forma «Inquisizione di»", dbGetQuery(cn, "SELECT soggetto FROM produttori WHERE fascicolo_id = 't1'")$soggetto, "Inquisizione di Modena")
  dbExecute(cn, "INSERT INTO produttori (fascicolo_id, soggetto) VALUES ('n1', 'Tribunale dell''Inquisizione di Napoli')")
  atteso("produttore: la prova di allinea_produttori non scrive", c(sum(allinea_produttori(cn, FALSE)$righe), nrow(dbGetQuery(cn, "SELECT 1 FROM produttori WHERE soggetto LIKE 'Tribunale dell%'"))), c(1L, 1L))
  allinea_produttori(cn, TRUE)
  atteso("produttore: allinea_produttori assorbe e non lascia doppioni", dbGetQuery(cn, "SELECT soggetto FROM produttori WHERE fascicolo_id = 'n1'")$soggetto, "Inquisizione di Napoli")
  atteso("produttore: la città si ricava ancora dal produttore", citta_da_produttore("Inquisizione di Napoli"), "Napoli")
  # città → ente, solo la città dell'archivio; il vuoto resta vuoto all'import
  f <- list(archivio = "Archivio Arcivescovile di Bologna", luogo_conservazione = "Bologna")
  atteso("import: la città dell'archivio diventa l'ente", .ente_conservatore(f)$luogo_conservazione, "Archivio Arcivescovile di Bologna")
  atteso("import: un campo vuoto resta vuoto (nessun default)", is.na(.ente_conservatore(list(archivio = "Archivio di Stato di Modena", luogo_conservazione = NA_character_))$luogo_conservazione), TRUE)
  atteso("import: un ente già scritto resta com'è", .ente_conservatore(list(archivio = "Archivio di Stato di Modena", luogo_conservazione = "Archivio Storico di Prova"))$luogo_conservazione, "Archivio Storico di Prova")
  atteso("import: un'altra città non si tocca", .ente_conservatore(list(archivio = "Archivio di Stato di Modena", luogo_conservazione = "Roma"))$luogo_conservazione, "Roma")
  # allineamento dei dati già nel database
  prova <- allinea_luogo_conservazione(cn, applica = FALSE)
  atteso("allineamento: la prova non scrive e dice cosa cambierebbe", c(sum(prova$schede), dbGetQuery(cn, "SELECT luogo_conservazione AS v FROM fascicoli WHERE id = 'b1'")$v), c("4", "Bologna"))
  allinea_luogo_conservazione(cn, applica = TRUE)
  atteso("allineamento: la città e il vuoto diventano l'ente, l'ente resta", dbGetQuery(cn, "SELECT luogo_conservazione AS v FROM fascicoli ORDER BY id")$v,
         c("Archivio Arcivescovile di Bologna", "Archivio Storico Diocesano di Napoli", "Archivio di Stato di Modena", "Archivio di Prova", "Archivio di Prova"))
  atteso("allineamento: ripetuto non cambia più niente", nrow(allinea_luogo_conservazione(cn, applica = FALSE)), 0L)
})

# --- niente valori di partenza di Modena; suggerimenti per fondo (12/10, revisione) ---
local({
  v <- Sys.getenv("TRAMA_DEFAULT_DI_PROVA"); Sys.unsetenv("TRAMA_DEFAULT_DI_PROVA"); on.exit(Sys.setenv(TRAMA_DEFAULT_DI_PROVA = v), add = TRUE)
  f <- fascicolo_vuoto()
  atteso("scheda vuota di produzione: archivio, fondo, serie e lingua non hanno un valore di partenza", list(f$archivio, f$fondo, f$serie, f$lingue), list(NA_character_, NA_character_, NA_character_, character(0)))
  Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")
  atteso("scheda di prova dei collaudi: i vecchi valori", c(fascicolo_vuoto()$archivio, fascicolo_vuoto()$lingue), c("Archivio di Stato di Modena", "Italiano"))
  Sys.unsetenv("TRAMA_DEFAULT_DI_PROVA")
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn), add = TRUE)
  mk <- function(id, fondo, n, persona, luogo, lingua) { x <- fascicolo_vuoto(); x$id <- id; x$archivio <- "A"; x$fondo <- fondo; x$busta <- 1L; x$fascicolo <- n
    x$persone <- persona; x$luoghi <- luogo; x$lingue <- lingua; salva_fascicolo(cn, x) }
  mk("a", "Fondo A", 1L, "Rossi, Mario", "Napoli", "Latino"); mk("b", "Fondo B", 1L, "Bianchi, Anna", "Modena", "Italiano")
  sa <- suggerimenti_da_db(cn, "Fondo A")
  atteso("suggerimenti: persone, luoghi e lingue sono quelli del fondo a schermo", list(sa$persona, sa$luogo, sa$lingua), list("Rossi, Mario", "Napoli", "Latino"))
  atteso("suggerimenti: un fondo nuovo non ha niente da altri fondi", lengths(suggerimenti_da_db(cn, "Fondo Nuovo")[c("persona", "luogo", "lingua")]), c(persona = 0L, luogo = 0L, lingua = 0L))
  atteso("suggerimenti: «tutti i fondi» li ha tutti", sort(suggerimenti_da_db(cn)$persona), c("Bianchi, Anna", "Rossi, Mario"))
})

# --- menu «Fondo»: archivio e località (12/10) ---
local({
  l <- localita_archivio(c("Archivio di Stato di Modena", "Archivio Storico Diocesano di Napoli", "Archiginnasio di Bologna",
                           "Archivio Arcivescovile di Bologna", "Archivio Storico Diocesano di Capua", "Archivio Comunale", "Archivio di Reggio Emilia"))
  atteso("località dell'archivio", l$localita, c("Modena", "Napoli", "Bologna", "Bologna", "Capua", NA, "Reggio Emilia"))
  atteso("nome dell'archivio senza la città", l$nome_breve, c("Archivio di Stato", "Archivio Storico Diocesano", "Archiginnasio", "Archivio Arcivescovile", "Archivio Storico Diocesano", "Archivio Comunale", "Archivio"))
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn), add = TRUE)
  mk <- function(id, archivio, fondo, n) { f <- fascicolo_vuoto(con_default = FALSE); f$id <- id; f$archivio <- archivio; f$fondo <- fondo; f$busta <- 1L; f$fascicolo <- n; salva_fascicolo(cn, f) }
  mk("a", "Archivio Storico Diocesano di Napoli", "Sant'Ufficio", 1L); mk("b", "Archivio Arcivescovile di Bologna", "Sant'Officio", 1L); mk("c", "Archivio Comunale", "Fondo C", 1L)
  atteso("etichette del menu: «Fondo» non si ripete se la denominazione comincia già così", c(con_parola_fondo("Fondo di Prova"), con_parola_fondo("fondo Z"), con_parola_fondo("Fondazione Y"), con_parola_fondo("Sant'Ufficio")),
         c("Fondo di Prova", "fondo Z", "Fondo Fondazione Y", "Fondo Sant'Ufficio"))
  atteso("etichette del menu: fondo e ente conservatore per esteso", etichetta_fondi(cn, c("Sant'Ufficio", "Sant'Officio", "Fondo C", "Fondo X"), c(367L, 110L, 5L, 2L)),
         c("Fondo Sant'Ufficio \u00b7 Archivio Storico Diocesano di Napoli (367)", "Fondo Sant'Officio \u00b7 Archivio Arcivescovile di Bologna (110)", "Fondo C \u00b7 Archivio Comunale (5)", "Fondo X (2)"))
  atteso("etichette del menu: il luogo di conservazione scritto vince sull'archivio", {
    f <- fascicolo_vuoto(con_default = FALSE); f$id <- "e1"; f$archivio <- "Archivio Comunale"; f$fondo <- "Fondo E"; f$busta <- 1L; f$fascicolo <- 1L
    f$luogo_conservazione <- "Biblioteca di Prova"; salva_fascicolo(cn, f); etichetta_fondi(cn, "Fondo E", 1L) }, "Fondo E \u00b7 Biblioteca di Prova (1)")
  atteso("etichette del menu: una città nel luogo di conservazione non entra, vale l'archivio", {
    f <- fascicolo_vuoto(con_default = FALSE); f$id <- "e2"; f$archivio <- "Archivio Arcivescovile di Bologna"; f$fondo <- "Fondo F"; f$busta <- 1L; f$fascicolo <- 1L
    f$luogo_conservazione <- "Bologna"; salva_fascicolo(cn, f); etichetta_fondi(cn, "Fondo F", 1L) }, "Fondo F \u00b7 Archivio Arcivescovile di Bologna (1)")
})

# --- copia degli export in Documenti (12/10) ---
local({
  d <- tempfile(); f <- tempfile(fileext = ".xlsx"); writeLines("dati", f)
  nl <- paste0("trama-localita-modena-", format(Sys.Date()), ".xlsx")   # la data è già nel nome: la copia lo lascia com'è
  dest <- salva_copia_export(f, nl, cartella = file.path(d, "export"))
  atteso("copia export: il file c'è, col nome dello scarico", c(basename(dest), file.exists(dest), readLines(dest)), c(nl, "TRUE", "dati"))
  writeLines("nuovi", f); salva_copia_export(f, nl, cartella = file.path(d, "export"))
  atteso("copia export: stesso nome, stesso giorno = si aggiorna", readLines(dest), "nuovi")
  atteso("nome della copia: fondo e data si aggiungono", nome_copia_export("asmo 243.xlsx", "tribunale-inquisizione-modena", as.Date("2026-10-12")), "asmo 243-tribunale-inquisizione-modena-2026-10-12.xlsx")
  atteso("nome della copia: due fondi con lo stesso nome non collidono", nome_copia_export("asmo 243.csv", "a", as.Date("2026-10-12")) != nome_copia_export("asmo 243.csv", "b", as.Date("2026-10-12")), TRUE)
  atteso("nome della copia: due giorni non collidono", nome_copia_export("asmo 243.csv", "a", as.Date("2026-10-12")) != nome_copia_export("asmo 243.csv", "a", as.Date("2026-10-13")), TRUE)
  atteso("nome della copia: con la data già dentro resta com'è", nome_copia_export("trama-localita-modena-2026-10-12.xlsx", "modena", as.Date("2026-10-12")), "trama-localita-modena-2026-10-12.xlsx")
  atteso("nome della copia: senza fondo solo la data", nome_copia_export("x.json", NULL, as.Date("2026-10-12")), "x-2026-10-12.json")
  atteso("nome della copia: senza estensione", nome_copia_export("asmo", "m", as.Date("2026-10-12")), "asmo-m-2026-10-12")
  atteso("copia export: il nome scritto porta il fondo e la data", basename(salva_copia_export(f, "asmo 243.xlsx", cartella = file.path(d, "export"), pezzo_fondo = "modena")), paste0("asmo 243-modena-", format(Sys.Date()), ".xlsx"))
  atteso("copia export: un nome con un percorso resta nella cartella", dirname(salva_copia_export(f, "../fuori.xlsx", cartella = file.path(d, "export"))), file.path(d, "export"))
  atteso("copia export: cartella impossibile = NULL, mai un errore", is.null(salva_copia_export(f, "x.xlsx", cartella = file.path(f, "sotto"))), TRUE)
})

# --- verifiche della fonte da fare: esame_da_fare e la parola `esame` (2/10) ---
local({
  d <- data.frame(tortura = c("n.d.", "sì", "n.d.", "no", NA), sentenza = c("n.d.", "n.d.", "n.d.", "no", "n.d."),
                  tortura_esaminata = c(0L, 0L, 1L, 0L, 0L), sentenza_esaminata = c(0L, 1L, 1L, 0L, 0L), stringsAsFactors = FALSE)
  # 1: entrambe n.d. senza spunta; 2: tortura sì, sentenza n.d. con spunta (fatto); 3: n.d. con spunta (fonte muta, fatto);
  # 4: sì/no noti; 5: tortura NA e sentenza n.d. senza spunta
  atteso("esame_da_fare: i cinque casi", esame_da_fare(d), c(TRUE, FALSE, FALSE, FALSE, TRUE))
  d$id <- paste0("f", 1:5); d$titolo <- NA; d$reati <- NA; d$luoghi <- NA; d$persone <- NA; d$busta <- 1L; d$fascicolo <- 1:5
  atteso("ricerca esame:da-fare", filtra_avanzato(d, "esame:da-fare")$id, c("f1", "f5"))
  atteso("ricerca esame:fatto",   filtra_avanzato(d, "esame:fatto")$id, c("f2", "f3", "f4"))
  atteso("ricerca check:pending (inglese)", filtra_avanzato(d, "check:pending")$id, c("f1", "f5"))
  atteso("ricerca esame: valore illegibile avvisa", length(avvisi_ricerca("esame:boh")), 1L)
  atteso("ricerca esame:da-fare non avvisa", length(avvisi_ricerca("esame:da-fare")), 0L)
  atteso("esame_da_fare: zero righe", length(esame_da_fare(d[0, ])), 0L)
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn))
  dbExecute(cn, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, creato, modificato) VALUES ('a', 'A', 'X', 1, 1, 'x', 'x')")
  e <- elenco_fascicoli(cn)
  atteso("elenco_fascicoli porta i due flag", all(c("tortura_esaminata", "sentenza_esaminata") %in% names(e)), TRUE)
  atteso("elenco_fascicoli + esame_da_fare: scheda nuova da esaminare", esame_da_fare(e), TRUE)
})

# --- un solo modello di export e di import (2/10) ----------------------
atteso("modello: 28 colonne", nrow(PROFILO_MODELLO), 28L)
atteso("modello: riconosciuto dalle sue intestazioni", riconosci_profilo(PROFILO_MODELLO$arianna), "Modello")
atteso("modello: i vecchi profili si leggono ancora", c(riconosci_profilo(PROFILI$ASDNa$arianna), riconosci_profilo(PROFILI$ASMo$arianna)), c("ASDNa", "ASMo"))
atteso("modello: l'export non ha più scelta di profilo", names(tabella_arianna(apri_db(":memory:") |> (function(cn) { dbExecute(cn, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, archivio, creato, modificato) VALUES ('m1', 'A', 'F', 1, 1, 'Archivio X', 'x', 'x')"); cn })(), "ASMo", intestazioni = c("a", "b"))), PROFILO_MODELLO$arianna)
local({
  # una riga dal vecchio formato ASMo: i nomi cambiano, il dato resta
  riga <- data.frame(`Segnatura` = "Archivio di Stato di Modena, Tribunale dell'Inquisizione di Modena, Processi, b. 1, fasc. 2",
                     `Consistenza (quantità)` = "3", `Consistenza (unità di misura)` = "carte", `Altezza` = "30",
                     `Organizzazioni citate` = "Camera apostolica", `Persone citate` = "Rossi, Anna",
                     check.names = FALSE, stringsAsFactors = FALSE)
  col <- setNames(names(riga), names(riga))
  f <- fascicolo_da_riga(riga, PROFILO_ASMO, as.list(col))
  atteso("import vecchio ASMo: consistenza col nome del modello", f$extra[["consistenza.quantità"]], "3")
  atteso("import vecchio ASMo: unità di misura col nome del modello", f$extra[["consistenza.unitàMisura"]], "carte")
  atteso("import vecchio ASMo: Altezza -> dimensioni/altezza", f$extra[["dimensioni/altezza"]], "30")
  atteso("import vecchio ASMo: Organizzazioni -> Soggetti citati o indicizzati", f$extra[["Soggetti citati o indicizzati"]], "Camera apostolica")
  # e l'export le riscrive nelle colonne del modello
  f$archivio <- "Archivio di Stato di Modena"; f$fondo <- "Tribunale"; f$busta <- 1L; f$fascicolo <- 2L
  r <- riga_da_fascicolo(f)
  atteso("export: le colonne del modello, nell'ordine", names(r), PROFILO_MODELLO$arianna)
  atteso("export: consistenza", c(r[["consistenza.quantità"]], r[["consistenza.unitàMisura"]]), c("3", "carte"))
  atteso("export: soggetti citati", r[["Soggetti citati o indicizzati"]], "Camera apostolica")
  # «Personaggi indicizzati» (ASDNa) sono persone: stanno con le persone citate
  riga2 <- data.frame(Segnatura = "Archivio Storico Diocesano di Napoli, Sant'Ufficio, b.1, f.1", `Persone citate` = "Rossi, Anna",
                      `Personaggi indicizzati` = "Bianchi, Paolo", check.names = FALSE, stringsAsFactors = FALSE)
  f2 <- fascicolo_da_riga(riga2, PROFILO_ASDNA, as.list(setNames(names(riga2), names(riga2))))
  atteso("import vecchio ASDNa: Personaggi indicizzati con le persone", f2$persone, c("Rossi, Anna", "Bianchi, Paolo"))
})
# La segnatura segue la convenzione dell'archivio; il resto del foglio è uguale per tutti.
local({
  f <- fascicolo_vuoto(); f$fondo <- "Sant'Ufficio"; f$busta <- 3L; f$fascicolo <- 7L
  f$archivio <- "Archivio Storico Diocesano di Napoli"
  atteso("segnatura: Napoli", riga_da_fascicolo(f)[["Segnatura"]], "Archivio Storico Diocesano di Napoli, Sant'Ufficio, b.3, f.7")
  f$archivio <- "Archivio di Stato di Modena"; f$serie <- "Processi"
  atteso("segnatura: Modena", grepl("b. 3, fasc. 7", riga_da_fascicolo(f)[["Segnatura"]], fixed = TRUE), TRUE)
  f$archivio <- "Archivio sconosciuto"
  atteso("segnatura: archivio ignoto con la serie", grepl("Processi", riga_da_fascicolo(f)[["Segnatura"]], fixed = TRUE), TRUE)
})
atteso("maschera: gli extra sono quelli del modello, più il campo che cambia la tipologia nel foglio e le due date intere", setdiff(setdiff(profilo_completo("Generale", character())$arianna[profilo_completo("Generale", character())$dove == "extra"], c(EXTRA_TIPOLOGIA_FOGLIO, EXTRA_DATA_PRECISA)), PROFILO_MODELLO$arianna[PROFILO_MODELLO$dove == "extra"]), character(0))

# Un foglio con sole intestazioni (il modello vuoto) non si importa, e «sostituisci» non cancella nulla.
local({
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn))
  dbExecute(cn, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, creato, modificato) VALUES ('v1', 'A', 'F', 1, 1, 'x', 'x')")
  a <- leggi_arianna("dati/modello_export.xlsx")
  atteso("modello vuoto: l'anteprima non va in errore", nrow(correzioni_import(a)), 0L)
  e <- tryCatch({ importa_arianna(cn, a, modo = "sostituisci"); "" }, error = function(e) conditionMessage(e))
  atteso("modello vuoto: l'import si rifiuta", grepl("nessuna riga", e), TRUE)
  atteso("modello vuoto: «sostituisci» non ha cancellato niente", dbGetQuery(cn, "SELECT COUNT(*) n FROM fascicoli")$n, 1L)
})

# «n.d.» nell'estremo remoto: data non disponibile, e torna «n.d.» nel foglio.
local({
  riga <- data.frame(Segnatura = "Archivio Storico Diocesano di Napoli, Sant'Ufficio, b.1, f.1", `Estremo remoto` = "n.d.", Titolo = "Atti circa X", check.names = FALSE, stringsAsFactors = FALSE)
  f <- fascicolo_da_riga(riga, PROFILO_MODELLO, as.list(setNames(names(riga), names(riga))))
  atteso("n.d. nell'anno: data_nd acceso", c(f$data_nd, is.na(f$estremo_remoto)), c(1L, TRUE))
  atteso("n.d. nell'anno: torna «n.d.» nell'export", riga_da_fascicolo(f)[["Estremo remoto"]], "n.d.")
  f$estremo_remoto <- 1560L
  atteso("n.d. nell'anno: con un anno vero esce l'anno", riga_da_fascicolo(f)[["Estremo remoto"]], "1560")
  riga$`Estremo remoto` <- "1561"
  atteso("anno normale: data_nd resta spento", fascicolo_da_riga(riga, PROFILO_MODELLO, as.list(setNames(names(riga), names(riga))))$data_nd, 0L)
})

# L'export è di un fondo solo: con due fondi e nessuna scelta non esce niente.
local({
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn))
  for (i in 1:3) dbExecute(cn, "INSERT INTO fascicoli (id, archivio, fondo, busta, fascicolo, creato, modificato) VALUES (?, 'A', ?, 1, ?, 'x', 'x')", params = list(paste0("e", i), if (i < 3) "A" else "B", i))
  atteso("export: due fondi senza scelta = errore", inherits(try(tabella_arianna(cn), silent = TRUE), "try-error"), TRUE)
  atteso("export: il fondo A ha solo le sue due schede", nrow(tabella_arianna(cn, fondo = "A")), 2L)
  atteso("export: il fondo B ha solo la sua", nrow(tabella_arianna(cn, fondo = "B")), 1L)
})

# --- Home: il fondo in uso (15/10) ------------------------------------
local({
  suppressMessages(source("R/home.R"))
  atteso("home: indice del fondo in uso", c(indice_fondo_attivo("B", c("A", "B")), indice_fondo_attivo(NULL, c("A", "B")), indice_fondo_attivo("", c("A", "B")),
                                              indice_fondo_attivo("Z", c("A", "B")), indice_fondo_attivo(NA_character_, "A"), indice_fondo_attivo("A", character(0))),
         c(2L, NA, NA, NA, NA, NA))
})

# --- Qualità ordinata (5/10) --------------------
local({
  q <- data.frame(controllo = c("Anno assente", "Tortura non esaminata", "Senza tipo di reato", "Regesto assente",
                                "Sentenza non esaminata", "Anno dichiarato n.d."),
                  casi = c(0L, 367L, 4L, 0L, 367L, 9L), nota = "", stringsAsFactors = FALSE)
  # Dal 15/10 gli zeri non stanno più nella tabella: `a_posto = TRUE` li dà (la riga chiusa «A posto: N controlli»).
  o <- rbind(qualita_ordinata(q), qualita_ordinata(q, a_posto = TRUE))
  atteso("qualità: prima i casi veri dal più numeroso, poi il lavoro di fondo, poi gli zeri", o$controllo,
         c("Anno dichiarato n.d.", "Senza tipo di reato", "Tortura non esaminata", "Sentenza non esaminata",
           "Anno assente", "Regesto assente"))
  atteso("qualità: i gruppi", o$gruppo, rep(c("Da vedere", "Lavoro di fondo, non anomalie", "A posto"), each = 2))
  atteso("qualità: la tabella non ha gli zeri", nrow(qualita_ordinata(q)), 4L)
  q$casi[2] <- 0L
  atteso("qualità: tortura a zero è «a posto», non lavoro di fondo",
         qualita_ordinata(q, a_posto = TRUE)$gruppo[qualita_ordinata(q, a_posto = TRUE)$controllo == "Tortura non esaminata"], "A posto")
})

# --- storia della scheda (5/10) -----------------------------------
local({
  cn <- apri_db(":memory:"); on.exit(dbDisconnect(cn))
  f <- fascicolo_vuoto(); f$id <- "x1"; f$fondo <- "F"; f$archivio <- "A"; f$busta <- 1L; f$fascicolo <- 1L
  f$persone <- c("Rossi, Mario", "Bianchi, Anna"); f$reati <- "Eresia"; f$estremo_remoto <- 1560L; f$luoghi <- "Napoli"
  salva_fascicolo(cn, f, storia = TRUE)
  atteso("storia: una scheda nuova non ha storia", nrow(storia_fascicolo(cn, "x1")), 0L)
  a <- leggi_fascicolo(cn, "x1")
  salva_fascicolo(cn, a, storia = TRUE)
  atteso("storia: un salvataggio che non cambia nulla non lascia voci", nrow(storia_fascicolo(cn, "x1")), 0L)
  g <- a; g$persone <- "Rossi, Mario"; g$note <- "prova"
  salva_fascicolo(cn, g)
  atteso("storia: senza storia = TRUE (import, blocchi) non si scrive", nrow(storia_fascicolo(cn, "x1")), 0L)
  salva_fascicolo(cn, a); salva_fascicolo(cn, g, storia = TRUE)
  atteso("storia: i campi cambiati, col nome a schermo",
         all(c("Note", "Persone citate") %in% strsplit(storia_fascicolo(cn, "x1")$campi, " \u00b7 ", fixed = TRUE)[[1]]), TRUE)
  h <- leggi_fascicolo(cn, "x1"); h$reati <- c("Eresia", "Magia"); salva_fascicolo(cn, h, storia = TRUE)
  atteso("storia: due modifiche, due voci, la più recente per prima",
         grepl("Soggettario", storia_fascicolo(cn, "x1")$campi), c(TRUE, FALSE))
  atteso("annulla: dice che cosa torna indietro", "Soggettario" %in% annulla_ultima_modifica(cn, "x1"), TRUE)
  atteso("annulla: il reato aggiunto non c'è più", leggi_fascicolo(cn, "x1")$reati, "Eresia")
  annulla_ultima_modifica(cn, "x1")
  b <- leggi_fascicolo(cn, "x1"); b$modificato <- a$modificato
  atteso("annulla due volte: la scheda è quella di partenza, in ogni campo", b, a)
  atteso("annulla senza storia: NULL, e nulla cambia", is.null(annulla_ultima_modifica(cn, "x1")), TRUE)
  for (i in 1:25) { z <- leggi_fascicolo(cn, "x1"); z$note <- paste("n", i); salva_fascicolo(cn, z, storia = TRUE) }
  atteso("storia: si tengono le ultime 20", nrow(storia_fascicolo(cn, "x1")), 20L)
  elimina_fascicoli(cn, "x1")
  atteso("storia: sparisce con la scheda", dbGetQuery(cn, "SELECT COUNT(*) n FROM storia_fascicoli")$n, 0L)
  atteso("campi cambiati: da assente a vuoto non è una modifica",
         campi_cambiati(list(serie = NA_character_, note = NA_character_, busta = 1L), list(serie = "", note = "x", busta = 1L)), "Note")
  atteso("campi cambiati: i flag hanno una voce sola", campi_cambiati(list(titolo_manuale = 0L, data_nd = 0L, modificato = "a"),
                                                                       list(titolo_manuale = 1L, data_nd = 1L, modificato = "b")), "Opzioni della scheda")
})

# --- Indietro: dove porta un posto della cronologia (5/10) ---------
local({
  source("R/cronologia.R")
  c1 <- function(id) id %in% c("a1", "a2")
  atteso("posto: una scheda qualsiasi", posto_da_richiesta(list(nav = "Luoghi citati", id = "a1"), c1), list(nav = "Luoghi citati", id = ""))
  atteso("posto: il fascicolo c'è", posto_da_richiesta(list(nav = "Scheda", id = "a1"), c1), list(nav = "Scheda", id = "a1"))
  atteso("posto: il fascicolo non c'è più (o è di un altro fondo): all'elenco", posto_da_richiesta(list(nav = "Scheda", id = "zz"), c1), list(nav = "Fascicoli", id = ""))
  atteso("posto: scheda nuova senza id", posto_da_richiesta(list(nav = "Scheda", id = ""), c1), list(nav = "Scheda", id = ""))
  atteso("posto: una scheda che non esiste si ignora", posto_da_richiesta(list(nav = "Inventata", id = ""), c1), NULL)
  atteso("posto: richiesta vuota si ignora", posto_da_richiesta(list(), c1), NULL)
  atteso("posto: l'id con le virgolette non rompe nulla", posto_da_richiesta(list(nav = "Scheda", id = "a'1\"; DROP"), c1), list(nav = "Fascicoli", id = ""))
  v <- list(Fascicoli = list(q = "eresia", inc = TRUE, busta = "", vista = "tabella", filtro = FALSE))
  atteso("posto: i filtri di quel passo passano com'sono", posto_da_richiesta(list(nav = "Fascicoli", id = "", v = v), c1)$v$Fascicoli$q, "eresia")
  atteso("posto: senza filtri nessun «v»", names(posto_da_richiesta(list(nav = "Fascicoli", id = ""), c1)), c("nav", "id"))
  atteso("posto: un fascicolo che non c'è porta comunque i filtri all'elenco", posto_da_richiesta(list(nav = "Scheda", id = "zz", v = v), c1)$nav, "Fascicoli")
  atteso("posto: tutte le schede della barra sono valide", all(c("Home", "Fascicoli", "Scheda", "Luoghi citati", "Import / Export", "Qualit\u00e0", "Gestione") %in% POSTI_VALIDI), TRUE)
})

# --- «aa.» al posto di «b.» nella segnatura (Bologna, 5/10) --------
local({
  bo <- "Archivio Arcivescovile di Bologna, Sant'Officio, Cause e Abiure, aa. 164, fasc. 1"
  x <- analizza_segnatura(bo)
  atteso("segnatura Bologna: archivio, fondo, serie", c(x$archivio, x$fondo, x$serie), c("Archivio Arcivescovile di Bologna", "Sant'Officio", "Cause e Abiure"))
  atteso("segnatura Bologna: «aa.» e il numero", c(x$sigla_busta, x$busta, x$fascicolo), c("aa.", "164", "1"))
  atteso("segnatura con «b.»: nessuna sigla propria", is.na(analizza_segnatura("Archivio X, Fondo, b. 5, fasc. 2")$sigla_busta), TRUE)
  atteso("segnatura con «b. 5» resta busta 5", analizza_segnatura("Archivio X, Fondo, b. 5, fasc. 2")$busta, 5L)
  atteso("«Armadio 3» (senza punto) non è una sigla", analizza_segnatura("Archivio X, Armadio 3, b. 5, fasc. 2")$fondo, "Armadio 3")
  atteso("«cart. 12» è una sigla", analizza_segnatura("Archivio X, Fondo, cart. 12, fasc. 1")$sigla_busta, "cart.")
  atteso("sigla: vuota = b.", c(sigla_busta(NULL), sigla_busta(NA), sigla_busta(""), sigla_busta("AA"), sigla_busta("aa.")), c("b.", "b.", "b.", "aa.", "aa."))
  atteso("componi con «aa.»", componi_segnatura("A", "F", "S", 164L, 1L, "", "aa."), "A, F, S, aa. 164, fasc. 1")
  atteso("componi senza sigla: come prima", componi_segnatura("A", "F", "S", 3L, 1L), "A, F, S, b. 3, fasc. 1")
  atteso("sigle_busta su un vettore", sigle_busta(c("aa.", NA, "")), c("aa.", "b.", "b."))

  cn <- apri_db(":memory:")
  f <- fascicolo_vuoto(); f$id <- "bo1"; f$archivio <- "Archivio Arcivescovile di Bologna"; f$fondo <- "Sant'Officio"; f$serie <- "Cause e Abiure"
  f$busta <- 164L; f$fascicolo <- 1L; f$sigla_busta <- "AA"; f$estremo_remoto <- 1645L; f$reati <- "Eresia"
  salva_fascicolo(cn, f)
  atteso("la sigla si salva in minuscolo e col punto", leggi_fascicolo(cn, "bo1")$sigla_busta, "aa.")
  g <- f; g$id <- "nap1"; g$busta <- 1L; g$fascicolo <- 2L; g$sigla_busta <- "b."; g$fondo <- "Altro"
  salva_fascicolo(cn, g)
  atteso("«b.» non si scrive nel database", is.na(leggi_fascicolo(cn, "nap1")$sigla_busta), TRUE)
  atteso("elenco_fascicoli porta la sigla", dbGetQuery(cn, "SELECT sigla_busta FROM fascicoli WHERE id = 'bo1'")$sigla_busta, "aa.")
  atteso("export: «aa. 164»", grepl(", aa. 164, fasc. 1$", tabella_arianna(cn, fondo = "Sant'Officio")$Segnatura), TRUE)
  # il giro del foglio: la sigla torna uguale
  file <- tempfile(fileext = ".xlsx"); esporta_arianna(cn, file, fondo = "Sant'Officio")
  c2 <- apri_db(":memory:"); importa_arianna(c2, leggi_arianna(file))
  atteso("giro xlsx: sigla e numero tornano", unlist(dbGetQuery(c2, "SELECT sigla_busta, busta FROM fascicoli")), c(sigla_busta = "aa.", busta = "164"))
  atteso("giro xlsx: la segnatura è uguale", tabella_arianna(c2, fondo = "Sant'Officio")$Segnatura, tabella_arianna(cn, fondo = "Sant'Officio")$Segnatura)
  # un database fatto prima della colonna la riceve da solo
  # un database fatto prima della colonna: lo schema vecchio meno sigla_busta, poi apri_db
  vecchio <- tempfile(fileext = ".sqlite"); cv <- apri_db(vecchio)
  DBI::dbExecute(cv, "DROP INDEX IF EXISTS idx_fasc_segnatura_unica")
  DBI::dbExecute(cv, "ALTER TABLE fascicoli DROP COLUMN sigla_busta"); DBI::dbDisconnect(cv)
  cm <- apri_db(vecchio)
  atteso("migrazione: la colonna sigla_busta torna da sola", "sigla_busta" %in% DBI::dbListFields(cm, "fascicoli"), TRUE)
  DBI::dbDisconnect(cm)
  for (k in list(cn, c2)) DBI::dbDisconnect(k)
})

# --- normalizzazione all'import (Bologna, 5/10): lingue, cognomi, colonne ----
local({
  atteso("lingue: «|» (Bologna)", .lingue_import("italiano|latino|slavo"), c("Italiano", "Latino", "Slavo"))
  atteso("lingue: «; » (modello)", .lingue_import("italiano; latino"), c("Italiano", "Latino"))
  atteso("lingue: virgola", .lingue_import("italiano, latino"), c("Italiano", "Latino"))
  atteso("lingue: maiuscole e doppioni", .lingue_import("ITALIANO; italiano; Latino"), c("Italiano", "Latino"))
  atteso("lingue: l'export le riscrive minuscole con «; »", .lingue_export(c("Italiano", "Latino", "Slavo")), "italiano; latino; slavo")
  atteso("cognomi: particella maiuscola in testa -> minuscola", .nome_persona_import("Di Taibi, Matteo"), "di Taibi, Matteo")
  atteso("cognomi: «Del Gesù»", .nome_persona_import("Del Gesù, Giovanni"), "del Gesù, Giovanni")
  atteso("cognomi: la forma dell'export si disfa", .nome_persona_import("Taibi di, Matteo"), "di Taibi, Matteo")
  atteso("cognomi: «da Valle» resta", .nome_persona_import("da Valle, Giacoma"), "da Valle, Giacoma")
  atteso("cognomi: con l'apostrofo non si tocca", .nome_persona_import("De'Bertolati, Diamante"), "De'Bertolati, Diamante")
  atteso("cognomi: senza particella non cambia", .nome_persona_import("Rossi, Mario"), "Rossi, Mario")
  atteso("cognomi: l'export mette la particella dopo il nome (guida al Tab, 14/10)", .nome_persona_export("di Taibi, Matteo"), "Taibi, Matteo di")
  atteso("cognomi: l'import legge anche la forma «Taibi, Matteo di»", .nome_persona_import("Taibi, Matteo di"), "di Taibi, Matteo")
  atteso("cognomi: con una qualifica in parentesi la particella sta prima della parentesi", .nome_persona_export("de Secondat, Charles-Louis (barone di Montesquieu)"), "Secondat, Charles-Louis de (barone di Montesquieu)")
  atteso("cognomi: e torna com'era", .nome_persona_import("Secondat, Charles-Louis de (barone di Montesquieu)"), "de Secondat, Charles-Louis (barone di Montesquieu)")
  atteso("cognomi: la guida con l'apostrofo non si tocca", .nome_persona_import("Este, Ercole II d’ (duca di Ferrara)"), "Este, Ercole II d’ (duca di Ferrara)")
  atteso("cognomi: un cognome con le varianti fra parentesi e la virgola dentro", .nome_persona_export("de Giordano (de Luca), Isabella"), "Giordano (de Luca), Isabella de")
  atteso("produttore: «Inquisizione di Roma» è «Sant'Uffizio» (guida al Tab, colonna U)", .produttore_canonico("Tribunale dell’Inquisizione di Roma"), "Sant'Uffizio")      # apostrofo dritto: D2, D13 (20/10)
  atteso("produttore: gli altri tribunali restano «Inquisizione di <Città>»", .produttore_canonico(c("Tribunale dell’Inquisizione di Udine", "Inquisizione di Modena")), c("Inquisizione di Udine", "Inquisizione di Modena"))
  atteso("cognomi: giro completo di dieci nomi veri", all(vapply(c("de Santis, Antonio", "da Tunisi, Caterina", "de la Ascia, Giovanni", "di Taibi, Matteo", "Rossi, Mario", "d'Africa, Ascia", "de' Rossi, Mario", "Cecra (Cedra, Cela?), Andrea", "de Giordano (de Luca), Isabella", "della Valle, Pietro"), function(n) identical(.nome_persona_import(.nome_persona_export(n)), n), TRUE)), TRUE)
  atteso("nomi con parentesi: «Marco d'Aviano (Cristofori, Carlo Domenico)» non si storpia", .estendi_nome("Marco d'Aviano (Cristofori, Carlo Domenico)"), "Marco d'Aviano (Cristofori, Carlo Domenico)")
  atteso("nomi con parentesi: indicizza lascia le formule di papi e sovrani", indicizza_nome("Louis XIV (Luigi XIV; re di Francia)"), "Louis XIV (Luigi XIV; re di Francia)")
  atteso("nomi con parentesi: la qualifica finale va in coda al nome esteso", .estendi_nome("Este, Ercole II d’ (duca di Ferrara)"), "Ercole II d’ Este (duca di Ferrara)")
  atteso("nomi con parentesi: le varianti del cognome con la virgola dentro", .estendi_nome("Cecra (Cedra, Cela?), Andrea"), "Andrea Cecra (Cedra, Cela?)")
  atteso("persone con la formula della guida non sono «senza cognome»", senza_cognome(c("Pius VII (Pio VII; Chiaramonti, Barnaba; papa)", "Marco d'Aviano (Cristofori, Carlo Domenico)", "Louis XIV (Luigi XIV; re di Francia)")), c(FALSE, FALSE, FALSE))
  # la colonna delle immagini di Bologna si riconosce
  modello <- names(openxlsx::read.xlsx("dati/modello_export.xlsx", sep.names = " "))
  riga <- stats::setNames(as.list(rep(NA_character_, length(modello))), modello)
  riga[["Segnatura"]] <- "Archivio X, Fondo, aa. 1, fasc. 1"; riga[["Titolo"]] <- "Processo contro Mario Rossi"; riga[["Estremo remoto"]] <- "1600"
  riga[["Persone citate o indicizzate"]] <- "Di Taibi, Matteo|Mascarino, Antonio|"; riga[["Lingua"]] <- "italiano|latino"
  riga[["Identificativo immagine o cartella"]] <- "AGABo_164 2-151"
  nm <- names(riga); nm[nm == "Identificativo immagine o cartella"] <- "INDICAZIONE IMMAGINI jpeg tiff"
  foglio <- as.data.frame(stats::setNames(riga, nm), check.names = FALSE, stringsAsFactors = FALSE)
  file <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(foglio, file)
  cn <- apri_db(":memory:"); a <- leggi_arianna(file); e <- importa_arianna(cn, a)
  atteso("Bologna: la colonna delle immagini non è fuori modello", "INDICAZIONE IMMAGINI jpeg tiff" %in% a$fuori_modello$colonna, FALSE)
  atteso("Bologna: importata", e$importati, 1L)
  g <- leggi_fascicolo(cn, dbGetQuery(cn, "SELECT id FROM fascicoli")$id)
  atteso("Bologna: le immagini nel database", unlist(g$extra[["Identificativo immagine o cartella"]]), "AGABo_164 2-151")
  atteso("Bologna: persone con la particella minuscola e senza il vuoto finale", sort(g$persone), c("di Taibi, Matteo", "Mascarino, Antonio"))
  atteso("Bologna: lingue", sort(g$lingue), c("Italiano", "Latino"))
  t <- tabella_arianna(cn, fondo = "Fondo")
  atteso("Bologna: export, immagini nella colonna del modello", t[["Identificativo immagine o cartella"]], "AGABo_164 2-151")
  atteso("Bologna: export, lingue «; » minuscole", t[["Lingua"]], "italiano; latino")
  atteso("Bologna: export, particella dopo il nome", grepl("Taibi, Matteo di", t[["Persone citate o indicizzate"]], fixed = TRUE), TRUE)
  atteso("Bologna: la tipologia del foglio è «Atti»", t[["Tipologia documentaria"]], "Atti")
  DBI::dbDisconnect(cn)
})

# --- le eccezioni alla convenzione della segnatura (5/10) ----------
local({
  r <- analizza_segnatura("Archivio X, Fondo, reg. 4, n. 12")
  atteso("eccezione: «reg. 4, n. 12» (busta e fascicolo con sigle proprie)", c(r$busta, r$sigla_busta, r$fascicolo, r$sigla_fascicolo), c("4", "reg.", "12", "n."))
  r <- analizza_segnatura("Archivio X, Fondo, b. 3, n. 12")
  atteso("eccezione: busta «b.» e fascicolo «n.»", c(r$busta, is.na(r$sigla_busta), r$fascicolo, r$sigla_fascicolo), c("3", "TRUE", "12", "n."))
  r <- analizza_segnatura("Archivio X, Fondo, fasc. 2, aa. 164")
  atteso("eccezione: l'ordine dei pezzi non conta, contano le etichette", c(r$busta, r$sigla_busta, r$fascicolo), c("164", "aa.", "2"))
  atteso("eccezione: «fasc.» e «b.» di sempre non lasciano sigle", is.na(analizza_segnatura("A, F, b. 3, fasc. 2")$sigla_fascicolo), TRUE)
  atteso("non letto: «b. 12A» resta fuori, non diventa serie",
         c(.scomponi_segnatura("A, F, b. 12A, fasc. 2")$non_letto, is.na(analizza_segnatura("A, F, b. 12A, fasc. 2")$serie)), c("b. 12A", "TRUE"))
  atteso("non letto: «aa. IV» (cifre romane) resta fuori", .scomponi_segnatura("A, F, S, aa. IV, fasc. 2")$non_letto, "aa. IV")
  atteso("non letto: un pezzo in più", .scomponi_segnatura("A, F, S, X, b. 1, fasc. 1")$non_letto, "X")
  atteso("non letto: una terza unità", .scomponi_segnatura("A, F, b. 1, fasc. 1, cart. 7")$non_letto, "cart. 7")
  atteso("«Armadio 3» è un nome, non un numero non letto", length(.scomponi_segnatura("A, Armadio 3, b. 5, fasc. 2")$non_letto), 0L)
  g <- segnature_da_guardare(c("A, F, S, aa. 164, fasc. 1", "A, F, S, b. 12A, fasc. 2", "A, F, S, b. 3", NA, "A, F, b. 1, fasc. 1, cart. 7"))
  atteso("anteprima: quali segnature segnalare (la sola busta è giusta)", g$riga, c(2L, 5L))
  atteso("anteprima: i motivi", g$motivo, c("non leggo: «b. 12A»", "non leggo: «cart. 7»"))
  # Il rifiuto riguarda solo un numero che prenderebbe il posto di busta o fascicolo e non so leggere (20/10, critique §4):
  # una terza unità («cc. 15-20», «n. 7», «vol. II») con busta e fascicolo già letti è non letto, non respinta, e si dice intera.
  atteso("rifiuto: «cc. 15-20» come terza unità non respinge e non si tronca",
         c(segnatura_non_ammessa("A, F, b. 3, fasc. 2, cc. 15-20"), .scomponi_segnatura("A, F, b. 3, fasc. 2, cc. 15-20")$non_letto), c("", "cc. 15-20"))
  atteso("rifiuto: «n. 7» e «vol. II» dopo busta e fascicolo non respingono",
         c(segnatura_non_ammessa("A, F, b. 3, fasc. 2, n. 7"), segnatura_non_ammessa("A, F, b. 3, fasc. 2, vol. II")), c("", ""))
  atteso("rifiuto: «b. 12A» e «aa. IV» al posto della busta respingono ancora",
         nzchar(c(segnatura_non_ammessa("A, F, b. 12A, fasc. 2"), segnatura_non_ammessa("A, F, S, aa. IV, fasc. 2"))), c(TRUE, TRUE))
  atteso("rifiuto: il numero illeggibile con la busta libera ma il fascicolo letto respinge", nzchar(segnatura_non_ammessa("A, F, vol. II, fasc. 2")), TRUE)
  atteso("posizione scritta: di sempre", posizione_scritta(NA, 3L, NA, 1L), "b. 3, fasc. 1")
  atteso("posizione scritta: suffissi NULL (scheda nuova), NA e vuoti non scrivono «NA»", c(posizione_scritta("b.", 1L, "fasc.", 25L, NULL, NULL), posizione_scritta("b.", 1L, "fasc.", 25L, NA, NA), posizione_scritta("b.", 1L, "fasc.", 25L, "", "")), rep("b. 1, fasc. 25", 3))
  atteso("Riapri: l'etichetta ha i suffissi di busta e fascicolo", c(etichetta_riapri(list(sigla_busta = NA, sigla_fascicolo = NA, busta = 232L, fascicolo = 5L, busta_suffisso = "-bis", fascicolo_suffisso = "-ter")),
    etichetta_riapri(list(sigla_busta = NA, sigla_fascicolo = NA, busta = 3L, fascicolo = 1L, busta_suffisso = NA, fascicolo_suffisso = NULL)),
    etichetta_riapri(list(sigla_busta = "aa.", sigla_fascicolo = NA, busta = 164L, fascicolo = 1L, busta_suffisso = "-bis", fascicolo_suffisso = ""))),
    c("Riapri b. 232-bis f. 5-ter", "Riapri b. 3 f. 1", "Riapri aa. 164-bis, fasc. 1"))
  # 16/10: i campi dei luoghi (Luoghi citati, Data topica, Luogo di conservazione) portano la regola «prima lettera maiuscola»; gli altri no
  .html_campo <- function(campo) as.character(campo_input(function(x) x, PROFILO_MODELLO[which(PROFILO_MODELLO$campo == campo)[1], ], valore = NULL))
  atteso("maiuscola iniziale: Luoghi citati, Data topica e Luogo di conservazione", c(grepl("maiuscolaIniziale", .html_campo("luogo")), grepl("maiuscolaIniziale", .html_campo("luogo_produzione")), grepl("maiuscolaIniziale", .html_campo("luogo_conservazione"))), c(TRUE, TRUE, TRUE))
  atteso("maiuscola iniziale: Persone e Lingua no", c(grepl("maiuscolaIniziale", .html_campo("persona")), grepl("maiuscolaIniziale", .html_campo("lingua"))), c(FALSE, FALSE))
  atteso("posizione scritta: i suffissi veri restano", posizione_scritta("b.", 232L, "fasc.", 5L, "-bis", "-ter"), "b. 232-ter, fasc. 5-bis")
  atteso("posizione scritta: «aa.»", posizione_scritta("aa.", 164L, NA, 1L), "aa. 164, fasc. 1")
  atteso("posizione scritta: «reg.» e «n.»", posizione_scritta("reg.", 4L, "n.", 12L, "-bis"), "reg. 4, n. 12-bis")
  atteso("posizione scritta: la sola busta (manoscritti, «Ms., b. 1860»)", posizione_scritta(NA, 1860L, NA, NA), "b. 1860")
  atteso("posizione scritta: il solo fascicolo, e niente", c(posizione_scritta(NA, NA, NA, 3L), posizione_scritta(NA, NA, NA, NA)), c("fasc. 3", "?"))
  atteso("segnatura di soli manoscritti: nessun avviso", nrow(segnature_da_guardare("Archiginnasio di Bologna, Ms., b. 1860")), 0L)
  atteso("segnatura senza busta né fascicolo: avviso", segnature_da_guardare("Archiginnasio di Bologna, Ms.")$motivo, "senza numero di busta né di fascicolo")
  # Salvataggio dalla Scheda: busta e fascicolo obbligatori solo per una scheda nuova (critica del 31/10/2026: una vecchia scheda senza numero non si poteva correggere).
  atteso("collocazione: scheda nuova senza fascicolo, rifiutata", !is.null(controllo_collocazione(list(busta = 1L, fascicolo = NA), nuova = TRUE)$errore), TRUE)
  atteso("collocazione: scheda nuova senza busta, rifiutata", !is.null(controllo_collocazione(list(busta = NA, fascicolo = 2L), nuova = TRUE)$errore), TRUE)
  atteso("collocazione: scheda esistente senza fascicolo, si salva con avviso", {
    r <- controllo_collocazione(list(busta = 1L, fascicolo = NA), nuova = FALSE); c(is.null(r$errore), length(r$avvisi), grepl("numero di fascicolo", r$avvisi)) }, c(TRUE, 1L, TRUE))
  atteso("collocazione: scheda esistente senza busta né fascicolo, due avvisi", length(controllo_collocazione(list(busta = NA, fascicolo = NA), nuova = FALSE)$avvisi), 2L)
  atteso("collocazione: scheda completa, nessun avviso", {
    r <- controllo_collocazione(list(busta = 1L, fascicolo = 2L), nuova = FALSE); c(is.null(r$errore), length(r$avvisi)) }, c(TRUE, 0L))
  atteso("posizione scritta: vettore", posizione_scritta(c(NA, "aa."), c(1L, 2L), NA, c(5L, 6L)), c("b. 1, fasc. 5", "aa. 2, fasc. 6"))
  atteso("componi con le due sigle", componi_segnatura("A", "F", "S", 4L, 12L, "", "reg.", "n."), "A, F, S, reg. 4, n. 12")

  cn <- apri_db(":memory:")
  f <- fascicolo_vuoto(); f$id <- "ec1"; f$archivio <- "Archivio X"; f$fondo <- "Fondo"; f$serie <- NA_character_
  f$busta <- 4L; f$fascicolo <- 12L; f$sigla_busta <- "REG"; f$sigla_fascicolo <- "n"; f$estremo_remoto <- 1600L
  salva_fascicolo(cn, f)
  g <- leggi_fascicolo(cn, "ec1")
  atteso("le due sigle si salvano normalizzate", c(g$sigla_busta, g$sigla_fascicolo), c("reg.", "n."))
  atteso("export con le due sigle", tabella_arianna(cn, fondo = "Fondo")$Segnatura, "Archivio X, Fondo, reg. 4, n. 12")
  file <- tempfile(fileext = ".xlsx"); esporta_arianna(cn, file, fondo = "Fondo")
  a <- leggi_arianna(file)
  atteso("anteprima: dice le due eccezioni", sum(grepl("si numera", correzioni_import(a)$regola)), 2L)
  c2 <- apri_db(":memory:"); importa_arianna(c2, a)
  atteso("giro xlsx: le due sigle tornano", unlist(dbGetQuery(c2, "SELECT sigla_busta, sigla_fascicolo FROM fascicoli")), c(sigla_busta = "reg.", sigla_fascicolo = "n."))
  for (k in list(cn, c2)) DBI::dbDisconnect(k)
})

# --- la busta con suffisso: «b. 232-bis», «b. 5-ter», «b. 7-quater» (ASMo, 8/10: 90 schede che l'import non leggeva) ---
local({
  p <- function(t) .scomponi_segnatura(t)
  r <- p("Archivio di Stato di Modena, Tribunale dell'Inquisizione di Modena, Processi, b. 232-bis, fasc. 41")
  atteso("busta -bis: numero, suffisso e fascicolo", c(r$busta, r$busta_suffisso, r$fascicolo, length(r$non_letto)), c("232", "-bis", "41", "0"))
  atteso("busta: -ter, -quater, «5 ter» (→ «-ter», 30/10/2026) e «3-2»", c(p("A, B, b. 5-ter, fasc. 1")$busta_suffisso, p("A, B, b. 7-quater, fasc. 1")$busta_suffisso,
                                                     p("A, B, b. 5 ter, fasc. 1")$busta_suffisso, p("A, B, b. 3-2, fasc. 1")$busta_suffisso), c("-ter", "-quater", "-ter", "-2"))
  atteso("busta con un'altra sigla e il suffisso: «aa. 164-bis»", {x <- p("A, B, aa. 164-bis, fasc. 1"); c(x$sigla_busta, x$busta, x$busta_suffisso)}, c("aa.", "164", "-bis"))
  atteso("senza suffisso la busta resta com'era", c(p("A, B, b.1, f.1")$busta, p("A, B, b.1, f.1")$busta_suffisso), c("1", ""))
  atteso("una lettera nel numero («b. 12A») resta fuori e si dice", {x <- p("A, B, b. 12A, fasc. 1"); c(is.na(x$busta), x$non_letto)}, c("TRUE", "b. 12A"))
  atteso("segnature_da_guardare: «b. 232-bis» non è più un problema", nrow(segnature_da_guardare("A, B, b. 232-bis, fasc. 1")), 0L)
  atteso("posizione scritta con la busta suffissata", posizione_scritta(NA, c(232L, 232L), NA, c(1L, 1L), c("", ""), c("", "-bis")), c("b. 232, fasc. 1", "b. 232-bis, fasc. 1"))
  atteso("segnatura composta con la busta suffissata", componi_segnatura("A", "B", "S", 232L, 1L, NULL, NULL, NULL, "-bis"), "A, B, S, b. 232-bis, fasc. 1")
  atteso("busta e fascicolo suffissati insieme", componi_segnatura("A", "B", NULL, 5L, 2L, "-bis", NULL, NULL, "-ter"), "A, B, b. 5-ter, fasc. 2-bis")

  cn <- apri_db(":memory:")
  base <- fascicolo_vuoto(); base$archivio <- "Archivio Prova"; base$fondo <- "Fondo Prova"; base$serie <- "Processi"; base$fascicolo <- 1L; base$estremo_remoto <- 1600L; base$reati <- "Eresia"
  a1 <- base; a1$id <- "a1"; a1$busta <- 232L
  a2 <- base; a2$id <- "a2"; a2$busta <- 232L; a2$busta_suffisso <- "-bis"
  a3 <- base; a3$id <- "a3"; a3$busta <- 232L; a3$busta_suffisso <- "-ter"
  for (f in list(a1, a2, a3)) salva_fascicolo(cn, f)
  atteso("b. 232, 232-bis e 232-ter con lo stesso fascicolo convivono", dbGetQuery(cn, "SELECT COUNT(*) AS n FROM fascicoli")$n, 3L)
  atteso("il suffisso della busta si salva e si rilegge", c(leggi_fascicolo(cn, "a2")$busta_suffisso, leggi_fascicolo(cn, "a1")$busta_suffisso), c("-bis", ""))
  doppio <- a2; doppio$id <- "a4"
  atteso("la stessa segnatura con suffisso non entra due volte", inherits(try(salva_fascicolo(cn, doppio), silent = TRUE), "try-error"), TRUE)
  e <- elenco_fascicoli(cn, "Fondo Prova")
  atteso("l'elenco porta il suffisso e ordina 232, 232-bis, 232-ter", e$suffisso_b, c("", "-bis", "-ter"))
  atteso("tabella_elenco scrive la posizione intera", grepl("b. 232-bis, fasc. 1", tabella_elenco(e)[[ncol(tabella_elenco(e)) - 0L]][2], fixed = TRUE) ||
         any(grepl("b. 232-bis", unlist(tabella_elenco(e)), fixed = TRUE)), TRUE)
  atteso("export: «b. 232-bis» nella segnatura", sort(tabella_arianna(cn, fondo = "Fondo Prova")$Segnatura, method = "radix"),
         c("Archivio Prova, Fondo Prova, Processi, b. 232, fasc. 1", "Archivio Prova, Fondo Prova, Processi, b. 232-bis, fasc. 1", "Archivio Prova, Fondo Prova, Processi, b. 232-ter, fasc. 1"))
  file <- tempfile(fileext = ".xlsx"); esporta_arianna(cn, file, fondo = "Fondo Prova")
  c2 <- apri_db(":memory:"); ris <- importa_arianna(c2, leggi_arianna(file))
  atteso("giro del foglio: le tre schede tornano, nessuna respinta", c(ris$importati, nrow(ris$respinti)), c(3L, 0L))
  atteso("giro del foglio: i suffissi tornano uguali", dbGetQuery(c2, "SELECT busta, busta_suffisso FROM fascicoli ORDER BY busta_suffisso")$busta_suffisso, c("", "-bis", "-ter"))
  f1 <- tempfile(fileext = ".xlsx"); esporta_arianna(c2, f1, fondo = "Fondo Prova")
  atteso("secondo giro identico al primo", identical(as.matrix(leggi_tabella(file)), as.matrix(leggi_tabella(f1))), TRUE)
  vecchio <- apri_db(":memory:")
  atteso("backup JSON: il suffisso della busta viaggia", {
    j <- tempfile(fileext = ".json"); esporta_backup_json(cn, j); ripristina_backup_json(vecchio, j, modo = "accoda")
    sort(dbGetQuery(vecchio, "SELECT busta_suffisso FROM fascicoli")$busta_suffisso) }, c("", "-bis", "-ter"))
  for (k in list(cn, c2, vecchio)) DBI::dbDisconnect(k)
})

# leggi_fascicoli (lettura in blocco, 9/10) dà esattamente ciò che dà leggi_fascicolo, scheda per scheda: persone, reati, luoghi in ordine,
# imputati, coppie, campi extra, e il vuoto delle schede senza righe nelle tabelle figlie.
local({
  cb <- apri_db(":memory:")
  nuova <- function(k, ...) { f <- fascicolo_vuoto(); f$id <- sprintf("blocco-%d", k); f$fondo <- "B"; f$busta <- 1L; f$fascicolo <- k; for (n in names(list(...))) f[[n]] <- list(...)[[n]]; salva_fascicolo(cb, f) }
  nuova(1L)                                                                                  # una scheda vuota: tutte le tabelle figlie senza righe
  nuova(2L, persone = c("Rossi, Mario", "Bianchi, Anna"), reati = c("Eresia", "Magia"), luoghi = c("Roma", "Napoli"), lingue = "Italiano", produttori = "Tribunale")
  nuova(3L, persone = "Verdi, Paolo", reati = "Magia", titolo = "Atti circa Paolo Verdi", extra = list(Numerazione = "12", "Consistenza (unità di misura)" = "p.5"))
  ids <- c("blocco-3", "blocco-1", "blocco-2")                                                # un ordine diverso da quello del database
  atteso("leggi_fascicoli: come leggi_fascicolo, scheda per scheda e nell'ordine degli id",
         leggi_fascicoli(cb, ids), lapply(ids, function(i) leggi_fascicolo(cb, i)))
  atteso("leggi_fascicoli: nessun id, nessuna scheda", leggi_fascicoli(cb, character(0)), list())
  atteso("leggi_fascicoli: un id che non esiste fa errore", inherits(try(leggi_fascicoli(cb, "non-c-e"), silent = TRUE), "try-error"), TRUE)
  atteso("leggi_fascicoli: due id uguali fanno errore, con l'id nel messaggio",
         grepl("id ripetuto (blocco-1)", conditionMessage(tryCatch(leggi_fascicoli(cb, c("blocco-1", "blocco-2", "blocco-1")), error = identity)), fixed = TRUE), TRUE)
  DBI::dbDisconnect(cb)
})

# --- parentesi non chiuse nei luoghi (ASMo, 12/10): il «|» dopo una «(» senza compagna separa ---
atteso("livello parentesi: bilanciate come prima", .livello_parentesi(strsplit("A (B|C) D", "")[[1]])[c(1, 4, 6, 9)], c(0L, 1L, 1L, 0L))
atteso("separa_luoghi: «(» mai chiusa non fonde due luoghi", separa_luoghi("Stuffione (Ravarino|Stuffione (Ravarino)"), c("Stuffione (Ravarino", "Stuffione (Ravarino)"))
atteso("separa_luoghi: la variante fra parentesi resta una", separa_luoghi("Solignano Vecchio (Solignano Nuovo|Castelvetro di Modena); Roma"), c("Solignano Vecchio (Solignano Nuovo|Castelvetro di Modena)", "Roma"))
atteso("pipe in parentesi: «(» mai chiusa non cambia il «|»", .pipe_in_parentesi("Stuffione (Ravarino|Stuffione (Ravarino)"), "Stuffione (Ravarino|Stuffione (Ravarino)")

local({
  f <- tempfile(fileext = ".csv")
  writeBin(charToRaw(paste0("\ufeff\"A\";\"B\"\r\n\"Atti:\r\nGrazioli\r\nSanti\";\"x \"\"y\"\"\"\r\n\"due\";\"\"\r\n")), f)
  d <- leggi_tabella(f)
  atteso("CSV: il «\\r\\n» dentro un campo fra virgolette resta com'è", d$A, c("Atti:\r\nGrazioli\r\nSanti", "due"))
  atteso("CSV: virgolette doppie e campo vuoto", d$B, c("x \"y\"", ""))
  f2 <- tempfile(fileext = ".csv"); writeBin(charToRaw("\"A\";\"B\"\n\"a\nb\";\"c\"\n"), f2)
  atteso("CSV senza «\\r»: come prima", leggi_tabella(f2)$A, "a\nb")
})

# --- buste -bis e -ter in piani, numerazione, scaffale, filtro (12/10) ---
local({
  cb <- apri_db(":memory:")
  nuova <- function(id, b, suf, k) { f <- fascicolo_vuoto(); f$id <- id; f$fondo <- "M"; f$busta <- b; f$busta_suffisso <- suf; f$fascicolo <- k; salva_fascicolo(cb, f) }
  nuova("m1", 232L, "", 1L); nuova("m2", 232L, "", 2L); nuova("m3", 232L, "-bis", 1L); nuova("m4", 5L, "", 1L)
  atteso("prossimo numero: la 232 e la 232-bis contano a parte",
         c(prossimo_numero(cb, "M", 232L), prossimo_numero(cb, "M", 232L, suffisso_b = "-bis"), prossimo_numero(cb, "M", 232L, suffisso_b = "-ter")), c(3L, 2L, 1L))
  atteso("prossimo numero di busta: dopo la più alta, suffissi compresi", prossimo_numero(cb, "M"), 233L)
  salva_piano(cb, "M", 232L, 1L, 4L); salva_piano(cb, "M", 232L, 1L, 2L, suffisso_b = "-bis")
  atteso("piano: 232 e 232-bis sono due piani", dbGetQuery(cb, "SELECT busta_suffisso FROM piani_busta ORDER BY busta_suffisso")$busta_suffisso, c("", "-bis"))
  atteso("piano 232: mancano 3 e 4", anteprima_piano(cb, "M", 232L, 1L, 4L)$mancanti, 3:4)
  atteso("piano 232-bis: manca 2", anteprima_piano(cb, "M", 232L, 1L, 2L, "-bis")$mancanti, 2L)
  atteso("piano: la 232-bis non conta le schede della 232", anteprima_piano(cb, "M", 232L, 1L, 4L, "-bis")$schedati, 1L)
  atteso("prossimo nel piano: per busta e suffisso", c(prossimo_nel_piano(cb, "M", 232L, NA), prossimo_nel_piano(cb, "M", 232L, NA, "-bis")), c(3L, 2L))
  mp <- mancanti_piani(cb)
  atteso("mancanti_piani porta il suffisso", paste0(mp$busta, mp$suffisso_b, ":", mp$numeri), c("232:3–4", "232-bis:2"))
  salva_piano(cb, "M", 232L, 1L, 6L, "-bis")
  atteso("piano: aggiornare la 232-bis non tocca la 232", dbGetQuery(cb, "SELECT ultimo FROM piani_busta ORDER BY busta_suffisso")$ultimo, c(4L, 6L))
  # backup JSON: il suffisso del piano fa il giro
  j <- tempfile(fileext = ".json"); esporta_backup_json(cb, j)
  c2 <- apri_db(":memory:"); ripristina_backup_json(c2, j, "sostituisci")
  atteso("backup JSON: i piani fanno il giro col suffisso", dbGetQuery(c2, "SELECT busta, busta_suffisso, primo, ultimo FROM piani_busta ORDER BY busta_suffisso"),
         dbGetQuery(cb, "SELECT busta, busta_suffisso, primo, ultimo FROM piani_busta ORDER BY busta_suffisso"))
  DBI::dbDisconnect(c2)
  # database di prima: piani_busta con la sola chiave (fondo, busta) si rifà senza perdere righe
  vecchio <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(vecchio, "CREATE TABLE piani_busta (fondo TEXT NOT NULL, busta INTEGER NOT NULL, primo INTEGER NOT NULL, ultimo INTEGER NOT NULL, creato TEXT NOT NULL, PRIMARY KEY (fondo, busta), CHECK (busta >= 1 AND primo >= 1 AND ultimo >= primo))")
  DBI::dbExecute(vecchio, "INSERT INTO piani_busta VALUES ('Z', 7, 1, 9, '2026-01-01')")
  .rifai_piani_busta(vecchio)
  atteso("migrazione dei piani: righe salve, suffisso vuoto", DBI::dbGetQuery(vecchio, "SELECT fondo, busta, busta_suffisso, primo, ultimo FROM piani_busta"),
         data.frame(fondo = "Z", busta = 7L, busta_suffisso = "", primo = 1L, ultimo = 9L))
  DBI::dbExecute(vecchio, "INSERT INTO piani_busta VALUES ('Z', 7, '-bis', 1, 3, '2026-01-02')")
  atteso("migrazione dei piani: ora 7 e 7-bis convivono", DBI::dbGetQuery(vecchio, "SELECT COUNT(*) AS n FROM piani_busta")$n, 2L)
  DBI::dbDisconnect(vecchio)
  # elenco, scaffale, filtro
  e <- elenco_fascicoli(cb, fondo = "M")
  atteso("chiave_busta: 232 e 232-bis diverse", chiave_busta(e$busta, e$suffisso_b), c("5", "232", "232", "232-bis"))
  sc <- as.character(scaffale_ui(e))
  atteso("scaffale: una tessera per 232 e una per 232-bis", c(grepl("tessera-nome\">b.232<", sc), grepl("tessera-nome\">b.232-bis<", sc), lengths(regmatches(sc, gregexpr("tessera-busta", sc)))[[1]]), c(TRUE, TRUE, 3L))
  atteso("scaffale: ordine 5, 232, 232-bis", regmatches(sc, gregexpr("tessera-nome\">[^<]*", sc))[[1]], c("tessera-nome\">b.5", "tessera-nome\">b.232", "tessera-nome\">b.232-bis"))
  o <- tabella_elenco(e)
  atteso("tabella: 232-bis dopo la 232 nell'ordine di segnatura", order(o$k_segn), order(c(5, 232, 232, 232) * 1e8 + c(0, 0, 0, 1) * 1e5 + c(1, 1, 2, 1)))
  # riordino in Gestione: la 232-bis è una busta a sé
  atteso("elenco_busta: 232 e 232-bis separate", c(nrow(elenco_busta(cb, "M", 232L)), nrow(elenco_busta(cb, "M", 232L, "-bis"))), c(2L, 1L))
  DBI::dbDisconnect(cb)
})

cat(sprintf("\nverifiche: %d passate, %d fallite\n", .esiti$ok, length(.esiti$falliti)))
if (length(.esiti$falliti)) {
  cat("ESITO: NON TORNA\n")
  if (!interactive()) quit(status = 1) else stop("verifiche fallite")
}
cat("ESITO: ok\n")
