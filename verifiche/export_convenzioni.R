Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages(for (f in list.files('R', pattern='[.]R$', full.names=TRUE)) source(f))
stopifnot(identical(.nome_persona_export('antonio de santis'), 'antonio de santis'),
 identical(.nome_persona_export('De Santis, Antonio'), 'Santis, Antonio de'),
 identical(.nome_persona_export('Santis de, Antonio'), 'Santis de, Antonio'),
 identical(.nome_persona_export('Di Maio, Luigi'), 'Maio, Luigi di'),
 identical(.nome_persona_export("D'Africa, Pietro"), "D'Africa, Pietro"))
stopifnot(identical(.nome_persona_export('Antonio del Sangro'), 'Antonio del Sangro'),
 identical(.nome_persona_export('Sangro del, Antonio'), 'Sangro del, Antonio'),
 identical(.nome_persona_export('di Napoli, Francesco'), 'Napoli, Francesco di'),
 identical(.nome_persona_export('della Valle, Pietro'), 'Valle, Pietro della'),
 # dal 5/10 anche «della» e «da» vanno in fondo; con l'apostrofo il cognome è uno solo
 identical(.nome_persona_export('da Tunisi, Caterina'), 'Tunisi, Caterina da'),
 identical(.nome_persona_export('da Cosenza, Ignazio'), 'Cosenza, Ignazio da'),
 identical(.nome_persona_export('degli Uberti, Farinata'), 'Uberti, Farinata degli'),
 identical(.nome_persona_export("d'Africa, Caterina"), "d'Africa, Caterina"),
 identical(.nome_persona_export("de' Rossi, Anna"), "de' Rossi, Anna"),
 identical(.nome_persona_export("dell'Abbate, Giovanni Tommaso"), "dell'Abbate, Giovanni Tommaso"),
 identical(.nome_persona_export('Dalmasso, Pietro'), 'Dalmasso, Pietro'),
 identical(.nome_persona_export('de la Ascia, Giovanni'), 'Ascia, Giovanni de la'),
 identical(.nome_persona_export('de Lama, Faustina'), 'Lama, Faustina de'), identical(.nome_persona_export('de Lauro, Giulia'), 'Lauro, Giulia de'),
 identical(.nome_persona_export('da Lama, Anna'), 'Lama, Anna da'),
 identical(.nome_persona_import('Ascia de la, Giovanni'), 'de la Ascia, Giovanni'),
 identical(.nome_persona_export('Rossi, Mario'), 'Rossi, Mario'))
for (n in c('de Santis, Antonio', 'di Maio, Luigi', 'del Sangro, Antonio', 'della Valle, Pietro', 'da Tunisi, Caterina', 'degli Uberti, Farinata',
            "d'Africa, Caterina", "dell'Abbate, Giovanni Tommaso", 'Dalmasso, Pietro', 'Rossi, Mario', 'de Madre Vella, Dio Lopes',
            'de Lama, Faustina', 'de Lagni, Giulia', 'de la Ascia, Giovanni', 'de Lauro, Giulia'))
  stopifnot(identical(.nome_persona_import(.nome_persona_export(n)), n))
stopifnot(.regesto_export("Testo") == "Testo.",
 .regesto_export("Testo.  ") == "Testo.", .regesto_export(NA_character_) == "",
 .regesto_export("   ") == "", .regesto_export("Testo...") == "Testo...")
con <- apri_db(':memory:')
f <- fascicolo_vuoto(); f$busta <- 1L; f$fascicolo <- 1L
f$estremo_remoto <- 1600L; f$estremo_recente <- 1600L
f$persone <- c('de Santis, Antonio', 'Di Maio, Luigi', 'del Sangro, Antonio'); f$lingue <- c('Italiano','LATINO')
f$note <- 'Nota di prova'; f$regesto <- 'Regesto di prova'; f$regesto_manuale <- 1L
salva_fascicolo(con,f)
prima <- leggi_fascicolo(con,f$id)
paths <- setNames(lapply(c('xlsx','csv','json'),function(e) tempfile(fileext=paste0('.',e))),c('xlsx','csv','json'))
esporta_arianna(con,paths$xlsx); esporta_csv(con,paths$csv); esporta_json_schede(con,paths$json)
for (d in list(leggi_tabella(paths$xlsx),leggi_tabella(paths$csv),jsonlite::fromJSON(paths$json))) {
 stopifnot(d[['Note libere']][1]=='Nota di prova.')
 stopifnot(d[['Regesto o descrizione sintetica']][1]=='Regesto di prova.')
 stopifnot(d[['Lingua']][1]=='italiano; latino',d[['Persone citate o indicizzate']][1]=='Santis, Antonio de|Maio, Luigi di|Sangro, Antonio del')
}
stopifnot(identical(prima,leggi_fascicolo(con,f$id)))
b <- tempfile(fileext='.json'); esporta_backup_json(con,b)
con2 <- apri_db(':memory:'); ripristina_backup_json(con2,b,'sostituisci')
stopifnot(identical(prima$note,leggi_fascicolo(con2,f$id)$note))
stopifnot(identical(prima$regesto,leggi_fascicolo(con2,f$id)$regesto))
stopifnot(identical(prima$persone,leggi_fascicolo(con2,f$id)$persone),identical(prima$lingue,leggi_fascicolo(con2,f$id)$lingue))
# Il foglio esportato, reimportato, deve ridare le persone e le lingue del
# database: le convenzioni dell'export si disfano leggendo (5/10; prima le lingue
# tornavano una voce sola «italiano; latino» e i «de» restavano in fondo).
for (e in c('xlsx','csv')) { c3 <- apri_db(':memory:'); r <- importa_arianna(c3, leggi_arianna(paths[[e]]), 'sostituisci')
  dopo <- leggi_fascicolo(c3, DBI::dbGetQuery(c3, 'SELECT id FROM fascicoli')$id[1])
  stopifnot(r$importati == 1L, # («Di Maio» torna «di Maio»: nel database la particella è minuscola, regola del 2/10)
            identical(sort(as.character(dopo$persone)), sort(c('de Santis, Antonio', 'di Maio, Luigi', 'del Sangro, Antonio'))),
            identical(as.character(dopo$lingue), c('Italiano','Latino')))
  dbDisconnect(c3) }
stopifnot(identical(.nome_persona_import('Santis, Antonio de'), 'de Santis, Antonio'), identical(.nome_persona_import('Santis de, Antonio'), 'de Santis, Antonio'), identical(.nome_persona_import('Valle della, Pietro'), 'della Valle, Pietro'), identical(.nome_persona_import('Tunisi da, Caterina'), 'da Tunisi, Caterina'),
          identical(.lingue_import('italiano; latino'), c('Italiano','Latino')))
dbDisconnect(con);dbDisconnect(con2)
cat('ESITO: ok - XLSX, CSV, JSON schede, database invariato e backup fedele\n')

# Il nome senza virgola l'export dal database non lo gira mai (23/10/2026, scelta A di Stefano): l'ha deciso l'anteprima, e il database tiene la scelta.
# Anche «fra Paolo» (titolo + nome) resta «fra Paolo», non «Paolo, fra».
stopifnot(identical(.nome_persona_export('fra Paolo'), 'fra Paolo'), identical(.nome_persona_export('suor Maria'), 'suor Maria'),
          identical(.nome_persona_export('don Carlo'), 'don Carlo'), identical(.nome_persona_export('padre Pio'), 'padre Pio'),
          identical(.nome_persona_export('Fra Paolo Sarpi'), 'Fra Paolo Sarpi'), identical(.nome_persona_export('Mario Rossi'), 'Mario Rossi'))
cat("ok   senza virgola: l'export non gira «fra Paolo», «Mario Rossi» e simili\n")
