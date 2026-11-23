suppressMessages(for(f in list.files('R',pattern='[.]R$',full.names=TRUE)) source(f))
con <- apri_db(':memory:')
a <- leggi_arianna(FOGLIO_ESEMPIO); a$dati <- a$dati[1:2,,drop=FALSE]
importa_arianna(con,a)
ids <- dbGetQuery(con,'SELECT id FROM fascicoli')$id
prima <- lapply(ids,function(id) leggi_fascicolo(con,id))
e <- importa_nuovo_archivio(con,a,'Archivio di prova','Dataset separato')
stopifnot(e$importati==2L,nrow(elenco_fondi(con))==2L,
 identical(prima,lapply(ids,function(id) leggi_fascicolo(con,id))),
 nrow(tabella_arianna(con,fondo='Dataset separato'))==2L)
nuovi <- dbGetQuery(con,"SELECT id,archivio FROM fascicoli WHERE fondo='Dataset separato'")
stopifnot(!any(nuovi$id %in% ids), all(nuovi$archivio=='Archivio di prova'),
 !is.null(leggi_fascicolo(con,nuovi$id[1])$extra$import_identita_originale))
# Lo stesso (archivio, fondo) è già occupato: respinto. Lo stesso NOME di fondo in un altro archivio è un fondo diverso (19/10/2026).
fallisce <- function(expr) inherits(try(expr,silent=TRUE),'try-error')
stopifnot(fallisce(importa_nuovo_archivio(con,a,'Archivio di prova','Dataset separato')))
stopifnot(!fallisce(importa_nuovo_archivio(con,a,'Altro','Dataset separato')), nrow(elenco_fondi(con))==3L)
stopifnot(identical(sort(dbGetQuery(con,"SELECT archivio FROM fondi WHERE denominazione='Dataset separato'")$archivio), c('Altro','Archivio di prova')))
dbExecute(con,"DELETE FROM fascicoli WHERE archivio='Altro'"); dbExecute(con,"DELETE FROM fondi WHERE archivio='Altro'")
# Righe duplicate: nessuna creazione parziale.
b <- a; b$dati <- a$dati[c(1,1),,drop=FALSE]
stopifnot(fallisce(importa_nuovo_archivio(con,b,'Altro','Da annullare')),
 !'Da annullare' %in% elenco_fondi(con)$fondo,
 dbGetQuery(con,'SELECT COUNT(*) n FROM fascicoli')$n==4L,
 fallisce(importa_nuovo_archivio(con,a,'','Senza nome')),
 !'Senza nome' %in% elenco_fondi(con)$fondo)
vuoto <- a; vuoto$dati <- a$dati[FALSE,,drop=FALSE]
stopifnot(fallisce(importa_nuovo_archivio(con,vuoto,'Altro','Vuoto')))
# Nessuna riga può entrare (18/11/2026): errore chiaro e nessun fondo vuoto lasciato in Home.
n_fondi <- nrow(elenco_fondi(con)); s <- a; s$dati[[1]] <- rep("", nrow(s$dati)); s$dati <- s$dati[1:2,,drop=FALSE]
stopifnot(fallisce(importa_nuovo_archivio(con,s,'Altro','Fondo senza righe valide')), nrow(elenco_fondi(con))==n_fondi)
dbDisconnect(con)
cat('ESITO: ok - dataset separato, sorgente conservata, rollback completo, collisioni e foglio vuoto\n')
