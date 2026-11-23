suppressMessages(for (f in list.files('R', pattern='[.]R$', full.names=TRUE)) source(f))
p <- tempfile(fileext='.json')
x <- list(versione=2, esportatoIl='2026-09-24', fascicoli=list(list(id='prova-electron',
 archivio='Archivio di prova', fondo='Fondo', busta=1, fascicolo=1, titolo='Titolo originale',
 regestoLeggibile='Testo originale', personeCitate=list('Rossi, Mario'), imputati=list('Rossi, Mario'),
 tipoReato=list('eresia'), applicazioneTortura='n.d.', sentenzaSpedizione='no',
 creato='2026-09-24', modificato='2026-09-24', campiOriginali=list('Altra segnatura'='A1'))),
 schedePersona=list(), coorti=list(), luoghiUtente=list())
writeLines(toJSON(x, auto_unbox=TRUE),p)
c <- apri_db(':memory:'); e <- ripristina_backup_json(c,p);stopifnot(e$importati==1)
f <- leggi_fascicolo(c,'prova-electron');stopifnot(f$titolo=='Titolo originale', f$regesto=='Testo originale',f$imputati$indicizzato=='Rossi, Mario',f$altra_segnatura=='A1')
raw <- fromJSON(f$extra[['Electron · record originale JSON']]);stopifnot(raw$id==f$id)
q <- tempfile();esporta_backup_json(c,q); b<-apri_db(':memory:');ripristina_backup_json(b,q)
stopifnot(identical(f,leggi_fascicolo(b,f$id)))
x$luoghiUtente<-list(list(nome='Luogo'));writeLines(toJSON(x,auto_unbox=TRUE),p)
stopifnot(inherits(try(ripristina_backup_json(c,p,'sostituisci'),silent=TRUE),'try-error'),identical(f,leggi_fascicolo(c,f$id)))
# 28/09: idAssorbiti, schede senza attestazioni, imputati non dedotti.
y <- list(versione=2, esportatoIl='2026-09-24', coorti=list(), luoghiUtente=list(), fascicoli=list(
  list(id='e1', archivio='A', fondo='F', busta=1, fascicolo=1, personeCitate=list('Rossi, Mario', 'Bianchi, Anna'),
       tipoReato=list('Eresia'), applicazioneTortura='n.d.', sentenzaSpedizione='n.d.', creato='2026-09-24', modificato='2026-09-24')),
  schedePersona=list(
    list(id='s1', formaCanonica='Rossi, Mario', varianti=list('Rosso, Mario'), note='', creato='x', modificato='x', idAssorbiti=list('vecchio-id')),
    list(id='s2', formaCanonica='Di Modena, Tizio', varianti=list(), note='', creato='x', modificato='x')))
writeLines(toJSON(y, auto_unbox=TRUE), p)
d <- apri_db(':memory:'); ripristina_backup_json(d, p)
stopifnot(identical(dbGetQuery(d, 'SELECT id FROM schede_persona')$id, 's1'),
          identical(dbGetQuery(d, 'SELECT id_assorbito FROM persona_assorbiti')$id_assorbito, 'vecchio-id'),
          nrow(leggi_fascicolo(d, 'e1')$imputati) == 0L,
          identical(sort(leggi_fascicolo(d, 'e1')$persone), c('Bianchi, Anna', 'Rossi, Mario')))
dbDisconnect(d)
dbDisconnect(c);dbDisconnect(b);cat('Electron: conversione, roundtrip e protezione dati verificati.\n')
