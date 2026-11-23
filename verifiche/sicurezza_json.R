# Regressioni distruttive riprodotte esclusivamente su database temporanei.
Sys.setenv(TRAMA_DEFAULT_DI_PROVA = "1")     # la scheda vuota dei collaudi ha i vecchi valori di prova (archivio, fondo, serie, lingua)
suppressMessages(for (f in list.files('R', pattern='[.]R$', full.names=TRUE)) source(f))
n <- 0L
verifica <- function(x) { stopifnot(isTRUE(x)); n <<- n+1L }
fallisce <- function(x, testo=NULL) {
  e <- tryCatch({force(x); NULL}, error=identity)
  verifica(inherits(e,'error'))
  if (!is.null(testo)) verifica(grepl(testo,conditionMessage(e),fixed=TRUE))
}
foto <- function(c) lapply(setNames(dbListTables(c),dbListTables(c)), function(t) dbReadTable(c,t))
nuova <- function(c,id,num) {
 f<-fascicolo_vuoto(FALSE);f$id<-id;f$archivio<-'Archivio';f$fondo<-'Fondo';f$busta<-1L;f$fascicolo<-num
 f$titolo<-'Originale';f$titolo_manuale<-1L;f$regesto<-'Testo';f$regesto_manuale<-1L;f$reati<-'eresia'
 salva_fascicolo(c,f);leggi_fascicolo(c,id)
}
c<-apri_db(':memory:');b<-apri_db(':memory:')
nuova(c,'esistente',9L);nuova(b,'nuovo',1L)
dbExecute(b,"INSERT INTO schede_persona VALUES ('persona','Persona','','oggi','oggi')")
j<-tempfile();esporta_backup_json(b,j);prima<-foto(c)
raw<-fromJSON(j,simplifyVector=FALSE)
scrivi<-function(x) {p<-tempfile();writeLines(toJSON(x,auto_unbox=TRUE,null='null',na='null'),p);p}
# Una scheda invalida non deve cancellare il database in Sostituisci.
x<-raw;x$fascicoli[[1]]$archivio<-NULL
fallisce(ripristina_backup_json(c,scrivi(x),'sostituisci'),'non è stato toccato');verifica(identical(prima,foto(c)))
# Nemmeno un errore sulle persone può essere nascosto.
x<-raw;x$schede_persona[[1]]$forma_canonica<-NULL
fallisce(ripristina_backup_json(c,scrivi(x),'sostituisci'),'Scheda persona');verifica(identical(prima,foto(c)))
x<-raw;x$schede_persona<-NULL
fallisce(ripristina_backup_json(c,scrivi(x),'sostituisci'),'Backup incompleto');verifica(identical(prima,foto(c)))
# Record doppi interni al file, UUID assente e scalari multipli: niente import parziali.
x<-raw;x$fascicoli<-c(x$fascicoli,x$fascicoli)
fallisce(ripristina_backup_json(c,scrivi(x)),'ripetuti');verifica(identical(prima,foto(c)))
x<-raw;x$fascicoli[[1]]$id<-NULL;fallisce(ripristina_backup_json(c,scrivi(x)),'identificativo')
x<-raw;x$fascicoli[[1]]$titolo<-list('A','B');fallisce(ripristina_backup_json(c,scrivi(x)),'un solo valore')
x<-raw;x$ordine_fascicoli<-list(list(fascicolo_id='assente',posizione=1));fallisce(ripristina_backup_json(c,scrivi(x)))
verifica(identical(prima,foto(c)))
x<-raw;x$fascicoli[[1]]$busta<-'testo';fallisce(ripristina_backup_json(c,scrivi(x)),'numero intero')
x<-raw;x$fascicoli[[1]]$reati<-list(list(nome='eresia'));fallisce(ripristina_backup_json(c,scrivi(x)),'elenco di testi')
# Errore solo nella destinazione: rollback anche dopo aver cancellato i vecchi dati.
dbExecute(c,"CREATE TRIGGER guasto BEFORE INSERT ON reati BEGIN SELECT RAISE(ABORT,'guasto simulato'); END")
fallisce(ripristina_backup_json(c,j,'sostituisci'),'nessuna modifica applicata');verifica(identical(prima,foto(c)))
dbExecute(c,'DROP TRIGGER guasto')
# Copia obbligatoria non scrivibile: nessuna mutazione.
cartella<-tempfile();writeLines('non una cartella',cartella)
fallisce(suppressWarnings(ripristina_backup_json(c,j,'sostituisci',cartella)))
verifica(identical(prima,foto(c)))
# Il backup di sicurezza contiene proprio lo stato precedente.
cartella<-tempfile();e<-ripristina_backup_json(c,j,'sostituisci',cartella)
verifica(e$importati==1L);copia<-apri_db(e$backup_sicurezza);verifica(identical(prima,foto(copia)));dbDisconnect(copia)
prima<-foto(c);e<-ripristina_backup_json(c,j,'accoda')
verifica(e$importati==0L && length(e$respinti)==1L);verifica(any(e$dettagli$motivo=='Identificativo già presente'));verifica(identical(prima,foto(c)))
# Conflitto per segnatura con UUID diverso: resoconto distinto.
x<-raw;x$fascicoli[[1]]$id<-'altro';e<-ripristina_backup_json(c,scrivi(x));verifica(any(e$dettagli$motivo=='Segnatura già occupata'));verifica(identical(prima,foto(c)))
# Salvataggio con errore in una tabella figlia: niente modifiche al padre.
f<-leggi_fascicolo(c,'nuovo');g<-f;g$titolo<-'Bozza'
dbExecute(c,"CREATE TRIGGER guasto BEFORE INSERT ON reati BEGIN SELECT RAISE(ABORT,'guasto'); END")
fallisce(salva_fascicolo(c,g,atteso=f));verifica(identical(f,leggi_fascicolo(c,f$id)))
dbExecute(c,'DROP TRIGGER guasto')
# Due aperture contemporanee: anche modifiche nello stesso secondo sono rilevate.
salva_fascicolo(c,g,atteso=f);prima<-foto(c)
fallisce(salva_fascicolo(c,f,atteso=f),"dopo l'apertura");verifica(identical(prima,foto(c)))
# Scheda rimossa nel frattempo: non ricrearla accidentalmente.
f<-leggi_fascicolo(c,'nuovo');dbExecute(c,"DELETE FROM fascicoli WHERE id='nuovo'")
fallisce(salva_fascicolo(c,f,atteso=f),'rimossa');verifica(is.null(leggi_fascicolo(c,f$id)))
# Buste sconosciute: la stessa segnatura senza busta non si ripete (22/10: in un indice UNIQUE i NULL sono tutti diversi),
# ma due schede senza busta con fascicolo diverso stanno insieme, e il backup le riporta.
z<-apri_db(':memory:');u<-nuova(z,'senza1',1L);u$busta<-NA_integer_;salva_fascicolo(z,u)
u$id<-'senza2';fallisce(salva_fascicolo(z,u),'UNIQUE');u$fascicolo<-2L;salva_fascicolo(z,u);p<-tempfile();esporta_backup_json(z,p)
w<-apri_db(':memory:');verifica(ripristina_backup_json(w,p)$importati==2L)
invisible(lapply(list(z,w),dbDisconnect))
invisible(lapply(list(c,b),dbDisconnect));cat(n,'verifiche sicurezza JSON e salvataggi passate.\n')
