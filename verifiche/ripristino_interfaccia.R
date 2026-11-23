# testServer su copia completa dell'app: mai sulla cartella con i dati reali.
if (!identical(Sys.getenv('DATESTRAMA_COPIA_COLLAUDO'), 'si')) stop('Eseguire soltanto sulla copia di collaudo con DATESTRAMA_COPIA_COLLAUDO=si.')
invisible(source('app.R'))
shiny::testServer(server, {
  prima <- backup_json(con)
  j <- tempfile(fileext='.json'); esporta_backup_json(con,j)
  session$setInputs(file_ripristino=data.frame(name='prova.json',datapath=j,size=file.info(j)$size,type='application/json'),
                   modo_ripristino='accoda')
  session$setInputs(esegui_ripristino=1)
  e <- risultato_ripristino(); stopifnot(!is.null(e),e$importati==0,file.exists(e$backup_sicurezza))
  stopifnot(length(e$respinti)==length(prima$fascicoli))
  # Sostituisci attende una seconda azione e non tocca i dati in anticipo.
  session$setInputs(modo_ripristino='sostituisci',esegui_ripristino=2)
  stopifnot(!is.null(ripristino_in_attesa()))
  session$setInputs(conferma_ripristino=1)
  stopifnot(is.null(ripristino_in_attesa()),risultato_ripristino()$importati==length(prima$fascicoli),is.null(aperto()))
  dopo <- backup_json(con); prima$esportato_il<-NULL;dopo$esportato_il<-NULL
  stopifnot(identical(prima,dopo))
  # Il ripristino passa dalla stessa guardia delle bozze non salvate.
  istantanea(list()); session$setInputs(titolo='Bozza da proteggere')
  session$setInputs(esegui_ripristino=3)
  stopifnot(is.function(in_attesa()),is.null(ripristino_in_attesa()))
  cat('Interfaccia: accoda, conferma sostituzione e protezione bozza verificate.\n')
})
