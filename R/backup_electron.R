# Adattatore del formato Electron; il record originale resta negli extra
# (EXTRA_NASCOSTI, R/profili.R).
.converti_backup_electron <- function(d) {
  if (length(d$luoghiUtente) || length(d$coorti))
    stop('Questo backup Electron contiene luoghi personali o ricerche da convertire: importazione interrotta prima di modificare il database.')
  vett <- function(x) as.character(unlist(x, use.names = FALSE))
  d$fascicoli <- lapply(d$fascicoli, function(x) {
    f <- fascicolo_vuoto(FALSE)
    diretti <- intersect(names(f), c('id','archivio','fondo','serie','busta','fascicolo','titolo','definizione','supporto','note'))
    for (k in diretti) if (!is.null(x[[k]])) f[[k]] <- x[[k]]
    mappa <- c(estremo_remoto='estremoRemoto', estremo_recente='estremoRecente',
      tipologia_documentaria='tipologiaDocumentaria', soggetto_conservatore='soggettoConservatore',
      luogo_produzione='luogoProduzione', tortura='applicazioneTortura', sentenza='sentenzaSpedizione',
      regesto='regestoLeggibile', creato='creato', modificato='modificato')
    for (k in names(mappa)) f[[k]] <- if (is.null(x[[mappa[[k]]]])) NA else x[[mappa[[k]]]]
    multi <- c(reati='tipoReato', persone='personeCitate', luoghi='luoghiCitati', lingue='lingua', produttori='soggettoProduttore')
    for (k in names(multi)) f[[k]] <- vett(x[[multi[[k]]]])
    # Conserva il testo Electron: una ricomposizione R non è equivalente.
    f$titolo_manuale <- 1L; f$regesto_manuale <- 1L
    # Solo gli imputati scritti nel record: senza, il vecchio Regestum li deduce dalle persone citate (come persone_imputate() qui), e copiarli li farebbe sembrare dichiarati.
    imp <- vett(x$imputati)
    if (length(imp)) f$imputati <- data.frame(esteso=vapply(imp, .estendi_nome, ''), indicizzato=imp)
    f$tortura_esaminata <- as.integer('applicazioneTortura' %in% vett(x$esaminati) || (!is.na(f$tortura) && f$tortura != 'n.d.'))
    f$sentenza_esaminata <- as.integer('sentenzaSpedizione' %in% vett(x$esaminati) || (!is.na(f$sentenza) && f$sentenza != 'n.d.'))
    # Campi privi di equivalente non diventano asserzioni inventate.
    f$extra <- lapply(x$campiOriginali, function(v) if (is.null(v)) character() else vett(v))
    f$extra[[EXTRA_NASCOSTI]] <- as.character(jsonlite::toJSON(x, auto_unbox=TRUE, null='null'))
    f$altra_segnatura <- if (is.null(x$campiOriginali[['Altra segnatura']])) NA else x$campiOriginali[['Altra segnatura']]
    f$luogo_conservazione <- if (is.null(x$campiOriginali[['Luogo di conservazione']])) NA else x$campiOriginali[['Luogo di conservazione']]
    .fascicolo_per_json(f)
  })
  # Solo le schede di persone citate in questi fascicoli (forma o variante): il backup Electron ne porta 5.863, quasi tutte di un fondo tolto dal database, e entrerebbero senza attestazioni.
  citate <- unique(unlist(lapply(d$fascicoli, function(f) c(vett(f$persone), vett(f$imputati)))))
  schede <- Filter(function(s) any(c(vett(s$formaCanonica), vett(s$varianti)) %in% citate), d$schedePersona)
  d$schede_persona <- lapply(schede, function(s) {
    # «idAssorbiti» nel vecchio Regestum; «assorbiti» nei primi backup.
    list(id=s$id, forma_canonica=s$formaCanonica, note=s$note, creato=s$creato,
         modificato=s$modificato, varianti=s$varianti,
         assorbiti=if (is.null(s$idAssorbiti)) s$assorbiti else s$idAssorbiti)
  })
  jsonlite::fromJSON(jsonlite::toJSON(d, auto_unbox=TRUE, na='null', null='null'), simplifyVector=FALSE)
}
