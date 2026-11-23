# ─────────────────────────────────────────────────────────────
# profili.R: descrizione delle colonne Arianna
# Una riga per colonna: il modulo di inserimento nasce da qui e l'import/export legge da qui.
# ─────────────────────────────────────────────────────────────

# Il profilo del materiale (profili/<nome>/profilo.json): se questo file è letto da solo, lo si carica prima.
if (!exists("PROFILO_DOMINIO", inherits = FALSE)) source("R/profilo_dominio.R")

# Il nome dell'extra che cambia la «Tipologia documentaria» scritta nel foglio.
EXTRA_TIPOLOGIA_FOGLIO <- "Tipologia documentaria nel foglio"
# Giorno e mese degli estremi, facoltativi («aaaa-mm-gg», «marzo 1644», «1644»): l'anno resta nei due campi numerici;
# qui sta la data intera, come esce nel foglio, valida solo se il suo anno è quello del campo.
EXTRA_DATA_PRECISA <- c(estremo_remoto = "Estremo remoto (data precisa)", estremo_recente = "Estremo recente (data precisa)")

.col <- function(arianna,
                 etichetta = trimws(arianna),
                 campo     = NA_character_,
                 dove      = "extra",
                 multi     = FALSE,
                 widget    = "testo",
                 sezione   = "Altro") {
  data.frame(arianna = arianna, etichetta = etichetta, campo = campo,
             dove = dove, multi = multi, widget = widget, sezione = sezione,
             stringsAsFactors = FALSE)
}


# ── L'UNICO MODELLO DI EXPORT E DI IMPORT ───────────
# 28 colonne, nell'ordine e con la grafia di dati/modello_export.xlsx. Ogni export (xlsx e CSV) esce così, qualunque sia l'archivio;
# ogni import lo riconosce e, se il foglio è in un formato vecchio (ASDNa, ASMo), lo converte dicendo che cosa ha cambiato.
# verifiche/roundtrip.R confronta le intestazioni lettera per lettera con la riga 1 del modello. Una colonna nuova è una riga qui.
# Rispetto al vecchio ASDNa: nomi più larghi per persone e luoghi, «Soggetti citati o indicizzati» al posto di «Altro soggetto»,
# «consistenza.quantità» e «consistenza.unitàMisura», «Manifest IIIF», «Identificativo immagine o cartella»;
# tolti «Soggetto conservatore», «Personaggi indicizzati», «Trascrizione», «Soggetto conservatore precedente».
# Le colonne extra (dove = "extra") stanno in campi_extra, col nome del modello.
PROFILO_MODELLO <- rbind(
  .col("Segnatura",                 dove = "derivato",                                        sezione = "Identità archivistica"),
  .col("Altra segnatura",           campo = "altra_segnatura",        dove = "fascicoli",     sezione = "Identità archivistica"),
  .col("Titolo",                    campo = "titolo",                 dove = "fascicoli",     sezione = "Identità archivistica"),
  .col("Estremo remoto",            campo = "estremo_remoto",         dove = "fascicoli", widget = "numero", sezione = "Identità archivistica"),
  .col("Estremo recente",           campo = "estremo_recente",        dove = "fascicoli", widget = "numero", sezione = "Identità archivistica"),
  .col("definizione.tipologia",     etichetta = "Definizione",        campo = "definizione",  dove = "fascicoli", widget = "scelta", sezione = "Identità archivistica"),
  .col("Supporto (materiale)",      etichetta = "Supporto",           campo = "supporto",     dove = "fascicoli", widget = "scelta", sezione = "Descrizione materiale"),
  .col("dimensioni/altezza",        etichetta = "Altezza",                                    sezione = "Descrizione materiale"),
  .col("dimensioni/larghezza",      etichetta = "Larghezza",                                  sezione = "Descrizione materiale"),
  .col("tipo di numerazione",       etichetta = "Tipo di numerazione",                        sezione = "Descrizione materiale"),
  .col("Numerazione",                                                                          sezione = "Descrizione materiale"),
  .col("epoca numerazione",         etichetta = "Epoca della numerazione",                    sezione = "Descrizione materiale"),
  .col("consistenza.quantità",      etichetta = "Consistenza (quantità)",                     sezione = "Descrizione materiale"),
  .col("consistenza.unitàMisura",   etichetta = "Consistenza (unità di misura)",              sezione = "Descrizione materiale"),
  .col("Tipologia documentaria",    campo = "tipologia_documentaria", dove = "fascicoli", widget = "scelta", sezione = "Identità archivistica"),
  .col("Regesto o descrizione sintetica", etichetta = "Regesto", campo = "regesto", dove = "fascicoli", widget = "testo_lungo", sezione = "Regesto"),
  .col("Persone citate o indicizzate", etichetta = "Persone citate", campo = "persona", dove = "persone", multi = TRUE, widget = "tag", sezione = "Indicizzazione"),
  .col("Luogo di conservazione",    campo = "luogo_conservazione",    dove = "fascicoli", widget = "auto",                        sezione = "Contesto"),
  .col("Luogo di produzione (data topica)", etichetta = "Data topica", campo = "luogo_produzione", dove = "fascicoli", widget = "auto", sezione = "Contesto"),
  .col("Luoghi citati o indicizzati", etichetta = "Luoghi citati",  campo = "luogo",    dove = "luoghi",  multi = TRUE, widget = "tag", sezione = "Indicizzazione"),
  .col("Soggetto produttore",       campo = "soggetto", dove = "produttori", multi = TRUE, widget = "tag",       sezione = "Contesto"),
  .col("Soggetti citati o indicizzati", etichetta = "Altri soggetti", multi = TRUE, widget = "tag",              sezione = "Indicizzazione"),
  .col("Soggettario",               campo = "reato",    dove = "reati",   multi = TRUE, widget = "vocabolario",  sezione = "Indicizzazione"),
  .col("Eventi",                                                          multi = TRUE, widget = "tag",          sezione = "Indicizzazione"),
  .col("Note libere",               etichetta = "Note",  campo = "note",  dove = "fascicoli", widget = "testo_lungo", sezione = "Note"),
  .col("Lingua",                    campo = "lingua",   dove = "lingue",  multi = TRUE, widget = "tag",          sezione = "Contesto"),
  .col("Manifest IIIF",             etichetta = "Manifest IIIF",                                              sezione = "Immagini"),
  .col("Identificativo immagine o cartella", etichetta = "Identificativo immagine o cartella",                 sezione = "Immagini")
)

# Profili di sola LETTURA: i fogli già in circolazione (ASDNa 28 colonne, ASMo 34) si convertono nel modello all'import; non si esporta in questi formati.
PROFILO_ASDNA <- rbind(
  .col("Segnatura",                 dove = "derivato",                                        sezione = "Identità archivistica"),
  .col("Altra segnatura",           campo = "altra_segnatura",        dove = "fascicoli",     sezione = "Identità archivistica"),
  .col("Titolo",                    campo = "titolo",                 dove = "fascicoli",     sezione = "Identità archivistica"),
  .col("Estremo remoto",            campo = "estremo_remoto",         dove = "fascicoli", widget = "numero", sezione = "Identità archivistica"),
  .col("Estremo recente",           campo = "estremo_recente",        dove = "fascicoli", widget = "numero", sezione = "Identità archivistica"),
  .col("definizione.tipologia",     etichetta = "Definizione",        campo = "definizione",  dove = "fascicoli", widget = "scelta", sezione = "Identità archivistica"),
  .col("Tipologia documentaria",    campo = "tipologia_documentaria", dove = "fascicoli", widget = "scelta", sezione = "Identità archivistica"),
  
  .col("Supporto (materiale)",      etichetta = "Supporto",           campo = "supporto",     dove = "fascicoli", widget = "scelta", sezione = "Descrizione materiale"),
  .col("dimensioni/altezza",        etichetta = "Altezza",                                    sezione = "Descrizione materiale"),
  .col("dimensioni/larghezza",      etichetta = "Larghezza",                                  sezione = "Descrizione materiale"),
  .col("tipo di numerazione",       etichetta = "Tipo di numerazione",                        sezione = "Descrizione materiale"),
  .col("Numerazione",                                                                          sezione = "Descrizione materiale"),
  .col("epoca numerazione",         etichetta = "Epoca della numerazione",                    sezione = "Descrizione materiale"),
  
  .col("Regesto o descrizione sintetica ", etichetta = "Regesto", campo = "regesto", dove = "fascicoli", widget = "testo_lungo", sezione = "Regesto"),
  
  .col("Persone citate",            campo = "persona",  dove = "persone", multi = TRUE, widget = "tag",          sezione = "Indicizzazione"),
  .col("Personaggi indicizzati",                                          multi = TRUE, widget = "tag",          sezione = "Indicizzazione"),
  .col("Luoghi citati",             campo = "luogo",    dove = "luoghi",  multi = TRUE, widget = "tag",          sezione = "Indicizzazione"),
  .col("Soggettario",               campo = "reato",    dove = "reati",   multi = TRUE, widget = "vocabolario",  sezione = "Indicizzazione"),
  .col("Altro soggetto (citato o indicizzato)", etichetta = "Altro soggetto", multi = TRUE, widget = "tag",      sezione = "Indicizzazione"),
  .col("Eventi",                                                          multi = TRUE, widget = "tag",          sezione = "Indicizzazione"),
  
  .col("Luogo di conservazione",    campo = "luogo_conservazione",    dove = "fascicoli", widget = "auto",                        sezione = "Contesto"),
  .col("Luogo di produzione (data topica ove presente)", etichetta = "Data topica", campo = "luogo_produzione", dove = "fascicoli", widget = "auto", sezione = "Contesto"),
  .col("Soggetto produttore",       campo = "soggetto", dove = "produttori", multi = TRUE, widget = "tag",       sezione = "Contesto"),
  .col("Soggetto conservatore",     dove = "derivato",                                                        sezione = "Contesto"),
  .col("Lingua",                    campo = "lingua",   dove = "lingue",  multi = TRUE, widget = "tag",          sezione = "Contesto"),
  
  .col("Trascrizione", widget = "testo_lungo", sezione = "Regesto"),
  .col("Soggetto conservatore precedente (ente)", sezione = "Contesto"),
  .col("Note libere",               etichetta = "Note",  campo = "note",  dove = "fascicoli", widget = "testo_lungo", sezione = "Note")
)

# Archivio di Stato di Modena, Tribunale dell'Inquisizione: 34 colonne, lette sull'inventario ASMo-Soggettario Arianna.
# Stessi campi dove il significato coincide, il resto negli extra, così nulla si perde. Serve a import ed export.
PROFILO_ASMO <- rbind(
  .col("Segnatura",                     dove = "derivato"),
  .col("Vecchia segnatura",             campo = "altra_segnatura",        dove = "fascicoli"),
  .col("Titolo",                        campo = "titolo",                 dove = "fascicoli"),
  .col("Titolo originale"),
  .col("Estremo remoto",                campo = "estremo_remoto",         dove = "fascicoli", widget = "numero"),
  .col("Estremo recente",               campo = "estremo_recente",        dove = "fascicoli", widget = "numero"),
  .col("Supporto (tipologia)",          campo = "definizione",            dove = "fascicoli", widget = "scelta"),
  .col("Supporto (materiale)",          campo = "supporto",               dove = "fascicoli", widget = "scelta"),
  .col("Altezza"), .col("Larghezza"), .col("Profondità"),
  .col("Consistenza (quantità)"), .col("Consistenza (unità di misura)"),
  .col("Legatura (tipologia)"), .col("Legatura (epoca)"),
  .col("Tipologia documentaria",        campo = "tipologia_documentaria", dove = "fascicoli", widget = "scelta"),
  .col("Trascrizione"),
  .col("Autore (persona)",              multi = TRUE),
  .col("Autore (organizzazione)",       campo = "soggetto", dove = "produttori", multi = TRUE, widget = "tag"),
  .col("Destinatario (persona)",        multi = TRUE),
  .col("Regesto o descrizione sintetica del documento", campo = "regesto", dove = "fascicoli", widget = "testo_lungo"),
  .col("Destinatario (organizzazione)", multi = TRUE),
  .col("Persone citate",                campo = "persona",  dove = "persone", multi = TRUE, widget = "tag"),
  .col("Organizzazioni citate",         multi = TRUE),
  .col("Data topica",                   campo = "luogo_produzione",       dove = "fascicoli", widget = "auto"),
  .col("Luoghi citati",                 campo = "luogo",    dove = "luoghi",  multi = TRUE, widget = "tag"),
  .col("Possessore (persona)",          multi = TRUE),
  .col("Donatore (persona)",            multi = TRUE),
  .col("Soggetto conservatore (ente)",  campo = "luogo_conservazione",    dove = "fascicoli"),
  .col("Soggetto conservatore precedente (ente)"),
  .col("Soggettario",                   campo = "reato",    dove = "reati",   multi = TRUE, widget = "vocabolario"),
  .col("Note libere",                   campo = "note",     dove = "fascicoli", widget = "testo_lungo"),
  .col("Lingua",                        campo = "lingua",   dove = "lingue",  multi = TRUE, widget = "tag")
)

# Formato della SEGNATURA per archivio: le colonne sono una sola, ma ogni sala di studio scrive la segnatura a modo suo
# («b.1, f.1» a Napoli, «b. 213, fasc. 13» a Modena): FORMATI_SEGNATURA, arianna_export.R. Un archivio sconosciuto usa quello di Napoli.
PROFILO_PER_ARCHIVIO <- c(
  "Archivio Storico Diocesano di Napoli" = "ASDNa",
  "Archivio di Stato di Modena"          = "ASMo"
)

# Con quale profilo si esporta un fondo: sempre il modello (2/10). Resta la
# funzione perché chiamarla non costa e il resto del codice la usa ancora.
profilo_del_fondo <- function(con, fondo = NULL) "Modello"


# Profili noti. Aggiungerne uno significa aggiungere una voce qui,
# SOTTO la sua tabella: l'elenco si valuta al caricamento del file.
PROFILI <- list(
  Modello = PROFILO_MODELLO,     # l'unico di uscita e di entrata
  ASDNa   = PROFILO_ASDNA,       # solo lettura: foglio nel vecchio formato ASDNa
  ASMo    = PROFILO_ASMO         # solo lettura: foglio nel vecchio formato ASMo
)
MODELLO <- "Modello"             # il nome del profilo con cui si esporta, sempre

# I profili di lettura e il perché: serve a dire, nell'anteprima dell'import,
# che il foglio è in un formato vecchio e verrà convertito.
PROFILI_LETTURA <- c("ASDNa", "ASMo")


# Campi propri di Trama, senza colonna Arianna dedicata (alcuni vengono serializzati dentro il Regesto).
CAMPI_TRAMA <- rbind(
  
  # Componenti della segnatura. Arianna le serializza tutte insieme
  # nell'unica colonna "Segnatura", quindi nessuna ha colonna propria.
  .col(NA_character_, etichetta = "Archivio",  campo = "archivio",  dove = "fascicoli", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Fondo",     campo = "fondo",     dove = "fascicoli", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Serie",     campo = "serie",     dove = "fascicoli", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Busta",     campo = "busta",     dove = "fascicoli", widget = "numero", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Fascicolo", campo = "fascicolo", dove = "fascicoli", widget = "numero", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Suffisso (bis, ter…)", campo = "fascicolo_suffisso", dove = "fascicoli", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Suffisso della busta (bis, ter, quater…)", campo = "busta_suffisso", dove = "fascicoli", sezione = "Segnatura"),
  # L'unità della numerazione prima del numero di busta: vuota = «b.»; a Bologna «aa.» (5/10).
  .col(NA_character_, etichetta = "Sigla prima del numero (vuota: b.)", campo = "sigla_busta", dove = "fascicoli", sezione = "Segnatura"),
  .col(NA_character_, etichetta = "Sigla prima del fascicolo (vuota: fasc.)", campo = "sigla_fascicolo", dove = "fascicoli", sezione = "Segnatura"),

  # «Soggetto conservatore» è eliminato: il modello non ha la colonna, l'ente conservatore sta in «Luogo di conservazione» (sempre un ente).
  
  # Il regesto strutturato. Vengono serializzati DENTRO il Regesto,
  # secondo il template del passo 5, non accanto ad esso.
  .col(NA_character_, etichetta = "Persone non nominate (ignoti): fonte guardata, nessuno da nominare",
       campo = "imputati_esaminati", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  # I tre flag sulla fonte: stato della scheda, mai colonne del modello (arianna = NA: non escono nell'export).
  .col(NA_character_, etichetta = "Reato ignoto (la fonte è stata guardata e non lo dice)",
       campo = "reato_ignoto", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Luogo del reato ignoto (la fonte è stata guardata e non lo dice)",
       campo = "luogo_ignoto", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Non è un procedimento (carte sciolte, catalogo, registro, lettere…)",
       campo = "non_procedimento", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Imputati",                   campo = "imputati",    dove = "imputati", multi = TRUE, widget = "imputati", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Identità delle persone dubbia (per inclusione, non per lettura: es. \"Atti per diversi\")",
       campo = "identita_imputato_dubbia", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  # Con più imputati e più capi, a chi va ciascun capo non è deducibile: 'n.d.' finché nessuno lo dichiara.
  # Con «per persona» le coppie si spuntano nella griglia (campo persona_reati).
  .col(NA_character_, etichetta = "Attribuzione dei capi (con più imputati e più capi)",
       campo = "attribuzione", dove = "fascicoli", widget = "scelta", sezione = "Regesto strutturato"),
  # Specchio di "attribuzione" sull'asse capo->luogo: con più capi e
  # più luoghi citati insieme, quale luogo è teatro di quale capo.
  .col(NA_character_, etichetta = "Attribuzione dei luoghi (con più capi e più luoghi)",
       campo = "attribuzione_luoghi", dove = "fascicoli", widget = "scelta", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Applicazione della tortura", campo = "tortura",     dove = "fascicoli", widget = "scelta",                sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Tortura esaminata nella fonte (spunta anche se la fonte tace)",
       campo = "tortura_esaminata", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Sentenza di spedizione",     campo = "sentenza",    dove = "fascicoli", widget = "scelta",                sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Sentenza esaminata nella fonte (spunta anche se la fonte tace)",
       campo = "sentenza_esaminata", dove = "fascicoli", widget = "flag", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Regesto scritto a mano (non ricomporre)", campo = "regesto_manuale", dove = "fascicoli", widget = "flag", sezione = "Regesto"),

  # Senza questo flag nel modulo, ogni Salva rimetteva titolo_manuale a 0 e
  # ricomponeva il titolo dagli imputati: sulle schede importate, un titolo vuoto.
  .col(NA_character_, etichetta = "Titolo scritto a mano (non ricomporre)", campo = "titolo_manuale", dove = "fascicoli", widget = "flag", sezione = "Identità archivistica"),

  # Tipo e connettore compongono il titolo insieme agli imputati
  # (TODO B10): "Processo contro NOME", "Atti circa NOME e NOME"...
  .col(NA_character_, etichetta = "Tipo",       campo = "tipo_titolo", dove = "fascicoli", widget = "scelta", sezione = "Identità archivistica"),
  .col(NA_character_, etichetta = "Connettore", campo = "connettore",  dove = "fascicoli", widget = "scelta", sezione = "Identità archivistica"),

  # Data assente per dichiarazione, non per dimenticanza: senza estremo remoto e senza questa spunta la maschera non salva. Solo Trama, nessuna colonna Arianna.
  # Le griglie delle coppie (widget «coppie», R/modulo.R) compaiono solo con attribuzione «per persona» / attribuzione_luoghi «per reato».
  .col(NA_character_, etichetta = "Chi risponde di quale capo",
       campo = "persona_reati", dove = "persona_reati", widget = "coppie", sezione = "Regesto strutturato"),
  .col(NA_character_, etichetta = "Quale capo, in quale luogo",
       campo = "reato_luoghi", dove = "reato_luoghi", widget = "coppie", sezione = "Regesto strutturato"),
  # I termini del tesauro sotto le classi scelte (D33): solo la maschera, nessuna colonna del foglio (esce la classe).
  .col(NA_character_, etichetta = "Termini sotto la classe (facoltativi)",
       campo = "reato_termini", dove = "reato_termini", multi = TRUE, widget = "termini", sezione = "Indicizzazione"),

  .col(NA_character_, etichetta = "Fascicolo non consultabile",
       campo = "non_consultabile", dove = "fascicoli", widget = "flag", sezione = "Note"),

  # La colonna «Tipologia documentaria» del foglio dice «Atti», salvo cambio a mano qui (un extra, solo di Trama);
  # il campo tipologia_documentaria regge il tipo del titolo e nel foglio non esce.
  .col(EXTRA_TIPOLOGIA_FOGLIO, etichetta = "Tipologia nel foglio (vuoto: «Atti»)", sezione = "Identità archivistica"),

  .col(EXTRA_DATA_PRECISA[["estremo_remoto"]], etichetta = "Estremo remoto con mese e giorno (facoltativo)", sezione = "Identità archivistica"),
  .col(EXTRA_DATA_PRECISA[["estremo_recente"]], etichetta = "Estremo recente con mese e giorno (facoltativo)", sezione = "Identità archivistica"),

  .col(NA_character_, etichetta = "Data non disponibile nella fonte (n.d.)",
       campo = "data_nd", dove = "fascicoli", widget = "flag", sezione = "Identità archivistica")
)

# --- vocabolari controllati: le chiavi corrispondono alla colonna `campo` del profilo, così il modulo li chiede senza casi speciali.

# I vocabolari del materiale (soggettario, supporti, tipologie, produttori, lingue, definizione) sono del profilo: profili/<nome>/profilo.json.
# Qui restano i valori che servono al programma qualunque sia il materiale (sì/no/n.d., attribuzione, tipo e connettore del titolo).
VOCABOLARI <- list(
  reato = .pd_chr(PROFILO_DOMINIO$vocabolari$reato),

  tortura  = c("n.d.", "sì", "no"),
  sentenza = c("n.d.", "sì", "no"),

  # «per persona» / «per reato» aprono nella maschera la griglia delle coppie (persona_reati / reato_luoghi), che legge R/attribuzione.R.
  attribuzione        = c("n.d.", "collettiva", "per persona"),
  attribuzione_luoghi = c("n.d.", "collettiva", "per reato"),

  tipo_titolo = names(TIPI_TITOLO),
  connettore  = c("circa", "contro", "su", "di"),

  definizione            = .pd_chr(PROFILO_DOMINIO$vocabolari$definizione),
  supporto               = .pd_chr(PROFILO_DOMINIO$vocabolari$supporto),
  tipologia_documentaria = .pd_chr(PROFILO_DOMINIO$vocabolari$tipologia_documentaria),
  soggetto               = .pd_chr(PROFILO_DOMINIO$vocabolari$soggetto),
  lingua                 = .pd_chr(PROFILO_DOMINIO$vocabolari$lingua)
)


# Varianti errate da ricondurre alla forma canonica (SPEC §8).
# «Eresia*»: l'asterisco è rumore del foglio ASMo, non un reato a sé: si riscrive «Eresia» e l'anteprima lo dichiara.
NORMALIZZAZIONI <- .pd_vett(PROFILO_DOMINIO$normalizzazioni)


vocabolario <- function(campo) {
  v <- VOCABOLARI[[campo]]
  if (is.null(v)) character(0) else v
}


# ── vocabolari per fondo e valori controllati (R/colonne.R) ─────
# Stanno qui, accanto a `vocabolario()`, perché li usano l'import e Qualità e ogni collaudo carica profili.R.

# Le colonne a vocabolario controllato che si rivedono per valore. `dove` dice in che
# tabella stanno (reati e lingue sono tabelle figlie, le altre colonne di `fascicoli`).
CAMPI_CONTROLLATI <- data.frame(
  campo     = c("reato", "lingua", "tipologia_documentaria", "supporto", "definizione"),
  dove      = c("reati", "lingue", "fascicoli", "fascicoli", "fascicoli"),
  etichetta = c("Soggettario (reato)", "Lingua", "Tipo del titolo", "Supporto", "Definizione"),
  stringsAsFactors = FALSE)

.riga_campo <- function(campo) CAMPI_CONTROLLATI[CAMPI_CONTROLLATI$campo == campo, , drop = FALSE]


# `forma` vuota: il valore è ammesso per il fondo (Bologna ha «Amministrazione» e «Tribunali locali»). `forma` piena: è una variante,
# e import e correzione in blocco la riscrivono in quella forma.
regole_fondo <- function(con, fondo) {
  fondo <- .fid(con, fondo)     # id o denominazione (R/fondi.R)
  if (.vuoto(fondo)) return(.con_equivalenze(data.frame(campo = character(0), valore = character(0), forma = character(0))))
  .con_equivalenze(dbGetQuery(con, "SELECT campo, valore, forma FROM vocabolario_fondo WHERE fondo_id = ? ORDER BY campo, valore", list(fondo)))
}

# Equivalenze fuori vocabolario, valide per ogni fondo: una voce che la Guida al Tab elenca sotto una classe (Bigamia → «Crimini sessuali»)
# o il plurale per l'astratto («Ebrei» → «Ebraismo») entra nella classe, e la correzione si vede. Le varianti registrate di un fondo le superano.
EQUIVALENZE <- lapply(PROFILO_DOMINIO$equivalenze, .pd_vett)
# Il tesauro dei reati: i termini specifici sotto le classi della Guida (profilo, sezione «tesauro»). Per ora è solo dato: l'import non lo usa ancora.
TERMINI_REATO <- {
  tt <- PROFILO_DOMINIO$tesauro$reato$termini
  if (!length(tt)) data.frame(id = character(0), termine = character(0), classe = character(0), sigla = character(0), origine = character(0), stringsAsFactors = FALSE)
  else do.call(rbind, lapply(tt, function(x) data.frame(id = x$id, termine = x$termine, classe = x$classe, sigla = if (is.null(x$sigla)) NA_character_ else x$sigla,
                                                       origine = x$origine, stringsAsFactors = FALSE)))
}
# Ogni termine (e variante) del tesauro è anche un nome da riportare alla sua classe, come «Bigamia» (D20); in più il termine resta registrato (reato_termini).
.NOMI_TERMINI <- {
  tt <- PROFILO_DOMINIO$tesauro$reato$termini
  r <- do.call(rbind, lapply(tt, function(x) { v <- c(x$termine, .pd_chr(x$varianti))
    data.frame(nome = v, termine = x$termine, classe = x$classe, stringsAsFactors = FALSE) }))
  if (is.null(r)) data.frame(nome = character(0), termine = character(0), classe = character(0), stringsAsFactors = FALSE) else r
}
.chiave_termine <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))
# Fra le equivalenze valgono anche i termini del tesauro; quelle scritte nel profilo restano distinte (`EQUIVALENZE_ESPLICITE`: sono la D20 e hanno la loro dicitura).
EQUIVALENZE_ESPLICITE <- EQUIVALENZE
if (nrow(.NOMI_TERMINI)) {
  nuovi <- stats::setNames(.NOMI_TERMINI$classe, .NOMI_TERMINI$nome)
  nuovi <- nuovi[!(tolower(names(nuovi)) %in% tolower(names(EQUIVALENZE$reato)))]
  EQUIVALENZE$reato <- c(EQUIVALENZE$reato, nuovi)
}
.equivalenze_come_regole <- function() {
  r <- do.call(rbind, lapply(names(EQUIVALENZE), function(k) {
    e <- EQUIVALENZE[[k]]; if (!length(e)) return(NULL)     # un profilo può non averne
    v <- c(names(e), tolower(names(e)))
    data.frame(campo = k, valore = v, forma = unname(c(e, e)), stringsAsFactors = FALSE) }))
  if (is.null(r)) return(data.frame(campo = character(0), valore = character(0), forma = character(0), stringsAsFactors = FALSE))
  r[!duplicated(r$valore), , drop = FALSE]
}
.con_equivalenze <- function(r) {
  g <- .equivalenze_come_regole()
  if (is.null(r) || !nrow(r)) return(g)
  rbind(r[, c("campo", "valore", "forma")], g[!(paste(g$campo, g$valore) %in% paste(r$campo, r$valore)), , drop = FALSE])
}

vocabolario_di <- function(con, campo, fondo = NULL) {
  r <- regole_fondo(con, fondo)
  unique(c(vocabolario(campo), r$valore[r$campo == campo & is.na(r$forma)]))
}

# Una decisione può anche dire «non è un valore di questa colonna: toglilo» («Modena» fra i reati): la forma è FORMA_ELIMINA.
FORMA_ELIMINA <- "(elimina)"

# La forma che l'import dà a un valore: una variante registrata, o la forma del vocabolario se cambiano solo maiuscole e spazi.
# Un refuso vicino non si corregge da solo: si propone. NA = il valore va tolto.
.canonico <- function(x, ok, alias) {
  if (is.na(x)) return(x)
  y <- trimws(gsub("\\s+", " ", x))
  if (y %in% ok) return(y)
  if (y %in% names(alias)) { forma <- unname(alias[[y]]); return(if (identical(forma, FORMA_ELIMINA)) NA_character_ else forma) }
  i <- match(tolower(y), tolower(ok))
  if (!is.na(i)) ok[i] else x
}

# All'import (e solo lì: quello che si scrive a mano nella maschera resta com'è).
# Il fondo si riconosce da archivio + nome (o dall'id, se l'import lo sa già): il solo nome può stare in più archivi.
normalizza_valori_fascicolo <- function(con, f, fondo_id = NULL) {
  if (.vuoto(fondo_id)) fondo_id <- fondo_id_da(con, f$archivio, f$fondo)
  if (.vuoto(fondo_id) || is.na(fondo_id))       # archivio assente o fondo nuovo: il solo nome, se non è in più archivi
    reg <- regole_fondo(con, f$fondo)             # un nome che sta in più archivi è un errore chiaro («indica quale»): non si sceglie una regola in silenzio (19/11/2026)
  else reg <- regole_fondo(con, fondo_id)
  for (i in seq_len(nrow(CAMPI_CONTROLLATI))) {
    campo <- CAMPI_CONTROLLATI$campo[i]; dove <- CAMPI_CONTROLLATI$dove[i]
    chiave <- if (dove == "fascicoli") campo else dove
    v <- f[[chiave]]
    if (!length(v) || all(is.na(v))) next
    r <- reg[reg$campo == campo, , drop = FALSE]
    ok <- unique(c(vocabolario(campo), r$valore[is.na(r$forma)]))
    alias <- stats::setNames(r$forma[!is.na(r$forma)], r$valore[!is.na(r$forma)])
    nuovo <- vapply(as.character(v), .canonico, "", ok = ok, alias = alias, USE.NAMES = FALSE)
    # Il termine del tesauro non si perde nella riscrittura: resta registrato sotto la classe in cui è finito (se una decisione del fondo lo ha mandato altrove, no).
    if (campo == "reato" && nrow(.NOMI_TERMINI)) {
      i <- match(.chiave_termine(v), .chiave_termine(.NOMI_TERMINI$nome))
      t <- data.frame(reato = nuovo, termine = .NOMI_TERMINI$termine[i], classe = .NOMI_TERMINI$classe[i], stringsAsFactors = FALSE)
      t <- t[!is.na(t$termine) & !is.na(t$reato) & t$reato == t$classe, c("reato", "termine"), drop = FALSE]
      if (nrow(t)) f$reato_termini <- unique(t)
    }
    f[[chiave]] <- if (dove == "fascicoli") nuovo else unique(nuovo[!is.na(nuovo)])
  }
  f
}




# ── le quattro regole di normalizzazione ──
# Applicate all'import (fascicolo_da_riga) e, una volta, alle schede già nel database; l'export scrive le forme normalizzate. Non toccano la maschera.
#  1. vocabolari (definizione, supporto, tipologia) nella forma del vocabolario, qualunque siano le maiuscole;
#  2. «Non consultabile» nelle Note diventa il campo non_consultabile;
#  3. il blocco «— persone già citate altrove (auto) — … — fine —» del vecchio Regestum si toglie (è ricalcolabile);
#  4. refusi: spazi doppi nel testo, virgola senza spazio nei nomi («Gallo,Matteo» -> «Gallo, Matteo»).
.CAMPI_A_VOCABOLARIO <- c("definizione", "supporto", "tipologia_documentaria")
.BLOCCO_AUTO   <- "(?s)\\s*— persone già citate altrove \\(auto\\) —.*?— fine —"
# La frase è un marcatore solo a inizio nota o riga: «Precedentemente non consultabile, ora disponibile.» non è lo stato corrente e non si converte.
.NON_CONSULT   <- "(?i)(?<![^\\n])[ \\t]*\\bnon consultabile\\b[[:space:][:punct:]]*"
.CAMPI_TESTO   <- c("titolo", "altra_segnatura", "luogo_produzione", "luogo_conservazione",
                    "regesto", "note")

# Dentro le parentesi stanno le varianti di un nome; il foglio ASMo le separa ora con «|» ora con «;»: forma unica «; ».
.pipe_in_parentesi <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !grepl("|", s, fixed = TRUE) || !grepl("(", s, fixed = TRUE)) return(s)
    car <- strsplit(s, "")[[1]]
    liv <- .livello_parentesi(car)
    dentro <- car == "|" & liv > 0
    car[dentro] <- ";"
    gsub(" *; *", "; ", paste(car, collapse = ""))
  }, "", USE.NAMES = FALSE)
}

normalizza_fascicolo <- function(f) {
  spazi <- function(x) if (length(x) == 0) x else gsub("[ \t]{2,}", " ", x)
  # 1
  for (k in .CAMPI_A_VOCABOLARIO) {
    v <- f[[k]]; voc <- vocabolario(k)
    if (length(v) != 1 || is.na(v) || !length(voc)) next
    i <- match(tolower(trimws(v)), tolower(voc))
    if (!is.na(i)) f[[k]] <- voc[i]
  }
  # 4
  for (k in .CAMPI_TESTO) if (length(f[[k]]) == 1 && !is.na(f[[k]])) f[[k]] <- spazi(f[[k]])
  if (length(f$persone)) f$persone <- gsub(",(?=[^ ,])", ", ", spazi(f$persone), perl = TRUE)
  if (length(f$luoghi))  f$luoghi  <- .pipe_in_parentesi(spazi(f$luoghi))
  if (length(f$persone)) f$persone <- .pipe_in_parentesi(f$persone)
  # Doppioni nelle liste e luogo del reato assente fra i luoghi citati: la maschera li sistema al salvataggio, l'import deve lasciare lo stesso dato.
  if (length(f$reati))   f$reati   <- unique(f$reati)
  if (length(f$persone)) f$persone <- unique(f$persone)
  if (length(f$luoghi))  f$luoghi  <- unique(f$luoghi)
  # Anche il luogo di produzione, che è una sola cella con più voci: «Roma|Roma» tiene una voce sola, come i luoghi citati (audit 18/11).
  if (length(f$luogo_produzione) == 1 && !is.na(f$luogo_produzione) && grepl("|", f$luogo_produzione, fixed = TRUE)) {
    vp <- trimws(strsplit(f$luogo_produzione, "|", fixed = TRUE)[[1]]); f$luogo_produzione <- paste(unique(vp[nzchar(vp)]), collapse = "|")
  }
  if (length(f$luogo_reato) == 1 && !is.na(f$luogo_reato)) {
    nuovi <- setdiff(separa_luoghi(f$luogo_reato), f$luoghi)
    if (length(nuovi)) f$luoghi <- c(nuovi, f$luoghi)
  }

  # 3 e 2
  n <- f$note
  if (length(n) == 1 && !is.na(n)) {
    n <- gsub(.BLOCCO_AUTO, "", n, perl = TRUE)
    if (grepl(.NON_CONSULT, n, perl = TRUE)) {
      f$non_consultabile <- 1L
      n <- gsub(.NON_CONSULT, "", n, perl = TRUE)
    }
    n <- trimws(n)
    f$note <- if (nzchar(n)) n else NA_character_
  }
  f
}

# L'inverso della regola 2, per l'export: Arianna non ha il campo, e
# l'informazione non deve perdersi. La frase torna in testa alle Note.
note_per_export <- function(f) {
  n <- if (length(f$note) == 1 && !is.na(f$note) && nzchar(f$note)) f$note else NULL
  if (isTRUE(suppressWarnings(as.integer(f$non_consultabile)) == 1))
    n <- paste(c("Non consultabile", n), collapse = "\n")
  if (is.null(n)) "" else n
}


normalizza <- function(valori) {
  v <- gsub("\\s+", " ", trimws(as.character(valori)))
  da_sostituire <- v %in% names(NORMALIZZAZIONI)
  v[da_sostituire] <- unname(NORMALIZZAZIONI[v[da_sostituire]])
  v
}

# Il record Electron com'era, come prova di provenienza (porta anche ciò che il modello non ha, es. un ruolo, o quali regesti erano a mano).
# Nascosto, non cancellato: la maschera non lo mostra (profilo_completo, R/modulo.R) e Salva non lo tocca.
EXTRA_NASCOSTI <- "Electron \u00b7 record originale JSON"
# Anche l'identità del foglio d'origine («Importa nuovo fondo»: archivio, fondo, serie e
# conservatore com'erano scritti): resta nel database, la maschera non la mostra (5/10).
EXTRA_PROVENIENZA <- "import_identita_originale"
