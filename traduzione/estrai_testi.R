# ─────────────────────────────────────────────────────────────
# estrai_testi.R: il vocabolario dell'interfaccia da tradurre (30/09)
# Stefano: «se mi dai un vocabolario di tutte le parole usate nell'app
# preparo la traduzione in inglese (anche di suggerimenti etc)».
# Legge app.R e R/*.R con il parser di R (getParseData), prende le
# stringhe scritte nel codice e tiene quelle che un utente legge:
# etichette, pulsanti, suggerimenti, messaggi, titoli dei grafici.
# Scarta codice (SQL, CSS, JavaScript, espressioni regolari, nomi di
# colonne e identificativi). Aggiunge le etichette dei campi dei profili
# e il vocabolario controllato con l'inglese che c'è già (i18n.R).
# Uscita: traduzione/testi_interfaccia.xlsx, colonna «english» vuota da
# riempire. Si rilancia quando il codice cambia: le righe si riconoscono
# dal testo italiano, così una traduzione già fatta si ritrova.
#
#   Rscript traduzione/estrai_testi.R
# ─────────────────────────────────────────────────────────────

suppressMessages({ library(openxlsx) })

file_r <- c("app.R", sort(list.files("R", pattern = "\\.R$", full.names = TRUE)))
# Fuori: il vocabolario (ha il suo foglio), i pezzi con cui si compongono
# titolo, regesto e segnatura e le grammatiche d'estrazione (restano
# italiani: finiscono nel database e nell'export), il tema (CSS, font),
# l'RDF (etichette del dataset, non dello schermo) e lo schema (SQL).
file_r <- setdiff(file_r, c("R/i18n.R", "R/compose.R", "R/estrazione.R", "R/tema.R", "R/rdf.R", "R/schema.R"))

.e_codice <- function(x) {
  grepl("^\\s*(SELECT|INSERT|UPDATE|DELETE|CREATE|PRAGMA|ALTER|WITH|DROP)\\b", x, ignore.case = FALSE) |
  grepl("[{};]\\s*$|\\bfunction\\s*\\(|=>|\\$\\(|document\\.|window\\.|Shiny\\.|var |\\.on\\(|===|!==", x) |
  grepl("^[#.][A-Za-z0-9_-]+", x) |                                          # selettori
  grepl("^[a-z0-9]+(-[a-z0-9]+)+(\\s+[a-z0-9]+(-[a-z0-9]+)*)*$|_", x) & !grepl("\\s[a-zà-ù]{3,}\\s", x) |  # classi css, id
  grepl("COALESCE|TRIM\\(|\\bIS NULL\\b|<>|\\bVALUES\\b|\\bAND [a-z]\\.|install\\.packages|\\($", x) |  # SQL, installazione
  grepl("\\b(btn|text|bg|m[tbsexy]|p[tbsexy]|d|gap|fw|align|justify|flex|col|border|rounded|table|alert|form)-[a-z0-9-]+\\b", x) &
    !grepl("[a-zà-ù]{4,} [a-zà-ù]{4,} [a-zà-ù]{4,}", x) |                    # classi Bootstrap
  grepl("^\\^|\\\\[sdwpbS(]|\\[\\^|\\(\\?:|\\$$", x) |                       # espressioni regolari
  grepl("^%|%[sd0-9.]*[sdf]", x) & !grepl("[a-zà-ù]{3,}", x) |               # formati puri
  grepl("^(https?://|www\\.|[A-Za-z]:\\\\|/)", x) |                         # indirizzi e percorsi
  grepl("^#[0-9A-Fa-f]{3,8}$|^rgba?\\(|\\bpx\\b|\\brem\\b|var\\(--", x) |     # colori e css
  grepl("^[A-Za-z0-9_.]+\\.(R|csv|json|xlsx|sqlite|ttl|js|css|png|md|txt)$", x) |
  grepl("^[a-z_]+[A-Z][A-Za-z]*$", x) |                                     # camelCase
  grepl("<[a-z/][^>]*>", x) |                                                # frammenti HTML
  grepl("constraint|failed|NULL|NA_|stringsAsFactors|ORDER BY|GROUP BY|\\browid\\b", x) |  # messaggi tecnici, SQL
  grepl("\\bWHERE\\b|^ ?AND ?$|IS \\?| IN \\(|ROLLBACK|\\(FROM\\||^input\\.", x) |
  !grepl("[A-Za-z]", x) & !grepl("\\s", x) & nchar(x) > 3 |                    # tabelle di caratteri (àáâ…)
  grepl("^([A-Z][a-z]+){3,}$", x)                                            # TopBottomLeftRight
}
# `riga`: il testo della riga di codice. Una parola sola, minuscola, conta
# solo accanto a un pulsante, un'icona o un'etichetta («tabella», «incompleti»).
.UI_RIGA <- "icon\\(|actionButton|tags\\$button|radioButtons|checkboxInput|textInput|numericInput|selectInput|selectizeInput|input_switch|labs\\(|title *=|placeholder|showNotification|nav_panel|card_header"
.e_testo <- function(x, riga = "") {
  parola_ui <- grepl("^[a-zà-ù][a-zà-ù'’]+$", x) & grepl(.UI_RIGA, riga) &
    !mapply(function(a, b) grepl(paste0('icon("', a, '"'), b, fixed = TRUE), x, riga)   # nomi di icone
  nzchar(trimws(x)) & nchar(x) >= 2 & grepl("[A-Za-zÀ-ÿ]", x) &
    (grepl("\\s", x) | grepl("^[A-ZÀ-Ý][a-zà-ÿ]", x) | grepl("[àèéìòù…«»’]", x) | parola_ui) & !.e_codice(x)
}

# Il contesto: la stessa parola è testo in actionButton("salva", "Salva")
# solo la seconda volta. Una stringa è «tecnica» se in OGNI punto della
# riga in cui compare è: il primo argomento di un input/output (il suo
# identificativo), il valore di un attributo tecnico (class, type, value…),
# il valore di una coppia "etichetta" = "valore", o il termine di un confronto.
.FN_ID <- "(?<![A-Za-z_.])(actionButton|downloadButton|textInput|textAreaInput|numericInput|selectInput|selectizeInput|checkboxInput|radioButtons|input_switch|uiOutput|textOutput|DTOutput|plotOutput|fileInput|updateTextInput|updateSelectizeInput|updateNumericInput|updateCheckboxInput|updateTextAreaInput|nav_select|leggi_impostazione|scrivi_impostazione|attr|getElementById|campo|ns|\\.manda_voci|invariati|sendCustomMessage|setInputValue|match\\.arg)\\([^()]*$"
.ATTR_TECNICI <- c("type", "class", "id", "value", "selected", "inputId", "outputId", "width", "height", "size", "style",
  "role", "target", "rel", "href", "fontface", "face", "family", "fontName", "modo", "widget", "dove", "campo",
  "archivio", "fondo", "serie", "definizione", "supporto", "lingua", "position", "placement", "side", "selection", "dom",
  "method", "sep", "collapse", "pattern", "format", "encoding", "fileEncoding", "mode", "units", "colour", "color", "fill",
  "halign", "valign", "border", "condition", "icon", "name", "by", "what", "open", "variant")
.tecnica <- function(x, riga) {
  ago <- paste0('"', x, '"'); pos <- gregexpr(ago, riga, fixed = TRUE)[[1]]
  if (is.na(riga) || pos[1] < 0) return(FALSE)
  all(vapply(pos, function(i) {
    prima <- substr(riga, 1, i - 1)
    if (grepl(.FN_ID, prima, perl = TRUE) && grepl("\\(\\s*$", prima)) return(TRUE)
    if (grepl("(==|!=|%in% c\\(|\\[\\[|identical\\([^()]*,)\\s*$", prima)) return(TRUE)
    if (grepl("\"\\s*=\\s*$", prima)) return(TRUE)                        # "etichetta" = "valore"
    m <- regmatches(prima, regexpr("[A-Za-z_.`-]+\\s*=\\s*$", prima))
    length(m) == 1 && (gsub("[`=[:space:]]", "", m) %in% .ATTR_TECNICI || grepl("^`?data-", m))
  }, logical(1)))
}

# Blocchi di dati dentro i file dell'interfaccia: tabelle, non schermo.
.BLOCCHI_DATI <- c("ALIAS_COLONNE", "CORREZIONI_SEGNATURA", "PROFILO_PER_ARCHIVIO", "PARTICELLE_COGNOME")
.righe_dei_blocchi <- function(sorgente) {
  fuori <- integer(0)
  for (nome in .BLOCCHI_DATI) {
    a <- grep(paste0("^", nome, " <- "), sorgente)
    for (i in a) {
      # Un'assegnazione su una riga sola (come CORREZIONI_SEGNATURA, ora letta dal profilo) non apre un blocco.
      if (lengths(regmatches(sorgente[i], gregexpr("\\(", sorgente[i]))) <= lengths(regmatches(sorgente[i], gregexpr("\\)", sorgente[i])))) { fuori <- c(fuori, i); next }
      z <- i + which(grepl("^\\)", sorgente[(i + 1):length(sorgente)]))[1]; fuori <- c(fuori, i:z)
    }
  }
  fuori
}

righe <- do.call(rbind, lapply(file_r, function(f) {
  p <- getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  s <- p[p$token == "STR_CONST", c("line1", "col1", "line2", "col2", "text")]
  if (!nrow(s)) return(NULL)
  sorgente <- readLines(f, encoding = "UTF-8", warn = FALSE)
  # Per le stringhe lunghe getParseData dà solo «[N chars quoted…]»: il
  # testo vero si ricostruisce dalle righe del file, con le posizioni.
  lungo <- grepl("^\\[[0-9]+ chars quoted", s$text)
  s$text[lungo] <- vapply(which(lungo), function(i) {
    r <- sorgente[s$line1[i]:s$line2[i]]
    r[length(r)] <- substr(r[length(r)], 1, s$col2[i])
    r[1] <- substring(r[1], s$col1[i])
    paste(r, collapse = "\n")
  }, "")
  s$testo <- vapply(s$text, function(t) tryCatch(eval(parse(text = t)), error = function(e) NA_character_), "")
  s$testo <- enc2utf8(s$testo)
  s <- s[!is.na(s$testo) & validUTF8(s$testo), , drop = FALSE]
  s$riga <- sorgente[s$line1]
  # Le stringhe con JavaScript o CSS dentro non si prendono intere: se ne
  # estraggono i testi fra apici che l'utente legge (title, etichette).
  js <- grepl("function\\s*\\(|\\{[^}]*:[^}]*;", s$testo)
  dal_js <- do.call(rbind, lapply(which(js), function(i) {
    # Via i commenti prima: i loro apostrofi («dell'utente») sballerebbero gli apici.
    codice <- gsub("/\\*[\\s\\S]*?\\*/", "", s$testo[i], perl = TRUE)
    codice <- gsub("(^|[^:])//[^\n]*", "\\1", codice, perl = TRUE)
    t <- regmatches(codice, gregexpr("'(?:[^'\\\\\n]|\\\\.)*'", codice, perl = TRUE))[[1]]
    t <- gsub("^'|'$", "", t)
    t <- vapply(t, function(x) tryCatch(eval(parse(text = paste0('"', gsub('"', '\\\\"', x), '"'))),
                                        error = function(e) x), "", USE.NAMES = FALSE)
    t <- enc2utf8(t); t <- t[validUTF8(t)]
    # Nel JavaScript i testi per l'utente cominciano con la maiuscola (o
    # sono «altro…»); le minuscole sono classi, eventi, selettori, tasti.
    t <- t[.e_testo(t) & grepl("[a-zà-ù]{3,}", t) & (grepl("^[A-ZÀ-Ý↺]", t) | grepl("…", t)) &
           !grepl("^(Arrow|Enter$|Escape$|Tab$|Home$|End$|Backspace$)|\\[", t)]
    if (length(t)) data.frame(testo = t, line1 = s$line1[i], stringsAsFactors = FALSE)
  }))
  s <- s[!js, , drop = FALSE]
  s <- s[nchar(s$testo) < 2000 & !s$line1 %in% .righe_dei_blocchi(sorgente), , drop = FALSE]
  s <- s[.e_testo(s$testo, s$riga) & !mapply(.tecnica, s$testo, s$riga), c("testo", "line1"), drop = FALSE]
  s <- rbind(s, dal_js)
  if (is.null(s) || !nrow(s)) return(NULL)
  data.frame(italiano = s$testo, dove = paste0(f, ":", s$line1), stringsAsFactors = FALSE)
}))

# Le intestazioni delle colonne Arianna e i valori del vocabolario non sono
# interfaccia: le prime restano italiane per contratto (export), i secondi
# hanno il loro foglio. profili.R è solo quello: fuori per intero.
suppressMessages(for (f in c("R/compose.R", "R/profili.R", "R/i18n.R")) source(f))
contratto <- trimws(unique(c(unlist(lapply(PROFILI, function(p) p$arianna)),
                             unlist(lapply(VOCABOLARI_EN, names)))))
righe <- righe[!trimws(righe$italiano) %in% contratto & !grepl("^R/profili\\.R:", righe$dove), , drop = FALSE]

# Gli intrusi che nessuna regola riconosce, trovati rileggendo il foglio
# riga per riga (Stefano, 1/10): identificativi composti a pezzi, nomi
# interni, il marchio, i nomi dei profili, le abbreviazioni della
# segnatura (b., fasc.: restano italiane come la segnatura).
.NON_TRADURRE <- c("applica", "attiva", "condition", "luoghi", "persone", "Sel", "Trama", "English", "Generale",
                   "b. ", ", b.", ", f.", ", fasc. ", "_MENU_ per pagina", "completo", "titolo")
righe <- righe[!righe$italiano %in% .NON_TRADURRE, , drop = FALSE]

# E i testi veri che il parser non vede come stringhe di testo: nomi di
# colonna usati come intestazioni, parole singole minuscole, pezzi del
# linguaggio di DataTables. Trovati con tramaTraduci.nonTradotti() girando
# l'app in inglese scheda per scheda (1/10).
.AGGIUNTI <- c("completa", "facoltativo", "identità dubbia", "reato ignoto", "luogo ignoto", "non è un procedimento", "tortura", "sentenza", "Tutte le %d persone", "Usa %d", "%d su %d", "accettata", "standard", "aggiuntiva", "Soggettario (reato)", "Tipo del titolo", "Reato", "Produttore", "Conservazione", "Materiale", "Cambia", "Aggiungi", "esaminata", "Salto (anni)",
               "Attestazioni", "Identità", "Verifica", "per pagina", "nessuna", "ultima ", "Segnatura", "Titolo", "Luoghi citati",
               "Persone citate", "Tortura", "Sentenza", "Stato", "Anno", "adesso", "Completo", "%d su 4", "incompleti", "Home: lo stato del fondo e da dove riprendere", "%d campi fondamentali su 4", "4 campi fondamentali su 4",
               ": passa da un gruppo di pillole al successivo; ", ": da una pillola all'altra; ", ": aggiunge ciò che hai scritto; ")
righe <- rbind(righe, data.frame(italiano = setdiff(.AGGIUNTI, righe$italiano), dove = "aggiunto a mano (estrai_testi.R)",
                                 stringsAsFactors = FALSE))

# Stesso testo in più punti: una riga sola, con tutti i luoghi.
interfaccia <- aggregate(dove ~ italiano, righe, function(x) paste(unique(x), collapse = ", "))
interfaccia$tipo <- ifelse(grepl("[.!?:]$", interfaccia$italiano) & nchar(interfaccia$italiano) > 40, "messaggio o suggerimento",
                    ifelse(nchar(interfaccia$italiano) <= 30, "etichetta o pulsante", "testo"))
interfaccia <- interfaccia[order(interfaccia$tipo, tolower(interfaccia$italiano)), c("italiano", "tipo", "dove")]
interfaccia$english <- ""

# Etichette dei campi (profili) e vocabolario controllato con l'inglese attuale.
suppressMessages(for (f in c("R/compose.R", "R/profili.R", "R/i18n.R")) source(f))
# Anche i campi propri di Trama (CAMPI_TRAMA: imputati, tortura esaminata,
# «scritto a mano»…), che stanno nel profilo completo e non nei fogli Arianna.
suppressMessages(source("R/modulo.R"))
campi <- unique(c(unlist(lapply(PROFILI, function(p) p$etichetta)),
                  unlist(lapply(c("Generale", names(PROFILI)), function(n) profilo_completo(n)$etichetta))))
campi <- data.frame(italiano = sort(campi[!is.na(campi) & nzchar(campi)]), tipo = "etichetta di campo",
                    dove = "R/profili.R", english = "", stringsAsFactors = FALSE)
voc <- do.call(rbind, lapply(names(VOCABOLARI_EN), function(k)
  data.frame(campo = k, italiano = names(VOCABOLARI_EN[[k]]), english = unname(VOCABOLARI_EN[[k]]),
             stringsAsFactors = FALSE)))

# Le traduzioni già scritte in un file precedente si conservano.
uscita <- "traduzione/testi_interfaccia.xlsx"
if (file.exists(uscita)) {
  for (foglio in c("Interfaccia", "Campi")) {
    vecchio <- tryCatch(read.xlsx(uscita, foglio), error = function(e) NULL)
    if (is.null(vecchio) || !"english" %in% names(vecchio)) next
    d <- if (foglio == "Interfaccia") interfaccia else campi
    # Excel può cambiare gli a capo (\r\n) e gli spazi ai bordi: si confronta senza.
    chiave <- function(x) trimws(gsub("\r", "", x))
    i <- match(chiave(d$italiano), chiave(vecchio$italiano))
    d$english[!is.na(i)] <- ifelse(is.na(vecchio$english[i[!is.na(i)]]), "", vecchio$english[i[!is.na(i)]])
    # La nota «traduzione automatica, da rivedere» segue la riga.
    if ("tipo" %in% names(vecchio)) { nota <- grepl("traduzione automatica", vecchio$tipo[i]) & !is.na(i)
      d$tipo[nota] <- paste0(d$tipo[nota], " \u00b7 traduzione automatica, da rivedere") }
    if (foglio == "Interfaccia") interfaccia <- d else campi <- d
  }
}

# I pezzi fissi dei titoli composti («Processo contro …», «Atti per diversi»):
# nel database restano italiani, ma a schermo in inglese si mostrano
# tradotti (Stefano, 2/10: «numerosi campi non tradotti in title»). I nomi
# delle persone non si toccano. L'inglese proposto automaticamente si corregge qui.
titoli <- data.frame(stringsAsFactors = FALSE,
  # «regesto» (5/10): i pezzi in più del regesto composto narrativo («Processi
  # per eresia; magia contro A e B», «Informazioni circa ignoti»).
  parte    = c(rep("tipo", 8), rep("connettore", 4), "coda", "fra i nomi", rep("regesto", 4)),
  italiano = c("Atti", "Processo", "Copia del processo", "Informazioni", "Atti difensivi", "Denuncia", "Denunce",
               "Atti del processo", "contro", "circa", "su", "di", "per diversi", "e",
               "Processi", "per", "ignoti", "diversi soggetti"),
  english  = c("Proceedings", "Trial", "Copy of the trial", "Informations", "Defence papers", "Denunciation", "Denunciations",
               "Trial proceedings", "against", "concerning", "on", "of", "against several persons", "and",
               "Trials", "for", "persons unknown", "several persons"))
if (file.exists(uscita)) {
  vecchio <- tryCatch(read.xlsx(uscita, "Titoli"), error = function(e) NULL)
  if (!is.null(vecchio)) { i <- match(paste(titoli$parte, titoli$italiano), paste(vecchio$parte, vecchio$italiano))
    ok <- !is.na(i) & !is.na(vecchio$english[i]) & nzchar(vecchio$english[i]); titoli$english[ok] <- vecchio$english[i[ok]] }
}

wb <- createWorkbook()
testa <- createStyle(textDecoration = "bold", fgFill = "#EDE5D2", border = "bottom")
a_capo <- createStyle(wrapText = TRUE, valign = "top")
leggimi <- data.frame(Istruzioni = c(
  "Scrivi l'inglese nella colonna «english». Non cambiare la colonna «italiano»: serve a ritrovare il testo nel codice.",
  "Interfaccia: pulsanti, etichette, suggerimenti e messaggi scritti nel codice (colonna «dove» = file:riga).",
  "Campi: le etichette dei campi della scheda (profili ASDNa, ASMo, Generale).",
  "Vocabolario: reati, tortura, sentenza… hanno già un inglese (i18n.R): correggilo se serve.",
  "Titoli: i pezzi fissi dei titoli composti (Processo, contro, per diversi…). A schermo «Processo contro Mario Rossi» diventa «Trial against Mario Rossi»; nel database resta italiano.",
  "I pezzi fra %s o %d sono numeri o nomi che l'app inserisce: lasciali nella traduzione, anche in altro ordine.",
  "Alcune righe sono frammenti di una frase composta a pezzi (es. «» diventa una variante di «»): traducili lasciando spazi e segni ai bordi; la colonna «dove» dice dove guardare.",
  "Se una riga non è testo per l'utente (un nome tecnico sfuggito al filtro), lascia «english» vuota e scrivi «no» nella colonna «tipo».",
  "Il database e gli export restano in italiano: si traduce solo ciò che si legge sullo schermo."))
for (x in list(list("Leggimi", leggimi, 110), list("Interfaccia", interfaccia, c(60, 22, 40, 60)),
               list("Campi", campi, c(45, 18, 14, 45)), list("Vocabolario", voc, c(24, 36, 36)),
               list("Titoli", titoli, c(16, 30, 36)))) {
  addWorksheet(wb, x[[1]]); writeData(wb, x[[1]], x[[2]], headerStyle = testa)
  setColWidths(wb, x[[1]], seq_along(x[[3]]), x[[3]])
  addStyle(wb, x[[1]], a_capo, rows = 2:(nrow(x[[2]]) + 1), cols = seq_len(ncol(x[[2]])), gridExpand = TRUE)
  freezePane(wb, x[[1]], firstRow = TRUE)
}
saveWorkbook(wb, uscita, overwrite = TRUE)
cat(sprintf("Interfaccia: %d testi · Campi: %d · Vocabolario: %d voci (già in inglese)\n→ %s\n",
            nrow(interfaccia), nrow(campi), nrow(voc), uscita))
