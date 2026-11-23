# ──────────────────────────────────────────────
# verifiche/formati_vecchi.R: i fogli nel formato vecchio (ASDNa: Torino, Bologna) si normalizzano come quelli nel modello (22/10/2026)
# Nato dal parere di Codex e da una prova sui sei tab veri (Torino, Bologna, Dublino, Napoli): con i nomi del vecchio profilo quasi tutti i
# controlli saltavano in silenzio, «m.1, inv.» e «m.1, non inv.» erano la stessa scheda (82 righe respinte su 186) e perdevano «inv.»,
# il regesto in prosa dava una ventina di avvisi inutili, un indirizzo nelle note riceveva un punto che lo rompeva.
#     Rscript verifiche/formati_vecchi.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
Sys.setenv(TRAMA_COLLAUDO = "1")
suppressMessages({ library(DBI); library(RSQLite); library(uuid); library(jsonlite); library(readxl); library(openxlsx) })
source("verifiche/aiuti.R")
suppressMessages(for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f))
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }

# 1. un foglio ASDNa costruito a mano (intestazioni del vecchio profilo)
intest <- PROFILI[["ASDNa"]]$arianna
d <- as.data.frame(matrix(NA_character_, 3, length(intest), dimnames = list(NULL, intest)), stringsAsFactors = FALSE)
riga <- function(i, ...) { x <- list(...); for (k in names(x)) d[[intest[trimws(intest) == k]]][i] <<- x[[k]] }      # le intestazioni vere hanno spazi in coda
base <- "Archivio di Stato di Torino, Materie ecclesiastiche, Inquisizione"
riga(1, Segnatura = paste0(base, ", m.1, inv., f.1"), Titolo = "Processo contro Mario Rossi", `Estremo remoto` = "1600",
     `Regesto o descrizione sintetica` = "Processo per eresia contro Mario Rossi", `Persone citate` = "Rossi, Mario|Bianchi Luca|Anna di Mosca",
     `Luoghi citati` = "Londra", Soggettario = "Eresia", Lingua = "Francese|Italiano", `Soggetto produttore` = "Inquisizione di Roma",
     `Tipologia documentaria` = "Unità archivistiche", `Note libere` = "Immagini: https://example.org/view/175857")
riga(2, Segnatura = paste0(base, ", m.1, non inv., f.1"), Titolo = "Processo contro Mario Rossi", `Estremo remoto` = "1601",
     `Regesto o descrizione sintetica` = "Richiesta del braccio per l'arresto di Mario Rossi per poligamia", `Persone citate` = "Rossi, Mario", Soggettario = "Eresia")
riga(3, Segnatura = paste0(base, ", m.1, inv., f.2"), Titolo = "Fascicolo", `Estremo remoto` = "1602", `Regesto o descrizione sintetica` = "Fascicolo processuale ccontro Michele Boccella")
p <- tempfile(fileext = ".xlsx"); write.xlsx(d, p)
a <- leggi_arianna(p)
v("il foglio è letto come ASDNa", identical(a$profilo, "ASDNa"))
dest <- apri_db(":memory:"); pr <- proposte_import(a, dest, NULL)
cella <- function(col, riga) { k <- which(pr$celle$colonna == col & pr$celle$riga == riga); if (length(k)) pr$celle$dopo[k[1]] else NA_character_ }
v("Lingua: «Francese|Italiano» → «francese; italiano»", identical(cella("Lingua", 2L), "francese; italiano"))
v("Soggetto produttore: Roma è «Sant'Uffizio»", identical(cella("Soggetto produttore", 2L), "Sant'Uffizio"))
v("Tipologia documentaria fuori dalla guida → «Atti»", identical(cella("Tipologia documentaria", 2L), "Atti"))
v("il refuso «ccontro» (formato vecchio) è proposto", identical(cella("Regesto o descrizione sintetica", 4L), "Fascicolo processuale contro Michele Boccella"))
testi <- vapply(pr$decisioni, function(x) x$testo, "")
v("«Bianchi Luca» senza virgola in una cella con virgole: si decide", any(grepl("«Bianchi Luca» non ha la virgola", testi, fixed = TRUE)))
v("«Anna di Mosca» non è «Cognome Nome»: nessuna domanda sulla virgola", !any(grepl("«Anna di Mosca» non ha la virgola", testi, fixed = TRUE)))
v("la prosa del regesto («per l'arresto di … per poligamia») non è letta come elenco di reati", !any(grepl("non è una classe", testi, fixed = TRUE) | grepl("due «per»", testi, fixed = TRUE)))
v("nessuna segnalazione di sequenza fra «inv.» e «non inv.»", !any(grepl("sequenza", testi, fixed = TRUE)))

# 2. «inv.» e «non inv.» sono due serie
s1 <- .scomponi_segnatura(paste0(base, ", m.1, inv., f.1")); s2 <- .scomponi_segnatura(paste0(base, ", m.1, non inv., f.1"))
v("«m.1, inv.» tiene la qualifica nel suffisso della busta", identical(s1$busta_suffisso, ", inv.") && identical(s2$busta_suffisso, ", non inv."))
v("una qualifica dopo il fascicolo o prima della busta non sposta nulla (la serie «Sottoserie» resta non letta)",
  length(.scomponi_segnatura("Archivio, Fondo, Serie, Sottoserie, b. 3, fasc. 2")$non_letto) == 1L)
a2 <- applica_proposte(a, pr, pr$celle$id[pr$celle$consigliata], list())
e <- importa_arianna(dest, a2, modo = "accoda")
v("tutte e tre le righe entrano (prima 'non inv.' era «già occupata»)", e$importati == 3L)
out <- tempfile(fileext = ".xlsx"); esporta_arianna(dest, out, fondo = elenco_fondi(dest)$id[1])
x <- as.data.frame(read_excel(out, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
v("la segnatura esportata conserva «inv.» e «non inv.»", sum(grepl(", inv.,", x$Segnatura, fixed = TRUE)) == 2L && sum(grepl(", non inv.,", x$Segnatura, fixed = TRUE)) == 1L)
v("l'indirizzo in fondo alle note non riceve il punto", any(x[["Note libere"]] == "Immagini: https://example.org/view/175857"))
pr2 <- proposte_import(leggi_arianna(out), apri_db(":memory:"), NULL)
v("il foglio esportato non ha più correzioni sicure da proporre", !any(pr2$celle$consigliata))

# 3. spazi doppi nella segnatura: dal 23/10 (D28, caso 51 del corpus) è una correzione proposta; qui il foglio è importato com'è, senza finestra: la riga è respinta e il motivo dà la forma giusta
sorg <- apri_db(":memory:"); f <- fascicolo_vuoto(FALSE); f$archivio <- "Archiginnasio"; f$fondo <- "Ms."; f$busta <- 1860L; f$titolo <- "T"; f$titolo_manuale <- 1L
salva_fascicolo(sorg, f); b <- tempfile(fileext = ".xlsx"); esporta_arianna(sorg, b, fondo = elenco_fondi(sorg)$id[1])
dd <- as.data.frame(read_excel(b, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE)
v("«b.  1860» con due spazi: respinta, con la forma giusta nel motivo", grepl("forma giusta: «Archiginnasio, Ms., b. 1860»", segnatura_non_ammessa("Archiginnasio, Ms., b.  1860"), fixed = TRUE))

# 4. un duplicato esatto non si chiama «Lascia com'è»: il database ne tiene uno
dd$Segnatura <- "Archiginnasio, Ms., b. 1860"; dd[["Persone citate o indicizzate"]] <- "Rossi, Mario|Rossi, Mario"; q2 <- tempfile(fileext = ".xlsx"); write.xlsx(dd, q2)
pr4 <- proposte_import(leggi_arianna(q2), apri_db(":memory:"), NULL)
dup <- Filter(function(x) grepl("duplicato esatto", x$testo, fixed = TRUE), pr4$decisioni)
v("il duplicato esatto dice che se ne tiene una sola", length(dup) == 1L && grepl("una voce sola", dup[[1]]$lascia %||% "", fixed = TRUE))

# 5. il tratto dopo «per»
v("«l'arresto di Mario Rossi» non è un reato", !.sembra_un_reato("l'arresto di Mario Rossi"))
v("«Pietro Terranei» è una persona", !.sembra_un_reato("Pietro Terranei"))
v("«stregoneria» e «bigamia» sì", .sembra_un_reato("stregoneria") && .sembra_un_reato("bigamia"))
v("«eresia per diversi» (ordine sbagliato delle parti) sì: resta l'avviso dei due «per»", .sembra_un_reato("eresia per diversi"))

if (length(falliti)) { cat("\nESITO: NON TORNA (", paste(falliti, collapse = "; "), ")\n"); quit(status = 1) } else cat("\nESITO: ok\n")
