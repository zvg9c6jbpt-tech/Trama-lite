# ──────────────────────────────────────────────
# verifiche/revisione_browser.R: la finestra di revisione dell'import in un browser vero (21/10/2026)
# Il JS della finestra non si prova con testServer: qui si apre l'app in un browser e si controlla che, per un luogo ignoto,
# «Aggiungi alla lista» ed «Elimina dalla scheda» si escludano a vicenda, e che «Togli tutte» svuoti tutte e due.
# Serve shinytest2 + chromote e un browser: se mancano, o senza TRAMA_BROWSER=1, il collaudo si salta e lo dice (riga `SALTATO: …`, ESITO: ok).
# Browser: quello di sistema, o `CHROMOTE_CHROME=/Applications/Brave\ Browser.app/Contents/MacOS/Brave\ Browser`.
# Database e cartella dati: temporanei, mai quelli veri.
#     TRAMA_BROWSER=1 Rscript verifiche/revisione_browser.R        atteso: ESITO: ok
# ──────────────────────────────────────────────
if (!identical(Sys.getenv("TRAMA_BROWSER"), "1") || !requireNamespace("shinytest2", quietly = TRUE) || !requireNamespace("chromote", quietly = TRUE)) {
  cat("SALTATO: serve TRAMA_BROWSER=1, shinytest2 e un browser\nESITO: ok - saltato\n"); quit(status = 0)
}
if (!nzchar(Sys.getenv("CHROMOTE_CHROME")) && file.exists("/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"))
  Sys.setenv(CHROMOTE_CHROME = "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
Sys.setenv(NOT_CRAN = "true")      # senza, shinytest2 si rifiuta di partire («On CRAN»)
Sys.setenv(TRAMA_DB = tempfile(fileext = ".sqlite"), TRAMA_HOME = tempfile("trama-home-"))
col <- names(readxl::read_excel("dati/modello_export.xlsx", n_max = 0))
riga <- stats::setNames(as.list(rep(NA_character_, length(col))), col); riga[["Segnatura"]] <- "b. 1, fasc. 1"; riga[["Luoghi citati o indicizzati"]] <- "Napoli|Xyzzy|Qwerty"
foglio <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(as.data.frame(riga, check.names = FALSE), foglio)
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
app <- shinytest2::AppDriver$new(".", name = "revisione", load_timeout = 60000, timeout = 20000)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)
app$upload_file(file_import = foglio)
app$run_js("Shiny.setInputValue('modo_import', 'nuovo'); null")
app$set_inputs(import_archivio = "Archivio Browser", import_fondo = "Fondo Browser", wait_ = FALSE)
app$wait_for_idle(); app$click("esegui_import")
app$wait_for_js("document.querySelector('.rev-luoghi-ignoti') !== null", timeout = 30000)
# I due gruppi di caselle: «Aggiungi» = id rev_dec_<n>, «Elimina» = id rev_dec_del_<n> (il secondo comincia come il primo: si distinguono col :not).
GRUPPO <- c(agg = ".rev-luoghi-ignoti [id^=rev_dec_]:not([id^=rev_dec_del_])", del = ".rev-luoghi-ignoti [id^=rev_dec_del_]")
stato <- function() jsonlite::fromJSON(app$get_js(sprintf("JSON.stringify({agg: Array.from(document.querySelectorAll('%s input:checked')).map(function(i) { return i.value; }), del: Array.from(document.querySelectorAll('%s input:checked')).map(function(i) { return i.value; })})", GRUPPO[["agg"]], GRUPPO[["del"]])))
casella <- function(gruppo, valore) sprintf("$('%s input').filter(function() { return this.value === '%s'; }).click(); null", GRUPPO[[gruppo]], valore)
app$run_js(casella("agg", "Xyzzy")); app$wait_for_idle()
s <- stato(); v("Aggiungi Xyzzy: spuntato", identical(s$agg, "Xyzzy") && length(s$del) == 0L)
app$run_js(casella("del", "Xyzzy")); app$wait_for_idle()
s <- stato(); v("Elimina Xyzzy toglie Aggiungi Xyzzy", length(s$agg) == 0L && identical(s$del, "Xyzzy"))
app$run_js(casella("agg", "Qwerty")); app$wait_for_idle()
s <- stato(); v("altro luogo: Aggiungi Qwerty non tocca Elimina Xyzzy", identical(s$agg, "Qwerty") && identical(s$del, "Xyzzy"))
app$run_js("$('button:contains(\"Togli tutte\")').click(); null"); app$wait_for_idle()
s <- stato(); v("«Togli tutte» svuota tutte e due", length(s$agg) == 0L && length(s$del) == 0L)
cat(if (length(falliti)) sprintf("ESITO: NON TORNA (%s)\n", paste(falliti, collapse = "; ")) else "ESITO: ok\n")
if (length(falliti)) quit(status = 1)
