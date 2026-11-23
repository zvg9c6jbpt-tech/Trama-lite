# ─────────────────────────────────────────────────────────────
# verifiche/secondo_profilo.R: la separazione fra metodo e profilo regge con un secondo profilo?
# Parte il programma con un profilo di tutt'altro materiale (verifiche/profili_prova/epistolario: un epistolario privato, non un materiale vero)
# in un processo a parte (TRAMA_PROFILI / TRAMA_PROFILO) e controlla che:
#   - tutto il codice carichi e l'app parta con quel profilo;
#   - i vocabolari siano quelli del secondo profilo e non quelli dell'Inquisizione;
#   - le convenzioni dell'Inquisizione non scattino (Roma non diventa «Sant'Uffizio», «S.t» non si scioglie, niente equivalenze);
#   - il foglio d'esempio dell'Inquisizione, letto con l'altro profilo, non spenga i controlli: i suoi reati sono valori nuovi da decidere.
#     Rscript verifiche/secondo_profilo.R
# ─────────────────────────────────────────────────────────────
falliti <- character(); v <- function(nome, ok) { cat(if (isTRUE(ok)) "ok  " else "NO  ", nome, "\n"); if (!isTRUE(ok)) falliti <<- c(falliti, nome) }
script <- tempfile(fileext = ".R")
writeLines(c(
  'Sys.setenv(TRAMA_COLLAUDO = "1")',
  'suppressMessages({ library(DBI); for (f in sort(list.files("R", pattern = "[.]R$", full.names = TRUE))) source(f) })',
  'ap <- "\\u0027"',
  'u <- function(k, x) cat(k, "=", paste(x, collapse = "~"), "\\n", sep = "")',
  'u("nome", PROFILO_DOMINIO$profilo$nome)',
  'u("reati", VOCABOLARI$reato)',
  'u("anni", ANNI_AMMESSI)',
  'u("s_t", sciogli_santi("S.t Severo"))',
  'u("produttore", .produttore_canonico(c("Archivio della Famiglia di Rossi", "famiglia di verdi", "Inquisizione di Roma")))',
  'u("citta", citta_da_produttore(c("Famiglia di Rossi", paste0("Sant", ap, "Uffizio"))))',
  'u("equivalenze", nrow(.equivalenze_come_regole()))',
  'u("sovrano_re", .formula_sovrano("Carlo (Charles; re)"))',
  'u("sovrano_papa", .formula_sovrano("Paolo (Paolo; papa)"))',
  'a <- leggi_arianna("dati/esempio/esempio.xlsx"); pr <- proposte_import(a, NULL, NULL)',
  'u("voc_decisioni", sum(vapply(pr$decisioni, function(d) identical(as.character(d$tipo)[1], "vocabolario"), TRUE)))',
  'u("app_parte", tryCatch({ shiny::testServer(".", { invisible(nrow(dati())) }); "si" }, error = function(e) conditionMessage(e)))'),
  script, useBytes = TRUE)
fuori <- system2("Rscript", script, env = c("TRAMA_PROFILI=verifiche/profili_prova", "TRAMA_PROFILO=epistolario"), stdout = TRUE, stderr = TRUE)
campo <- function(k) { r <- grep(paste0("^", k, "="), fuori, value = TRUE); if (length(r)) sub(paste0("^", k, "="), "", r[1]) else NA_character_ }
v("il programma carica e risponde con il secondo profilo", identical(campo("nome"), "epistolario"))
v("i vocabolari sono quelli del secondo profilo", identical(campo("reati"), "Corrispondenza familiare~Corrispondenza d'affari"))
v("gli anni ammessi sono quelli del secondo profilo", identical(campo("anni"), "1500~1800"))
v("«S.t» non si scioglie (regola dell'altro profilo)", identical(campo("s_t"), "S.t Severo"))
v("il produttore segue il secondo profilo; Roma non è Sant'Uffizio", identical(campo("produttore"), "Famiglia di Rossi~Famiglia di Verdi~Inquisizione di Roma"))
v("«Sant'Uffizio» non indica più Roma", identical(campo("citta"), "Rossi"))
v("senza equivalenze il programma regge", identical(campo("equivalenze"), "0"))
v("i titoli dei sovrani sono del profilo", identical(campo("sovrano_re"), "TRUE") && identical(campo("sovrano_papa"), "FALSE"))
v("i reati dell'Inquisizione sono valori nuovi da decidere", !is.na(campo("voc_decisioni")) && as.integer(campo("voc_decisioni")) > 0L)
v("l'app parte e legge i dati", identical(campo("app_parte"), "si"))
if (length(falliti)) { cat("\nFALLITI:", length(falliti), "\n"); if (!is.na(campo("nome"))) NULL else cat(paste(utils::tail(fuori, 15), collapse = "\n"), "\n"); quit(status = 1) }
cat("ESITO: ok - il programma gira con un secondo profilo\n")
