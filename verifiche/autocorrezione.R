# ─────────────────────────────────────────────────────────────
# verifiche/autocorrezione.R: l'autocorrezione di battitura e la maiuscola dei Luoghi citati (16/10)
# JS_AUTOCORREZIONE (R/modulo.R) gira nel browser: qui la si estrae dal file e la si prova con Node (senza browser): `autocorreggi`,
# `maiuscolaIniziale` e l'aggancio ai menu (la voce NUOVA di un menu con l'opzione `maiuscolaIniziale` nasce con la prima lettera maiuscola;
# le altre voci no). Se Node non c'è, il collaudo si salta e lo dice.
#     Rscript verifiche/autocorrezione.R        atteso: ESITO: ok
# ─────────────────────────────────────────────────────────────
nodo <- Sys.which("node")
if (!nzchar(nodo)) { cat("SALTATO: Node non è installato\nESITO: ok - saltato\n"); quit(status = 0) }
src <- paste(readLines("R/modulo.R", encoding = "UTF-8", warn = FALSE), collapse = "\n")
i <- regexpr('JS_AUTOCORREZIONE <- "', src, fixed = TRUE); stopifnot(i > 0)
j <- regexpr('\n"(\n|$)', substr(src, i, nchar(src)), perl = TRUE); stopifnot(j > 0)      # la costante è l'ultima cosa del file
js_r <- substr(src, i + nchar('JS_AUTOCORREZIONE <- "') - 1L, i + j - 1L)           # il letterale R, con i suoi escape
js <- eval(parse(text = paste0('"', sub('^"', "", sub('"$', "", trimws(js_r, "right"))), '"')))   # → il testo JavaScript vero
prova <- r"(
const gestori = {};
global.window = global;
global.document = {};
global.$ = function(x) { return { on: function(ev, a, b) { gestori[typeof a === 'function' ? ev : ev + ' ' + a] = (typeof a === 'function' ? a : b); return this; }, each: function() { return this; }, trigger: function() { return this; } }; };
$.fn = {};
eval(CODICE);
let ko = [];
const eq = (nome, a, b) => { if (a !== b) ko.push(nome + ': atteso «' + b + '», ottenuto «' + a + '»'); };
eq('maiuscola semplice', window.maiuscolaIniziale('napoli'), 'Napoli');
eq('già maiuscola', window.maiuscolaIniziale('Napoli'), 'Napoli');
eq('solo la prima lettera', window.maiuscolaIniziale('san possidonio'), 'San possidonio');
eq('lettera accentata', window.maiuscolaIniziale('élan'), 'Élan');
eq('apostrofo davanti resta', window.maiuscolaIniziale("'s-hertogenbosch"), "'S-hertogenbosch");
eq('numero invariato', window.maiuscolaIniziale('1234'), '1234');
eq('vuoto invariato', window.maiuscolaIniziale(''), '');
eq('autocorreggi: doppia maiuscola', window.autocorreggi('MAria'), 'Maria');
eq('autocorreggi: spazi doppi', window.autocorreggi('Roma  Vecchia'), 'Roma Vecchia');
function falso(opzioni) { const el = { selectize: { settings: Object.assign({ create: true, valueField: 'value', labelField: 'text' }, opzioni) } }; gestori['shiny:bound']({ target: el }); return el.selectize.settings.create; }
const luoghi = falso({ maiuscolaIniziale: true });
eq('menu dei luoghi: «napoli» nasce «Napoli»', luoghi('napoli').value, 'Napoli');
eq('menu dei luoghi: spazi davanti tolti', luoghi('  modena').value, 'Modena');
eq('menu dei luoghi: anche l\'etichetta', luoghi('napoli').text, 'Napoli');
eq('menu dei luoghi: autocorrezione e maiuscola insieme', luoghi('mAria  nova').value, 'Maria nova');
const altro = falso({});
eq('un altro menu non maiuscola', altro('napoli').value, 'napoli');
if (ko.length) { console.log('NON TORNA: ' + ko.join(' | ')); process.exit(1); }
console.log('ok');
)"
f <- tempfile(fileext = ".js"); writeLines(c(paste0("const CODICE = ", jsonlite::toJSON(js, auto_unbox = TRUE), ";"), prova), f)
r <- suppressWarnings(system2(nodo, f, stdout = TRUE, stderr = TRUE)); st <- attr(r, "status") %||% 0L
cat(r, sep = "\n")
if (st != 0L || !identical(tail(r, 1), "ok")) { cat("ESITO: NON TORNA\n"); quit(status = 1) }
cat("ESITO: ok - autocorrezione e maiuscola iniziale dei Luoghi citati\n")
