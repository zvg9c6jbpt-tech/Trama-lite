#!/bin/bash
# ─────────────────────────────────────────────────────────────
# distribuzione/windows/costruisci.sh - Trama-Lite-Windows.zip, costruito da Mac o da Linux (29/10/2026)
#
# Per chi ha GIÀ R installato su Windows: lo zip contiene il programma, avvia.bat e una cartella «libreria» con i pacchetti
# già pronti per Windows (i binari di CRAN, con le loro dipendenze). Si scompatta, doppio clic su avvia.bat: niente
# installazioni, niente rete. R stesso non è nello zip: lo installa chi lo usa (https://cran.r-project.org).
#
#     bash distribuzione/windows/costruisci.sh [cartella_di_uscita]      (serve rete: scarica i binari da CRAN)
#     R_VERSIONE=4.6 bash distribuzione/windows/costruisci.sh            (altra versione di R; default: quella di questo computer)
#
# Una libreria vale per UNA versione minore di R (4.6.x): avvia.bat la usa solo se R è di quella versione, altrimenti
# lascia che R cerchi i pacchetti nella sua libreria (e dice che cosa manca). I pacchetti sono quelli di R/richiesti.R più
# quelli di servizio, con le dipendenze Depends e Imports (non quelle di sola compilazione). Le versioni sono quelle
# correnti di CRAN per Windows, non quelle di renv.lock (CRAN tiene i binari solo dell'ultima versione): le differenze
# si vedono in build/Trama-Lite-Windows/libreria/VERSIONI.txt.
# ─────────────────────────────────────────────────────────────
set -euo pipefail
cd "$(dirname "$0")/../.."
PROGETTO="$PWD"
NOME="Trama-Lite-Windows"; ELENCO="distribuzione/programma.txt"
OUT="${1:-build}/$NOME"
RV="${R_VERSIONE:-$(Rscript -e 'cat(paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = "."))')}"
CRAN="${CRAN:-https://cran.r-project.org}"
CONTRIB="$CRAN/bin/windows/contrib/$RV"

rm -rf "$OUT"; mkdir -p "$OUT/libreria"

echo "1/4 scarico i pacchetti per Windows (R $RV)"
CACHE="$(dirname "$OUT")/.cache-cran-windows-$RV"; mkdir -p "$CACHE"      # i file già scaricati non si riscaricano
Rscript - "$CONTRIB" "$OUT/libreria" "$PROGETTO" "$CACHE" <<'R'
a <- commandArgs(TRUE); contrib <- a[1]; lib <- a[2]; progetto <- a[3]; cache <- a[4]
source(file.path(progetto, "R/richiesti.R"))
options(timeout = 600)
av <- available.packages(contriburl = contrib, type = "win.binary")
if (!nrow(av)) stop("Nessun pacchetto a ", contrib)
base <- rownames(installed.packages(priority = "base"))
radici <- c(names(PACCHETTI_RICHIESTI), PACCHETTI_DI_SERVIZIO)
dip <- tools::package_dependencies(radici, db = av, which = c("Depends", "Imports"), recursive = TRUE)
pacchetti <- setdiff(sort(unique(c(radici, unlist(dip)))), base)
mancano <- setdiff(pacchetti, rownames(av)); if (length(mancano)) stop("Non ci sono per Windows: ", paste(mancano, collapse = ", "))
scarica <- function(url, dest) {      # CRAN a volte chiude la connessione: fino a 6 tentativi
  for (i in 1:6) {
    ok <- tryCatch({ suppressWarnings(utils::download.file(url, dest, mode = "wb", quiet = TRUE)) == 0L }, error = function(e) FALSE)
    if (ok && file.exists(dest) && file.size(dest) > 0) return(invisible(TRUE))
    unlink(dest); Sys.sleep(2 * i)
  }
  stop("Scarico non riuscito: ", url)
}
for (p in pacchetti) {
  zipf <- file.path(cache, sprintf("%s_%s.zip", p, av[p, "Version"]))
  if (!file.exists(zipf)) scarica(sprintf("%s/%s_%s.zip", contrib, p, av[p, "Version"]), zipf)
  utils::unzip(zipf, exdir = lib)
}
lock <- tryCatch(renv::lockfile_read(file.path(progetto, "renv.lock"))$Packages, error = function(e) NULL)
righe <- vapply(pacchetti, function(p) { v <- as.character(av[p, "Version"]); l <- if (!is.null(lock[[p]])) lock[[p]]$Version else "-"
  sprintf("%-14s %-12s renv.lock %-12s%s", p, v, l, if (!identical(l, "-") && !identical(l, v)) "  (diversa)" else "") }, "")
writeLines(c(sprintf("Pacchetti per Windows, R %s, da %s", basename(contrib), dirname(dirname(dirname(contrib)))), righe), file.path(lib, "VERSIONI.txt"))
cat(length(pacchetti), "pacchetti,", sum(grepl("diversa", righe)), "con versione diversa da renv.lock\n")
R
echo "$RV" > "$OUT/libreria/R-versione.txt"

echo "2/4 controllo che siano binari per Windows e della versione giusta"
Rscript - "$OUT/libreria" "$RV" <<'R'
a <- commandArgs(TRUE); lib <- a[1]; rv <- a[2]
d <- list.dirs(lib, recursive = FALSE, full.names = TRUE)
ko <- character()
for (p in d) {
  f <- file.path(p, "DESCRIPTION"); if (!file.exists(f)) { ko <- c(ko, paste(basename(p), "senza DESCRIPTION")); next }
  b <- read.dcf(f, "Built")[1, 1]
  if (is.na(b) || !grepl(sprintf("^R %s\\.", gsub(".", "\\.", rv, fixed = TRUE)), b)) ko <- c(ko, paste(basename(p), "costruito per", b))
  # un pacchetto con codice compilato deve avere la sua DLL per Windows
  if (dir.exists(file.path(p, "libs")) && !length(list.files(file.path(p, "libs"), pattern = "\\.dll$", recursive = TRUE))) ko <- c(ko, paste(basename(p), "senza .dll"))
}
if (length(ko)) { cat("NON TORNA:\n", paste(" ", ko, collapse = "\n"), "\n"); quit(status = 1) }
cat(length(d), "pacchetti: tutti per R", rv, "e per Windows\n")
R

echo "3/4 copio il programma (solo quello che dice $ELENCO)"
grep -vE '^[[:space:]]*(#|$)' "$ELENCO" | while read -r voce; do
  for f in $voce; do
    f="${f%/}"
    mkdir -p "$OUT/$(dirname "$f")"
    if [ -d "${PROGETTO}/$f" ]; then rsync -a "$PROGETTO/$f" "$OUT/$(dirname "$f")/"; else cp "$PROGETTO/$f" "$OUT/$f"; fi
  done
done
cp "$PROGETTO/avvia.bat" "$OUT/avvia.bat"
sed 's/$/\r/' "$PROGETTO/distribuzione/windows/LEGGIMI.txt" > "$OUT/LEGGIMI.txt"

echo "4/4 faccio lo zip"
( cd "$(dirname "$OUT")" && rm -f "$NOME.zip" && zip -qr -X "$NOME.zip" "$NOME" )
echo "Fatto: $(dirname "$OUT")/$NOME.zip ($(du -h "$(dirname "$OUT")/$NOME.zip" | cut -f1))"
