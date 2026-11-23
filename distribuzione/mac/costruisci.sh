#!/bin/bash
# ─────────────────────────────────────────────────────────────
# distribuzione/mac/costruisci.sh - Trama-Lite.app con R dentro (prototipo, 30/09; Trama Lite dal 21/10)
#
# Costruisce build/Trama-Lite.app dal framework R installato (CRAN) e dalla
# libreria di renv: R viene copiato e reso spostabile (install_name_tool:
# i binari cercano le librerie accanto a sé, non in /Library/Frameworks),
# poi firmato ad-hoc (obbligatorio su Apple Silicon dopo la modifica).
# Il .app costruito vale per l'architettura di QUESTO Mac (arm64 o x86_64):
# per l'altra serve rifarlo su un Mac dell'altra architettura.
#
#     bash distribuzione/mac/costruisci.sh [cartella_di_uscita]
#
# I pacchetti sono la chiusura delle dipendenze di quelli che Trama Lite chiede (R/richiesti.R), fra quelli di renv.lock. I dati stanno
# in ~/Documents/Trama Lite, mai dentro il .app, e non sono quelli di Trama.
# ─────────────────────────────────────────────────────────────
set -euo pipefail
cd "$(dirname "$0")/../.."
PROGETTO="$PWD"
NOME="Trama-Lite"; ELENCO="distribuzione/programma.txt"
OUT="${1:-build}/$NOME.app"
FW="$(Rscript -e 'cat(R.home())')"               # .../R.framework/Resources
VERSIONE="$(Rscript -e 'cat(paste(R.version$major, R.version$minor, sep="."))')"
LIB_PROGETTO="$(Rscript -e 'cat(.libPaths()[1])')"
[ -d "$LIB_PROGETTO" ] || { echo "Libreria renv non trovata"; exit 1; }

rm -rf "$OUT"
R="$OUT/Contents/Resources/R"
mkdir -p "$OUT/Contents/MacOS" "$R" "$OUT/Contents/Resources/lib" "$OUT/Contents/Resources/app"

echo "1/5 copio R $VERSIONE"
rsync -a --exclude 'doc' --exclude 'tests' --exclude 'tcltk' --exclude 'include' \
      --exclude 'library/*/doc' --exclude 'library/*/tests' --exclude 'modules/R_X11.so' \
      --exclude 'modules/vfonts.so' --exclude 'modules/R_de.so' --exclude 'library/grDevices/libs/cairo.so' --exclude '*.dSYM' \
      --exclude 'library/*' "$FW/" "$R/"
# Della libreria di sistema solo i pacchetti di R base: gli altri (terra, sf, lintr, languageserver...) sono
# strumenti di lavoro di chi costruisce e non servono a Trama Lite (i suoi stanno in lib/, passo 2): 524 -> ~170 MB (20/10).
mkdir -p "$R/library"
BASE="$(Rscript -e 'cat(rownames(installed.packages(priority = "base")), sep = "\n")')"
for p in $BASE; do
  [ "$p" = "tcltk" ] && continue          # lega Tcl/Tk e X11 (/opt/R, /opt/X11): Trama Lite non lo usa
  rsync -a --exclude 'doc' --exclude 'tests' --exclude 'help' --exclude '*.dSYM' --exclude 'libs/cairo.so' "$FW/library/$p" "$R/library/"
done

echo "2/5 copio i pacchetti (versioni di renv.lock)"
# Solo i pacchetti di renv.lock: la libreria di renv può avere strumenti di lavoro (languageserver, lintr, styler) che non vanno nel programma
# I pacchetti che Trama Lite chiede più quelli di servizio, con tutte le loro dipendenze, fra quelli di renv.lock.
PACCHETTI="$(Rscript -e 'source("R/richiesti.R"); lock <- names(renv::lockfile_read("renv.lock")$Packages); ip <- installed.packages(lib.loc = .libPaths()[1]); r <- c(names(PACCHETTI_RICHIESTI), PACCHETTI_DI_SERVIZIO); d <- tools::package_dependencies(r, db = ip, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE); cat(sort(intersect(unique(c(r, unlist(d))), lock)), sep = "\n")')"
echo "$PACCHETTI" | while read -r p; do
  [ -d "$LIB_PROGETTO/$p" ] || { echo "Manca nella libreria renv: $p"; exit 1; }
  rsync -aL --exclude '*.dSYM' --exclude 'doc' --exclude 'tests' --exclude 'help' "$LIB_PROGETTO/$p" "$OUT/Contents/Resources/lib/"
done

echo "3/5 copio il programma (solo quello che dice $ELENCO)"
# L'elenco e' l'unica definizione di «programma»: niente dati d'archivio, niente script di lavoro,
# niente file spuri (in R/ c'erano export veri di ASDNa, *.numbers: finivano nel pacchetto).
APP="$OUT/Contents/Resources/app"
grep -vE '^[[:space:]]*(#|$)' "$ELENCO" | while read -r voce; do
  for f in $voce; do                      # senza virgolette: espande «R/*.R»
    f="${f%/}"
    mkdir -p "$APP/$(dirname "$f")"
    if [ -d "$PROGETTO/$f" ]; then rsync -a "$PROGETTO/$f" "$APP/$(dirname "$f")/"; else cp "$PROGETTO/$f" "$APP/$f"; fi
  done
done

echo "4/5 rendo R spostabile e firmo"
python3 - "$OUT/Contents/Resources" "$FW" <<'PY'
import os, re, subprocess, sys
res, fw = sys.argv[1], sys.argv[2]          # fw = .../Versions/4.6/Resources
vecchio = re.compile(r"^/Library/Frameworks/R\.framework/(Versions/[^/]+/)?Resources/")
R = os.path.join(res, "R")
def mach_o(p):
    with open(p, "rb") as f: m = f.read(4)
    return m in (b"\xcf\xfa\xed\xfe", b"\xca\xfe\xba\xbe", b"\xfe\xed\xfa\xcf")
n = 0
for base in (res,):
    for d, _, files in os.walk(base):
        for nome in files:
            p = os.path.join(d, nome)
            if os.path.islink(p) or not os.path.isfile(p) or ".dSYM" in p: continue
            try:
                if not mach_o(p): continue
            except OSError: continue
            out = subprocess.run(["otool", "-L", p], capture_output=True, text=True).stdout.splitlines()[1:]
            cambi = []
            for riga in out:
                dep = riga.strip().split(" (")[0]
                m = vecchio.match(dep)
                if m:
                    nuovo = os.path.join(R, dep[m.end():])
                    rel = os.path.relpath(nuovo, os.path.dirname(p))
                    cambi += ["-change", dep, "@loader_path/" + rel]
            if nome.endswith(".dylib"):
                cambi += ["-id", "@loader_path/" + nome]
            if cambi:
                subprocess.run(["install_name_tool"] + cambi + [p], check=True, capture_output=True)
                n += 1
            subprocess.run(["codesign", "--force", "-s", "-", p], check=True, capture_output=True)
print("   binari riscritti:", n)
PY

echo "5/5 lanciatore"
cp distribuzione/mac/lanciatore.sh "$OUT/Contents/MacOS/Trama-Lite"
INFO="$(sed "s/@VERSIONE@/$(date +%Y.%m.%d)/" distribuzione/mac/Info.plist)"
chmod +x "$OUT/Contents/MacOS/Trama-Lite"
echo "$INFO" > "$OUT/Contents/Info.plist"
codesign --force --deep -s - "$OUT" 2>&1 | tail -1 || true

# Controllo finale: nessun binario può legarsi a librerie fuori dal .app (salvo quelle di sistema di macOS).
# Un percorso come /opt/R o /opt/X11 funziona sul Mac di chi costruisce e non su quello di una collega.
FUORI="$(find "$OUT" \( -name '*.so' -o -name '*.dylib' \) -print0 | xargs -0 otool -L 2>/dev/null \
  | grep -E '^[[:space:]]+/(opt|usr/local|Users|Library/Frameworks)/' | sort -u || true)"
if [ -n "$FUORI" ]; then
  echo "ERRORE: il pacchetto lega librerie fuori dal .app:"; echo "$FUORI"; exit 1
fi
du -sh "$OUT"
# Controlli: in R/library solo pacchetti di base, e il programma non deve crescere senza che nessuno se ne accorga.
ESTRANEI="$(for d in "$R"/library/*/; do n="$(basename "$d")"; echo "$BASE" | grep -qx "$n" || echo "$n"; done)"
[ -z "$ESTRANEI" ] || { echo "ERRORE: pacchetti non di base in R/library: $ESTRANEI"; exit 1; }
KB="$(du -sk "$OUT" | cut -f1)"
[ "$KB" -lt 250000 ] || { echo "ERRORE: Trama-Lite.app pesa $((KB / 1024)) MB (soglia 250 MB)"; exit 1; }
ZIP="$(dirname "$OUT")/$NOME-mac.zip"
rm -f "$ZIP"; ditto -c -k --sequesterRsrc --keepParent "$OUT" "$ZIP"
echo "Zip: $ZIP ($(du -h "$ZIP" | cut -f1))"
echo "Fatto: $OUT"
