#!/bin/bash
# Trama Lite - avvio con doppio clic (macOS). Serve R installato.
cd "$(dirname "$0")" || exit 1
RS="$(command -v Rscript)"
[ -z "$RS" ] && [ -x /usr/local/bin/Rscript ] && RS=/usr/local/bin/Rscript
[ -z "$RS" ] && [ -x /opt/homebrew/bin/Rscript ] && RS=/opt/homebrew/bin/Rscript
[ -z "$RS" ] && [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ] && RS=/Library/Frameworks/R.framework/Resources/bin/Rscript
if [ -z "$RS" ]; then
  echo "R non trovato. Installalo da https://cran.r-project.org e riprova."
  read -r -p "Premi Invio per chiudere."
  exit 1
fi
# renv attiva la libreria del progetto, fatta di collegamenti alla cache di QUESTO Mac: su un altro computer i collegamenti non portano
# a niente, renv cercherebbe la rete e i pacchetti finirebbero dove renv non guarda. Se la libreria non funziona, si usa quella di R.
if ! ls -d renv/library/macos/*/*/shiny >/dev/null 2>&1; then export RENV_CONFIG_AUTOLOADER_ENABLED=FALSE; fi
# Il controllo dei pacchetti viene prima di runApp: senza shiny, runApp
# fallirebbe da solo e con un errore che non dice che cosa fare.
if ! "$RS" -e "source('R/richiesti.R'); controlla_pacchetti()"; then
  echo
  read -r -p "Premi Invio per chiudere."
  exit 1
fi

"$RS" -e "shiny::runApp('.', launch.browser = TRUE)" || {
  echo
  read -r -p "Trama Lite si è fermata. Premi Invio per chiudere."
}
