#!/bin/bash
# Trama Lite - lanciatore (Contents/MacOS/Trama-Lite). Avvia R incluso nel .app,
# server solo su 127.0.0.1, apre il browser; si ferma da solo quando si
# chiude l'ultima scheda (TRAMA_AUTOSTOP). Nessuna rete, nessun R di sistema.
RES="$(cd "$(dirname "$0")/../Resources" && pwd)"
export R_HOME="$RES/R"
export R_SHARE_DIR="$R_HOME/share" R_INCLUDE_DIR="$R_HOME/include" R_DOC_DIR="$R_HOME/doc"
export R_LIBS="$RES/lib" R_LIBS_USER="/nonexistent" R_LIBS_SITE=""
export R_ENVIRON="" R_ENVIRON_USER="" R_PROFILE="" R_PROFILE_USER=""
export TRAMA_AUTOSTOP=1
export LANG="${LANG:-it_IT.UTF-8}"
# Non ereditare nulla dall'R eventualmente installato.
unset R_DEFAULT_PACKAGES R_LIBS_USER_OLD
DATI="${TRAMA_HOME:-$HOME/Documents/Trama Lite}"
mkdir -p "$DATI"
LOG="$DATI/trama.log"
cd "$RES/app" || exit 1
"$R_HOME/bin/exec/R" --no-echo --no-restore --no-save --no-init-file -e '
  p <- httpuv::randomPort()
  options(shiny.port = p, shiny.host = "127.0.0.1",
          shiny.launch.browser = if (nzchar(Sys.getenv("TRAMA_NO_BROWSER"))) FALSE else TRUE)
  cat("Trama Lite su http://127.0.0.1:", p, "\n", sep = "")
  shiny::runApp(".")' >> "$LOG" 2>&1
STATO=$?
# Chiusa da un segnale voluto (129 SIGHUP, 130 SIGINT, 143 SIGTERM: «Esci» dal Dock, spegnimento): niente finestra.
# 134 (abort), 137 (kill, memoria), 139 (segfault) sono guasti veri e la finestra resta.
if [ $STATO -ne 0 ] && [ $STATO -ne 129 ] && [ $STATO -ne 130 ] && [ $STATO -ne 143 ]; then
  osascript -e "display dialog \"Trama Lite si è fermata con un errore.\n\nDettagli in $LOG\" buttons {\"OK\"} with icon stop" >/dev/null 2>&1
fi
exit $STATO
