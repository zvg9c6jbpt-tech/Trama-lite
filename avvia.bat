@echo off
rem Trama Lite - avvio con doppio clic (Windows). Serve R installato.
cd /d "%~dp0"
set "RS="
rem L'installer di R non lo mette nel PATH: lo si cerca dove lo mette lui.
rem Tre posti: Programmi, Programmi (x86) e l'installazione per il solo
rem utente, che e' quella che parte quando non si hanno i permessi da
rem amministratore - il caso tipico di un computer d'ufficio.
for /d %%D in ("%ProgramFiles%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RS=%%D\bin\Rscript.exe"
for /d %%D in ("%ProgramFiles(x86)%\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RS=%%D\bin\Rscript.exe"
for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do if exist "%%D\bin\Rscript.exe" set "RS=%%D\bin\Rscript.exe"
if not defined RS (
  where Rscript >nul 2>nul && set "RS=Rscript"
)
if not defined RS (
  echo R non trovato. Installalo da https://cran.r-project.org e riprova.
  pause
  exit /b 1
)

rem renv attiva la libreria del progetto; su un computer che non l'ha costruita cercherebbe la rete e i pacchetti
rem finirebbero dove renv non guarda. Senza renv\library\windows si usa la libreria di R.
if not exist "renv\library\windows" set "RENV_CONFIG_AUTOLOADER_ENABLED=FALSE"

rem Libreria inclusa nel pacchetto (cartella "libreria": pacchetti gia' pronti per Windows, scaricati da CRAN). La si usa
rem solo se R e' della versione per cui e' stata costruita (R-versione.txt, es. 4.6): altrimenti R cerca nella sua libreria.
if exist "libreria\R-versione.txt" (
  "%RS%" -e "v <- readLines('libreria/R-versione.txt', warn = FALSE)[1]; quit(status = !startsWith(as.character(getRversion()), v))"
  if errorlevel 1 (
    echo La cartella libreria e' per un'altra versione di R: non la uso.
  ) else (
    set "R_LIBS=%~dp0libreria"
  )
)

rem Il controllo dei pacchetti viene prima di runApp: senza shiny, runApp
rem fallirebbe da solo e con un errore che non dice che cosa fare.
"%RS%" -e "source('R/richiesti.R'); controlla_pacchetti()"
if errorlevel 1 (
  pause
  exit /b 1
)

"%RS%" -e "shiny::runApp('.', launch.browser = TRUE)"
if errorlevel 1 pause
