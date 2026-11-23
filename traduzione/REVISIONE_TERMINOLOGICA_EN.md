# Revisione della terminologia inglese - 1 ottobre 2026

File verificato: `outputs/traduzione_interfaccia/testi_interfaccia_inglese.xlsx`.
Esaminate tutte le 828 voci inglesi: 725 in **Interfaccia**, 71 in **Campi**,
32 in **Vocabolario**. Migliorate 118 celle rispetto al commit `301ec52`:
74 in Interfaccia, 34 in Campi e 10 in Vocabolario.

La revisione riguarda il foglio di traduzione. I testi italiani, il codice R,
il database e i valori degli scambi restano invariati.

## Scelte terminologiche

| Italiano | Inglese adottato | Criterio |
| --- | --- | --- |
| Imputato / imputati | Defendant / defendants | Ruolo processuale; distinto dalle persone semplicemente citate e dai testimoni. Uso attestato negli studi sulla procedura dell'Inquisizione romana. |
| Capo d'accusa | Charge | Imputazione attribuita a un defendant; distinto dalla categoria di reato. |
| Reato | Offence | Categoria registrata nel catalogo; non implica una condanna. Grafia britannica coerente con *catalogue*, *organisation*, *normalised*. |
| Luogo del reato | Location of the alleged offence | La localizzazione riguarda il fatto contestato e non ne certifica la commissione. |
| Applicazione della tortura | Use of judicial torture | La voce del Dizionario riguarda la tortura giudiziaria, distinta da una pena. |
| Sentenza | Sentence | Uso storiografico attestato; non sostituito con *verdict*, che può evocare una decisione di giuria. |
| Sentenza di spedizione | Final sentence | Traduzione contestuale conservata: provvedimento conclusivo del procedimento. Non una sentenza di invio o trasporto. |
| Fascicolo | Case file | Aggregazione documentaria relativa a un procedimento; distinta dal file informatico e dalla scheda descrittiva. |
| Scheda | Catalogue record | Nelle principali etichette della maschera e nelle azioni sulla scheda. Nei messaggi tecnici JSON *record* mantiene il significato informatico. |
| Fondo | Archive group | Scelta stabilita con Stefano nella revisione precedente; singolare e plurale coerenti. *Fonds* è attestato nei glossari, ma non è la dicitura adottata nell'interfaccia. |
| Segnatura / altra segnatura | Reference code / alternative reference code | Identificativo archivistico dell'unità. |
| Consistenza | Extent | Quantità fisica dell'unità descritta. |
| Estremo remoto / recente | Earliest date / latest date | Estremi cronologici della documentazione, senza suggerire che siano necessariamente inizio e fine del processo. |
| Estremi cronologici | Inclusive dates | Intervallo fra la data più antica e quella più recente. |
| Data topica | Place of creation | Il campo registra un luogo di produzione, non una data né necessariamente il luogo di emissione di un atto. |
| Regesto | Abstract | Sintesi del contenuto documentario. |
| Soggetto produttore | Creator | Responsabile della produzione o accumulazione della documentazione. |
| Soggetto conservatore | Holding institution | Ente istituzionale che conserva la documentazione, come un archivio; distinto dal luogo fisico di conservazione (*place of custody*). Scelta precisata con Stefano dopo il commit `571ba1c`: sostituisce *custodian*, più generico perché può indicare anche una persona. |
| Organizzazione / ente | Corporate body | Terminologia archivistica per un organismo che agisce come entità. |
| Altro soggetto | Other subject | Voce di indicizzazione: non necessariamente una persona o un ente, quindi non *other agent*. |
| Definizione | Archival unit type | Il campo contiene, per esempio, *fascicolo*. |
| Supporto materiale / tipologia | Physical medium / physical format | Distingue la materia dalla forma fisica; il secondo campo corrisponde alla definizione dell'unità nel profilo ASMo. |
| Tipologia documentaria | Documentary form | Distinta dal materiale e dalla forma fisica. |
| Atti | Proceedings | Scelta contestuale per il vocabolario della documentazione processuale. Non una sostituzione universale di *records* per qualunque archivio. |
| Omonimie | Name disambiguation | Il pannello verifica se attestazioni dello stesso nome appartengano a persone diverse. |
| Tortura / sentenza esaminata | Source checked for torture / a sentence | Indica il controllo della fonte anche quando essa tace, non la presenza del fatto. |
| n.d. | not stated | Nei valori visualizzati del vocabolario. Non significa *no* né *not applicable*. I valori tecnici e gli esempi di input conservano `n.d.`. |

**Correzione di significato:** Interfaccia D198, «Individuali o multipli»,
diventa **One or more people mentioned**. `R/figure.R` conta le persone
citate, compresi i testimoni, e non il numero dei soli imputati.

**Categorie storiche conservate:** *Magia → Magic*, *Ebraismo → Judaism*,
*Santità → Sanctity*, *Sacramenti → Sacraments*, *Giurisdizione → Jurisdiction*.
Non sono state ristrette a stregoneria, giudaizzazione, simulata santità
o a un particolare abuso: queste specificazioni non sono nel termine italiano.
*Crimini sessuali* è uniformato a **Sexual offences**; *Inquisizione di Napoli*
è corretto in **Inquisition of Naples**.

Precisazione successiva: **Soggetto conservatore precedente** diventa
**Previous holding institution**. La stessa scelta è applicata al testo
dell'export Excel in Interfaccia D460 e alle etichette Campi D56–D58.

## Fonti consultate

Il **Dizionario storico dell'Inquisizione** è stato consultato per il significato
storico italiano, non trattato come un dizionario bilingue. Le equivalenze inglesi
sono scelte traduttive confrontate con glossari archivistici e uso storiografico.

- Prosperi, Adriano, con Vincenzo Lavenia e John Tedeschi, *Dizionario storico dell'Inquisizione*, Edizioni della Normale, 2010, volumi I–III. Consultati i passi su fonti inquisitoriali e ruoli di imputati/testimoni (vol. II, pp. 611–612), le distinzioni relative alla magia (vol. II, pp. 958–959), e la voce «Tortura» (vol. III, pp. 1594–1595). [Edizione e volumi aperti](https://edizioni.sns.it/prodotto/dizionario-storico-dell-inquisizione-2/).
- Henry Ansgar Kelly, «Galileo's Non-Trial (1616), Pre-Trial (1632–1633), and Trial (May 10, 1633): A Review of Procedure, Featuring Routine Violations of the Forum of Conscience», *Church History*, 2016. Uso di *defendant*, *charges*, *offenses*, *sentence* e *final sentence* nel contesto della procedura romana. [Articolo](https://www.cambridge.org/core/journals/church-history/article/galileos-nontrial-1616-pretrial-16321633-and-trial-may-10-1633-a-review-of-procedure-featuring-routine-violations-of-the-forum-of-conscience/F5BB9632FA3F4C34B33C119A401EB3BC).
- Society of American Archivists, *Dictionary of Archives Terminology*: [case file](https://dictionary.archivists.org/entry/case-file.html), [catalog record](https://dictionary.archivists.org/entry/catalog-record.html), [reference code](https://dictionary.archivists.org/entry/reference-code.html), [extent](https://dictionary.archivists.org/entry/extent.html), [inclusive dates](https://dictionary.archivists.org/entry/inclusive-dates.html), [abstract](https://dictionary.archivists.org/entry/abstract.html), [creator](https://dictionary.archivists.org/entry/creator.html), [custodian](https://dictionary.archivists.org/entry/custodian.html), [repository](https://dictionary.archivists.org/entry/repository.html), [corporate body](https://dictionary.archivists.org/entry/corporate-body.html), [subject](https://dictionary.archivists.org/entry/subject.html), [medium](https://dictionary.archivists.org/entry/medium.html), [physical form](https://dictionary.archivists.org/entry/physical-form.html), [form](https://dictionary.archivists.org/entry/form.html), [fonds](https://dictionary.archivists.org/entry/fonds.html).
- Treccani, voce [spedizione](https://www.treccani.it/vocabolario/spedizione/), per il significato giudiziario italiano.
- International Council on Archives, [Records in Contexts Ontology 1.1](https://www.ica.org/standards/RiC/RiC-O_1-1.html), per la distinzione fra agenti, luoghi, forme documentarie e supporti.

## Verifiche

- Tutte le 828 traduzioni presenti e non vuote.
- Segnaposto `%d` e `%s`, spazi e a capo ai bordi conservati.
- Confronto con il file originale: cambiano solo le celle inglesi dei tre fogli;
  testi italiani, intestazioni, stili, ordine dei fogli e altre parti del file invariati.
- Confronto con il commit precedente: 118 celle inglesi migliorate; nessuna
  modifica alle altre colonne o al foglio Leggimi.
- Controllo visivo di campi, ruoli processuali e vocabolario.

## Quando si integrerà il foglio nell'app

Alcuni messaggi sono concatenazioni di frammenti e suffissi italiani,
non frasi autonome: per esempio Interfaccia D3, D26, D435, D511 e D652.
I segnaposto sono conservati, ma la costruzione dei plurali va localizzata
nel codice prima di usare queste righe come messaggi inglesi. Gli esempi
di ricerca (D593, D595 e D684) mantengono le chiavi italiane riconosciute
dal parser. Questa revisione non modifica il codice dell'app.
