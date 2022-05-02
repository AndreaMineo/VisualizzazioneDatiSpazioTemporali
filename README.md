# VisualizzazioneDatiSpazioTemporali


Pacchetto R per la visualizzazione di dati spazio-temporali, al fine di supportare l'analisi nelle fasi che richiedono la generazione di rappresentazioni grafiche dei dati.

All’interno del pacchetto sono presenti due funzioni:
* VisualizzazioneDatiAreali : per la visualizzazione di dati con connotazione spaziale di tipo areale
* VisualizzazioneDatiPuntuali : per la visualizzazione di dati con
connotazione spaziale di tipo puntuale

Entrambe le funzioni permettono di eseguire delle applicazioni web, sviluppate attraverso il framework shiny, le quali possono essere utilizzate dall'utente per generare automaticamente dei grafici interattivi, a partire da un file dati contenente le serie storiche delle osservazioni effettuate sul fenomeno e un file map contenente le informazioni spaziali sulle aree/località geografiche dove tali osservazioni sono state effettuate, al fine di supportare l’analisi della distribuzione spaziale e temporale dei dati.

Nel caso si utilizzi la funzione VisualizzazioneDatiAreali, l'applicazione corrispondente genererà un grafico spaziale all'interno del quale le entità spaziali descritte nel file map saranno rappresentate come poligoni. Nel caso si utilizzi la funzione VisualizzazioneDatiPuntuali l'applicazione corrispondente genererà un grafico spaziale all'interno del quale le entità spaziali descritte nel file map saranno rappresentate tramite punti.

Affinché l’applicazione possa estrapolare correttamente le informazioni sulle
serie temporali archiviate nel file dati è necessario che quest’ultimo rispetti le
seguenti specifiche:
* Il file dati deve essere un file di tipo "comma-separeted-values" (csv) o una
cartella di lavoro excel (xlsx). Nel primo caso all’utente verrà richiesto
di specificare il delimitatore utilizzato all’interno del file. L’insieme dei
possibili valori per il delimitatore è il seguente (",",";","/")
* Il file dati verrà rappresentato all’interno dell’applicazione come un’istanza
della classe data.frame. Affinché sia possibile interpretare correttamente i
dati è necessario che la prima colonna sia di tipo character o numeric, e
che contenga i timestamps delle serie temporali definite nel file dati. Nel
caso la prima colonna contenga elementi di tipo character essi devono
essere convertibili in oggetti di tipo POSIXct
* Il file dati dovrà contenere un colonna che riporta le informazioni sulle
denominazioni delle località/aree geografiche dove sono state effettuate le
osservazioni
* Il file dati dovrà contenere almeno una colonna con elementi di tipo
numerico

Affinché l’applicazione possa estrapolare correttamente le informazioni spaziali contenute nel file map è necessario che quest’ultimo rispetti le seguenti specifiche:
* Il file map deve essere uno shape file o un file RData (file utilizzato per
archiviare oggetti nell’ambiente R avente estensione "RData" o "rda").
Nel caso di shape file è necessario effettuare un upload multiplo caricando
all’interno dell’applicazione i file con estensione shp, dbf, rpj e shx.
* Nel caso si utilizzi uno shape file per caricare all’interno dell’applicazione le
informazioni spaziali è necessario che le entità spaziali definite all’interno
del file siano poligoni o punti (a seconda che si usi la funzione per la
visualizzazione di dati areali o la funzione per la visualizzazione di dati
puntuali). Nel caso di dati puntuali è possibile caricare un file con entità
spaziali di tipo poligono. In questo caso l’entità spaziale, seppur di tipo
areale, verrà rappresentata come un punto nello spazio localizzato nel
centroide del poligono
* Nel caso si utilizzi un file RData per caricare le informazioni spaziali all’interno dell’applicazione è necessario che il file contenga almeno
un’istanza di una delle seguenti classi R: sf, SpatVector, SpatialPolygonsDataFrame, SpatialPointsDataFrame. L’applicazione considererà le
informazioni presenti nella prima istanza valida trovata. Ovviamente la
classe SpatialPointsDataFrame specifica dati di tipo puntuale è non sarà
considerata validata dalla funzione VisualizzazioneDatiAreali
* Nel caso si utilizzi un file con RData e che all’interno del file le infor-
mazioni spaziali siano contenute in un oggetto di tipo sf, la funzione
VisualizzazioneDatiAreali accetta come valide istanze aventi geometria di
tipo MULTIPOLYGON o POLYGON, mentre, la funzione Visualizza-
zioneDatiPuntuali accetta come valide istanze aventi geometria di tipo
MULTIPOINT, POINT, MULTIPOLYGON, POLYGON
* Nel caso si utilizzi un file RData e che all’interno del file le informazioni
spaziali siano contenute in un oggetto di tipo SpatVector, la funzione
VisualizzazioneDatiAreali accetta come valide istanze aventi geometria di
tipo polygons, mentre, la funzione VisualizzazioneDatiPuntuali accetta
come valide istanze aventi geometria di tipo polygons o points
* Tra gli attributi non spaziali delle entità spaziali definite all’interno del
file map deve essere necessariamente presente la denominazione delle
località/aree geografiche rappresentate. Tale informazione sarà utilizzata
dall’applicazione per associare a ciascuna entità spaziale le corrispondenti
serie temporali ottenute dal file dati. Affinché sia possibile generare
correttamente il grafico spaziale è necessario che l’intersezione tra l’insieme
delle denominazioni ottenuto dal file map e l’insieme delle denominazioni
ottenuto dal file dati sia non vuota

L’applicazione esegue tutti i controlli necessari a garantire che i dati inseriti
dall’utente siano conformi alle specifiche descritte in precedenza. In caso uno
o più test di validazione falliscano, vengono restituiti dei messaggi di errore
pensati per supportare l’utente nell’individuazione e correzione dell’errore.

L’applicazione, inoltre, permette all’utente di:
* Selezionare, tra le possibili colonne del data.frame ottenuto dal file dati,
quale utilizzare come variabile per la generazione dei grafici
* Selezionare, tra i possibili timestamp specificati nella prima colonna del
data.frame ottenuto dal file dati, quale utilizzare per generare il grafico
spaziale
* Selezionare, tra tutte le possibili località/aree geografiche disponibili,
quali includere all’interno del grafico delle serie temporali. Nel caso si
selezioni il valore "all", il grafico includerà la serie temporale di ciascuna
località/area disponibile
* Inserire gli estremi degli intervalli di valori da utilizzare per la legenda
del grafico spaziale. L’applicazione in automatico, una volta inseriti i
dati e selezionata una variabile valida, determina i valori quantizzando
uniformemente l’intervallo [valore_minimo,valore_massimo] in cinque
sotto-intervalli. Nel caso si volessero utilizzare intervalli differenti è
necessario inserire i valori degli estremi separati da virgola
* Effettuare il download dei due grafici nei formati html, pdf e immagine.
* Definire la modalità di visualizzazione del grafico spaziale. Le due possibili
alternative sono:
  * Con basemap: in questo caso nel grafico spaziale verrà visualizzato come layer sottostante una basemap che riporta i principali confini         
    amministrativi dell’area di studio. Tale modalità arricchisce il contenuto informativo della mappa, poiché permette di individuare visivamente la 
    collocazione geografica di ciascuna entità spaziale rappresentata, utilizzando come riferimento i confini delle unità amministrative riportati dalla     basemap. Affinché la visualizzazione sia corretta è necessario utilizzare un’opportuna proiezione nella definizione dell’entità spaziali.
  * Senza basemap: in questo caso nel grafico spaziale verrà visualizzato come layer sottostante uno sfondo neutro di colore grigio. Modalità
    particolarmente indicata nel caso in cui non sia nota la proiezione da utilizzare per i dati spaziali da analizzare



