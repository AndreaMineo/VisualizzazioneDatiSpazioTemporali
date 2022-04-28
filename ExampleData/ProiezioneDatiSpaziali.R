### dataset pm10 disponibile al seguente link
### "https://github.com/guidofioravanti/spde_spatio_temporal_pm10_modelling_italy/blob/master/FMCC/data/pm10.rda"
load("/home/andrea/Documenti/TESI/pm10.rda")

### le coordinate sono nelle colonne 3 e 4 del dataframe
informazioni_spaziali <- unique(pm10[,c(3,4)])
### la colonna con le denominazioni è la colonna id_centralina
denominazione <- data.frame(id_centralina=unique(pm10$id_centralina))

### per creare un'istanza di SpatialPointsDataFrame con attributi non spaziali basta usare il costruttore specifico al quale
### passare come primo argomento la matrice con le informazioni spaziali, come secondo il dataframe con gli attributi non spaziali
### per definire che tipo di proiezione usare basta usare l'argomento proj4string, al quale va passata un'istanza di CRS
### space object senza proiezione:
shape_senza_proiezione <- sp::SpatialPointsDataFrame(informazioni_spaziali,denominazione)


### in questo caso utilizzo la proiezione UTM (la più utilizzata per mappe locali)
### Questa proiezione, nota come Universal Transverse Mercator, suddivide il globo in 60 zone per l'emisfero nord e 60 per quello sud
### per ottenere la giusta proiezione bisogna specificare il parametro zone che va da 1 a 60
### Le linkero via e-mail un sito dove visualizzare le zone per capire quale utilizzare
### NB Nel caso l'area di studio copra più zone bisogna selezionare la prima da sinistra
### Nel caso la zona sia collocata nell'emisfero sud bisogna aggiungere il parametro south (basta scrivere +south all'interno della stringa passata a CRS)
### la zona relativa all'italia è 32
### Il parametro datum definisce il modello di ellissoide utilizzato per approssimare la superfice terreste (il più usato è WGS84)
### shape object con proiezione:
shape_con_proiezione <- sp::SpatialPointsDataFrame(informazioni_spaziali,denominazione,proj4string = sp::CRS("+proj=utm +zone=32 +datum=WGS84"))

save(shape_senza_proiezione,file="/home/andrea/Documenti/TESi/ShapeSenzaProiezione.RData")
save(shape_con_proiezione,file="/home/andrea/Documenti/TESI/ShapeConProiezione.RData")

### affinchè il dataset sia compatibile con l'applicazione bisogna che la prima colonna contenga le date
### ho inoltre rimosse le colonne non significative per la visualizzazione dei dati
df <- data.frame(data=pm10$yymmdd,id_centralina=pm10$id_centralina,PM10=pm10$pm10)
write.csv(df,"/home/andrea/Documenti/TESI/pm10.csv",row.names = FALSE)
