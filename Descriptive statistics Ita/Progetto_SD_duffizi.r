# PUNTO 1) Partiamo dando una variabile al Dataframe
re_texas <- read.csv("realestate_texas.csv", sep = ",")

#Controllo che tutto sia corretto osservando le prime 5 entrate con funzione head
head(re_texas, 5)

# PUNTO 2) 
#Variabili contenute nel dataset: Invece di leggerle dall'head le estraggo con names()
names(re_texas)

# city(v. qualitativa scala nominale)
# year(variabile quantitativa discreta su scala di intervalli)
# month(variabile quantitativa discreta su scala di rapporti)
# sales(variabile quantitativa discreta su scala di rapporti)
# volume(variabile quantitativa continua su scala di rapporti)
# median_price(variabile quantitativa discreta su scala di rapporti)
# listings (variabile quantitativa continua su scala di rapporti)
# months_inventory variabile quantitativa discreta su scala di rapporti)

# uso la funzione attach su re_texas per accedere più velocemente alle variabili
attach(re_texas)

# PUNTO 3) Cerco indici di posizione per le variabili e i loro metodi alternativi
# Minimo con funzione
min(year)

# Metodo alternativo
sort(year)[1]

#Similmente faccio per il max
max(year)

n_year <- length(year) # per sapere il numero di osservazioni
sort(year)[n_year]

#Trova la media, in due modi
mean(year)

sum(year)/n_year

#Trovo la moda
table(year)

#Trovo la mediana, nei due modi
median(year)

sum(sort(year[c(120,121)]))/2  #Uso la funzione sort per mettere in ordine i dati contenuti in year

#Quartili calcolati e con la funzione
n_year/4
sort(year)[60]

quantile(year)

# Il tutto avrei potuto ottenerlo con la funzione summary che userò, per brevità, anche sulle altre variabili
# Gli altri tipi di media, armonica e geometrica sono difficilmente applicabili alle variabili in oggetto, con alcune eccezioni.
# Può avere un senso studiare la media ponderata tra alcune variabili come Prezzo ponderate alle vendite, 
# Così da avere un idea della media del Prezzo per una possibile vendita generica nelle varie città, nel periodo studiato.
# Media Ponderata Prezzo-Vendite
# Prima devo calcolare il prezzo medio di ogni vendita, e giacché il valore Volume è in milioni devo anche aggiustare la scala
prezzo_medio_lista <- (volume/sales)*10^6

#Ora eseguo la media ponderata
sum(prezzo_medio_lista*sales)/sum(sales)  # Ora sappiamo che la media di una vendita generica è di 161240.4 $

weighted.mean(prezzo_medio_lista,sales) # metodo veloce

#Media ponderata Vendite-Inserzioni, per sapere quante inserzioni in media servono per fare una vendita generica per tutte le città nel periodo in oggetto di esame.
#Questa misura ci dà un idea della richiesta nel mercato immobiliare .

weighted.mean(listings/sales, sales) 
# Abbiamo scoperto che servono 9.038 inserzioni per effettuare una vendita. Ma le inserzioni sono variabili discrete quindi dovrò fare la conversione.

as.integer(weighted.mean(listings/sales, sales))

(sum(listings))/sum(sales) # questa non è la media ma arriva lo stesso al risultato perché l'efficacia delle inserzioni può essere semplificato come come rapporto di n-volantini su n-vendite

#Ponderata Inserzioni su volume
weighted.mean(listings/volume,volume) 
#Così so che con 56.05 Inserzioni in media si ottengono 1 milione di dollari in volume di vendita. le insersioni sono variabili discrete. 
#Quindi le riporto ad int con la funzione as.integer. 

as.integer(weighted.mean(listings/volume,volume))

#Gli indici di posizione possono essere trovati più agilmente con la funzione summary
summary(year)
summary(sales)
summary(volume)
summary(median_price)
summary(listings)
summary(months_inventory)


# Per la variabile city non ha senso trovare gli indici di posizione,forma, o variabilità giacché è una variabile qualitativa.
# Cercherò piuttosto le classi contenute nella variabile e le loro occorrenze.
# Cerco le classi
unique(city)

# Cerco le occorrenze
table(city)

#Rappresento le occorrenze in un grafico
barplot(table(city),
        main = "Distribuzione Vendite",
        xlab = "Città",
        ylab = "Numero di Vendite",
        ylim = c(0,80),
        col = "lightblue",
        names.arg = row.names(city))

# Il grafico non mostra la classe Bryan-College Station perché ha un nome troppo lungo, quindi devo fare degli aggiustamenti.
nomi_città_accorciati <- c("Beaumont", "B-C Station", "Tyler", "Wichita Falls")

barplot(table(city),
        main = "Distribuzione Vendite",
        xlab = "Città",
        ylab = "Numero di Vendite",
        ylim = c(0,80),
        col = "lightblue",
        names.arg = nomi_città_accorciati)

# altro metodo alternativo a Table per ottenere le frequenze di city
aggregate(city, by=list(city), FUN=length)

# Inoltre tramite un subset di table, usando la funzione names, posso specificare la frequenza della classe che mi interessa.
table(city)[names(table(city))=="Tyler"]


# Scopro gli indici di variabilità

# Intervallo di variazione
max(volume)-min(volume) 
#lo posso ottenere anche con:
range(volume) 
range(volume)[2]-range(volume)[1]

#Range interquartile
IQR(volume)

# Varianza e Standard Deviation
n <- length(volume) 
mu <- mean(volume) 

sigma2 <- sum((volume-mu)^2)/n 
std <- sqrt(sigma2) 
sigma2 # varianza
std # Standard Deviation

var(volume) # controllo il risultato con la funzione. 
sd(volume) # controllo il risultato con le funzione  

#Spesso ho utlizziato più di una modalità per ottenere una informazone non perché ritengo sia necessario usare ogni mezzo a disposizione
#Ma perché essendo questo un progetto di analisi oggetto di valutazione ho preferito mostrare in questo modo la mia comprensione dell'argomento R e la statistica descrittiva.
#Ovviamente dovendo poi produrre una analisi per un qualsiasi datore di lavoro userei il metodo più rapido

# Indice di Gini, creiamo la funzione e mostriamo il valore. 
# Ricordo se indice è vicino a 0 allora la variabile sarà omogenea, se è vicino all'1 sarà eterogenea con i valori distribuiti quasi equamente
gini.index <- function(x){ 
        ni=table(x) 
        fi=ni/length(x) 
        fi2=fi^2 
        j=length(table(x)) 
        gini = 1-sum(fi2) 
        gini.normalizzato = gini/((j-1)/j) 
        return(gini.normalizzato) 

} 

gini.index(volume)  # la variabile volume è eterogenea il valore è 0.99

#Coefficiente di Variazione,
CV <- function(x){return(sd(x)/mean(x)*100)} # creo la funzione
CV(volume) 

#Indici di forma con la formula e poi estratti con pacchetto "moments"
#Asimmetria
plot(density(volume)) #per rappresentarlo brevemente
m3 <- sum((volume-mu)^3)/n 
Asim.index = m3/(std^3) 
Asim.index 

#curtosi 
m4 <- sum((volume-mu)^4)/n 
curtosi <- (m4/(std^4)) -3 
curtosi 

#Con pacchetto moments 
install.packages("moments") 
library(moments) 
skewness(volume)   # Il valore è positivo quindi la variabile avrà distribuzione positiva.
kurtosis(volume) - 3   # gli indici tornano, il risultato è 0.17 un valore < di 0 quindi la funzione è platicurtica

# Raggruppo e sintetizzo gli indici per variabile, eccetto city per le ragioni già trattate(è una variabile qualitativa nominale).
# Sebbene non abbia molto senso trovare alcuni indici per certe variabili come il range degli year e dei month, li lascio per completezza e velocità. 
# Infatti ripeterò lo stesso blocco di funzioni per le varie variabili.

#YEAR
# indici di posizione
summary(year)
table(year) 
# indici di variabilità
max(year)-min(year)
range(year)  
IQR(year)
var(year)
sd(year)
gini.index(year)
CV(year)
# indici di forma
skewness(year)   
kurtosis(year) - 3
plot(density(year)) # grafico per avere una visualizzazione della distribuzione dei dati 

# MONTH 
#indici di posizione
summary(month)
table(month) 
# indici di variabilità
max(month)-min(month)
range(month)  
IQR(month)
var(month)
sd(month)
gini.index(month)
CV(month)
# indici di forma
skewness(month)   
kurtosis(month) - 3
plot(density(month)) # grafico per avere una visualizzazione della distribuzione dei dati 

# SALES
#indici di posizione
summary(sales)
table(sales) 
# indici di variabilità
max(sales)-min(sales)
range(sales)  
IQR(sales)
var(sales)
sd(sales)
gini.index(sales)
CV(sales)
# indici di forma
skewness(sales)   
kurtosis(sales) - 3
plot(density(sales)) # grafico 

# VOLUME
#indici di posizione
summary(volume)
table(volume) 
# indici di variabilità
max(volume)-min(volume)
range(volume)  
IQR(volume)
var(volume)
sd(volume)
gini.index(volume)
CV(volume)
# indici di forma
skewness(volume)   
kurtosis(volume) - 3
plot(density(volume)) # grafico 

# MEDIAN PRICE
#indici di posizione
summary(median_price)
table(median_price) 
# indici di variabilità
max(median_price)-min(median_price)
range(median_price)  
IQR(median_price)
var(median_price)
sd(median_price)
gini.index(median_price)
CV(median_price)
# indici di forma
skewness(median_price)   
kurtosis(median_price) - 3
plot(density(median_price)) # grafico 

# LISTINGS
#indici di posizione
summary(listings)
table(listings) 
# indici di variabilità
max(listings)-min(listings)
range(listings)  
IQR(listings)
var(listings)
sd(listings)
gini.index(listings)
CV(listings)
# indici di forma
skewness(listings)   
kurtosis(listings) - 3
plot(density(listings)) # grafico 

# MONTH INVENTORY
#indici di posizione
summary(months_inventory)
table(months_inventory) 
# indici di variabilità
max(months_inventory)-min(months_inventory)
range(months_inventory)  
IQR(months_inventory)
var(months_inventory)
sd(months_inventory)
gini.index(months_inventory)
CV(months_inventory)
# indici di forma
skewness(months_inventory)   
kurtosis(months_inventory) - 3
plot(density(months_inventory)) # grafico 


# PUNTO 4) Scoprire la variabile più asimmetrica e con più variabilità
# Ho il compito di trovare la variabile più asimmetrica
# Asimmetria
lista_variabili <- c(colnames(re_texas))
lista_variabili
lista_variabili_senza_city <- lista_variabili[lista_variabili != "city"]
lista_variabili_senza_city 

lista_asimmetrie <- c(skewness(year),
                skewness(month),
                skewness(sales),
                skewness(volume),
                skewness(median_price),
                skewness(listings),
                skewness(months_inventory))

abs_lista_asimmetrie<- abs(lista_asimmetrie)# uso la funzione abs per trovare il valore assoluto così posso trovare l'assimetria max, indipendentemente dal fatto che sia positiva o negativa

data_frame1 <- data.frame(lista_variabili_senza_city,abs_lista_asimmetrie ) #qui ho il dataframe con ordinati le variabili e il loro valore di asimetteria
max_asimmetry <- max(data_frame1["abs_lista_asimmetrie"]) # conosco il valore massimo di asimmetria 0.8847

which(abs_lista_asimmetrie == max_asimmetry) # Così so il numero di posizione corrispondente al massimo di asimmetria
lista_variabili_senza_city[4] # So che corrisponde al Volume
skewness(volume) # ricalcolo per assicurarmi che il valore non fosse negativo, giacché precendentemente abbiamo preso solo valori assoluti.
#Conclusione: il volume è la variabile con maggiore asimmetria, pari a 0.884. Essendo un valore maggiore di 0 risulta essere una asimmetria positiva.


#VARIABILE CON VARIAZIONE MASSIMA
lista_range <- c(max(year)-min(year),
                max(month)-min(month), 
               max(sales)-min(sales), 
                max(volume)-min(volume), 
                max(median_price)-min(median_price), 
                max(listings)-min(listings), 
                max(months_inventory)-min(months_inventory))

lista_sd <- c(sd(year),
                sd(month),
                sd(sales),
                sd(volume),
                sd(median_price),
                sd(listings),
                sd(months_inventory))                

#Lo strumento da utilizzare per vedere chi ha varianza maggiore è il CV il coefficiente
#Ma prima provo con una mia idea: la variazione percentuale del minimo, ossia dal valore minimo quale percentuale necessito per raggiunge il massimo
#Così da avere un valore proporzionato per ogni variabile della loro escursione. Secondo la proporzione min(x) : max(x) = 100 : x
#Giacché sono tutte variabili positive non mi preoccupo che il loro min o max possa essere negativo.
#Quindi non uso la funzione abs

lista_variazioni_percentuali <- c(((max(year)*100)/min(year)),
                                ((max(month)*100)/min(month)),
                                ((max(sales)*100)/min(sales)),
                                ((max(volume)*100)/min(volume)),
                                ((max(median_price)*100)/min(median_price)),
                                ((max(listings)*100)/min(listings)),
                                ((max(months_inventory)*100)/min(months_inventory)))

data_frame2 <- data.frame(lista_variabili_senza_city,lista_range,lista_sd,lista_variazioni_percentuali) # per avere un idea delle informazioni estratte

max(lista_variazioni_percentuali)
which(lista_variazioni_percentuali == max(lista_variazioni_percentuali))
lista_variabili_senza_city[2] 

# scopro quindi che i mesi hanno avuto un aumento percentuale maggiore!!!??? HO COMMESSO UN ERRORE! Ovviamente le variabili mesi e anni non possono essere inseriti in un calcolo simile.

max(lista_variazioni_percentuali[-2]) # escludo i mesi 
which(lista_variazioni_percentuali == max(lista_variazioni_percentuali[-2]))
lista_variabili_senza_city[4]

data_frame2 # visualizzo il dataframe per confermare che sia il "volume" ad aver avuto maggior variazione
# Dal minimo al massimo valore di "volume" vi è una percentuale di 1023,1%

# Vediamo se Volume risulta essere la variabile con più variabilità dal Coefficiente di variazione
lista_CV <- c(CV(year), CV(month), CV(sales),CV(volume),CV(median_price),CV(listings),CV(months_inventory))# mantengo CV year e month solo per avere un numero equivalente di valori quando vorrò unire la lista al Data frame
max(lista_CV) # è il secondo valore corrispondente al CV del volume

#aggiungiamo la lista Cv al df
cbind(data_frame2,lista_CV)

#Ritrovo il maggiore rispetto al CV, 
# Il Coefficiente di Variazione è l'indice che si usa per confrontare diverse variabili con diverse unità di misura.
which(lista_CV == max(lista_CV[-1:-2]))
lista_variabili_senza_city[4] # Sempre il "volume".

# Posso quindi concludere che è il volume ad essere la variabile, non banale(esclusi anni e mesi), con maggior variazone (CV). 
# Ma è anche quella con il più alto range relativo di escursione tra i valori (la percentuale di incremento calcoalta prima)


# PUNTO 5) DIVIDERE UNA VARIABILE IN CLASSI
# userò la variabile sales, numero di vendite. Per prima cosa ripendiamo il range
range(sales)

# dividerò sales in 5 classi quindi con 100 elementi al loro interno.

sales_cl <- cut(sales,seq(0,500,100))
table(sales_cl)

# Distribuzione di frequenza
distr_freq_sales <- as.data.frame(     
        cbind( 
            ni = table(sales_cl), 
            fi = table(sales_cl)/n, 
            Ni = cumsum(table(sales_cl)), 
            Fi = cumsum(table(sales_cl)/n))) 
distr_freq_sales

#Grafico a barre
ni = table(sales_cl) 
ni
barplot(ni,    
        main = "Distribuzione in classi del numero di vendite per mese",
        xlab = "Vendite per Mese", 
        ylab = "Numero di Mesi", 
        ylim = c(0,150), 
        col = "lightblue",  
        names.arg = row.names(ni))  

# PUNTO 6) INDICE DI GINI DELLE CLASSI DI SALES
gini.index(sales_cl) # risultato 0.7796 la funzione è più Eterogenea che Omogenea

#Indovinare l'indice di Gini della variabile city
table(city)
#Ogni variabile di city ha 60 elementi, perciò l'indice di Gini sarà: 1. Equidistribuzione del numero degli elementi.
gini.index(city) # Conferma

# PUNTO 7) Probabilità che una riga a caso presenti la variabile a city di "Beumont"
length(city)
table(city)["Beaumont"] # numero di osservazioni con Beumont

probabilità_beaumont <- table(city)["Beaumont"]/length(city)
probabilità_beaumont*100
# Probabilità 0.25, ossai 25%, infatti 60/240 è uguale a 1/4 che è uguale a 0.25

#Probabilità che riporti il mese di luglio
length(month)
table(month)["7"] # Come sopra
probabilità_luglio <- table(month)["7"]/length(month) 
probabilità_luglio *100
#Probabilità dell' 8,33%

#Probabilità condizionata mese: dicembre, anno 2012
length(year)
length(month)
table(year)["2012"]
table(month)["12"]

prob_2012 <- (table(year)["2012"]/length(year))
prob_2012

prob_con_dic_2012 <- (table(month)["12"]/length(month))*(table(year)["2012"]/length(year))
prob_con_dic_2012*100

# Probabilità del 1.66%. Per le probabiltià condizionate bisogna moltiplicare P1*P2 
# La probabilità che si estragga dicembre del 2012 è appunto 1.66%, ossia la probabilità di rilevare il valore dicembre,
# combinata alla probabilità di rilevare il valore 2012 ossia 20%
print(8.33*0.2) #un ulteriore calcolo di conferma il 20 % di 8.33 fa 1.66% come precedentemente estratto.

# PUNTO 8) Trovare Prezzo medio e aggiungere al DF una colonna col prezzo medio.
prezzo_medio_lista # l'avevamo assegnato precendentemente. Egli è stato definito come = (volume/ sales)*(10/6)

re_texas_aggiunto <- cbind(re_texas,prezzo_medio_lista) # ecco una nuova riga col prezzo medio di ogni mese di vendita

# PUNTO 9)Crea una colonna con l'efficacia delle inserzioni. 
#Possiamo rapportare gli annunci al numero di vendite che generano, oppure al numero di entrate(volume)
#Entrambi sono misure valide in quanto andrebbero valutati elementi esterni al dataframe per avere un'idea della reale efficacia degli annunci.
#Infatti alcuni annunci potrebbero generare meno vendite ma più volume se per qualche ragione: stato del mercato immobiliare, zona della città, picco di fama dell'agenzia immobiliare.
#Allo stesso modo gli annunci potrebbero essere considerati efficaci se nello stesso luogo, per la stessa stagione dell'anno, producono più vendite rispetto all'anno precedente.
#La domanda: l'annuncio è al massimo grado efficace se ha il più alto possibile grado di conversione in vendita, o l'annuncio è al massimo efficace se mi trova conpratori, anche pochi, ma disposti acquistare case costossissime?
#Prenderemo entrabe le strade, anche se credo che l'indice più rilevante sia quello tra annunci e volume


listings_per_volume <- (listings/(volume))
listings_per_volume # Questo valore indica quanti annunci servono per produrre un milione di dollari, il valore minimo sarà ovviamente 1
perc_listings_per_volume <- (volume*100)/listings
perc_listings_per_volume  # L'equivalente in percentuale ossia la percentuale di conversione in milioni di dollari di un annuncio.


#aggiustiamo il valore, e normalizziamolo
efficacia_listings_mill <- prezzo_medio_lista*perc_listings_per_volume/10^3 # efficacia di produrre dollari su scala delle migliaia
efficacia_listings_norm <- (efficacia_listings_mill - min(efficacia_listings_mill))/(max(efficacia_listings_mill)-min(efficacia_listings_mill))  
efficacia_listings_norm*10 # Così diamo un valore da 0 a 10 della loro efficacia relativa perché li abbiamo normalizzati e l'osservazione ched avrà valore di 1 sarà il massimo e il migliore nel gruppo
plot(efficacia_listings_norm*10,c(1:240)) #Un semplice gradfico per avere un idea della distribuzione dell'indice di efficacia

max(efficacia_listings_mill) #Valore massimo
which(efficacia_listings_mill == max(efficacia_listings_mill)) #Ora sappiamo che è il valore 115 del Data Frame ad avere avuto efficacia massima tra gli annunci. 
#Similmente possiamo fare col minimo
min(efficacia_listings_mill) #Valore minimo di conversione annunci Ricavi
which(efficacia_listings_mill == min(efficacia_listings_mill))

re_texas_aggiunto[115,] 
re_texas_aggiunto[193,] #mostro il migliore ed il peggiore.

# Per sintesi utilizzo una scorciatoria per trovare il più efficace nel generare vendite

listings_per_sales <- (listings/sales)
listings_per_sales # Questo valore indica quanti annunci servono per generare una vendita, il valore minimo sarà ovviamente 1
perc_listings_per_sales <- (100*sales)/listings
perc_listings_per_sales  # L'equivalente in percentuale. Percentuale di conversione

max(perc_listings_per_sales)
which(perc_listings_per_sales == min(perc_listings_per_sales))
re_texas_aggiunto[133,] # la stagione del anno 2011 a Tyler mese di Gennaio ha il migior ratio di conversione annunci in vendite

# PUNTO 10)SUMMARY di Sales sulle variabili condizionate a City, Year, Month
# senza Dplyr
# Dividiamo city

city_divisa <- split(re_texas,city) #divido in gruppi

mean(city_divisa$Beaumont$sales)
summary(city_divisa$Beaumont$sales) # summary dei sales in Beumont

mean(city_divisa$Tyler$sales)
summary(city_divisa$Tyler$sales) # summary dei sales in Tyler

# Con dplyr
install.packages("dplyr")
library(dplyr)

re_texas %>%
  group_by(city) %>%
  summarise(media=mean(sales),
  dev.std= sd(sales)) # decisamente il metodo più veloce

# Per il volume
re_texas %>%
  group_by(city) %>%
  summarise(media=mean(volume),
  dev.std= sd(volume))  

# Per gli annunci
re_texas %>%
  group_by(city) %>%
  summarise(media=mean(listings),
  dev.std= sd(listings)) 

# summary di Sales per la variabile Year

re_texas %>%
  group_by(year) %>%
  summarise(media=mean(sales),
  dev.std= sd(sales)) 


re_texas %>%
  group_by(year) %>%
  summarise(media=mean(volume),
  dev.std= sd(volume))  

re_texas %>%
  group_by(year) %>%
  summarise(media=mean(listings),
  dev.std= sd(listings)) 


# Summary di Sales per la variabile Month

re_texas %>%
  group_by(month) %>%
  summarise(media=mean(sales),
  dev.std= sd(sales)) 


re_texas %>%
  group_by(month) %>%
  summarise(media=mean(volume),
  dev.std= sd(volume))  

re_texas %>%
  group_by(month) %>%
  summarise(media=mean(listings),
  dev.std= sd(listings)) 

#Visualizzazione
install.packages("ggplot2")
library(ggplot2)

# City per Sales
ggplot(re_texas)+
        geom_col(aes(x=city, y = sales),
                fill = "lightblue")+ 
        labs(title = "Vendite per Città", 
                x ="Città", 
                y = "Numero di Vendite")+
        scale_y_continuous(breaks = seq(0, 20000, 5000),
                label = c( "0","5k", "10k","15k", "20k"))+
        theme_classic()   

#Notiamo che la città con più vendite è Tyler, la città con meno vendite è Wichita falls
#facciamo la stessa cosa per volume
      
 p1 <-ggplot(re_texas)+
        geom_col(aes(x=city, y = volume),
                fill = "darkgreen")+ 
        labs(title = "Volume per Città", 
                x ="Città", 
                y = "Prezzo vendite in Miliardi")+
        scale_y_continuous(breaks = seq(0, 3000, 1000),
                label = c( "0","1B", "2B","3B"))+
        theme_classic()       
p1
#sempre Tyler la città con più volume
#vediamo i listings

p2 <- ggplot(re_texas)+
        geom_col(aes(x=city, y = listings),
                fill = "#68722f")+  
        labs(title = "Annunci per Città", 
                x ="Città", 
                y = "Numero di Annunci")+
        scale_y_continuous(breaks = seq(0, 150000, 50000),
                label = c( "0","50k", "100K","150K"))+
        theme_classic()      
p2

#Già possiamo notare qualcosa di interessante, ossia che nonostante BS Collage abbia meno annunci di Beumont, BS College supera  Beumont in volume.
#Proviamo ad affiancare il grafico così da averlo ben chiaro

install.packages("gridExtra")        
library("gridExtra")

grid.arrange(p1, p2, ncol = 2) # Qui li vediamo affiancati

# Grafico Vendite per Mesi, su tutti gli anni, per sapere in quale mese dell'anno si vende di più
ggplot(re_texas)+
        geom_col(aes(x=month, y = sales),
                fill = "lightblue"    
                )+ 
        labs(title = "Vendite per Mese", 
                x ="Mese", 
                y = "Vendite")+
        scale_y_continuous(breaks = seq(0, 6000, 1000),
                label = c( "0","1k", "2k","3k", "4k","5k", "6k"))+
        scale_x_continuous(breaks = seq(1, 12, 1),
                label = c( "Gen","Feb", "Mar","Apr", "Mag","Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic"))+       
        theme_classic() 

# Volume per Mesi
ggplot(re_texas)+
        geom_col(aes(x=month, y = volume),
                fill = "darkgreen"    
                )+ 
        labs(title = "Volume per Mesi", 
                x ="Mese", 
                y = "Volume in Milioni")+
        scale_y_continuous(breaks = seq(0, 800, 100),
                label = c( "0","100M", "200M","300M", "400M","500M", "600M","700M","800M"))+
        scale_x_continuous(breaks = seq(1, 12, 1),
                label = c( "Gen","Feb", "Mar","Apr", "Mag","Giu", "Lug", "Ago", "Set", "Ott", "Nov", "Dic"))+       
        theme_classic() 

split2 <- split(re_texas,month)
sum(split2$"1"$volume) #Piccolo controllo del grafico
# Come si può vedere le vendite a Gennaio sono poco sotto le 400M, esattamente come ci dà la formula.


#Grafico su vendite e anni
p3 <- ggplot(re_texas)+
        geom_col(aes(x=year, y = sales),
                fill = "#810909"    
                )+ 
        labs(title = "Vendite per Anno", 
                x ="Anno", 
                y = "Vendite")+
         scale_y_continuous(breaks = seq(0, 10000, 2500),
                label = c( "0","2.5k", "5k","7.5K", "10k"))+
        theme_classic()
p3

#Sui listings
p4 <- ggplot(re_texas)+
        geom_col(aes(x=year, y = listings),
                fill = "lightblue"    
                )+ 
        labs(title = "Annunci per Anno", 
                x ="Anno", 
                y = "Annunci")+
         scale_y_continuous(breaks = seq(0, 100000, 25000),
                label = c( "0","25k", "50k","75K", "100k"))+
        theme_classic()
p4
#A quanto pare gli annunci sono diminuiti ma le vendite sono aumentate

#Sul volume
p5<- ggplot(re_texas)+
        geom_col(aes(x=year, y = volume),
                fill = "#15504d"    
                )+ 
        labs(title = "Annunci per Anno", 
                x ="Anno", 
                y = "Volume in milioni")+
        theme_classic()
p5
#Scopriamo la stessa cosa delle vendite, gli annunci sono diminuiti ma il volume di vendita è aumentato

grid.arrange(p3, p5, p4, ncol = 3)

# PUNTO 11) Boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città

ggplot(re_texas)+
        geom_boxplot(aes(x=city, y = median_price),
                fill = "#68722f")+  
        labs(title = "Boxplot dei Prezzi Mediani per Città", 
                x ="Città", 
                y = "Prezzo Mediano")+
        scale_y_continuous(breaks = seq(75000, 175000, 25000),
                label = c( "75k","100k", "125K","150K","175K"))+
                theme_classic()
                
# Possiamo subito notare come BS Station abbia il prezzo situato più alto di tutti, seguito da Tyler
# Wichita Falls ha gli immobili più economici, nessun immobile di WF arriva ai prezzi di BC Station
# Vi sono tre Outliers, forse tre osservazioni mal inserite. 
# Il primo quartile di BC Station è inferiore all'ultimo, viceversa accade per Wichita falls

# PUNTO 12) confrontare la distribuzione del volume totale delle vendite tra le varie città ma anche tra i vari anni.
ggplot(re_texas)+
        geom_col(aes(x=city, y = volume,fill=year))+ 
        labs(title = "Volume per Città", 
                x ="Città", 
                y = "Prezzo vendite in Miliardi")+
        scale_y_continuous(breaks = seq(0, 3000, 1000),
                label = c( "0","1B", "2B","3B"))+
        theme_classic()

ggplot(re_texas)+
        geom_col(aes(x=city,y =volume, group =year),position = "dodge", fill = "darkred")+
        labs(title = "Volume per Città", 
                x ="Città", 
                y = "Vendite in Miliardi")+
        theme_classic()

# Notiamo come negli anni tutti eccetto Wichita Falls hanno avuto un aumento nel Volume di vendite.
#Per i primi due anni Beumont e BC Station non hanno avuto aumenti.

#Provo un modo più diretto
#Grafico volume, anni, e città
ggplot(re_texas, aes(x=year,y = volume, fill=city))+
        geom_bar(stat = "identity", position = "dodge")

#BOXPLOT città-volume
ggplot()+ 
        geom_boxplot(aes(x=city,y = volume, fill=year))+
        labs(title = "Boxplot City per Volume", 
        x ="Città", 
        y = "Volume in Miliardi")+
        theme_classic()

# Boxplot distribuzione del volume.
ggplot(re_texas, aes(x=year,y = volume))+
        geom_boxplot()+
        labs(title = "Boxplot Anni per Volume", 
        x ="Anno", 
        y = "Vendite in Miliardi")+
        theme_classic()


# PUNTO 13) Gradico a barre sovrapposto
# Per il volume
ggplot(re_texas)+
        geom_col(aes(x=year, y = volume,fill=city))+ 
        labs(title = "Volume per Anno", 
                x ="Anno", 
                y = "Prezzo vendite in Miliardi")+
        scale_y_continuous(breaks = seq(0, 3000, 1000),
                label = c( "0","1B", "2B","3B"))+
        theme_classic()

# Per le vendite
ggplot(re_texas)+
        geom_col(aes(x=year, y = sales,fill=city))+ 
        labs(title = "Vendite per anno Barre sovrapposte", 
                x ="Anno", 
                y = "Vendite")+
        theme_classic()

#Grafico a barre con variabile Sales Normalizzata, normalizzo la variabile
sales_norm <- (sales - min(sales))/(max(sales)-min(sales)) 

ggplot(re_texas)+
        geom_col(aes(x=year, y = sales_norm,fill=city))+ 
        labs(title = "Vendite per anno Barre sovrapposte Sales Normalizzato", 
                x ="Anno", 
                y = "Vendite normalizzate")+
        theme_classic()


#Normalizzazione del Grafico a barre, qui normalizzo invece il grafico in generale.

data_anni <- split(re_texas,year)
data_anni
n_2010 <- sum(data_anni$"2010"$sales)
n_2011 <- sum(data_anni$"2011"$sales)
n_2012 <- sum(data_anni$"2012"$sales)
n_2013 <- sum(data_anni$"2013"$sales)
n_2014 <- sum(data_anni$"2014"$sales)
vector_norm <- c(n_2010,n_2011,n_2012,n_2013,n_2014)
anni <- c(2010,2011,2012,2013,2014)
graf_norm <- (vector_norm - min(vector_norm))/(max(vector_norm)-min(vector_norm)) 
graf_norm
df_3 <- data.frame(anni,graf_norm)

ggplot(df_3)+
        geom_col(aes(x=anni, y = graf_norm), fill=2)+ 
        labs(title = "Vendite per anno Barre Grafico Normalizzato", 
                x ="Anno", 
                y = "Valore annuale normalizzato")+
        theme_classic()

#il 2014 è il massimo e il 2011 è il minimo. Quindi il 2014 è stato l'anno migliore, mentre l'anno peggiore per le vendite è stato il 2011


# Notiamo quindi un miglioramento complessivo delle vndite dall'anno 2012. Un iniziale declino nell'anno 2011.
# La prominenza di vendite nella città di Tyler è stata maggiore a quella di tutti gli altri per tutti i cinque anni in esame.
# Tendenzialmente le vendite di Wichita Falls non siano migliorate ne peggiorate
# WC è l'ultima per numero di vendite

# PUNTO 14) Linechart della variabile prezzo medio. I prezzi si prestano per essere ben visualizzati sui linechart
# fortunatamente il DF è già organizzato in maniera cronologica. Per estrarre la linechart chiamerò parti del DF usando la strada più diretta.

ggplot()+
        geom_line(data=re_texas_aggiunto[0:60,], aes(x= seq(1,60,1), y =re_texas_aggiunto[0:60,]$prezzo_medio_lista, col="Beaumont"))+
        geom_line(data=re_texas_aggiunto[61:120,], aes(x= seq(1,60,1), y =re_texas_aggiunto[61:120,]$prezzo_medio_lista, col="B-C College"))+
        geom_line(data=re_texas_aggiunto[121:180,], aes(x= seq(1,60,1), y =re_texas_aggiunto[121:180,]$prezzo_medio_lista, col="Tyler"))+
        geom_line(data=re_texas_aggiunto[181:240,], aes(x= seq(1,60,1), y =re_texas_aggiunto[181:240,]$prezzo_medio_lista, col="Wichita Falls"))+
        labs(x = "Anno", y = "Prezzo medio", title= "Andamento annuale prezzo medio immobili per le 4 città analizzate")+
        scale_x_continuous(breaks = seq(1,61,12), 
                label = c("2010","2011","2012","2013","2014","2015"))+
         scale_y_continuous(breaks= seq(90000,210000,15000), 
                label= c("90k","105k","120k","135k","150k","165k","180k","195k","210k"))+
    scale_color_manual( 
        name = "Legenda",
        breaks = c("Beaumont","B-C College", "Tyler","Wichita Falls"),
        values = c("green3","red","purple","blue"),  
        labels = c("Beaumont","B-C College", "Tyler","Wichita Falls")  
    )

#Grafico prezzo medio tra le varie città
prezzo_medio_tra_città<- (re_texas_aggiunto[0:60,]$prezzo_medio_lista + 
        re_texas_aggiunto[61:120,]$prezzo_medio_lista +
        re_texas_aggiunto[121:180,]$prezzo_medio_lista +
        re_texas_aggiunto[181:240,]$prezzo_medio_lista)/4
prezzo_medio_tra_città

ggplot()+
        geom_line(data=re_texas_aggiunto[0:60,], 
                aes(x= seq(1,60,1), y =prezzo_medio_tra_città), col="red")+
        scale_x_continuous(breaks = seq(1,61,12), 
                label = c("2010","2011","2012","2013","2014","2015"))+
        scale_y_continuous(breaks= seq(140000,170000,10000), 
                label= c("140k","150k","160k","170k"))+
        labs(x = "Anno", y = "Prezzo medio generale", title= "Andamento annuale prezzo medio immobili nelle 4 città analizzate")+
        theme_light()

#piccolo controllo
sum(re_texas_aggiunto[c(60,120,180,240),]$prezzo_medio_lista)/4 # Il risultato della media dell'ultimo mese registrato su tutti e quattro le città
#Il risultato è 170171 che è poco sopra i 170 mila così come mostra il grafico della variazione del prezzo medio generale (combinato tra le città)

#Considerazioni: Il mercato mostra un trend positivo in salita, o meglio era in discesa fino la fine del 2012 e poi si è ripreso
#Sembra inoltre che ad inizio di ogni anno si ha drastici cali nel prezzo, mentre il prezzo cresce fino ad un suo picco annuale verso metà anno.

#Perfetto dalla nostra analisi possiamo dire che ora sappiamo quando è più conveniente comprare case in Texas!