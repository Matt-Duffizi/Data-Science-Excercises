ds_nascite <- read.csv("neonati.csv")
head(ds_nascite,5)

#Task 2 Descrivi il Dataset
nrow(ds_nascite) # ci sono 2500 osservazioni
ncol(ds_nascite) # su 10 variabili
names(ds_nascite) # Il nome delle variabili
attach(ds_nascite)

# anni madre, variabile quantitativa discreta, espressa in anni
# N.gravidanze, variabile quantitativa discreta
# fumatrici, variabile qualitativa nominale con valori 0 o 1. 1 se la madre è una fumatrice, 0 se non lo è.
unique(Fumatrici)
# Gestazione, variabile quantitativa discreta espressa in mesi
# Peso, varaibile quantiativa discreta, espressa in grammi, peso del bambino.
# Lunghezza, lunghezza del bambino, viaribile quantitativa discreta, espressa in mm
# Cranio, ampiezza del cranio del bambino, varaibile quantitativa discreta, espressa in mm.
# Tipo, tipo di parto, variabile qualitatita con valori Nat e Ces. Parto naturale e cesareo.
unique(Tipo.parto)
# Ospedale, variabile qualitativa nominale con valori Osp 1, Osp 2, Osp 3
# Sesso del bambino, variabile qualitativa nominale con valori M e F
unique(Sesso)
# Si vuole indagare circa il peso del neonato alla nascita che sarà la nostra variabile risposta,
# Si metterà particolare attenzione alle relazioni tra le varaibili della madre e il peso del neonato


#TASK 3 Analisi descrittiva
# Partiamo con un summary
summary(ds_nascite)
numero_fumatrici <- sum(Fumatrici)
numero_fumatrici # 104 fumatrici su 2500
numero_cesarei <- Tipo.parto == "Ces"
sum(numero_cesarei) # 728 Parti cesarei su 2500
probabilità_cesareo <- sum(numero_cesarei)/length(numero_cesarei)
probabilità_cesareo
numero_Maschi <- Sesso == "M"
sum(numero_Maschi) # 1244 neonati maschi su 2500

table(Fumatrici)
table(Tipo.parto)
table(Sesso)
table(Ospedale)

#Visualizzo le varaibili qualitative
x11()
par(mfrow = c(2,2))
barplot(table(Fumatrici))
barplot(table(Tipo.parto))
barplot(table(Sesso))
barplot(table(Ospedale))

#Visualizzo le variabili quantitative

table(Anni.madre)

# Noto che vi sono due valori errati, i primi due

which(Anni.madre == min(Anni.madre)) #il 1380 perché l'età della madre è di 0 anni
which(Anni.madre == 1) # il 1152 perché l'età della madre è di 1 anno

par(mfrow =c(1,1)) #Grafico della distribuzione per Anni madre
plot(density(Anni.madre)) # distribuzione circa normale
abline(v=mean(Anni.madre))

# Analisi N.Gravidanze
table(N.gravidanze)
#Grafico della distribuzione per N.Gravidanze
hist(N.gravidanze)

# Analisi Gestazione
table(Gestazione)
#Grafico della distribuzione per Gestazione
hist(Gestazione)
plot(density(Gestazione))

# Analisi Peso
table(Peso)
#Grafico della distribuzione per Peso
hist(Peso)
plot(density(Peso))
abline(v=mean(Peso)) # distribuzione circa normale

# Analisi Lunghezza
table(Lunghezza)
#Grafico della distribuzione per Lunghezza
hist(Lunghezza)
plot(density(Lunghezza))
abline(v=mean(Lunghezza)) # distribuzione circa normale

# Analisi Cranio
table(Cranio)
#Grafico della distribuzione per Cranio
hist(Cranio)
plot(density(Cranio))
abline(v=mean(Cranio)) # distribuzione circa normale

#TASK 4 Ipotesi che la media del peso e della lunghezza del campione non sia differenti da quelli della popolazione
# Il peso medio di un bambino è di 3.3 kg . Riferimento : sito: ospedalebambinogesu.it
# La lunghezza media di un bambino è di 500 mm. Riferimento : sito: ospedalebambinogesu.it

mean(Peso)
mean(Lunghezza)
t.test(Peso,mu=3300, conf.level = 0.95, alternative = "two.sided") # Con un P value di 0.12 non si rifiuta l'H0 ossia il campione è significativamente uguale alla popolazione
t.test(Lunghezza,mu=500, conf.level = 0.95, alternative = "two.sided") # Qui sembra invece che la lunghezza dei neonati sia significativamente inferiore a quella della media.

#Provo con un z Testa
install.packages("TeachingDemos")
library(TeachingDemos)

z.test(Peso,3300,3300*0.341, alternative = "two.sided", conf.level = 0.95)
z.test(Lunghezza,500,500*0.341, alternative = "two.sided", conf.level = 0.95)
a <- rnorm(10000,mean=500, sd= 500*0.341)
# Con lo Z test invece entrambi sono nella media generale.

#Task 5 Per le stesse variabili, o per altre per le quali ha senso farlo, verifica differenze significative tra i due sessi

#Peso - Sesso
t.test(Peso ~ Sesso) # H nulla rifiutata c'è differenza tra il peso dei due sessi.
boxplot(Peso ~ Sesso)

#Lunghezza - Sesso
t.test(Lunghezza ~ Sesso) # H nulla rifiutata c'è differenza tra la lunghezza dei due sessi.
boxplot(Lunghezza ~ Sesso)

#Cranio - Sesso
t.test(Cranio ~ Sesso) # H nulla rifiutata c'è differenza tra le dimensioni del cranio dei due sessi.
boxplot(Cranio ~ Sesso)

#Eseguiamo la verifica su alcune varaibili che non dovrebbero essere correlate con il sesso del nascituro.
#fumatrice - Sesso
t.test(Fumatrici ~ Sesso) # H nulla non è rifiutata, non c'è relazione tra l'essere fumatrice della madre  il sesso del neonato
boxplot(Fumatrici ~ Sesso)

#Anni.madre- Sesso
t.test(Anni.madre ~ Sesso) # H nulla non è rifiutata, non c'è relazione tra l'età della madre il sesso del neonato
boxplot(Anni.madre ~ Sesso)

#Gestazione- Sesso
t.test(Gestazione ~ Sesso) # Risultato interessante,  H nulla è rifiutata, sembra che i Maschi abbiano un maggiore numero di mesi di Gestazione necessari
#La media del gruppo M è di 39.22 e quella di F è di 38.7 .Ed è una differenza significativa.
boxplot(Gestazione ~ Sesso) #il grafico presenta una forma particolare giacché il boxplot dei maschi ha un 50 percentile vicinissimo al 75 percentile
# Indagando alcune fonti ho riscontrato pareri discordanti ma nessuno convincente.
# Le ricerche sia contro che a favore della relazione trovata non erano eseguite in Italia, perciò il patrimonio genetico delle madri potrebbe variare fin troppo da potermi fornire una chiusura al quesito soddisfacente
# Posso solo intuire che

#N.gravidanze- Sesso
t.test(N.gravidanze ~ Sesso) # H nulla non è rifiutata, non c'è relazione tra il numero di gravidanze della madre e il sesso del neonato
boxplot(N.gravidanze ~ Sesso)

# Il tipo di parto e sopratutto l'Ospedale dove si partorisce sono variabili che non ha senso rapportarle al sesso del bambino.

#Task 6 in quale ospedale si fanno più cesarei

table(Ospedale[Tipo.parto == "Ces"])
sum(table(Ospedale[Tipo.parto == "Ces"])) 

#728 cesarei totali dal campione come trovato sopra. Nell'ospedale 2 dal campione si sono fatti più cesarei
# Ora vediamo se possiamogeneralizzare l'ipotesi

table(Ospedale)
media_cesarei_per_osp <- table(Ospedale[Tipo.parto == "Ces"])/table(Ospedale)
media_cesarei_per_osp # L'ospedale 2 risulta quello con più parti cesarei in media dal campione, vediamo se è significativamente maggiore
mean(media_cesarei_per_osp) # la media delle medie, per poi sfruttare il teorema del limite centrale

# creo una variabile dummy

Tipo_parto_numerico <- ifelse(Tipo.parto == "Ces", 1,0)
aggiunta_dummy1 <- cbind(ds_nascite,Tipo_parto_numerico)
mean(Tipo_parto_numerico)
osp2 <- aggiunta_dummy1[aggiunta_dummy1$Ospedale == "osp2", "Tipo_parto_numerico"]
osp2
mean(osp2)
t.test(osp2,mu = mean(media_cesarei_per_osp)) #Per Mu potrei passare anche la media dei dummy1, il valore infatti è equivalente.

# Con un P value di 0.6 non rifiuto l'ipotesi nulla. Nessun ospedale fa significativamente più cesarei.
# L'Ospedale 2 non si discosta dalla media significativamente.

#ANALISI MULTIDIMENSIONALE
#Task 1
#Partiamo con un sommario, e una visualizzazione delle relazioni 
summary(aggiunta_dummy1) # Uso il dataframe con l'aggiunta del dummy per lavorare anche con la variabile Tipo.parto.
n <- nrow(aggiunta_dummy1)
n # Numero di osservazioni, può sempre servire.

aggiunta_dummy1$Tipo.parto <- NULL
aggiunta_dummy1$Ospedale <- NULL
aggiunta_dummy1$Sesso <- NULL
Sesso_numerico <- ifelse(Sesso == "M", 1,0)
aggiunta_dummy1_2 <- cbind(aggiunta_dummy1,Sesso_numerico)
head(aggiunta_dummy1_2) # Ora ho un DF con solo variabili numeriche.

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(aggiunta_dummy1_2,upper.panel = panel.smooth, lower.panel= panel.cor)

# Riguardo la nostra variabile risposta, sembra che vi siano correlazioni tra:
# Peso e Lunghezza, Peso e Cranio, Peso e Gestazioni (quest'ultima sembra avere una relazione parabolica)
# Dal grafico risultano altre relazioni interessanti tra Lunghezza-Cranio, Cranio- Gestazione e Lunghezza - Gestazione

# Indago graficamente le variabili dummy che sul grafico a scatterplot le loro relazioni non emergono
boxplot(Peso ~ Tipo_parto_numerico) # Non sembra esserci una differenza sostanziale tra le due variabili
t.test(Peso ~ Tipo_parto_numerico) # Il P value di 0.89 mi conferma quanto sopra

boxplot(Peso ~ Fumatrici) #Appare una relazione, sembra che le madri fumatrici partoriscano figli con Peso inferiore.
mean(Peso[Fumatrici == 1]) # Controllo con le medie
mean(Peso[Fumatrici == 0])
t.test(Peso ~ Fumatrici) #Eppure dal T test emerge che la differenza tra le due variabili non è significativa.

#Giacché lo scopo dell'indagine è verificare l'influenza dello stato di salute della madre sul figlio indago i rapporti della variabile fumatrici
boxplot(Gestazione ~ Fumatrici)
t.test(Gestazione ~ Fumatrici) # Vi è una differenza significativa che la gestazione di una madre fumatrice sia più lunga .

boxplot(Lunghezza ~ Fumatrici)
t.test(Lunghezza ~ Fumatrici) # non vi è differenza tra la lunghezza di un neonato da madre fumatrice o non

t.test(Cranio ~ Fumatrici) # non vi è differenza tra la lunghezza

# Task 2 Ricordati qual è l’obbiettivo dello studio e indaga le relazioni a due a due, soprattutto con la variabile risposta

mod1 <- lm(Peso ~., data= aggiunta_dummy1_2)
summary(mod1)
# Sembra che le relazioni significative con il peso, siano Lunghezza,Cranio, Gestazione, Sesso, e in misura inferiore con lo 0.013% e il 0.12% di Pvalue il numero di gravidanze e il tipo di parto
# La variabile con maggiore variazione è il sesso, seguita da Gestazione e fumatrici.
# Sembra che fumatrici non sia una variabile significativa.
#R quadro del modello è di 0.727


#Task 3 Cerca il modello “migliore”, utilizzando tutti i criteri di selezione che conosci e spiegali.
#Togliamo le variabili poco influenti

mod2 <- update(mod1, ~. -Anni.madre) #Senza l'età della madre
summary(mod2)

mod3<- update(mod2, ~. -Fumatrici) #Senza la variabile Fumatrici
summary(mod3)

# Le R quadrato sono le stesse per i 3 modelli

BIC(mod1,mod2,mod3) # Ha il BIC più piccolo degli altri, seppur di poco

anova(mod3, mod1) #con un P valore di 0.36, ci dice che aggiungendo i parametri tolti non si aggiungono informazioni rilevanti
car::vif(mod3) # tutt i valori sono sotto 5 quindi non ci sono probelmi di multicolinearità

#Cerco conferma con il pacchetto MASS
stepwise.mod <- MASS::stepAIC(mod1, direction="both",l=log(n))
summary(stepwise.mod) 
summary(mod3) 
BIC(mod3,stepwise.mod) #Il modello combacia col modello 3

# Task 4 Effetti di interazioni tra variabili
# Indago la possibilità di effetti congiunti circa il periodo di Gestazione
mod4 <- update(mod3, ~. +Gestazione:Fumatrici)
summary(mod4)
mod5 <- update(mod3, ~. +Gestazione:Tipo_parto_numerico)
summary(mod5)
#Nessun risutlato soddisfacente.
# Indago la possibilità di effetti congiunti circa la lunghezza
mod6 <- update(mod3, ~. +Lunghezza:Tipo_parto_numerico)
summary(mod6)
mod7 <- update(mod3, ~. +Lunghezza:Cranio)
summary(mod7)
# Indago la possibilità di effetti congiunti circa il numero di gravidanze
mod8 <- update(mod3, ~. +N.gravidanze:Cranio)
summary(mod8)
mod9 <- update(mod3, ~. +N.gravidanze:Gestazione)
summary(mod9)
# Non sembrano esserci effetti congiunti, ma protebbero esserci effetti non lineari
# Dalla visualizzazione delle relazioni, quella fatta prima con pairs, sembra che la linea nel grafico Peso-Gestazioni formi una curva
# L'andamento potrebbe essere parabolico.

# TASK 5 Analisi dei residui

par(mfrow=c(2,2))
plot(mod3)

# Residui disposti casualmente attorno a 0 senza pattern. Media di poco ricurvo
# Normale, quasi tutta sulla bisettrice
# Scale location, nessun pattern visualizzato
# 1 punto nella distanza di cook 1551, sopra la soglia di avvertimento 0.5, ma non sopra la soglia di allarme 1

#Valori di leva
lev <- hatvalues(mod3)
par(mfrow=c(1,1))
p <- sum(lev)
soglia= 2*p/n
plot(lev)
abline(h=soglia, col=2)
lev[lev>soglia]

#Test sui residui
shapiro.test(residuals(mod3)) # si rifiuta
plot(density(residuals(mod3))) # al grafico appare come una normale, quindi non vorrei ancora scartare il mod3
lmtest::bptest(mod3) # si rifiuto l'ipotesi nulla, quindi vi è eteroschedasticità, i residui hanno varianza non costante
lmtest::dwtest(mod3) # I residui sono sono autocorrelati


# TASK6 Bontà del modello
# è dovuto dall Rquadro che è sufficientemente buono a 0,727
# Solo un valore supera la distanza di cook, di 0,5 ma non quella di allarme di 1

#TASK 7 Previsione per il peso di una neonata, considerato che la madre è alla terza gravidanza e partorirà alla 39esima settimana

predict.lm(mod3, newdata=data.frame( N.gravidanze= 3,
                                    Gestazione= 39,
                                    Lunghezza=mean(Lunghezza),
                                    Cranio=mean(Cranio),
                                    Tipo_parto_numerico= mean(Tipo_parto_numerico),
                                    Sesso_numerico= 0))
# Peso 3271,687 Grammi
# Se vogliamo semplificare la previsione possiamo supporre che il parto non sia Cesareo dato che ha probabilità 0.29
probabilità_cesareo

predict.lm(mod3, newdata=data.frame( N.gravidanze= 3,
                                    Gestazione= 39,
                                    Lunghezza=mean(Lunghezza),
                                    Cranio=mean(Cranio),
                                    Tipo_parto_numerico= 0,
                                    Sesso_numerico= 0))

# Previsione peso: 3280.43


#TASK 8
# Visualizzazione variabili in 3D
install.packages(c("rgl", "car"))
library(car)
library(rgl)
scatter3d(Peso ~ Lunghezza + Gestazione)
scatter3d(Peso ~ Lunghezza + Cranio)
scatter3d(Peso ~ Lunghezza + N.gravidanze)

#Grafici 2-D
install.packages("ggplot")
library(ggplot2)

#Grafico di peso per lunghezza
x11()
ggplot(data=aggiunta_dummy1_2)+
    geom_point(aes(x=Peso,
                    y=Lunghezza, col=Sesso))+
    geom_smooth(aes(x=Peso,
                    y=Lunghezza, col=Sesso),se=F,method="lm")+               
    geom_smooth(aes(x=Peso,
                    y=Lunghezza),col="black",se=F,method="lm")

#Vediamo che vi è una relazione lineare proporzionale tra il peso e la lunghezza del neonato 


#Grafico di peso per mesi di Gestazione
ggplot(data=aggiunta_dummy1_2)+
    geom_point(aes(x=Peso,
                    y=Gestazione, col=Tipo.parto))+
    geom_smooth(aes(x=Peso,
                    y=Gestazione, col=Tipo.parto),se=F,method="lm")+               
    geom_smooth(aes(x=Peso,
                    y=Gestazione),col="black",se=F,method="lm")

#Vediamo che vi è una relazione lineare proporzionale tra il peso e i mesi di gestazione

ggplot(data=aggiunta_dummy1_2)+
    geom_point(aes(x=Peso,
                    y=Anni.madre, col=Sesso))+
    geom_smooth(aes(x=Peso,
                    y=Anni.madre, col=Sesso),se=F,method="lm")+               
    geom_smooth(aes(x=Peso,
                    y=Anni.madre),col="black",se=F,method="lm")

#Vediamo che non vi è relazione tra gli anni della madre e il peso del neonato, infatti i punti sono sparsi in una nuvola omogenea e la retta è circa orizzontale.




