# R_code.exam

# LISTA DEI CODICI:

# 1. R_code_first.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial2.r
# 4. R_code_point_pattern   
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r  
# 11. R_code_crop.r
# 12. Species Distribution Modelling
# 13. Exam project

############################################
############################################
############################################
 
# 1. R_code_first.r
  
# PRIMO CODICE R PER IL CORSO DI ECOLOGIA DEL PAESAGGIO.

# I COMMENTI SONO INSERITI AL DI SOPRA DEI VARI COMANDI.

# PER L'INSTALLAZIONE DI UN PACCHETTO O LIBERERIA, CIOÈ BLOCCHI DI FUNZIONI CHE POSSONO ESSERE IMPORTATI SU R, SI UTILIZZA IL COMANDO
# install.packages("..."), UTILIZZANDO LE VIRGOLETTE PERCHÈ SI FA DIALOGRARE R CON L'ESTERNO.

install.packages("sp")

# UNA VOLTA INSTALLATO UN PACCHETTO, PUÒ ESSERE RICHIAMATO ATTRAVERSO IL COMANDO library (...). NEL CASO SPECIFICO, IL PACCHETTO sp
# RIGUARDA I DATI SPAZIALI.

library(sp)

# require(...) è un altro comando per far partire le librerie.

# meuse È UN DATASET DISPONIBILE ALL'INTERNO DELL PACCHETTO sp, E SI RICHIAMA ATTRAVERSO LA FUNZIONE data (...).

data(meuse)

# SCRIVENDO SOLO meuse ALL'INTERNO DI R, È POSSIBILE OSSERVARE TUTTA LA TABELLA; SI TRATTA DI UNA TABELLA CON NUMEROSI RECORD
# CONTENTENTI OGNUNO COORDINATE X ED Y. 

meuse

# LA FUNZIONE head, PERMETTE DI OSSERVARE SOLO LA PARTE INIZIALE DELLA TABELLA, IN QUESTO CASO LE PRIME 6 RIGHE.

head(meuse)

# LA FUNZIONE names PERMETTE DI OSSERVARE I NOMI DELLE VARIABILI All'INTERNO DEL SET.

names(meuse)

# LA FUNZIONE summary PERMETTE DI FARE UN ABSTRACT DI TUTTE LE STATISTICHE UNIVARIATE PRESENTI ALL'INTERNO DEL SET.

summary(meuse)

# LA FUNZIONE pairs PERMETTE DI OTTENERE UN GRAFICO CHE CONTIENE I VARI PLOT CHE MOSTRANO GRAFICAMENTE LE CORRELAZIONI TRA LE VARIABILI.
# IN QUESTO CASO SI TRATTA DI UN NUMERO LIMITATO DI VARIABILI. 

pairs(meuse)

# ~ È UN SIMBOLO CHE IN R RAPPRESENTA IL SIMBOLO = ; IN QUESTO CASO VENGONO MESSE IN RELAZIONE I VARI ELEMENTI, UTILIZZANDO
# LE VIRGOLE CHE FUNGONO DA SEPARATORI DEGLI ARGOMENTI A FUNZIONE. GLI ARGOMENTI A FUNZIONE VANNO MESSI TRA PARENTESI.

pairs(~ cadmium + copper + lead , data = meuse)

# Esercizio: ripetere la funzione pairs, aggiungendo cadmio, rame, piombo e zinco.

pairs(~ cadmium + copper + lead + zinc , data = meuse)

# PER FARE UN SUBSET DI meuse, AD ESEMPIO DALLA 3 ALLA 6 VARIABILE, SI PUÒ UTILIZZARE IL COMANDO pairs(meuse[,3:6]), DOVE LE PARENTESI
# QUADRE RAPPRESENTANO IL SUBSET, I NUMERI IDENTIFICANO LE VARIABILI E LA VIRGOLA IN QUESTO CASO INDICA IL PUNTO DI PARTENZA, QUINDI 
# DALLA COLONNA 3 ALLA COLONNA 6.

pairs(meuse[,3:6])

# AL COMANDO SI PUÒ AGGIUNGERE UN ULTERIORE ARGOMENTO A FUNZIONE, CIOÈ col="...", CHE PERMETTE DI CAMBIARE IL COLORE, IN QUESTO CASO
# IN ROSSO.

pairs(meuse[,3:6], col="red")

# PER CAMBIARE IL CARATTERE SI UTILIZZA IL POINT CHARACTER, RAPPRESENTATO DAL COMANDO pch= ..., IN QUESTO CASO IL 19, CHE RAPPRESENTA
# IL SIMBOLO DEL PUNTO CHIUSO.

pairs(meuse[,3:6], col="red", pch=19)

# PER AUMENTARE LA DIMESNIONE DEI PUNTI SI UTILIZZA CHARACTER ESAGERATION, RAPPRESENTATO DAL COMANDO cex=..., IN QUESTO CASO 3, DUNQUE 
# AUMENTATO DI TRE VOLTE.

pairs(meuse[,3:6], col="red", pch=19, cex=3)

# PER DARE UN TITOLO AL GRAFICO SI UTILIZZA main="...".

pairs(meuse[,3:6], col="red", pch=19, cex=3, main="Primo pairs")

# esercizio: ripetere lo stesso pairs, includendo anche elevation; ELEVATION RAPPRESENTA LA VARIABILE NUMERO 7.

pairs(meuse[,3:7], col="red", pch=19, cex=3, main="Primo pairs")

# panel.correlations È LA CORRELAZIONE TRA DUE VARIABILI; <- SERVE AD ATTRIBUIRE UN NOME ALLA FUNZIONE.

panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(0, 1, 0, 1))
r1=cor(x,y,use="pairwise.complete.obs")
r <- abs(cor(x, y,use="pairwise.complete.obs"))
txt <- format(c(r1, 0.123456789), digits=digits)[1]
txt <- paste(prefix, txt, sep="")
if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
text(0.5, 0.5, txt, cex = cex * r)
}

# panel.smoothing FA UNA REGRESSIONE TRA LE VARIABILI.
 
panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
points(x, y, pch = pch, col = col, bg = bg, cex = cex)
ok <- is.finite(x) & is.finite(y)
if (any(ok))
lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
col = 1, ...)
}
 
# panel.histograms PERMETTE DI CREARE GLI ISTOGRAMMI.

panel.histograms <- function(x, ...)
{
usr <- par("usr"); on.exit(par(usr))
par(usr = c(usr[1:2], 0, 1.5) )
h <- hist(x, plot = FALSE)
breaks <- h$breaks; nB <- length(breaks)
y <- h$counts; y <- y/max(y)
rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# lower.panel È LA PARTE SOTTO LA DIAGONALE, upper.panel È LA PARTE SOPRA LA DIAGONALE, diag.panel È LA DIAGONALE.

pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

# Esercizio: mettere come lower panel lo smoothing, come diagonal panel gli istogrammi e come upper panel le correlazioni.

pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)

# LA FUNZIONE plot SERVE A PLOTTARE DUE O PIÙ VARIABILI E ANCHE IN QUESTO CASO GLI ARGOMENTI A FUNZIONE SONO TRA PARENTESI; plot È
# UNA FUNZONE GENERICA PER LA RAPPRESENTAZIONE GRAFICA DI OGGETTI IN R
# $ IN R SERVE A COLLEGARE DUE PEZZI, IN QUESTO CASO LA COLONNA E IL DATASET.

plot(meuse$cadmium,meuse$copper)

# LA FUNZIONE attach(...) PERMETTE DI EVITARE DI DOVER COLLEGARE OGNI VOLTA LE VARIABILI AL DATAFRAME.

attach(meuse)

# IN QUESTO CASO DOPO attach(meuse) SI PUÒ PLOTTARE DIRETTAMENTE SENZA IL SIMBOLO $.

plot(cadmium,copper) 

# PER CAMBIARE IL SIMBOLO IN TRIANGOLO, IL COLORE IN VERDE E IL TITOLO, SI UTILIZZA ANCHE IN QUESTO CASO pch, col E main.

plot(cadmium, copper, pch=17, col="green", main="primo plot")

# CON xlab= e ylab= È POSSIBILE CAMBIARE IL NOME ALLE LABELS DEL GRAFICO.

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame")

# AGGIUNGENDO COME ARGOMENTO ANCHE cex.lab=... e cex=... È POSSIBILE CAMBIARE LA GRANDEZZA DEI CARATTERI DELLE LABELS, IN QUESTO CASO
# DEL DOPPIO, E DEI SIMBOLI DEI PUNTI SUL GRAFICO.

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2) 

############################################
############################################
############################################
 
# 2. R_code_spatial.r  

# R spaziale: funzioni spaziali in Ecologia del paesaggio.

#multiframe automatico.

# INSTALLARE IL PACCHETTO GGally.

install.packages("GGally")

# RICHIAMARE LA LIBRERIA GGally.

library(GGally)

library(sp)

data(meuse)

head(meuse)

attach(meuse)

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

# PER CAMBIARE L'ETICHETTA.

plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinc")

#multiframe o multipanel: VISUALIZZAZIONE DI PIÙ GRAFICI ALL'INTERNO DELLA FINESTRA.

par(mfrow=c(1,2))

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

#invertiamo riga colonna in colonna riga.

par(mfrow=c(2,1))

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

ggpairs(meuse[,3:6])

#spatial

head(meuse)

# FAR LEGGERE AL SOFTWARE LE COORDINATE X E Y.

coordinates(meuse)=~x+y

spplot(mesue,"zinc")

############################################
############################################
############################################
 
# 3. R_code_spatial2.r

# R spatial

# libreria sp

# RICHIAMARE LA LIBRERIA sp.

library(sp)

# dati da utilizzare

data(meuse)

head(meuse)

#coordinate del dataset

coordinates(meuse)=~x+y

# spplot dei dati zinco; DISTRIBUZIONE SPAZIALE DELLA VARIABILE ZINCO. I DIVERSI COLORI RAPPRESENTANO I VALORI.

spplot(meuse,"zinc")

#spplot del rame; DISTRIBUZIONE SPAZIALE DELLA VARIABILE RAME.

# per vedere il nome si può usare names o head

spplot(meuse,"copper")

# bubble; DISTRIBUZIONE SPAZIALE DELLE VARIABILI RAPPRESENTATA DA PUNTI ATTRAVERSO BOLLE LE CUI DIMENSIONI NE RISPECCHIANO IL VALORE.

bubble(meuse,"zinc")

# bubble del rame colorato di rosso

bubble(meuse,"copper",col="red")

# foraminiferi (Sofia), carbon capture.
#array; CREAZIONE DI UN VETTORE; LA LETTERA c RAPPRESENTA IL TERMINE CONCATENATE E LA FRECCIA SERVE AD ATTRIBUIRE IL NOME. 

foram<-c(10, 20, 35, 55, 67, 80)

carbon <- c(5, 15, 30, 70, 85, 99)

plot(foram,carbon,col="green",cex=2,pch=19)

# dati dall'esterno covid-19.

# cartella da creare su Windows c:/lab; ambiente R È ORIENTATO SU UNA DIRECTORY SPECIFICA DEL COMPUTER.

setwd("c:/lab")

# funzione per leggere la tabella; TABELLA CON IL NUMERO DI CASI DI COVID IN VARI PAESI CON RELATIVE COORDINATE.
# header = TRUE: IDENTIFICA LA PRIMA RIGA DELLA MATRICE DEI DATI COME QUELLA CONTENENTE I NOMI DELLE VARIABILI.

read.table("covid_agg.csv",head=TRUE)

# PER ATTRIBUIRE ALLA FUNZIONE IL NOME covid.

covid <- read.table("covid_agg.csv",head=TRUE)

# L'ERRORE ERA DOVUTO ALLA PRESENZA DI VIRGOLETTE NEL FILE EXCEL, fill=TRUE FORZAVA MA NON RISOLVEVA IL PROBLEMA.

############################################
############################################
############################################
 
# 4. R_code_point_pattern 

# INSTALLAZIONE DEL PACCHETTO ggplot2.

install.packages("ggplot2")

# INSTALLAZIONE DEL PACCHETTO spatstat.

install.packages("spatstat")

# RICHIAMARE LA LIBRERIA spatstat.

library(spatstat)

# RICHIAMARE LA LIBRERIA ggplot2

library(ggplot2)

# Codice per analisi dei point pattern.

setwd("C:/lab")

covid <- read.table("covid_agg.csv", head=T)

# per visualizzare le prime righe.

head(covid)

# si può fare attach(covid) oppure si usa il simbolo $

plot(covid$country,covid$cases)

# ETICHETTE PARALLELE: 0; las SERVE A POSIZIONERE LE ETICHETTE SUL GRAFICO. CON IL NUMERO 0 AD ESEMPIO SI OTTENGONO ETICHETTE PARALLELE.

plot(covid$country,covid$cases,las=0)

# ETICHETTE ORIZZONTALI:1

plot(covid$country,covid$cases,las=1)

# ETICHETTE PERPENDICOLARI:2

plot(covid$country,covid$cases,las=2)

# ETICHETTE VERTICALI:3

plot(covid$country,covid$cases,las=3)

# per visualizzare più paesi.

plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5

#ggplot2

data(mpg)

head(mpg)

# IN QUESTO PACCHETTO È NECESSARIO SPECIFICARE LE COMPONENTI:
     
# data: IN QUESTO CASO mpg.
     
# aes: STA PER AESTHETIC; ALL'INTERNO DELLE PARENTESI SI SPECIFICANO LE COORDINATE X ED Y.
     
# tipo di geometria: VARIE OPZIONI RAPPRESENTATE DA LINEE, POLIGONI, PUNTI ETC...

ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# cambiamo gemoetria

ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()

ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

# ggplot per covid, richiamiamo prima i nomi con names(covid); IN QUESTO CASO SI RIPETE UTILIZZANDO I DATI RELATIVI ALLA TABELLA COVID
# CON L'AGGIUNTA DELL'ARGOMENTO size= PER ATTRIBUIRE LA DIMENSIONI AI PUNTI SUL GRAFICO, CHE RAPPRESENTANO IL NUMERO DI CASI.

ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# density
# creare un dataset per spatstat
# prima attach covid

attach(covid)

# IL COMANDO ppp PERMETTE DI CONVERTIRE I DATI IN UN POINT PATTERN.    
     
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
     
# CON density SI PUÒ OSSERVARE LA DENSITÀ DEI PUNTI.

d <- density(covids)

plot(d)
     
# FUNZIONE PER DISEGNARE UNA SEQUENZA DI PUNTI.
     
points(covids)

# PER SALVARE.
     
q()
     
# seconda parte 
     
# INSTALLAZIONE DELLA LIBRERIA rgdal.    
     
install.packages("rgdal")
     
# RICHIAMARE LA LIBRERIA rgdal; 
# DATI GEOSPAZIALI.     
     
library(rgdal)      
     
setwd("C:/lab")
     
# FUNZIONE PER RICHIAMARE NUOVAMENTE I DATI SALVATI PRECEDENTEMENTE.     
     
load(".RData")     

# ls() RESTITUISCE I NOMI DELLE VARIBILI.   
     
ls()
     
# RICHIAMARE LA LIBRERIA spatstat.     
     
library(spatstat)     
    
plot(d)
     
# palette per cambiare i colori del grafico; IL 100 RAPPRESENTA LA GRADAZIONE DEL COLORE.
     
cl <- colorRampPalette(c('yellow','orange','red')) (100)    
     
plot(d,col=cl) 
     
# Esercizio: cambiare i colori dal verde al blu.
     
cl <- colorRampPalette(c('green','yellow','purple','blue')) (100)

# PLOTTAGGIO PER VISUALIZZARE LA MAPPA CON I COLORI MODIFICATI.
     
plot(d,col=cl)  
     
points(covids) 
     
# LINEE COSTIERE DEL MONDO; ne_10m_coastline.shp È UN'IMMAGINE CARICATA DALL'ESTERNO, SALVATA PRECEDENTEMENTE NELLA CARTELLA lab.

coastlines <- readOGR("ne_10m_coastline.shp")

# add=T AGGIUNTA DI UN OGGETTO AL GRAFICO.
     
plot(coastlines, add=T)    
     
# Esercizio cambiare i colori alla mappa.
     
cl <- colorRampPalette(c('orange','light blue','blue','orange')) (100)
     
plot(d,col=cl)     
     
plot(coastlines, add=T, col= "black")    
     
# Esercizio: creare mappa di densità con i dati covid.
     
library(spatstat)

library(rgdal) # per le coastlines.

setwd("C:/lab")

load("point_pattern.RData")

ls()

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200) 

plot(d, col=cl5, main="density")

points(covids)

coastlines <- readOGR("ne_10m_coastline.shp")

plot(coastlines, add=T)
     
# interpolazione
     
library(sptatstat)
     
attach(covid)
     
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
     
# marks PERMETTE DI ATTRIBUIRE VALORI AI DATI DEL POINT PATTERN ASSOCIATI ALLA COLONNA cases.
     
marks(covids) <- covid$cases
     
# Smooth(...) FUNZIONE DI INTERPOLAZIONE DEI DATI.

s <- Smooth(covids)
     
plot(s)
     
# Esercizio: plottare s con i punti delle costalines.

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
     
plot(s, col=cl5, main="density")

points(covids)
     
# readOGR PERMETTE DI LEGGERE DATI OGR (TIPI DI DATI VETTORIALI).
     
coastlines <- readOGR("ne_10m_coastline.shp")

plot(coastlines, add=T)
     
# mappa finale
     
# DUE RIGHE E UNA COLONNA.

par(mfrow=c(2,1))

# densità
     
d <- density(covids)
     
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
     
plot(d, col=cl5, main="density")
     
points(covids)
     
coastlines <- readOGR("ne_10m_coastline.shp")
     
plot(coastlines, add=T)

# interpolazione del numero di casi

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
     
plot(s, col=cl5, main="estimate of cases")
     
points(covids)
     
coastlines <- readOGR("ne_10m_coastline.shp")
     
plot(coastlines, add=T
     
# ANNULLA I PLOT PRECEDENTI.
 
dev.off
     
# dati San Marino

# library spatstat
     
setwd("C:/lab")  
     
load("Tesi.RData")
 
ls()
     
head(Tesi)

attach(Tesi)
     
summary(Tesi)
     
# POINT PATTERN E COOORDINATE DEI PUNTI DI CAMPIONAMENTO.
     
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95)) 
     
dT <- density(Tesippp)
     
plot(dT)

points(Tesippp, col="black")
     
load("sanmarino.RData")     
     
ls() 
     
library(spatstat)
     
plot(dT)

points(Tesippp, col="green")
     
head(Tesi)
     
marks(Tesippp) <- Tesi$Species_richness
     
# INTERPOLAZIONE DEI DATI DI RICCHEZZA SPECIFICA.
     
interpol <- Smooth(Tesippp)
     
# GRAFICO RAPPRESENTANTE L'INTERPOLAZIONE.
     
plot(interpol)
     
points(Tesippp, col="green")
     
library(rgdal)
     
sanmarino <- readOGR("San_Marino.shp")
     
plot(sanmarino)
     
plot(interpol, add=T)
     
points(Tesippp,col="green")
 
plot(sanmarino,add=T)

#Esercizio: plot multiframe di densità e interpolazione 
     
par(mfrow=c(2,1))
     
plot(dT, main="Density of points")
     
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
     
points(Tesippp,col="green")

# Esercizio:multiframe di densità e interolazione uno a fianco all'altro.
     
par(mfrow=c(1,2))

plot(dT, main="Density of points")
     
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
     
points(Tesippp,col="green")

############################################
############################################
############################################
 
# 5. R_code_teleril.r 

# Codice r per analisi di immagini satellitari

# pacchetto raster.
     
# INSTALLARE IL PACCHETTO raster.
     
install.packages("raster")
     
# RICHIAMARE raster

library(raster)

setwd("C:/lab")
          
# DEFORESTAZIONE
# brick PERMETTE DI IMPORTARE I FILE RASTER; IMMAGINE RELATIVA ALL'ANNO 2011.

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

# RIFLETTANZA
   
# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# Cambiare i colori delle immagini.

cl <- colorRampPalette(c('black','grey','light grey'))(100) 

plot(p224r63_2011,col=cl)

cllow <- colorRampPalette(c('black','grey','light grey'))(5)  

plot(p224r63_2011, col=cllow) 

names(p224r63_2011)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

# attach NON FUNZIONA COL PACCHETTO raster E DUNQUE SI USA $

plot(p224r63_2011$B1_sre,col=clb)

# esrcizio: plottare con cambio colore rosso, arancione e giallo

clnir <- colorRampPalette(c('red','orange','yellow'))(100)

plot(p224r63_2011$B4_sre,col=clnir)

# multiframe

par(mfrow=c(2,2))

# blue

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

plot(p224r63_2011$B1_sre,col=clb)

# green

clg <- colorRampPalette(c('dark green','green','light green'))(100)

plot(p224r63_2011$B2_sre,col=clg)

# red

clr <- colorRampPalette(c('dark red','red','pink'))(100)

plot(p224r63_2011$B3_sre,col=clr)

# nir

clnir <- colorRampPalette(c('red','orange','yellow'))(100)

plot(p224r63_2011$B4_sre,col=clnir)

# per chiudere le imamagini

dev.off()

# il pc usa RGB per restituire immagini; stretch=lin PERMETTE DI AMPLIARE LA GAMMA DI COLORI.

plotRGB(p224r63_2011, red=3, green=2, blue=1, stretch="Lin")
   
# SCALARE DI UNO PER nir (4).
     
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# salvataggio immagine in pdf.

pdf("primografico.pdf")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off ()

# multiframe

par(mfrow=c(1,2))

plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()

# Esercizio: nir nella compontente R (red).
    
plotRGB(p224r63_2011, r = 4, g = 3, b = 2, stretch = "Lin")
     
# Esercizio: nir nella compontente G (green).

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# Esercizio: nir nella componente B (blue)

plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

# Giorno 2

library(raster)

setwd("C:/lab")

load("teleril.RData")
     
# IMMAGINE REALTIVA ALL'ANNO 1988.

p224r63_1988 <- brick("p224r63_1988_masked.grd")

plot(p224r63_1988)

par(mfrow=c(2,2))

# multiframe

par(mfrow=c(2,2))

# blue

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 

plot(p224r63_1988$B1_sre, col=clb)

# green

clg <- colorRampPalette(c('dark green','green','light green'))(100) # 

plot(p224r63_1988$B2_sre, col=clg)

# red

clr <- colorRampPalette(c('dark red','red','pink'))(100) # 

plot(p224r63_1988$B3_sre, col=clr)

# nir

clnir <- colorRampPalette(c('red','orange','yellow'))(100) # 

plot(p224r63_1988$B4_sre, col=clnir)

# B1: blue - 1
# B2: green - 2
# B3: red - 3
# B4: near infrared (nir) - 4

plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# Esercizio: plottare con nir

plo8tRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

# PLOTTAGGIO DELLE DUE IMMAGINI RELATIVE AL 1988 E AL 2011 PER UN CONFRONTO DIRETTO.

par(mfrow=c(2,1))

plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

dev.off()

# INDICE SPETTRALE; DVI STA PER DIFFERENCE VEGETATION INDEX.

# dvi= nir1988-red1988

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre

plot(dvi1988)

# Esercizio: calcolo del dvi del 2011.

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre

plot(dvi2011)

cldvi <- colorRampPalette(c('light blue','light green','green'))(100) 

plot(dvi2011, col=cldvi)

# Analisi multitemporale.

difdvi<- dvi2011 - dvi1988

plot(difdvi)

cldifdvi <- colorRampPalette(c('red','white','blue'))(100) 

plot(difdvi, col=cldifdvi)

# Visualizzazione dell'output

# multiframe 1988RGB  2011RGB (difdiv) 

par(mfrow=c(3,1))

plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plot(difdvi, col=cldifdvi)
 
dev.off()

# aggregate(...) PERMETTE DI VARIARE LA GRANA DELL'IMMAGINE; IMPOSTANDO fact=10 I PIXEL AUMENTANO DI DIECI VOLTE.

p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

# per vedere le caratteristiche dell'immagine

p224r63_2011
 
par(mfrow=c(2,1))

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# RISOLUZIONE ANCORA PIÙ BASSA.

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)

par(mfrow=c(3,1))

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dev.off()
     
# dvi2011 bassa risoluzione.

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre

plot(dvi2011lr50)

# dvi1988 bassa risoluzione.

p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
 
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

# DIFFERENZA DI DVI CON BASSA RISOLUZIONE.     
     
difdvilr50 <- dvi2011lr50 - dvi1988lr50

plot(dvi1988lr50)

plot(difdvilr50,col=cldifdvi)

# MULTIFRAME CON DVI A BASSA RISOLUZIONE.    
     
par(mfrow=c(2,1))

plot(difdvi, col=cldifdvi)

plot(difdvilr50, col=cldifdvi)
     
############################################
############################################
############################################    

# 6. R_code_landcover.r  
    
# CODICE R LANDCOVER.

# RICHIAMARE LIBRERIA raster.
     
libray(raster)

# INSTALLARE LIBRERIA RStoolbox.
     
install.packages("RStoolbox").

# RICHIAMARE LIBRERIA RStoolbox.     
     
library(RStoolbox)

setwd("c:/lab")
     
# IMPORTARE IL FILE RASTER.

p224r63_2011 <- brick("p224r63_2011_masked.grd

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# unsuperClass RAPPRESENTA LA CLASSIFICAZIONE NON SUPERVISIONATA.

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

p224r63_2011c

plot(p224r63_2011c$map)

clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 

plot(p224r63_2011c$map, col=clclass)
 
# ESPERIMENTO PER VEDERE LA SENSIBILITÀ RISPETTO AL NUMERO DI CLASSI; IN QUESTO CASO SI RIDUCE IL NUMERO DI CLASSI. 

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
  
plot(p224r63_2011c$map)

############################################
############################################
############################################    
   
# R_code_multitemp.r  

# Analisi multitemporale variazione della land cover.

setwd("C:/lab/")

# RICHIAMARE LA LIBRERIA raster.
 
library(raster)

RICHIAMARE LA LIBRERIA RStoolbox.

library(RStoolbox)

# RICHIAMARE LA LIBRERIA ggplot2

library(ggplot2)

# INSTALLARE PACCHETTO gridExtra.

install.packages("gridExtra")

# RICHIAMARE LIBRERIA gridExtra.

library(gridExtra)

# IMPORTAZIONE IMMAGINI.

defor1 <- brick("defor1_.jpg")
 
defor2 <- brick("defor2_.jpg")
 
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

# PER VISUALIZZARE IL NOME DEI CAMPI DELL'OGGETTO. IN ALTERNATIVA names(...)

defor1

# names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green

# Esercizio: plot della seconda data.
 
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")
 
par(mfrow=c(2,1))

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# classificazione non supervisionata

d1c <- unsuperClass(defor1, nClasses=2)

plot(d1c$map)

cl <- colorRampPalette(c('black','green'))(100) 

plot(d1c$map, col=cl)

# possibilità 2

cl <- colorRampPalette(c('green','black'))(100) # 

plot(d1c$map, col=cl)

# classificazione di defor2.

# Esercizio: classificare con due classi l'immagine satellitare defor2.

d2c <- unsuperClass(defor2, nClasses=2)

plot(d2c$map, col=cl)

# PER ANNULLARE LE MAPPE E VEDERLE INSIEME.

dev.off()
 
par(mfrow=c(2,1))

plot(d1c$map, col=cl)

plot(d2c$map, col=cl)

par(mfrow=c(1,2))

plot(d1c$map, col=cl)

plot(d2c$map, col=cl)

# freq PERMETTE DI VEDERE LA FREQUENZA DI PIXEL.

freq(d1c$map)

# 1) 308038   2) 33254   # tot = 341284 ; SOMMARE PER OTTENERE IL NUMERO MAX DI PIXEL DELLA PRIMA IMMAGINE.

totd1<- 33254 + 308038

# PERCENTUALE FORESTA: 90.26     AREE APERTE: 9.8

percent1 <- freq(d1c$map) * 100 / totd1

# FREQUENZA PIXEL IMMAGINE 2.

freq(d2c$map)

# tot = 342726

totd2<- 178381 + 164345

# PERCENTUALE FORESTA: 52.05     AREE APERTE: 47.95; 
# DA QUESTI VALORI SI EVINCE PURTROPPO UN FENOMENO MOLTO ACCENTUATO DI DEFORESTAZIONE NEL CORSO DEGLI ANNI.

percent2 <- freq(d2c$map) * 100 / totd2

# ANALISI GRAFICA VETTORIALE DELLA LANDCOVER IN BASE AI VALORI OTTENUTI.

cover <- c("Agriculture","Forest")
 
cover <- c("Agriculture","Forest")

# VALORI PRECEDENTI ALLA DEFORESTAZIONE ASSOCIATI AL TERMINE before.

before <- c(9.8,90.26)

# VALORI SUCCESSIVI ALLA DEFORESTAZIONE ASSOCIATI AL TERMINE after.

after <- c(47.95,52.05)

# DATAFRAME ATTRAVERSO I DATI PRECEDENTI.

output <- data.frame(cover,before,after)
 
View(output)

load("defor.RData")
        
par(mfrow=c(1,2))
        
cl <- colorRampPalette(c('black','green'))(100)
        
plot(d1c$map, col=cl)
        
plot(d2c$map, col=cl)

# ISTOGRAMMI % PRIMA DELLA DEFORESTAZIONE. 

ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")

# Eserciio: fare lo stesso per il periodo successivo alla deforestazione.

ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

# ATTRIBUIZIONE DEL NOME grafico 1 e grafico 2.
        
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
        
# grid.arrange PERMETTE IL PLOTTAGGIO DEI DUE GRAFICI NELLA STESSA FINESTRA.

grid.arrange(grafico1, grafico2, nrow = 1)

# spiegare al software il limite di y a 100.

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grid.arrange(grafico1, grafico2, nrow = 1)

############################################
############################################
############################################  

# 8. R_code_multitemp_NO2.r  

# codice per analisi ESA cambiamento NO2 da gennaio a marzo 2020

library(raster)

# Esercizio: caricare tutte le immagini; IMMAGINI SALVATE ANCHE IN QUESTO CASO NELLA CARTELLA lab. IL CARICAMENTO RIGUARDA UN'IMMAGINE 
# ALLA VOLTA.

EN01 <- raster("EN_0001.png")

EN02 <- raster("EN_0002.png")
 
EN03 <- raster("EN_0003.png")
 
EN04 <- raster("EN_0004.png")
 
EN05 <- raster("EN_0005.png")

EN06 <- raster("EN_0006.png")
 
EN07 <- raster("EN_0007.png")
 
EN08 <- raster("EN_0008.png")

EN09 <- raster("EN_0009.png")
 
EN10 <- raster("EN_0010.png")

EN11 <- raster("EN_0011.png")

EN12 <- raster("EN_0012.png")
 
EN13 <- raster("EN_0013.png")

cl <- colorRampPalette(c('red','orange','yellow'))(100) #

plot(EN01, col=cl)

plot(EN13, col=cl)

par(mfrow=c(1,2))

plot(EN01, col=cl)

plot(EN13, col=cl)

dev.off() 

# DIFFERENZA TRA IMMAGINE 13 E 1.

difno2 <- EN13 - EN01

cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)

# PLOTTAGGIO DI TUTTE E TREDICI LE IMMAGINI.

par(mfrow=c(4,4))
 
plot(EN01, col=cl)

plot(EN02, col=cl)

plot(EN03, col=cl)

plot(EN04, col=cl)

plot(EN05, col=cl)

plot(EN06, col=cl)

plot(EN07, col=cl)

plot(EN08, col=cl)

plot(EN09, col=cl)

plot(EN10, col=cl)

plot(EN11, col=cl

plot(EN12, col=cl)

plot(EN13, col=cl)

# ESSENDO MOLTO DISPENDIOSO E SCOMODO L'IMPORTAZIONE SINGOLA DI CIASCUN FILE, È POSSIBILE ANCHE L'IMPORTAZIONE SIMULTANEA DI TUTTI 
# I FILE.

# PRIMA DI PROCEDERE SI CREA UN'APPOSITA SOTTOCARTELLA ALL'INTERNO DELLA CARTELLA lab, DENTRO CUI SI INSERISCONO TUTTI I 13 FILE.

# SUCCESSIVAMENTE SI CAMBIA IL SETTAGGIO DELLA WORKING DIRECTORY, IMPOSTANDOLO SULLA NUOVA SOTTOCARTELLA DENOMINATA esa_no2.

setwd("C:/lab/")     

setwd("C:/lab/esa_no2/")

# rlist PERMETTE DI OSSERVARE TUTTA LISTA CONTENENTE I FILE CON ESTENSIONE .png.

rlist <- list.files(pattern=".png")
     
# PER IL CARICAMENTO SIMULTANEO SI UTTILIZA LA FUNZIONE lapply.
     
listafinale <- lapply(rlist, raster)

# CON stack UNIONE DI BANDE E CREAZIONE DI UN PACCHETTO DI DATI.
     
EN <- stack(listafinale)
     
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(EN, col=cl)
     
setwd("C:/lab/esa_no2")

rlist <- list.files(pattern=".png")
      
EN <- stack(listafinale)

# DIFFERENZA PIXEL IMMAGINE 13 E 1.

difEN <- EN$EN_0013 - EN$EN_0001
 
cld <- colorRampPalette(c('blue','white','red'))(100) # 

plot(difEN, col=cld)
     
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
 
plot(EN, col=cl)

# RANGE DATI CON MEDIA E MEDIANA.
 
boxplot(EN)
 
boxplot(EN, horizontal=T)

# ELIMINAZIONE DEI PUNTI CHE SI DISCOSTANO DALLA MEDIA.

boxplot(EN, horizontal=T,outline=F)

# CON axes AGGIUNTA ASSI.
 
boxplot(EN, horizontal=T,outline=F,axes=T)

############################################
############################################
############################################  
       
# 9. R_code_snow.r 

# COPERTURA NEVOSA

setwd("C:/lab/")

# INSTALLAZIONE PACCHETTO ncdf4.

install.packages("ncdf4")

# RICHIAMARE LIBRERIA ncdfr.

library(ncdf4)

# RICHIAMARE LIBRERIA raster.

library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
 
plot(snowmay,col=cl)

# CAMBIARE IL SETTAGGIO DELLA WORKING DIRECTORY IMPOSTANDOLO SULLA SOTTOCARTELLA snow ALL'INTERNO DELLA CARTELLA lab.

setwd("C:/lab/snow/")

# per importarli tutti insieme; IMPOSTARE L'ESTENSIONE .tif

rlist=list.files(pattern=".tif")

list_rast=lapply(rlist, raster)

snow.multitemp <- stack(list_rast)

par(mfrow=c(1,2))

# IN QUESTO CASO I VALORI NELLA LEGENDA SONO DIVERSI.

plot(snow.multitemp$snow2000r, col=cl)

plot(snow.multitemp$snow2020r, col=cl)

# PER FARE IN MODO CHE LA LEGENDA ABBIA GLI STESSI VALORI.

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))

plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

# DIFFERENZA TRA LA MAPPA DEL 2020 E DEL 2000.

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
 
cldiff <- colorRampPalette(c('blue','white','red'))(100)

plot(difsnow, col=cldiff)

plot(snow)

# PREVISIONE ANNO 2025

source("prediction.r")

# ESSENDO MOLTO LENTO, CARICARE DIRETTAMENTE IL FILE CHIAMATO predicted snow.

# PER INTERROMPERE L'IMPORTAZIONE PREMERE esc.

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
 
plot(predicted.snow.2025.norm, col=cl)

############################################
############################################
############################################

# 10. R_code_patches.r

setwd("C:/lab/")

# CARICARE LIBRERIA raster.

library(raster)

# CARICARE LIBRERIA ggplot2.

library(ggplot2)

# INSTALLARE PACCHETTO igraph.

install.packages("igraph")

# RICHIAMARE LIBRERIA igraph.

library(igraph)

# CARICAMENTO FILE ASSOCIANDO IL NOME d1c e d2c.

d1c <- raster("d1c.tif")

d2c <- raster("d2c.tif

par(mfrow=c(1,2))
                      
# IN QUESTO CASO LA MAPPA RISULTA SBAGLIATA PERCHÈ LA FORESTA È RAPPRESENTATA IN NERO.                      

cl <- colorRampPalette(c('green','black'))(100)

plot(d1c,col=cl)

plot(d2c,col=cl)

# PER ATTRIBUIRE LE GIUSTE COLORAZIONI BASTA INVERTIRE black E green.

# foresta: classe 1 ; agricoltura: classe 2
                                            
cl <- colorRampPalette(c('black','green'))(100) #

plot(d1c,col=cl)

plot(d2c,col=cl)
                      
# ANNULLARE I VALORI RELATIVI ALL'AGRICOLTURA; reclassify PERMETTE DI TRASFORMARE LA CLASSE 1 IN NA (INDICATORE DI VALORE MANCANTE).                     

d1c.for <- reclassify(d1c, cbind(1,NA))

par(mfrow=c(1,2))

cl <- colorRampPalette(c('black','green'))(100) #

plot(d1c,col=cl)

# SI ANNULLANO ANCHE NELLA SECONDA IMMAGINE.                      

d2c.for <- reclassify(d2c, cbind(1,NA))

par(mfrow=c(1,2))

# PLOTTAGGIO RELATIVO ALLA SOLA FORESTA.
                      
plot(d1c)

plot(d2c)                    

d1c.for.pacthes <- clump(d1c.for)

d2c.for.pacthes<- clump(d2c.for)
                      
writeRaster(d1c.for.pacthes, "d1c.for.patches.tif")

writeRaster(d2c.for.pacthes, "d2c.for.patches.tif")

# Esercizio: plottare entrambe le mappe una accanto all'altra

par(mfrow=c(1,2))

plot(d1c.for.pacthes)

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) #

par(mfrow=c(1,2))

plot(d1c.for.pacthes, col=clp)

plot(d2c.for.pacthes, col=clp)
                      
# PER VEDERE IL NUMERO DI PATCHES DI ENTRAMBI.

# d1c.for.pacthes = 301 patches

# d2c.for.pacthes = 1212 patches                      
                      
d1c.for.pacthes
                      
d2c.for.pacthes                      
                    
time <- c("Before deforestation","After deforestation")

npatches <- c(301,1212)
 
output <- data.frame(time,npatches

attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

############################################
############################################
############################################

# 11. R_code_crop.r

# RICHIAMARE LA LIBRERIA raster.

library(raster)
                                          
setwd("C:/lab/snow")
                     
# Esercizio: caricare le immagini snow.
                     
# IN QUESTO CASO COME PATTERN INSERIAMO snow PER METTERE AL SOFTWARE DI RICONOSCERE SOLO I FILE CHE LO CONTENGONO.

rlist <- list.files(pattern="snow")
                     
rlist
                                          
list_rast <- lapply(rlist, raster)
                     
snow.multitemp <- stack(list_rast)
                     
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(snow.multitemp,col=clb)
                     
# PER VEDERE I NOMI
                     
snow.multitemp
                     
plot(snow.multitemp$snow2010r, col=clb)

# zoom
                     
# INDICARE L'ESTENSIONE CORRETTA PER CONCENTRARCI SULL'ITALIA; IN QUESTO CASO MANCA ANCORA QUALCHE GRADO RIGUARDANTE L'ITALIA
# MERIDIONALE. 
                     
extension <- c(6, 18, 40, 50)
                     
# UTILIZZO FUNZIONE zoom PER APPLICARE L'INGRANDIMENTO 
                     
zoom(snow.multitemp$snow2010r, ext=extension)
                     
# CAMBIAREO L'ESTENSIONE.
                     
extension <- c(6, 20, 35, 50)
                     
# PLOTTANDO NUOVAMENTE CON LA NUOVA ESTENSIONE, OTTENIAMO L'IMMAGINE INTERA DELL'ITALIA.                     
                     
zoom(snow.multitemp$snow2010r, ext=extension) 
                     
# RILANCIARE IL PLOT DELL'IMMAGINE ORIGINALE.                     
                  
plot(snow.multitemp$snow2010r, col=clb)                    

# PER FARE UN RETTANGOLO A MANO.                    
   
zoom(snow.multitemp$snow2010r, ext=drawExtent())
                      
# FUNZIONE crop PERMETTE DI OTTENERE L'IMMAGINE RITAGLIATA.
                    
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
                     
plot(snow2010r.italy, col=clb)
                     
# Esercizio: eseguire crop dell'Italia con stack intero.
                     
# IN QUESTO CASO LE LEGENDE SONO DIVERSE
                     
snow.multitemp.italy <- crop(snow.multitemp, extension)
 
plot(snow.multitemp.italy, col=clb)
                     
# PER UNIFORMARE LA LEGENDA
                     
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))

# ATTRAVERSO IL boxplot SI PUÒ NOTARE COME IL VALORE MASSIMO DI COPERTURA NEVOSA TENDE A DIMINUIRE CON IL PASSARE DEL TEMPO.              
                     
boxplot(snow.multitemp.italy, horizontal=T,outline=F)
                     
############################################
############################################
############################################                     
                     
# 12. Species Distribution Modelling                     
                     
# IN QUESTO CASO NESSUN SETTAGGIO, MA SI UTLIZZANO DATI INTERNI AL PACCHETTO sdm.
                     
# INSTALLARE IL PACCHETTO sdm.                  
                     
install.packages("sdm")
                     
# RICHIAMARE LIBRERIA sdm.   
                     
library(sdm)

# RICHIAMARE LIBRERIA raster.                     
                     
library(raster)

# RICHIAMARE LIBRERIA rgdal.                     
                     
library(rgdal)
                     
# FUNZIONE system.file PERMETTE DI CARICARE CORRETTAMENTE I FILE DI QUESTO PACCHETTO.                     

file <- system.file("external/species.shp", package="sdm")
                     
species <- shapefile(file)

# PER VEDERE LE CARATTERISTICHE.                    
                     
species                     
                     
plot(species)
                     
plot(species[species$Occurrence == 1,],col='blue',pch=16)                     
                     
# PER AGGIUNGERE PUNTI AL plot PRECEDENTE.
                     
points(species[species$Occurrence == 0,],col='red',pch=16)
                     
path <- system.file("external", package="sdm")
                     
# FARE UNA LISTA DI FILE.                     

lst <- list.files(path=path,pattern='asc$',full.names = T) #       
       
# ALL'INTERNO ABBIAMO ELEVATION, PRECIPITATION, TEMPERATURE E VEGETATION.
                     
lst 
                     
# PREDITTORE PER LA DISTRIBUZIONE DELLA SPECIE.                     
                     
preds <- stack(lst)
                      
cl <- colorRampPalette(c('yellow','orange','red')) (100)
                      
cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)

# SI PUÒ OSSERVARE COME LA SPECIE SI DISTRIBUISCE PROBABILMENTE IN BASE ALLE VARIE VARIABILI.

plot(preds, col=cl)                     

# PLOTTARE SOLO VARIABILE ELEVATION.                     
                     
plot(preds$elevation, col=cl)
                                           
# AGGIUNGERE SOLO I PUNTI DOVE LA SPECIE RISULTA PRESENTE.
                     
points(species[species$Occurrence == 1,], pch=16)                     

# PLOTTARE SOLO LA VARIABILE TEMPERATURE.                     
                     
plot(preds$temperature, col=cl)
                     
# AGGIUNGENDO I PUNTI SI EVINCE CHE LA SPECIE NON GRADISCE MOLTO LE BASSE TEMPERATURE.                   
                     
points(species[species$Occurrence == 1,], pch=16)                    
                     
# PLOTTARE SOLO LA VARIABILE PRECIPITATION.
                     
plot(preds$precipitation, col=cl)
                     
# AGGIUNGENDO I PUNTI SI EVINCE CHE LA SPECIE MANIFESTA GRADIMENTO PER LE NORMALI PRECIPITAZIONI.
                     
points(species[species$Occurrence == 1,], pch=16)                     
                     
# PLOTTARE SOLO LA VARIABILE VEGETATION.
                     
plot(preds$vegetation, col=cl)
                     
# AGGIUNGENDO I PUNTI SI EVINCE CHE LA SPECIE GRADISCE UNA BUONA COPERTURA VEGETATIVA.
                     
points(species[species$Occurrence == 1,], pch=16)                     
                     
# FUNZIONE sdmData ?        ASSOCIAMO IL NOME d CHE STA PER DATI.           
                     
d <- sdmData(train=species, predictors=preds)                     
                     
# MODELLO
                     
m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm')
                      
# PREDIZIONE.
                     
p1 <- predict(m1, newdata=preds) 
                     
plot(p1, col=cl)
                     
points(species[species$Occurrence == 1,], pch=16)                     

############################################
############################################
############################################    
                     
# exam project
                     
# LINK PER IL SITO COPERNICUS

https://land.copernicus.vgt.vito.be/PDF/portal/Application.html



       
 
        
        


        
 









 



 
 





 
 
 
    
      
     



