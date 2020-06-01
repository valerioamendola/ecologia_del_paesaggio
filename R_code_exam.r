# R_code.exam

# LISTA DEI CODICI:

1. R_code_first.r   
2. R_code_spatial.r   
3. R_code_spatial2.r
4. R_code_point_pattern   
5. R_code_teleril.r   
6. R_code_landcover.r   
7. R_code_multitemp.r   
8. R_code_multitemp_NO2.r   
9. R_code_snow.r   
10. R_code_patches.r  

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
# IN ROSSO

pairs(meuse[,3:6], col="red")

# PER CAMBIARE IL CARATTERE SI UTILIZZA IL POINT CHARACTER, RAPPRESENTATO DAL COMANDO pch= ..., IN QUESTO CASO IL 19, CHE RAPPRESENTA
# IL SIMBOLO DEL PUNTO CHIUSO.

pairs(meuse[,3:6], col="red", pch=19)

# PER AUMENTARE LA DIMESNIONE DEI PUNTI SI UTILIZZA CHARACTER ESAGERATION, RAPPRESENTATO DAL COMANDO cex=..., IN QUESTO CASO 3, DUNQUE 
# AUMENTATO DI TRE VOLTE.

pairs(meuse[,3:6], col="red", pch=19, cex=3)

# PER DARE UN TITOLO AL GRAFICO SI UTILIZZA main="..."

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

# panel.smoothing FA UNA REGRESSIONE TRA LE VARIABILI
 
panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
points(x, y, pch = pch, col = col, bg = bg, cex = cex)
ok <- is.finite(x) & is.finite(y)
if (any(ok))
lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
col = 1, ...)
}
 
# panel.histograms PERMETTE DI CREARE GLI ISTOGRAMMI

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

# Esercizio: mettere come lower panel lo smoothing, come diagonal apnel gli istogrammi e come upper panel le correlazioni 

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
 
2. R_code_spatial.r  

# R spaziale: funzioni spaziali in Ecologia del paesaggio

library(sp)

data(meuse)

head(meuse)

attach(meuse)

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

# PER CAMBIARE L'ETICHETTA:

plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinc")

#multiframe o multipanel: VISUALIZZAZIONE DI PIÙ GRAFICI ALL'INTERNO DELLA FINESTRA.

par(mfrow=c(1,2))

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

#invertiamo riga colonna in colonna riga:

par(mfrow=c(2,1))

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

#multiframe automatico:

install.packages("GGally")

library(GGally)

ggpairs(meuse[,3:6])

#spatial

head(meuse)

# FAR LEGGERE AL SOFTWARE LE COORDINATE X E Y.

coordinates(meuse)=~x+y

spplot(mesue,"zinc")

############################################
############################################
############################################
 
3. R_code_spatial2.r

# R spatial

# libreria sp

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

# foraminiferi (sofia), carbon capture)
#array; CREAZIONE DI UN VETTORE; LA LETTERA c RAPPRESENTA IL TERMINE CONCATENATE E LA FRECCIA SERVE AD ATTRIBUIRE IL NOME. 

foram<-c(10, 20, 35, 55, 67, 80)

carbon <- c(5, 15, 30, 70, 85, 99)

plot(foram,carbon,col="green",cex=2,pch=19)

#dati dall'esterno covid-19

# cartella da creare su Windows c:/lab; ambiente R È ORIENTATO SU UNA DIRECTORY SPECIFICA DEL COMPUTER.

setwd("c:/lab")

# funzione per leggere la tabella; TABELLA CON IL NUMERO DI CASI DI COVID IN VARI PAESI CON RELATIVE COORDINATE.
# header = TRUE: IDENTIFICA LA PRIMA RIGA DELLA MATRICE DEI DATI COME QUELLA CONTENENTE I NOMI DELLE VARIABILI.

read.table("covid_agg.csv",head=TRUE)

# PER ATTRIBUIRE ALLA FUNZIONE IL NOME covid.

covid <- read.table("covid_agg.csv",head=TRUE)

# L'ERRORE ERA DOVUTO ALLA PRESENZA DI VIRGOLETTE NEL FILE EXCEL, FILL=TRUE FORZAVA MA NON RISOLVEVA IL PROBLEMA.

############################################
############################################
############################################
 
4. R_code_point_pattern 

# INSTALLAZIONE DEL PACCHETTO ggplot2.

install.packages("ggplot2")

# INSTALLAZIONE DEL PACCHETTO spatstat.

install.packages("spatstat")

# SI RICHIAMANO LE LIBRERIE CHE SERVONO.

library(spatstat)

library(ggplot2)

# Codice per analisi dei point pattern.

setwd("C:/lab")

covid <- read.table("covid_agg.csv", head=T)

# per visualizzare le prime righe

head(covid)

# si può fare attach(covid) oppure si usa il simbolo $

plot(covid$country,covid$cases)

# ETICHETTE PARALLELE: 0; las SERVE A POSIZIONERE LE ETICHETTE SUL GRAFICO. CON IL NUMERO 0 AD ESEMPIO SI OTTENGONO ETICHETTE PARALLELE.

plot(covid$country,covid$cases,las=0)

# ETICHETTE ORIZZONTALI:1

plot(covid$country,covid$cases,las=1)

# ETCHETTE PERPENDICOLARI:2

plot(covid$country,covid$cases,las=2)

# ETICHETTE VERTICALI:3

plot(covid$country,covid$cases,las=3)

# per visualizzare più paesi

plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5

#ggplot2

data(mpg)

head(mpg)

# IN QUESTO PACCHETTO È NECESSARIO SPECIFICARE LE COMPONENTI:
     
# data: IN QUESTO CASO mpg
     
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

#  IL COMANDO ppp PERMETTE DI CONVERTIRE I DATI IN UN POINT PATTERN    
     
covids <- ppp(lon, lat, c(-180,180), c(-90,90))
     
# CON density SI PUÒ OSSERVARE LA DENSITÀ DEI PUNTI.

d <- density(covids)

plot(d)

points(covids)

# PER SALVARE.
     
q()
     
# seconda parte     
     
setwd("C:/lab")
     
load(".RData")     

ls()
     
library(spatstat)     
    
plot(d)
     
# palette per cambiare i colori del grafico
     
cl <- colorRampPalette(c('yellow','orange','red')) (100)    
     
plot(d,col=cl) 
     
# esercizio  colori dal verde al blu
     
cl <- colorRampPalette(c('green','yellow','purple','blue')) (100)
     
plot(d,col=cl)  
     
points(covids)     

coastlines <- readOGR("ne_10m_coastline.shp")
     
install.packages("rgdal")
     
library(rgdal) 
     
plot(coastlines, add=T)    
     
# esercizio cambiare i colori alla mappa
     
cl <- colorRampPalette(c('orange','light blue','blue','orange')) (100)
     
plot(d,col=cl)     
     
plot(coastlines, add=T, col= "black")    
     
# Exercise 22/04/2020
     
library(spatstat)

library(rgdal) # for the coastlines

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
     
marks(covids) <- covid$cases

s <- Smooth(covids)
     
plot(s)
     
# Exercise: plot(s) with points and coastlines

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
     
plot(s, col=cl5, main="density")

points(covids)
     
coastlines <- readOGR("ne_10m_coastline.shp")

plot(coastlines, add=T)
     
# mappa finale

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
 
dev.off
     
# dati San Marino

# library spatstat
     
setwd("C:/lab")  
     
load("Tesi.RData")
 
ls()
     
head(Tesi)

attach(Tesi)
     
summary(Tesi)
     
Tesippp <- ppp(Longitude, Latitude, c(12.41,12.47), c(43.9,43.95)) 
     
dT <- density(Tesippp)
     
plot(dT)

points(Tesippp, col="black")
     
load("sanmarino.RData")

#ppp point pattern   dt density map   Tesi dataset originale       
     
ls() 
     
library(spatstat)
     
plot(dT)

points(Tesippp, col="green")
     
head(Tesi)
     
marks(Tesippp) <- Tesi$Species_richness
     
interpol <- Smooth(Tesippp)
     
plot(interpol)
     
points(Tesippp, col="green")
     
library(rgdal)
     
sanmarino <- readOGR("San_Marino.shp")
     
plot(sanmarino)
     
plot(interpol, add=T
     
points(Tesippp,col="green")
 
plot(sanmarino,add=T)

#Exercise plot multiframe di densità e interpolazione 
     
par(mfrow=c(2,1))
     
plot(dT, main="Density of points")
     
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
     
points(Tesippp,col="green")

#Exercise
     
par(mfrow=c(1,2))

plot(dT, main="Density of points")
     
points(Tesippp,col="green")

plot(interpol, main="Estimate of species richness")
     
points(Tesippp,col="green")

############################################
############################################
############################################
 
5. R_code_teleril.r 

# Codice r per analisi di immagini satellitari

# package raster

library(raster)

setwd("C:/lab")

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

# B1: blue
# B2: green
# B3: red
# B4: near infrared (nir)
# B5: medium infrared
# B6: thermal infrared
# B7: medium infrared

# cambiamo i colori delle immagini

cl <- colorRampPalette(c('black','grey','light grey'))(100) 

plot(p224r63_2011,col=cl)

cllow <- colorRampPalette(c('black','grey','light grey'))(5)  

plot(p224r63_2011, col=cllow) 

names(p224r63_2011)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

# attach non funziona col pacchetto raster, si usa il $

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

# il pc usa RGB per restituire immagini

plotRGB(p224r63_2011, red=3, green=2, blue=1, stretch="Lin")

# nir, per utilizzarla scaliamo tutti di uno in modo tale da poter utilizzare il nir

# false colours

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# salvataggio immagine in pdf

pdf("primografico.pdf")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off ()

# multiframe

par(mfrow=c(1,2))

plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

dev.off()

# Exercise: nir nella componente  G (green)

plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# Exercise: nir nella componente B (blue)

plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

# day 2

library(raster)

setwd("C:/lab")

load("teleril.RData")

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

# Exercise plot con nir

plo8tRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

# plot due immagini 1988 e 2011 per fare un confronto

par(mfrow=c(2,1))

plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

dev.off()

# spectral indices

# dvi= nir1988-red1988

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre

plot(dvi1988)

# Excercise dvi 2011

dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre

plot(dvi2011)

cldvi <- colorRampPalette(c('light blue','light green','green'))(100) 

plot(dvi2011, col=cldvi)

# multitemporl analysis

difdvi<- dvi2011 - dvi1988

plot(difdvi)

cldifdvi <- colorRampPalette(c('red','white','blue'))(100) 

plot(difdvi, col=cldifdvi)

# visualize the output

# multiframe 1988RGB  2011RGB difdiv 

par(mfrow=c(3,1))

plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plot(difdvi, col=cldifdvi)
 
dev.off()

# changing the grain

p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

# per vedere le caratteristiche dell'immagine

p224r63_2011
 
par(mfrow=c(2,1))

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# lower resolution

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)

par(mfrow=c(3,1))

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")

dev.off()

# dvi2011 low resolution

dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre

plot(dvi2011lr50)

# dvi1988 low resolution 

p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
 
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

difdvilr50 <- dvi2011lr50 - dvi1988lr50

plot(dvi1988lr50)

plot(difdvilr50,col=cldifdvi)

par(mfrow=c(2,1))

plot(difdvi, col=cldifdvi)

plot(difdvilr50, col=cldifdvi)
     
############################################
############################################
############################################    

6. R_code_landcover.r  
    
# R code landcover

libray(raster)

library(RStoolbox)

setwd("c:/lab")

p224r63_2011 <- brick("p224r63_2011_masked.grd

plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

p224r63_2011c

plot(p224r63_2011c$map)

clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 

plot(p224r63_2011c$map, col=clclass)
 
# esperimento per vedere la sensibilità rispetto al numero di classi

p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
  
plot(p224r63_2011c$map)

############################################
############################################
############################################    
   
7. R_code_multitemp.r  

# Analisi multitemporale variazione della land cover

setwd("C:/lab/")
 
library(raster)

library(RStoolbox)

library(ggplot2)

library(gridExtra)
 
defor1 <- brick("defor1_.jpg")
 
defor2 <- brick("defor2_.jpg")
 
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
 
# excercise
 
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")
 
par(mfrow=c(2,1))

plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")

plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# classificazione non supervisionata

d1c <- unsuperClass(defor1, nClasses=2)

plot(d1c$map)

cl <- colorRampPalette(c('black','green'))(100) 

plot(d1c$map, col=cl)

d2c <- unsuperClass(defor2, nClasses=2)

plot(d2c$map, col=cl)

# per annullare la mappa e vederle insieme

dev.off()
 
par(mfrow=c(2,1))

plot(d1c$map, col=cl)

plot(d2c$map, col=cl)

par(mfrow=c(1,2))

plot(d1c$map, col=cl)

plot(d2c$map, col=cl)

freq(d1c$map)

# 1) 308038   2) 33254   # tot = 341284 

totd1<- 33254 + 308038

# percentuale  foresta 90.26     aree aperte 9.8

percent1 <- freq(d1c$map) * 100 / totd1

freq(d2c$map)

# tot = 342726

totd2<- 178381 + 164345

# percentuale  foresta 52.05     aree aperte 47.95

percent2 <- freq(d2c$map) * 100 / totd2

cover <- c("Agriculture","Forest")
 
cover <- c("Agriculture","Forest")

before <- c(9.8,90.26)

after <- c(47.95,52.05)

output <- data.frame(cover,before,after)
 
View(output)

load("defor.RData")
        
par(mfrow=c(1,2))
        
cl <- colorRampPalette(c('black','green'))(100)
        
plot(d1c$map, col=cl)
        
plot(d2c$map, col=cl)

# istogrammi % prima della deforestazione 

ggplot(output, aes(x=cover, y=before, color=cover)) +
        
geom_bar(stat="identity", fill="white")

# excerise   dopo la deforestazione

ggplot(output, aes(x=cover, y=after, color=cover)) +
        
geom_bar(stat="identity", fill="white")
 
install.packages("gridExtra") 
        
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
        
# uso di grid.arrange per il plot dei due grafici

grid.arrange(grafico1, grafico2, nrow = 1)

# spiegare al software il limite di y a 100

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
 
grid.arrange(grafico1, grafico2, nrow = 1)

############################################
############################################
############################################  

8. R_code_multitemp_NO2.r  

# codice per analisi ESA cambiamento NO2 da gennaio a marzo 2020

library(raster)

# excercise caricare tutte le immagini

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

# differenze

difno2 <- EN13 - EN01

cldif <- colorRampPalette(c('blue','black','yellow'))(100) #

plot(difno2, col=cldif)

# plot di tutte le immagini

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
 
setwd("C:/lab/")
     
setwd("C:/lab/esa_no2/")

rlist <- list.files(pattern=".png")
     
# per caricare immagini in una volta
     
listafinale <- lapply(rlist, raster)
     
EN <- stack(listafinale)
     
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
     
plot(EN, col=cl)
     
setwd("C:/lab/esa_no2")

rlist <- list.files(pattern=".png")
      
EN <- stack(listafinale)

difEN <- EN$EN_0013 - EN$EN_0001
 
cld <- colorRampPalette(c('blue','white','red'))(100) # 

plot(difEN, col=cld)
     
cl <- colorRampPalette(c('red','orange','yellow'))(100) #
 
plot(EN, col=cl)
 
boxplot(EN)
 
boxplot(EN, horizontal=T)
 
boxplot(EN, horizontal=T,outline=F)
 
boxplot(EN, horizontal=T,outline=F,axes=T)

############################################
############################################
############################################  
       
9. R_code_snow.r 

setwd("C:/lab/")

install.packages("ncdf4")

library(ncdf4)

library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
 
plot(snowmay,col=cl)

setwd("C:/lab/snow/")

# per importarli tutti insieme

rlist=list.files(pattern=".tif")

list_rast=lapply(rlist, raster)

snow.multitemp <- stack(list_rast)

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl)

plot(snow.multitemp$snow2020r, col=cl)

# per fare in modo che entrambe abbiano gli stessi valori

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))

plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
 
cldiff <- colorRampPalette(c('blue','white','red'))(100)

plot(difsnow, col=cldiff)

plot(snow)

# previsione 2025

source("prediction.r")

# essendo molto lento posso caricare direttamente il file predicted snow

# per interrompere source premere esc

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
 
plot(predicted.snow.2025.norm, col=cl)

############################################
############################################
############################################

10. R_code_patches.r

setwd("C:/lab/")

library(raster)

library(ggplot2)

install.packages("igraph")

library(igraph)

d1c <- raster("d1c.tif")

d2c <- raster("d2c.tif

par(mfrow=c(1,2))

cl <- colorRampPalette(c('green','black'))(100) #

# mappa sbagliata, la foresta è nera

plot(d1c,col=cl)

plot(d2c,col=cl)

# per correggere invertire

# foresta: classe 1 ; agricoltura: classe 2

cl <- colorRampPalette(c('black','green'))(100) #

plot(d1c,col=cl)

plot(d2c,col=cl)

d1c.for <- reclassify(d1c, cbind(1,NA))

par(mfrow=c(1,2))

cl <- colorRampPalette(c('black','green'))(100) #

plot(d1c,col=cl)

d2c.for <- reclassify(d2c, cbind(1,NA))

par(mfrow=c(1,2))

plot(d1c)

plot(d2c)

d1c.for.pacthes <- clump(d1c.for)

d2c.for.pacthes <- clump(d2c.for)

writeRaster(d1c.for.pacthes, "d1c.for.patches.tif")

writeRaster(d2c.for.pacthes, "d2c.for.patches.tif")

# Exercise: plottare entrambe le mappe una accanto all'altra

par(mfrow=c(1,2))

plot(d1c.for.pacthes)

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) #

par(mfrow=c(1,2))

plot(d1c.for.pacthes, col=clp)

plot(d2c.for.pacthes, col=clp)

time <- c("Before deforestation","After deforestation")

npatches <- c(301,1212)
 
output <- data.frame(time,npatches

attach(output)

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

############################################
############################################
############################################

# LINK PER IL SITO COPERNICUS

https://land.copernicus.vgt.vito.be/PDF/portal/Application.html



       
 
        
        


        
 









 



 
 





 
 
 
    
      
     



