install.packages("ggplot2")
install.packages("spatstat")
library(spatstat)
library(ggplot2)

# Codice per analisi dei point pattern

setwd("C:/lab")

covid <- read.table("covid_agg.csv", head=T)

# per visualizzare le prime righe

head(covid)

# si può fare attach(covid) oppure si usa il simbolo $

plot(covid$country,covid$cases)

# etichette parallele 0

plot(covid$country,covid$cases,las=0)

# etichette orizzontali 1

plot(covid$country,covid$cases,las=1)

#etichette perpendicolari 2

plot(covid$country,covid$cases,las=2)

# etichette verticali 3

plot(covid$country,covid$cases,las=3)

# per visualizzare più paesi

plot(covid$country,covid$cases,las=3,cex.lab=0.5, cex.axis=0.5

#ggplot2

data(mpg)

head(mpg)

# data
# aes
# tipo di geometria

ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()

# cambiamo gemoetria

ggplot(mpg,aes(x=displ,y=hwy)) + geom_line()

ggplot(mpg,aes(x=displ,y=hwy)) + geom_polygon()

# ggplot per covid, richiamiamo prima i nomi con names(covid)

ggplot(covid,aes(x=lon,y=lat,size=cases)) + geom_point()

# density
# create dataset for spatstat
#  prima attach covid

attach(covid)

covids <- ppp(lon, lat, c(-180,180), c(-90,90))

d <- density(covids)

plot(d)

points(covids)

# per salvare 
     
q()
     
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
     
