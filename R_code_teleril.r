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

# nir, per utilizzarma scaliamo tutti di uno in modo tale da poter utilizzare il nir

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
 
 


 
 




