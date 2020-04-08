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

cllow <- colorRampPalette(c('black','grey','light grey'))(5) # 
plot(p224r63_2011, col=cllow) 

names(p224r63_2011)

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)

# attach non funziona col pacchetto raster, si usa il $

plot(p224r63_2011$B1_sre,col=clb)

# esrcizio: plottare con cambio colore rosso,arancione e giallo

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


 










