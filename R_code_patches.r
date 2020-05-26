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

# Exercise: plottare entrambe le mappe una accanto all'

par(mfrow=c(1,2))

plot(d1c.for.pacthes)

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) #

par(mfrow=c(1,2))

plot(d1c.for.pacthes, col=clp)

plot(d2c.for.pacthes, col=clp)

time <- c("Before deforestation","After deforestation")

npatches <- c(301,1212)
 
output <- data.frame(time,npatches

attach(output

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
