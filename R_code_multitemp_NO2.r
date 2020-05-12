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
       
