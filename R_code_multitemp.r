# analisi multitemporale variazione della land cover

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

# 1 308038   2   33254   # tot = 341284 

totd1<- 33254 + 308038

# percentuale  foresta 90.26     aree aperte 9.8

percent1 <- freq(d1c$map) * 100 / totd1

freq(d2c$map)

# tot = 342726

totd2<- 178381 + 164345

# percentuale  foresta 52.05      aree aperte 47.95

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
        
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +         
geom_bar(stat="identity", fill="white")
        
# uso di grid.arrange per il plot dei due grafici

grid.arrange(grafico1, grafico2, nrow = 1)

# spiegare al software il limite di y a 100

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

 grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)
 
grid.arrange(grafico1, grafico2, nrow = 1)
 
       
 
        
        


        
 









 
