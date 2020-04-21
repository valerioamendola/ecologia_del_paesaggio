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
