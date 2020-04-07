# Codice r per analisi di immagini satellitari

# package raster

library(raster)

setwd("C:/lab")

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)
