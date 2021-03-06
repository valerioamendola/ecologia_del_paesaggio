# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO

install.packages("sp")

library(sp)

# require(sp) è un altro comando per far partire le librerie

data(meuse)

meuse

head(meuse)

names(meuse)

summary(meuse)

pairs(meuse)

pairs(~ cadmium + copper + lead , data = meuse)

# Exercie: cadmium copper lead zinc

pairs(meuse[,3:6])

pairs(meuse[,3:6], col="red")

pairs(meuse[,3:6], col="red", pch=19)

pairs(meuse[,3:6], col="red", pch=19, cex=3)

pairs(meuse[,3:6], col="red", pch=19, cex=3, main="Primo pairs")

# Exercise: do the same for the relationship between elevation and the elements

pairs(meuse[,3:7], col="red", pch=19, cex=3, main="Primo pairs")

# funzione plot

plot(meuse$cadmium,meuse$copper)

attach(meuse)

plot(cadmium,copper) 

plot(cadmium, copper, pch=17, col="green", main="primo plot")

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame") 

plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame", cex.lab=2, cex=2) 
