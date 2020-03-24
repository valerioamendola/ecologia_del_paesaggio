# R spaziale: funzioni spaziali in Ecologia del paesaggio

library(sp)

data(meuse)

head(meuse)

attach(meuse)

plot(cadmium,lead,col="red",pch=19,cex=2)

plot(copper,zinc,col="green",pch=17,cex=2)

# per cambiare etichetta
plot(copper,zinc,col="green",pch=17,cex=2,xlab="rame",ylab="zinc")

#multiframe o multipanel

par(mfrow=c(1,2))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

#invertiamo riga colonna in colonna riga
par(mfrow=c(2,1))
plot(cadmium,lead,col="red",pch=19,cex=2)
plot(copper,zinc,col="green",pch=17,cex=2)

#multiframe automatico

install.packages("GGally")

library(GGally)

ggpairs(meuse[,3:6])

#spatial

head(mesue)

coordinates(meuse)=~x+y

spplot(mesue,"zinc")


