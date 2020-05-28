# R spatial

# libreria sp

library(sp)

# dati da utilizzare

data(meuse)

head(meuse)

#coordinate del dataset

coordinates(meuse)=~x+y

# spplot dei dati zinco

spplot(meuse,"zinc")

#spplot del rame

# per vedere il nome si può usare names o head

spplot(meuse,"copper")

# bubble

bubble(meuse,"zinc")

# bubble del rame colorato di rosso

bubble(meuse,"copper",col="red")

# foraminiferi (sofia), carbon capture)

#array

foram<-c(10, 20, 35, 55, 67, 80)

carbon <- c(5, 15, 30, 70, 85, 99)

plot(foram,carbon,col="green",cex=2,pch=19)

#dati dall'esterno covid-19

# cartella da creare su Windows c:/lab

setwd("c:/lab")

# funzione per leggere la tabella

read.table("covid_agg.csv",head=TRUE)

# Per leggere la tabella

covid <- read.table("covid_agg.csv",head=TRUE)

# L'errore era dovuto alla presenza di virgolette nel fil excel, fill=TRUE forzava ma non risolveva

