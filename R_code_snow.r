setwd("C:/lab/")

install.packages("ncdf4")

library(ncdf4)

library(raster)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
 
plot(snowmay,col=cl)

setwd("C:/lab/snow/")

# per importarli tutti insieme

rlist=list.files(pattern=".tif")

list_rast=lapply(rlist, raster)

snow.multitemp <- stack(list_rast)

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl)

plot(snow.multitemp$snow2020r, col=cl)

# per fare in modo che entrambe abbiano gli stessi valori

par(mfrow=c(1,2))

plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250))

plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))

difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
 
cldiff <- colorRampPalette(c('blue','white','red'))(100)

plot(difsnow, col=cldiff)

plot(snow)

# previsione 2025

source("prediction.r")

# essendo molto lento posso caricare direttamente il file predicted snow

# per interrompere source premere esc

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
 
plot(predicted.snow.2025.norm, col=cl)
 


