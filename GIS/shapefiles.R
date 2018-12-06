install.packages("ggmap")
install.packages('rgl')
install.packages('rasterVis')

library (ggmap)
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(maptools) 
library(rgl)
library(rasterVis)


cp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Cernohorsky-podrobne.shp")
hp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Hutsky-podrobne.shp")
stanice=readOGR("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/VUV-profily.shp")
tokyh=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Toky-hlavni.shp")
tokym=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Toky-male.shp")
sjezd=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/sjezdovky.shp")
vlek=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/vlekyalanovky.shp")
metstanice=readOGR("/media/martin/COPERNICUS/VUVsnih/GIS/metstanice.shp")


setwd("/media/martin/COPERNICUS/VUVsnih/GIS")
povodi=raster('dtm_povodi')
m=c(0, 600, 1, 600, 800, 2, 800, 1000, 3, 1000, 1200, 4, 1200, 1600, 5)
rclmat = matrix(m, ncol=3, byrow=TRUE)
recdem = reclassify(povodi, rclmat)
#recdempol= rasterToPolygons(povodi, fun=function(x){400<x;x<600}, dissolve=FALSE)


povodi=raster('dtm_cernohor')
#povodi=raster('dtm_hutsky')
#povodi=raster('dtm_zeleny')
#povodi=raster('dtm_svatopetr')

breaks=c(0, 600)
cp1=cut(povodi, breaks = breaks)
z1=mask(povodi, cp1)

breaks=c(600, 800)
cp2=cut(povodi, breaks = breaks)
z2=mask(povodi,cp2)

breaks=c(800, 1000)
cp3=cut(povodi, breaks = breaks)
z3=mask(povodi,cp3)

breaks=c(1000, 1200)
cp4=cut(povodi, breaks = breaks)
z4=mask(povodi,cp4)

breaks=c(1200, 1600)
cp5=cut(povodi, breaks = breaks)
z5=mask(povodi,cp5)

zone=c(z1,z2,z3,z4,z5)
tab=list()

for ( i in zone){
  
m = mean(i@data@values, na.rm=TRUE)
tab=rbind(tab,m)
}

tab


a=10.364339
b=-0.004103
TAB=list()
for (x in 1:length(tab)){ 
temp= a + (tab[[x]] * b)

TAB=rbind(TAB,temp)
}

temptab=cbind(tab,TAB)
temptab
#-----------------------------------------------------------------------------
##### vypocitat ochlazeni na jednom vyskovem metru, vzit nejblizsi stanici 
#### a prepocitat teploty pro kazdou prumernou vysku zony
#----------------------------------------------------------------------------

#linearni model vypocet teploty v zavislosti na vysce
h=metstanice$Z
t=metstanice$avrgtrmp

plot(h,t)
fit=(lm(t ~ h))
fit

a=10.364339
b=-0.004103
x= 600   #vyska pro hledanou teplotu
temp= a + (x * b)
temp


plot(cp,lty=1,lwd=2)
plot(tokym,col='blue', add=TRUE)
plot(stanice, col= 'red', pch=16, add=TRUE)
plot(sjezd, col='darkgreen',lty=1, add=TRUE)
plot(vlek, col='red',lty=2, add=TRUE)

povodi=SpatialPolygons(list(cp,hp))

map1 <- get_map(location = "Praha",
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 10)
ggmap(map1)
