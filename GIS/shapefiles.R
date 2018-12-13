install.packages("ggmap")
install.packages('rgl')
install.packages('rasterVis')
install.packages("sp")
install.packages("raster")
install.packages('rgdal')
install.packages('rgeos')
install.packages('maptools')
install.packages('lubridate')
install.packages('dplyr')

library (ggmap)
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(rgeos)  # geometry ops
library(maptools) 
library(rgl)
library(rasterVis)
library(data.table)
library(lubridate)
library(dplyr)


cp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Cernohorsky-podrobne.shp")
hp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Hutsky-podrobne.shp")
stanice=readOGR("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/VUV-profily.shp")
tokyh=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Toky-hlavni.shp")
tokym=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Toky-male.shp")
sjezd=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/sjezdovky.shp")
vlek=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/vlekyalanovky.shp")
metstanice=readOGR("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/metstanice.shp")


setwd("~/Plocha/DATA/GITHUB/VUVhydromod/GIS")
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

#sestaveni tabulek teplot
setwd("~/Plocha/DATA/GITHUB/VUVhydromod/Data/Data/Teploty")

st1=data.frame(read.table("SSLATO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st1=data.table(st1)
st1=na.omit(st1)
st1=st1[,mean(T), by = Date]
st1$Date <- as.Date(st1$Date)
st1=with(st1, st1[(Date >= "2017-12-01")])

st2=data.frame(read.table("SSLUTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st2=data.table(st2)
st2=na.omit(st2)
st2=st2[,mean(T), by = Date]
st2$Date <- as.Date(st2$Date)
st2=with(st2, st2[(Date >= "2017-12-01")])

st3=data.frame(read.table("SSMETO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st3=data.table(st3)
st3=na.omit(st3)
st3=st3[,mean(T), by = Date]
st3$Date <- as.Date(st3$Date)
st3=with(st3, st3[(Date >= "2017-12-01")])

st4=data.frame(read.table("SSPLTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st4=data.table(st4)
st4=na.omit(st4)
st4=st4[,mean(T), by = Date]
st4$Date <- as.Date(st4$Date)
st4=with(st4, st4[(Date >= "2017-12-01")])

st5=data.frame(read.table("SSSPTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st5=data.table(st5)
st5=na.omit(st5)
st5=st5[,mean(T), by = Date]
st5$Date <- as.Date(st5$Date)
st5=with(st5, st5[(Date >= "2017-12-01")])

st6=data.frame(read.table("SSVLTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st6=data.table(st6)
st6=na.omit(st6)
st6=st6[,mean(T), by = Date]
st6$Date <- as.Date(st6$Date)
st6=with(st6, st6[(Date >= "2017-12-01")])

st7=data.frame(read.table("MLPRTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st7=data.table(st7)
st7=na.omit(st7)
st7=st7[,mean(T), by = Date]
st7$Date <- as.Date(st7$Date)
st7=with(st7, st7[(Date >= "2017-12-01")])

st8=data.frame(read.table("SSCHTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st8=data.table(st8)
st8=na.omit(st8)
st8=st8[,mean(T), by = Date]
st8$Date <- as.Date(st8$Date)
st8=with(st8, st8[(Date >= "2017-12-01")])

st9=data.frame(read.table("JKDSTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st9=data.table(st9)
st9=na.omit(st9)
st9=st9[,mean(T), by = Date]
st9$Date <- as.Date(st9$Date)
st9=with(st9, st9[(Date >= "2017-12-01")])


st10=data.frame(read.table("JIDSTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st10=data.table(st10)
st10=na.omit(st10)
st10=st10[,mean(T), by = Date]
st10$Date <- as.Date(st10$Date)
st10=with(st10, st10[(Date >= "2017-12-01")])

### Doplneni chybejicich terminu s NA !!!!!!
full_dat=data.frame(Date=seq(as.Date("2017-12-01"), as.Date("2018-08-01"), by="days"))
st5<- merge(full_dat,st5, by = "Date", all.x = TRUE)


stanice_t=cbind(st1[,2],st2[,2],st3[,2],st4[,2],st5[,2],st6[,2],st7[,2],st8[,2],st9[,2],st10[,2])

povodi=raster('dtm_cernohor')
h=metstanice$Z
fun<- function(x) {a + (x * b)}
zone=c(z1,z2,z3,z4,z5)
#tab=data.frame()
zonetab=data.frame()

for (i in length(stanice_t$V1)) {         #n2ejak to neseje...a neuklada do zonetab
  
t = as.numeric(as.vector(stanice_t[i,]))
  
fit=(lm(t ~ h))
a=fit$coefficients[1]
b=fit$coefficients[2]

povodi_t=calc(povodi, fun = fun)

tab=data.frame()

for ( j in zone){
  
  zone_t = mask(povodi_t,j)
  m = mean(zone_t@data@values, na.rm=TRUE)
  tab=cbind(tab,m)
}

zonetab=rbind (zonetab, tab)
 
}





t= as.numeric(as.vector(stanice_t[1,]))

fit=(lm(t ~ h))
a=fit$coefficients[1]
b=fit$coefficients[2]









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
