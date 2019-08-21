install.packages("devtools")
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
install.packages('gstat')
install.packages("RColorBrewer")
install.packages('plotly')
install.packages("data.table")

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
library(bilan)
library(gstat)
library(TUWmodel)
library(RColorBrewer)
library(plotly)
library(reshape2)




vp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Zeleny-podrobne.shp")
hp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Hutsky-podrobne.shp")
stanice=readOGR("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/VUV-profily.shp")
tokyh=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Toky-hlavni.shp")
tokym=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Toky-male.shp")
sjezd=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/sjezdovky.shp")
vlek=readShapeLines("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/vlekyalanovky.shp")
metstanice=readOGR("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/metstanice.shp")

#vp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Zeleny-potok.shp")
#hp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Hutsky-potok.shp")
#sp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Svatopetrskypotok.shp")
#cp=readShapePoly("~/Plocha/DATA/GITHUB/VUVhydromod/GIS/Rozvodnice-Cernohorsky-potok.shp")

#oseknuti rastru na povodi k prvnimu mernemu profilu VUV
vp=vp[c(2,6),]
dtm_vlci_p1=mask(raster('dtm_zeleny'),vp)
writeRaster(dtm_vlci_p1,"dtm_vlci_p1")



setwd("~/Plocha/DATA/GITHUB/VUVhydromod/GIS")
povodi=raster('dtm_vlci_p1')
m=c(0, 600, 1, 600, 799, 2, 799, 1000, 3, 1000, 1200, 4, 1200, 1600, 5) #upravit zpet na 8000
rclmat = matrix(m, ncol=3, byrow=TRUE)
recdem = reclassify(povodi, rclmat)
#recdempol= rasterToPolygons(povodi, fun=function(x){400<x;x<600}, dissolve=FALSE)


povodi=raster('dtm_vlci_p1')
#povodi=raster('dtm_hutsky')
#povodi=raster('dtm_zeleny')
#povodi=raster('dtm_svatopetr')

breaks=c(0, 600)
cp1=cut(povodi, breaks = breaks)
z1=mask(povodi, cp1)

breaks=c(600, 799)
cp2=cut(povodi, breaks = breaks)
z2=mask(povodi,cp2)

breaks=c(799, 1000)
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




#---------------------------------------------------------------------------------------------------------------------------------------
##### vypocitat ochlazeni na jednom vyskovem metru, vzit nejblizsi stanici 
#### a prepocitat teploty pro kazdou prumernou vysku zony

#### Sestaveni tabulek teplot #######################################################


###rozdeleni hromadne tabulky na jednotlive stanice

setwd("~/Plocha/DATA/GITHUB/VUVhydromod/Data/Data/Teploty")
tdat=data.frame(read.table("teploty.dsv",header=TRUE, col.names=c("Station","T", "Time"), sep=";"))
nam=levels(tdat$Station)

#st1=tdat[tdat$Station==(nam[1]),]
td=data.table(dcast(tdat, DateTime ~ Station, value.var = "T" ))
Hours <- format(as.POSIXct(td$DateTime, "%d.%m.%Y %H:%M:%S", tz = ""), format = "%H:%M")
Dates <- format(as.Date(td$DateTime,"%d.%m.%Y %H:%M:%S"), format = "%d.%m.%Y")
td$Time=Hours
td$Date=Dates

stanice=levels(tdat$Station)
for (i in seq_along(stanice)){
  st=tdat[tdat$Station == stanice[i],]
  Dates <- format(as.Date(st$Time,"%d.%m.%Y %H:%M:%S"), format = "%d.%m.%Y")
  st$Date=Dates

  ########  ROzFACHAT prumerovani v cyklu ################
  st=st[,mean(T), by = Date]
  st[, mean(T), by = .(Station, day("Date"))]
}



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
stanice_dat=st1[,1]

setwd("~/Plocha/DATA/GITHUB/VUVhydromod/GIS")
povodi=raster('dtm_vlci_p1')
h=metstanice$Z
fun<- function(x) {a + (x * b)}
zone=c(z1,z2,z3,z4,z5)
#tab=data.frame()
zonetab=data.frame()



for (i in 1:length(stanice_t$V1)) {         
  
t = as.numeric(as.vector(stanice_t[i,]))
  
fit=(lm(t ~ h))
a=fit$coefficients[1]
b=fit$coefficients[2]

povodi_t=calc(povodi, fun = fun)

tab=data.frame(Date=stanice_dat[i,])

    for ( j in zone){
  
       zone_t = mask(povodi_t,j)
       m = mean(zone_t@data@values, na.rm=TRUE)
       tab=cbind(tab,m)
  
}

zonetab=rbind (zonetab, tab)
 
}

#uprava pro TUWmodel
zoneT=zonetab
rownames(zoneT)=(zonetab[[1]])
zoneT$Date=NULL
colnames(zoneT)=c("del","del", "T", "T", "T")
zoneT$del=NULL
zoneT$del=NULL



###### Vypocet PET ##########################################################################################################

zonePET=data.frame(Date=zonetab[1])

for (i in 1:3 ) { 

#a= 50   # rozloha km2
b <- bil.new("d")
bil.set.values(b, input_vars = zoneT[i], init_date = "2017-12-01") #pokud nejde, pouzit zoneT jako DF, ne jako matrix, radek 228
bil.pet(b)
bil.get.values(b)
#bil.set.area(b, a) # a=plocha povodi Dolni Sytova = 321.64 km2
#bil.set.optim(b, method = "DE", crit = "MSE", DE_type = "best_one_bin", n_comp = 4,
#              comp_size = 10, cross = 0.95, mutat_f = 0.95, mutat_k = 0.85, maxn_shuffles = 5,
#              n_gen_comp = 10, ens_count = 5, seed = 446, weight_BF = 0, init_GS = 50)
#bil.optimize(b)
#bil.run(b)  
bil.get.params(b)
res=bil.get.values(b)
res=data.table(res$vars)
PET=res$PET

zonePET =cbind(zonePET,PET)


}


#uprava pro TUWmodel
zonePET$Date=NULL
colnames(zonePET)=c("", "", "")
rownames(zonePET)=(zonetab[[1]])
zonePET=data.matrix(zonePET)

zoneT=data.matrix(zoneT)

####### Srazky ####

#idw <- idw(formula =  your_rainfall ~ 1, locations = met_stations, newdata = grd)


setwd("~/Plocha/DATA/GITHUB/VUVhydromod/Data/Data/Srazky")
st8=data.frame(read.table("SSCHSR01.txt",header=TRUE, col.names=c("Date", "Time", "T")))
st8=data.table(st8)
st8=na.omit(st8)
st8=st8[,sum(T), by = Date]
st8$Date <- as.Date(st8$Date)
st8=with(st8, st8[(Date >= "2017-12-01")])

st8$V2=st8$V1
st8$V3=st8$V1
#st8$V4=st8$V1
#st8$V5=st8$V1
zoneP=st8
#colnames(zoneP)=c("Date", "P", "P", "P", "P")
zoneP$Date=NULL
colnames(zoneP)=c( "", "", "")

zoneP=as.data.frame(zoneP)
rownames(zoneP)=(zonetab[[1]])
zoneP=data.matrix(zoneP)


#### Prutoky  #######

setwd("~/Plocha/DATA/GITHUB/VUVhydromod/Data/Data/Prutoky")
Q_obs=data.frame(read.table("Prutok-Vlci2.txt",header=TRUE, col.names=c("Date", "Time", "Q")))
Q_obs=data.table(Q_obs)
Q_obs=na.omit(Q_obs)
Q_obs=Q_obs[,mean(Q), by = Date]
Q_obs$Date <- format(as.Date(Q_obs$Date, format = "%d.%m.%Y"), "%Y-%m-%d")
Q_obs=with(Q_obs, Q_obs[(Date < "2018-08-02")])

a=4.187

Q_obs$R=(((Q_obs$V1*60*60*24)/(a*1000000))*1000)
Q_obs$V1=NULL  #odstrani prutoky v m3/s



########################################################################################################

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

plot(metstanice)
pointLabel(coordinates(metstanice),labels=metstanice$Jméno_sig) #pridani popisku

povodi=raster('dtm_povodi')
plot(povodi,main ="Pozorovaná povodí a meteostanice" ,col=rev(brewer.pal(11,'RdYlGn')),axes=FALSE)
plot(vp, add=TRUE)
pointLabel(coordinates(vp),labels=vp$NAZEV_TOK, cex = 1) 
plot(hp, add=TRUE)
pointLabel(coordinates(hp),labels=hp$NAZEV_TOK, cex=1) 
plot(sp, add=TRUE)
pointLabel(coordinates(sp),labels=sp$NAZEV_TOK, cex=1) 
plot(cp, add=TRUE)
pointLabel(coordinates(cp),labels=cp$NAZEV_TOK, cex=1) 
plot(metstanice,add=TRUE, pch=20)
pointLabel(coordinates(metstanice),labels=metstanice$Stanice, cex = 0.7) 



povodi=SpatialPolygons(list(cp,hp))

map1 <- get_map(location = "Praha",
                maptype = "terrain",
                source = "google",
                crop = FALSE,
                zoom = 10)
ggmap(map1)

###### Model #################################################################################################3333333

f=freq(recdem, useNA="no")
s=sum(f[,2])
zoneAreas=c(f[1,2]/s, f[2,2]/s, f[3,2]/s )   #f[4,2]/s, f[5,2]/s
#zoneP[2]=NULL

parametri <- matrix(rep(c(1.1,2.70,2.5,0,-0.336,
                          0.934,70,15.52,
                          0.2,10.06,142,
                          2.1,2.38,10,25), 3), ncol=3)  #nastavit pocet zon !
parametri[2,] <- c( 1.9, 2.1, 2.3)                       # DDF pro jednotlive zony, treba upravit dle poctu zon
simDist2 <- TUWmodel(prec = zoneP,
                     airt = zoneT, 
                     ep = zonePET, 
                     area = zoneAreas,
                     param = parametri)
plot(as.Date(Q_obs$Date), Q_obs$R, type="l", xlab="Měsíce", ylab = "Odtok [mm/den]", main="TUWmodel", xlim=as.Date(c('2018-02-01','2018-08-01')))
lines(as.Date(rownames(zoneT)), simDist2$q, col=2)
legend("topleft", legend = c("Pozorovane","Simulovane"), col = c(1, 2), lty = 1, bty = "n")

plot(as.Date(rownames(zoneT)), simDist2$q, col=2, type="l")


#tvorba tabulky pro AirGR a Bilan

BasinObs=zonetab[1]
BasinObs$Date=as.POSIXlt(zonetab$Date)

zoneT$avg = apply(zoneT,1,mean,na.rm=TRUE)

BasinObs$T=zoneT$avg
BasinObs$P=zoneP[,1]

zonePET$avg=apply(zonePET,1,mean,na.rm=TRUE)
BasinObs$E=zonePET$avg

BasinObs$R=Q_obs$R
saveRDS(BasinObs,"BasinObs_Vlci")
