install.packages("~/Plocha/VUV/bilan/bilan", 
                 repos = NULL, 
                 type = "source")
install.packages("Rcpp")


library(Rcpp)
library(bilan)
library(data.table)


# plocha povodi

a= 4.187


# Tavg = ... define time series of temperature
setwd("~/Plocha/VUV/Data/Data/Teploty")
dt=data.frame(read.table("JIDSTO01.txt",header=TRUE, col.names=c("Date", "Time", "T")))


# Precip = ... define time series of precipitation
setwd("~/Plocha/VUV/Data/Data/Srazky")
dp=data.frame(read.table("JIDSSR01.txt",header=TRUE, col.names=c("Date", "Time", "P")))


# Qdischarge 
setwd("~/Plocha/VUV/Data/Data/Prutoky")
dq=data.frame(read.table("JIDS_Q01.txt",header=TRUE, col.names=c("Date", "Time", "Q")))

# Merge table

dt$P<-dp$P
#dt$Q<-dq$Q


dt=data.table(dt)
dt=na.omit(dt)
dtavg=dt[,mean(T), by = Date]
dpsum=dt[,sum (P),by=Date]

setnames(dtavg,"V1","T")
MET=dtavg
MET$P<-dpsum$V1
#MET$PET<-0
MET$R<-0


setwd("~/Plocha/DATA/GITHUB/VUVhydromod")
MET=readRDS("BasinObs_Vlci")

### bilan

b <- bil.new("d")
#bil.set.values(b, input_vars = MET[, 2:ncol(MET)], init_date = MET$Date)
bil.set.values(b, data.frame(T = MET$T, R = MET$R, P = MET$P), MET$Date[1])
bil.pet(b)
bil.get.values(b)
bil.set.area(b, a) # a=plocha povodi Dolni Sytova = 321.64 km2
bil.set.optim(b, method = "DE", crit = "MSE", DE_type = "best_one_bin", n_comp = 4,
              comp_size = 10, cross = 0.95, mutat_f = 0.95, mutat_k = 0.85, maxn_shuffles = 5,
              n_gen_comp = 10, ens_count = 5, seed = 446, weight_BF = 0, init_GS = 50)
#bil.set.params(b,list(Mec=0.2),"lower")
bil.optimize(b)
bil.run(b)  
bil.get.params(b)
res=bil.get.values(b)
res=data.table(res$vars)
#PET=res$PET

plot(as.Date(res$DTM), res$R, type="l", xlab="Měsíce", ylab = "Odtok [mm/den]",main="Bilan",xlim=as.Date(c('2018-02-01','2018-08-01')))
lines(as.Date(res$DTM), res$RM, col=2)
legend("topleft", legend = c("Pozorovane","Simulovane"), col = c(1, 2), lty = 1, bty = "n")
 
plot(as.Date(res$DTM), simDist2$swe[,2], type="l", xlab="Měsíce", ylab = "Vodní hodnota sněhu [mm]",main="Porovnání zásob sněhu v povodí Vlčího potoka",xlim=as.Date(c('2017-12-02','2018-08-01')))
lines(as.Date(res$DTM), res$SS, col=2)
lines(as.Date(OutputsModel$DatesR), OutputsModel$CemaNeigeLayers$Layer03$SnowPack, col=3)
legend("topleft", legend = c("TUWmodel","Bilan","AirGR"), col = c(1, 2,3), lty = 1, bty = "n")

# Doplneni vypoctene PET
MET$PET<-PET

# Doplneni o prumerny prutok

dt$Q<-dq$Q
dt=na.omit(dt)
dqagr=dt[,mean(Q), by = Date]
MET$Q<-dqagr$V1

#prepocet prutoku na odtok [mm]

MET$R<-(((MET$Q*60*60*24)/(a*1000000))*1000)

saveRDS(MET,"MET")
