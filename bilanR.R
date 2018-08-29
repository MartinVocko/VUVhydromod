install.packages("/home/martin/Plocha/VUV/bilan/bilan", 
                 repos = NULL, 
                 type = "source")
library(bilan)


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
dtavg=dt[,(T= mean(T), by = Date]
dpsum=dt[,sum (P),by=Date]

setnames(dtavg,"V1","T")
MET=dtavg
MET$P<-dpsum$V1
MET$PET<-0
MET$R<-0



b <- bil.new("d") # BILAN in daily step
bil.set.values(b, MET)
bil.set.values(b, MET[, P = Precip, T = Tavg, PET = 0, R = 0]) # musi byt data.frame, nebo data.table
bil.pet(b)
bil.optimize(b)
bilRES <- bil.run(b)
bilRES$PET  