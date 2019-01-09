install.packages("~/Plocha/VUV/bilan/bilan", 
                 repos = NULL, 
                 type = "source")
library(bilan)

# plocha povodi

a= 321.64


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

### bilan

b <- bil.new("d")
bil.set.values(b, input_vars = MET[, 2:ncol(MET)], init_date = MET[1, Date])
bil.pet(b)
bil.get.values(b)
bil.set.area(b, a) # a=plocha povodi Dolni Sytova = 321.64 km2
bil.set.optim(b, method = "DE", crit = "MSE", DE_type = "best_one_bin", n_comp = 4,
              comp_size = 10, cross = 0.95, mutat_f = 0.95, mutat_k = 0.85, maxn_shuffles = 5,
              n_gen_comp = 10, ens_count = 5, seed = 446, weight_BF = 0, init_GS = 50)
bil.optimize(b)
bil.run(b)  
bil.get.params(b)
res=bil.get.values(b)
res=data.table(res$vars)
PET=res$PET


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
