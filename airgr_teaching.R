install.packages("airGRteaching")
library(airGRteaching)

BasinObs=readRDS("MET")

BasinObs$DatesR<-as.POSIXct(BasinObs$Date)

BasinObs$Date=NULL
BasinObs$Q=NULL
names(BasinObs)[4]<-"E"
names(BasinObs)[3]<-"Qmm"
setcolorder(BasinObs,c(5,2,4,3,1))

#PREP=PrepGR(ObsDF=BasinObs, HydroModel = "GR4J", CemaNeige=TRUE)
DatesR= as.POSIXct(as.Date(BasinObs$Date),tz="UTC")
attributes(DatesR)$tzone <- "UTC"
PREP=PrepGR(DatesR=DatesR,Precip=BasinObs$P, PotEvap = BasinObs$PET, Qobs = BasinObs$R, TempMean = BasinObs$T, HydroModel = "GR4J", CemaNeige = TRUE)

#CALIBRATION

CAL <- CalGR(PrepGR = PREP, CalCrit="NSE", WupPer = NULL, CalPer = c("2012-01-01", "2013-12-31"))

#SIMULATION

SIM <- SimGR(PrepGR = PREP, CalGR = CAL, EffCrit = "NSE", WupPer= NULL, SimPer = c("2014-01-01", "2018-08-01"))

plot(PREP, main = "Observation")

plot(CAL, which="perf")

plot(CAL, which="iter")


dyplot(SIM, main="Simulation")
