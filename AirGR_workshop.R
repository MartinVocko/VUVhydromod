###### AirGR ########


install.packages(c('airGRteaching','shiny', 'data.table'))

library(airGRteaching)
library(shiny)
library(data.table)

#DATA UPLOAD ####

data(L0123002)

BasinObs
BasinInfo

#DEFINITION OF INPUT DATA AND MODEL TYPE ####


PREP=PrepGR(DatesR = BasinObs$DatesR, Precip = BasinObs$P,
            PotEvap = BasinObs$E, Qobs = BasinObs$Qmm, TempMean = BasinObs$T, HydroModel = "GR4J", 
            CemaNeige = TRUE, HypsoData = BasinInfo$HypsoData, ZInputs = median(BasinInfo$HypsoData),  NLayers = 5)

#CALIBRATION ####

CAL <- CalGR(PrepGR = PREP, CalCrit="NSE", WupPer = NULL, CalPer = c("1984-01-01", "1993-12-31"))

#SIMULATION ####

SIM <- SimGR(PrepGR = PREP, CalGR = CAL, EffCrit = "NSE", WupPer= NULL, SimPer = c("1994-01-01", "2012-12-31"))

plot(PREP, main = "Observation")

plot(CAL, which="perf")

plot(CAL, which="iter")


###### SHINY ####

BasinObs = readRDS("airGRdata")

BasinObs 

ShinyGR(ObsDF = BasinObs, SimPer= c("2012-01-01", "2018-08-01")) 





