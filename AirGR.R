install.packages("airGR")

library(airGR)

# v kombinaci se snehovym modelem pouze v dennim kroku
RunModel_CemaNeigeGR4J() #: combined use of GR4J and CemaNeige
RunModel_CemaNeigeGR5J() #: combined use of GR5J and CemaNeige
RunModel_CemaNeigeGR6J() #: combined use of GR6J and CemaNeige


#BasinObs = readRDS("airGRdata")  #data naformatovana ve skriptu airgr_teaching
setwd("~/Plocha/DATA/GITHUB/VUVhydromod")
BasinObs = readRDS("BasinObs_Vlci")
summary(BasinObs)


#INPUTS MODEL OBJECT
h=seq(from = 750, to = 1350, by = (600/100) )  #set of altitudes
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J, DatesR = BasinObs$Date,
                                 Precip = BasinObs$P, PotEvap = BasinObs$E, TempMean = BasinObs$T,HypsoData = h, ZInputs = median(h))
str(InputsModel)

#RUN OPTIONS OBJECT
Ind_Run <- seq(which(format(BasinObs$Date, format = "%d/%m/%Y")=="01/02/2018"), 
               which(format(BasinObs$Date, format = "%d/%m/%Y")=="01/08/2018"))
str(Ind_Run)

Ind_warm <- seq(which(format(BasinObs$Date, format = "%d/%m/%Y")=="01/12/2017"), 
                which(format(BasinObs$Date, format = "%d/%m/%Y")=="31/01/2018"))

### !!!!!! kouknonout na snehova kriteria !!!!!  #####
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
                               IniStates = NULL, IniResLevels = NULL, IndPeriod_WarmUp = Ind_warm ) #MeanAnSolidPrecip = c(70, 90, 110, 130, 150)

str(RunOptions)

#INPUTS CRIT OBJECT
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModel, 
                               RunOptions = RunOptions, Qobs = BasinObs$R[Ind_Run])
str(InputsCrit)

#CALIB OPTIONS OBJECT

CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_CemaNeigeGR4J, FUN_CALIB = Calibration_Michel, SearchRanges = NULL) 
str(CalibOptions)

#CALIBRATION

OutputsCalib <- Calibration_Michel(InputsModel = InputsModel, RunOptions = RunOptions,
                                   InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   FUN_MOD = RunModel_CemaNeigeGR4J, FUN_CRIT = ErrorCrit_KGE)

Param <- OutputsCalib$ParamFinalR
<<<<<<< HEAD
OutputsModel <- RunModel_GR4J(InputsModel = InputsModel,
                              RunOptions = RunOptions, Param = Param)

## simulation
#Param <- c(734.568, -0.840, 109.809, 1.971)
#OutputsModel <- RunModel(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param,
                        # FUN_MOD = RunModel_GR4J)

## results preview
plot(OutputsModel, Qobs = BasinObs$R[Ind_Run])
## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Qobs = BasinObs$R[Ind_Run])
OutputsCrit <- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)

## efficiency criterion: Kling-Gupta Efficiency
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Qobs = BasinObs$R[Ind_Run])
=======
Param

#SIMULATION
OutputsModel <- RunModel_CemaNeigeGR4J(InputsModel = InputsModel, RunOptions = RunOptions, Param = Param)
str(OutputsModel)

plot(OutputsModel, Qobs = BasinObs$R[Ind_Run])

>>>>>>> 50919ff1cf34ebdae09da7cef60c205ed1ef6ea3
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
