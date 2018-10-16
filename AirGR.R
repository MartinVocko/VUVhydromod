install.packages("airGR")

library(airGR)

# v kombinaci se snehovym modelem pouze v dennim kroku
#RunModel_CemaNeigeGR4J() #: combined use of GR4J and CemaNeige
#RunModel_CemaNeigeGR5J() #: combined use of GR5J and CemaNeige
#RunModel_CemaNeigeGR6J() #: combined use of GR6J and CemaNeige


## loading catchment data
BasinObs=readRDS("MET")
## preparation of the InputsModel object
InputsModel <- CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = as.POSIXct( BasinObs$Date),
                                 Precip = BasinObs$P, PotEvap = BasinObs$PET)
## run period selection
Ind_Run <- seq(which(format(BasinObs$Date, format = "%YYYY-%mm-%dd")=="2012-01-01"),
               which(format(BasinObs$Date, format = "%YYYY-%mm-%dd")=="2018-08-01"))
## preparation of the RunOptions object
RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR4J,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run)

## calibration criterion: preparation of the InputsCrit object
InputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel,
                               RunOptions = RunOptions, Qobs = BasinObs$R[Ind_Run])
## preparation of CalibOptions object
CalibOptions <- CreateCalibOptions(FUN_MOD = RunModel_GR4J, FUN_CALIB = Calibration_Michel)
## calibration
OutputsCalib <- Calibration_Michel(InputsModel = InputsModel, RunOptions = RunOptions,
                                   InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                                   FUN_MOD = RunModel_GR4J, FUN_CRIT = ErrorCrit_NSE)


## simulation
Param <- OutputsCalib$ParamFinalR
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
OutputsCrit <- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = OutputsModel)
