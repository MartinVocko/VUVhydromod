#install.packages(c(("TUWmodel"),("DEoptim"),("hydroGOF")))

library(TUWmodel, quietly = TRUE)
library(DEoptim, quietly = TRUE)
library(hydroGOF, quietly = TRUE)

ModelInput <- readRDS("~/Plocha/VUV/Vocko/ModelInput.rds")
ObsDischarge <- readRDS("~/Plocha/VUV/Vocko/ObsDischarges.rds")

#..................................
#nahrani dat s PET 
#..................................
ModelInput=MET[,c(4,6):=NULL]
ObsDischarge=MET[,c(1,4,6)]
# ------------------------------------------------------------------------------
# ---- CALIBRATION AND VALIDATION PERIODS ----
# ------------------------------------------------------------------------------

#sapply(list(ObsDischarge$dic, ModelInput$temp), length) # Full times series is 4383, divide on cal and val periods
#Period_cal <- cbind(ModelInput[1:2192, ], runoff = ObsDischarge[1:2192, 4])
#Period_val <- cbind(ModelInput[2193:nrow(ModelInput), ], runoff = ObsDischarge[2193:nrow(ObsDischarge), 4])

sapply(list(ObsDischarge$R, ModelInput$T), length) # Full times series is 4383, divide on cal and val periods
Period_cal <- cbind(ModelInput[1:1202, ], ObsDischarge[1:1202, 2])
Period_val <- cbind(ModelInput[1203:nrow(ModelInput), ], ObsDischarge[1203:nrow(ObsDischarge), 2])



# ------------------------------------------------------------------------------
# ---- MODEL FUNCTION ----
# ------------------------------------------------------------------------------

TUW <- function(x = c(SCF = 1.2, DDF = 1.2, Tr = 2, 
                      Ts = -2, Tm = 0, LPrat = 0.9, FC = 200, BETA = 3.3, 
                      k0 = 0.5, k1 = 9, k2 = 105, lsuz = 50, cperc = 2,
                      bmax = 10, croute = 26.5), 
                R, prec, ep, airt) {
  #TUW_call <- as.list(match.call())
     TUW_results <- TUWmodel(prec = prec,                                       # precipitation TS
                    airt = airt,                                                # air temperature TS
                    ep = ep,                                                    # potential Evapotranspiration 
                    area = 1,                                                   # only one zone, if more zones than sum has to equal 1
                    param = c(SCF    = x[1],                                    # snow correction factor [-] (e.g., 0.9-1.5)
                              DDF    = x[2],                                    # DDF degree day factor [mm/degC/timestep] (e.g., 0.0-5.0 mm/degC/day)
                              Tr     = x[3],                                    # Tr threshold temperature above which precipitation is rain [degC] (e.g., 1.0-3.0 degC)
                              Ts     = x[4],                                    # Ts threshold temperature below which precipitation is snow [degC] (e.g., -3.0-1.0 degC)
                              Tm     = x[5],                                    # Tm threshold temperature above which melt starts [degC] (e.g., -2.0-2.0 degC);
                              LPrat  = x[6],
                              FC     = x[7],
                              BETA   = x[8],
                              k0     = x[9],
                              k1     = x[10],
                              k2     = x[11],
                              lsuz   = x[12],
                              cperc  = x[13],
                              bmax   = x[14],
                              croute = x[15]),                                  # list of parameters ()
                    incon = c(SSM0 = 50,                                        # vector of initial conditions
                              SWE0 = 0,
                              SUZ0 = 2.5,
                              SLZ0 = 2.5), 
                    itsteps = NULL)                                             # all time series are used
    
     RM <- TUW_results$q[1, ]                                                           
  # --- actual objective function
  # Kling-Gupta efficiency, others in hydroGOF, one year warm-up   
  # objective_function <- (-1*KGE(sim = RM[366:length(RM)], obs = R[366:length(R)],
  #                           s = c(1,1,1), method = "2012"))
    objective_function <- mae(sim = RM[366:length(RM)], obs = R[366:length(R)])
  return(objective_function)
}
# ---- ACTUAL OPTIMA OF OBJECTIVE FUNCTION (CALIBRATION CRITERIA) --------------
# OF_optima <- -1 # passed to VTR in DEoptim.control
OF_optima <- 0 # passed to VTR in DEoptim.control
#-------------------------------------------------------------------------------
DE_control <- expression(DEoptim.control(VTR = OF_optima, strategy = 1, bs = FALSE,
                                         NP = 400, itermax = 1000, CR = 0.5, F = 0.8, trace = 10, 
                                         initialpop = NULL, storepopfrom = 1000 + 1, storepopfreq = 1, 
                                         p = 0.1, c = 0.05, reltol = 1e-02, steptol = 50, parallelType = 0, cluster = NULL, 
                                         packages = c(), parVar = c(), foreachArgs = list()))
#-------------------------------------------------------------------------------
# 
TUW_optimized <- DEoptim(fn = TUW, 
                         lower = c(SCF = 0.9, DDF = 1.2, Tr = 1.0, Ts = -3.0, Tm = -2, LPrat = 0.0,
                                   FC = 150.0, BETA = 0.5, k0 = 0.0, k1 = 2, k2 = 30, lsuz = 1.0,
                                   cperc = 0.0, bmax = 10.0, croute = 0.0), 
                         upper = c(SCF = 1.5, DDF = 2.0, Tr = 3.0, Ts = 1.0, Tm = 2, LPrat = 1.0,
                                   FC = 500.0, BETA = 20.0, k0 = 2.0, k1 = 30, k2 = 250, lsuz = 100,
                                   cperc = 8.0, bmax = 30.0, croute = 50.0), 
                         control = eval(DE_control), 
                         R = Period_cal$R,   #Period_cal$runoff
                         prec = Period_cal$P,    #Period_cal$prec
                         ep = Period_cal$PET,      #Period_cal$pet 
                         airt = Period_cal$T)   #Period_cal$temp

# TUW_sim should then contain components of 
#   swe  = snow water equaivalent   [mm],
#   melt = snowmelt equivalent      [mm/timestep],
#   snow = snow solid precipitation [mm/timestep]
TUW_pars <- TUW_optimized$optim$bestmem
TUW_res <- TUWmodel(prec = ModelInput$P, #ModelInput$prec
                    ep   = ModelInput$PET,   #ModelInput$pet                    # potential ET
                    airt = ModelInput$T,   #ModelInput$temp                  # mean air temperature
                    area = 1,                                                   # only one zone, if more zones than sum has to equal 1
                    param = TUW_pars,
                    incon = c(SSM0 = 50,                                        # vector of initial conditions
                              SWE0 = 0,
                              SUZ0 = 2.5,
                              SLZ0 = 2.5), 
                    itsteps = NULL)
plot(ObsDischarge$R, type = "l")       #ObsDischarge$disc
lines(TUW_res$q[1, ], col = "red")

# ---- PARAMETER POPULATIONS
boxplot(TUW_optimized$member$pop[, 1:6])

# ---- EXTRAS ----
#-------------------------------------------------------------------------------
# MINIMAL WORKING EXAMPLE OF DEoptim
#-------------------------------------------------------------------------------

# fn <- function(x) {
#   return(x[1]^2+2*x[2])
# }
# OPTIM_res <- DEoptim(fn = fn, lower = c(-0.5, 2), upper = c(2.5, 15))
# summary(OPTIM_res)
# plot(OPTIM_res$member$bestvalit, xlab = "Iteration", ylab = "Value", type = "b") # best value each iteration



