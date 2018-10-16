<<<<<<< HEAD
library(TUWmodel)
data(example_TUWmodel)
# TUW CELISTVY - neoptimalizovany, vazeny prumer vstupu
# ------------------------------------------------------------------------------
simLump <- TUWmodel(prec = apply(P_Vils, 1, weighted.mean, w = areas_Vils), 
                    airt = apply(T_Vils, 1, weighted.mean, w = areas_Vils),
                    ep = apply(PET_Vils, 1, weighted.mean, w = areas_Vils), 
                    area = sum(areas_Vils),
                    param = c(1.02,
                              1.70,
                              2,
                              0,
                              -0.336,
                              0.934,
                              121,
                              2.52,
                              0.473,
                              9.06,
                              142,
                              50.1,
                              2.38,
                              10,
                              25))
plot(as.Date(names(Q_Vils)), 
     Q_Vils, 
     type = "l", 
     xlab = "", 
     ylab = "Odtok [mm/den]")
lines(as.Date(rownames(T_Vils)), simLump$q, col = 2)
legend("topleft", legend = c("Pozorovane","Simulovane"), col = c(1,2), lty = 1, bty = "n")
plot(as.Date(rownames(SWE_Vils)), apply(SWE_Vils, 1, weighted.mean, w=areas_Vils), 
     type = "l", xlab = "", ylab = "Vodni hodnota snehu [mm]")
lines(as.Date(rownames(T_Vils)), simLump$swe, col = 2)
# ------------------------------------------------------------------------------
# TUW CELISTVY - optimalizovany, vazeny prumer vstupu
library(TUWmodel)
MET=readRDS('MET')

# KALIBRACNI KRIERIUM
# ----------------------------------------------------------------------------
# Funkce MSE predstavuje kalibracni kriterium, vraci stredni kvadratickou chybu
MSE <- function(param, precip, temp, potevap, runoff, area) {
  simu <- TUWmodel(prec = as.numeric(precip), 
                   airt = as.numeric(temp), 
                   ep = as.numeric(potevap), 
                   area = area, 
                   param)$q
  simu[is.nan(simu)] <- -999
  simu <- simu[-c(1:365)]  # bez zapocitani warm-up periody 1 rok
  obse <- runoff[-c(1:365)]  # bez zapocitani warm-up periody 1 rok
  mobs <- mean(obse, na.rm = TRUE)
  mean((simu - obse)^2, na.rm = TRUE)  # mean square error
}
# ------------------------------------------------------------------------------
m_input <- readRDS(file.choose()) # 
names(m_input) <- c("D", "M", "Y", "P", "T", "Q")
SI <- c(0.19, 0.23, 0.27, 0.31, 0.35, 0.37, 0.36, 0.33, 0.29, 0.25, 0.20, 0.18)
area <- 250                         # podle hlavicky vstupniho souboru

# Nasledujici kod vytvori casovou radu
DTM <- as.Date(strptime(paste(m_input[, 1], m_input[, 2], m_input[, 3]), format = "%d %m %Y"))
Prec <- zoo(m_input[,4], order.by = DTM)    # denni srazka na povodi (mm/d)
Temp <- zoo(m_input[,5], order.by = DTM)    # prumerna denni teplota na povodi (Â°C)
Q <- zoo(m_input[,6], order.by = DTM)    # prumerny denni mereny prutok (m3/s)

# Nasledujici kod vypocte potencialni evapotranspiraci dle modifikovane Blaney-Criddle metody (Schroedter, 1985)
repeated_SI <- SI[as.numeric(format(index(Temp), '%m'))]
EP <- -1.55 + 0.96*(8.128 + 0.457*Temp) * repeated_SI
EP[EP < 0] <- 0                     # denni potencialni evapotranspirace (mm/d)


# Nyni rozdelime soubor na dve po sobe jdouci casove rady
yr <- unique(as.numeric(format(DTM, "%Y")))
nyr <- length(yr)

P1 <- window(Prec, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))
T1 <- window(Temp, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))
EP1 <- window(EP, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))
Q1 <- window(Q, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))

P2 <- window(Prec, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))
T2 <- window(Temp, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))
EP2 <- window(EP, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))
Q2 <- window(Q, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))


calibrate_period1 <- DEoptim(fn = MSE, 
                             lower = c(0.9, 0.0, 1.0, -3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 1.0, 0.0, 0.0, 0.0),
                             upper = c(1.5, 5.0, 3.0, 1.0, 2.0, 1.0, 600.0, 20.0, 2.0, 30.0, 250.0, 100.0, 8.0, 30.0, 50.0),
                             control = DEoptim.control(NP = NA, 
                                                       itermax = 50, 
                                                       reltol = 1e-4, 
                                                       steptol = 50, 
                                                       trace = 10, 
                                                       parallelType = 0),
                             precip = P1, 
                             temp = T1, 
                             potevap = EP1, 
                             runoff = Q1, 
                             area = area)
simulation1_cal1 <- TUWmodel(prec = as.numeric(P1), 
                             airt = as.numeric(T1), 
                             ep = as.numeric(EP1), 
                             area = area, 
                             param = calibrate_period1$optim$bestmem)
calibrate_period2 <- DEoptim(fn = MSE, 
                             lower = c(0.9, 0.0, 1.0, -3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 1.0, 0.0, 0.0, 0.0),
                             upper = c(1.5, 5.0, 3.0, 1.0, 2.0, 1.0, 600.0, 20.0, 2.0, 30.0, 250.0, 100.0, 8.0, 30.0, 50.0),
                             control = DEoptim.control(NP = NA, 
                                                       itermax = 100, 
                                                       reltol = 1e-4, 
                                                       steptol = 50, 
                                                       trace = 10, 
                                                       parallelType = 0),
                             precip = P2, 
                             temp = T2, 
                             potevap = EP2, 
                             runoff = Q2, 
                             area = area)
simulation1_cal2 <- TUWmodel(prec = as.numeric(P2), 
                             airt = as.numeric(T2), 
                             ep = as.numeric(EP2), 
                             area = area, 
                             param = calibrate_period2$optim$bestmem)

simulation2_val1 <- TUWmodel(prec = as.numeric(P1), 
                             airt = as.numeric(T1), 
                             ep = as.numeric(EP1), 
                             area = area, 
                             param = calibrate_period2$optim$bestmem)

plot(x = DTM[1:4749], y = Q1, 
     type = "l", 
     xlab = "", 
     ylab = "Odtok [mm/den]")
lines(x = DTM[1:4749], y = simulation1_cal1$q[1,], col = "red")
legend("topleft", legend = c("Pozorovane", "Simulovane"), col = c(1,2), lty = 1, bty = "n")

plot(x = DTM[1:4748], y = Q2, 
     type = "l", 
     xlab = "", 
     ylab = "Odtok [mm/den]")
lines(x = DTM[1:4748], y = simulation1_cal2$q[1,], col = "red")
legend("topleft", legend = c("Pozorovane", "Simulovane"), col = c(1,2), lty = 1, bty = "n")

EMs <- function (sim, obs, warmup=365) {
  # obs = observed runoff in mm/d (class numeric)
  # sim = simulated runoff in mm/d (class numeric)
  # warmup = warm-up period in d
  simu <- sim[-c(1:warmup)]
  obse <- obs[-c(1:warmup)]
  
  # RMSE = root mean square error (mm/d)
  RMSE <- sqrt(mean((simu - obse)^2, na.rm=TRUE))
  # NE = Nash efficiency ()
  mobse <- mean(obse, na.rm=TRUE)
  NE <- 1 - sum((simu - obse)^2, na.rm=TRUE)/sum((obse - mobse)^2, na.rm=TRUE)
  # lNE = log Nash efficiency ()
  mlobse <- mean(log(obse), na.rm=TRUE)
  lNE <- 1 - sum((log(simu) - log(obse))^2, na.rm=TRUE)/sum((log(obse) - mlobse)^2, na.rm=TRUE)
  # B = bias (mm/d)
  B <- mean(simu - obse, na.rm=TRUE)
  # MAE = mean absolute error (mm/d)
  MAE <- mean(abs(simu - obse), na.rm=TRUE)
  # MAlE = mean absolute log error (mm/d)
  MAlE <- exp(mean(abs(log(simu) - log(obse)), na.rm=TRUE))
  # VE = volume error (%/%)
  VE <- (sum(simu[!is.na(obse)]) - sum(obse, na.rm=TRUE))/sum(obse, na.rm=TRUE)
  
  output <- c(RMSE, NE, lNE, B, MAE, MAlE, VE)
  names(output) <- c("RMSE (mm/d)", "Nash-Sutcliffe efficiency ()", "log N-S efficiency ()", "bias (mm/d)", 
                     "mean absolute error (mm/d)", "mean absolute log error (mm/d)", "volume error (%/%)")
  return(output)
}

efficiencies <- rbind(EMs(3.6*24*as.numeric(simulation1_cal1$q)/area, 3.6*24*as.numeric(Q1)/area),
                      EMs(3.6*24*as.numeric(simulation2_val1$q)/area, 3.6*24*as.numeric(Q1)/area))
rownames(efficiencies) <- c("cal1", "cal2", "val1", "val2")
print(signif(efficiencies, 3))


flowdurations <- function (sim, obs, warmup=365, xlab="F(x > X)", ylab="x", ...) {
  # obs = observed runoff (class numeric)
  # sim = simulated runoff (class numeric)
  # warmup = warm-up period in d
  simu <- sim[-c(1:warmup)]
  obse <- obs[-c(1:warmup)]
  
  quantili <- c(1,.999,.995, seq(.99,.01, by=-.01), .005,.001,0)
  obs_fdc <- quantile(obse, prob=quantili, na.rm=TRUE)
  sim_fdc <- quantile(simu, prob=quantili, na.rm=TRUE)
  
  plot(1 - quantili, obs_fdc, type="l", xlab=xlab, ylab=ylab, ...)
  lines(1 - quantili, sim_fdc, col="red")
}

flowdurations(simulation1_cal1$q, Q1, log="y", ylab="Cal period 1")
flowdurations(simulation2_val1$q, Q1, log="y", ylab="Val period 1")

# DISTRIBUOVANY MODEL
# ------------------------------------------------------------------------------
data(example_TUWmodel)
parametri <- matrix(rep(c(1.02,1.70,2,0,-0.336,
                          0.934,121,2.52,
                          0.473,9.06,142,
                          50.1,2.38,10,25), 6), ncol=6)
parametri[2,] <- c(1.4, 1.7, 1.9, 2.2, 2.4, 3.0)
simDist2 <- TUWmodel(prec = P_Vils,
                     airt = T_Vils, 
                     ep = PET_Vils, 
                     area = areas_Vils/sum(areas_Vils),
                     param = parametri)
plot(as.Date(names(Q_Vils)), Q_Vils, type="l", xlab="", ylab = "Odtok [mm/den]")
lines(as.Date(rownames(T_Vils)), simDist2$q, col=2)
legend("topleft", legend = c("Pozorovane","Simulovane"), col = c(1, 2), lty = 1, bty = "n")
=======
library(TUWmodel)
data(example_TUWmodel)
# TUW CELISTVY - neoptimalizovany, vazeny prumer vstupu
# ------------------------------------------------------------------------------
simLump <- TUWmodel(prec = apply(P_Vils, 1, weighted.mean, w = areas_Vils), 
                    airt = apply(T_Vils, 1, weighted.mean, w = areas_Vils),
                    ep = apply(PET_Vils, 1, weighted.mean, w = areas_Vils), 
                    area = sum(areas_Vils),
                    param = c(1.02,
                              1.70,
                              2,
                              0,
                              -0.336,
                              0.934,
                              121,
                              2.52,
                              0.473,
                              9.06,
                              142,
                              50.1,
                              2.38,
                              10,
                              25))
plot(as.Date(names(Q_Vils)), 
     Q_Vils, 
     type = "l", 
     xlab = "", 
     ylab = "Odtok [mm/den]")
lines(as.Date(rownames(T_Vils)), simLump$q, col = 2)
legend("topleft", legend = c("Pozorovane","Simulovane"), col = c(1,2), lty = 1, bty = "n")
plot(as.Date(rownames(SWE_Vils)), apply(SWE_Vils, 1, weighted.mean, w=areas_Vils), 
     type = "l", xlab = "", ylab = "Vodni hodnota snehu [mm]")
lines(as.Date(rownames(T_Vils)), simLump$swe, col = 2)
# ------------------------------------------------------------------------------
# TUW CELISTVY - optimalizovany, vazeny prumer vstupu
library(TUWmodel)
# KALIBRACNI KRIERIUM
# ----------------------------------------------------------------------------
# Funkce MSE predstavuje kalibracni kriterium, vraci stredni kvadratickou chybu
MSE <- function(param, precip, temp, potevap, runoff, area) {
  simu <- TUWmodel(prec = as.numeric(precip), 
                   airt = as.numeric(temp), 
                   ep = as.numeric(potevap), 
                   area = area, 
                   param)$q
  simu[is.nan(simu)] <- -999
  simu <- simu[-c(1:365)]  # bez zapocitani warm-up periody 1 rok
  obse <- runoff[-c(1:365)]  # bez zapocitani warm-up periody 1 rok
  mobs <- mean(obse, na.rm = TRUE)
  mean((simu - obse)^2, na.rm = TRUE)  # mean square error
}
# ------------------------------------------------------------------------------
m_input <- read.table(file.choose()) # 
names(m_input) <- c("D", "M", "Y", "P", "T", "Q")
SI <- c(0.19, 0.23, 0.27, 0.31, 0.35, 0.37, 0.36, 0.33, 0.29, 0.25, 0.20, 0.18)
area <- 250                         # podle hlavicky vstupniho souboru

# Nasledujici kod vytvori casovou radu
DTM <- as.Date(strptime(paste(m_input[, 1], m_input[, 2], m_input[, 3]), format = "%d %m %Y"))
Prec <- zoo(m_input[,4], order.by = DTM)    # denni srazka na povodi (mm/d)
Temp <- zoo(m_input[,5], order.by = DTM)    # prumerna denni teplota na povodi (Â°C)
Q <- zoo(m_input[,6], order.by = DTM)    # prumerny denni mereny prutok (m3/s)

# Nasledujici kod vypocte potencialni evapotranspiraci dle modifikovane Blaney-Criddle metody (Schroedter, 1985)
repeated_SI <- SI[as.numeric(format(index(Temp), '%m'))]
EP <- -1.55 + 0.96*(8.128 + 0.457*Temp) * repeated_SI
EP[EP < 0] <- 0                     # denni potencialni evapotranspirace (mm/d)


# Nyni rozdelime soubor na dve po sobe jdouci casove rady
yr <- unique(as.numeric(format(DTM, "%Y")))
nyr <- length(yr)

P1 <- window(Prec, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))
T1 <- window(Temp, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))
EP1 <- window(EP, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))
Q1 <- window(Q, end = as.Date(strptime(paste("31 12", yr[floor(nyr/2)]), format = "%d %m %Y")))

P2 <- window(Prec, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))
T2 <- window(Temp, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))
EP2 <- window(EP, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))
Q2 <- window(Q, start = as.Date(strptime(paste("1 1", yr[nyr - floor(nyr/2) + 1]), format = "%d %m %Y")))


calibrate_period1 <- DEoptim(fn = MSE, 
                             lower = c(0.9, 0.0, 1.0, -3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 1.0, 0.0, 0.0, 0.0),
                             upper = c(1.5, 5.0, 3.0, 1.0, 2.0, 1.0, 600.0, 20.0, 2.0, 30.0, 250.0, 100.0, 8.0, 30.0, 50.0),
                             control = DEoptim.control(NP = NA, 
                                                       itermax = 50, 
                                                       reltol = 1e-4, 
                                                       steptol = 50, 
                                                       trace = 10, 
                                                       parallelType = 0),
                             precip = P1, 
                             temp = T1, 
                             potevap = EP1, 
                             runoff = Q1, 
                             area = area)
simulation1_cal1 <- TUWmodel(prec = as.numeric(P1), 
                             airt = as.numeric(T1), 
                             ep = as.numeric(EP1), 
                             area = area, 
                             param = calibrate_period1$optim$bestmem)
calibrate_period2 <- DEoptim(fn = MSE, 
                             lower = c(0.9, 0.0, 1.0, -3.0, -2.0, 0.0, 0.0, 0.0, 0.0, 2.0, 30.0, 1.0, 0.0, 0.0, 0.0),
                             upper = c(1.5, 5.0, 3.0, 1.0, 2.0, 1.0, 600.0, 20.0, 2.0, 30.0, 250.0, 100.0, 8.0, 30.0, 50.0),
                             control = DEoptim.control(NP = NA, 
                                                       itermax = 100, 
                                                       reltol = 1e-4, 
                                                       steptol = 50, 
                                                       trace = 10, 
                                                       parallelType = 0),
                             precip = P2, 
                             temp = T2, 
                             potevap = EP2, 
                             runoff = Q2, 
                             area = area)
simulation1_cal2 <- TUWmodel(prec = as.numeric(P2), 
                             airt = as.numeric(T2), 
                             ep = as.numeric(EP2), 
                             area = area, 
                             param = calibrate_period2$optim$bestmem)

simulation2_val1 <- TUWmodel(prec = as.numeric(P1), 
                             airt = as.numeric(T1), 
                             ep = as.numeric(EP1), 
                             area = area, 
                             param = calibrate_period2$optim$bestmem)

plot(x = DTM[1:4749], y = Q1, 
     type = "l", 
     xlab = "", 
     ylab = "Odtok [mm/den]")
lines(x = DTM[1:4749], y = simulation1_cal1$q[1,], col = "red")
legend("topleft", legend = c("Pozorovane", "Simulovane"), col = c(1,2), lty = 1, bty = "n")

plot(x = DTM[1:4748], y = Q2, 
     type = "l", 
     xlab = "", 
     ylab = "Odtok [mm/den]")
lines(x = DTM[1:4748], y = simulation1_cal2$q[1,], col = "red")
legend("topleft", legend = c("Pozorovane", "Simulovane"), col = c(1,2), lty = 1, bty = "n")

EMs <- function (sim, obs, warmup=365) {
  # obs = observed runoff in mm/d (class numeric)
  # sim = simulated runoff in mm/d (class numeric)
  # warmup = warm-up period in d
  simu <- sim[-c(1:warmup)]
  obse <- obs[-c(1:warmup)]
  
  # RMSE = root mean square error (mm/d)
  RMSE <- sqrt(mean((simu - obse)^2, na.rm=TRUE))
  # NE = Nash efficiency ()
  mobse <- mean(obse, na.rm=TRUE)
  NE <- 1 - sum((simu - obse)^2, na.rm=TRUE)/sum((obse - mobse)^2, na.rm=TRUE)
  # lNE = log Nash efficiency ()
  mlobse <- mean(log(obse), na.rm=TRUE)
  lNE <- 1 - sum((log(simu) - log(obse))^2, na.rm=TRUE)/sum((log(obse) - mlobse)^2, na.rm=TRUE)
  # B = bias (mm/d)
  B <- mean(simu - obse, na.rm=TRUE)
  # MAE = mean absolute error (mm/d)
  MAE <- mean(abs(simu - obse), na.rm=TRUE)
  # MAlE = mean absolute log error (mm/d)
  MAlE <- exp(mean(abs(log(simu) - log(obse)), na.rm=TRUE))
  # VE = volume error (%/%)
  VE <- (sum(simu[!is.na(obse)]) - sum(obse, na.rm=TRUE))/sum(obse, na.rm=TRUE)
  
  output <- c(RMSE, NE, lNE, B, MAE, MAlE, VE)
  names(output) <- c("RMSE (mm/d)", "Nash-Sutcliffe efficiency ()", "log N-S efficiency ()", "bias (mm/d)", 
                     "mean absolute error (mm/d)", "mean absolute log error (mm/d)", "volume error (%/%)")
  return(output)
}

efficiencies <- rbind(EMs(3.6*24*as.numeric(simulation1_cal1$q)/area, 3.6*24*as.numeric(Q1)/area),
                      EMs(3.6*24*as.numeric(simulation2_val1$q)/area, 3.6*24*as.numeric(Q1)/area))
rownames(efficiencies) <- c("cal1", "cal2", "val1", "val2")
print(signif(efficiencies, 3))


flowdurations <- function (sim, obs, warmup=365, xlab="F(x > X)", ylab="x", ...) {
  # obs = observed runoff (class numeric)
  # sim = simulated runoff (class numeric)
  # warmup = warm-up period in d
  simu <- sim[-c(1:warmup)]
  obse <- obs[-c(1:warmup)]
  
  quantili <- c(1,.999,.995, seq(.99,.01, by=-.01), .005,.001,0)
  obs_fdc <- quantile(obse, prob=quantili, na.rm=TRUE)
  sim_fdc <- quantile(simu, prob=quantili, na.rm=TRUE)
  
  plot(1 - quantili, obs_fdc, type="l", xlab=xlab, ylab=ylab, ...)
  lines(1 - quantili, sim_fdc, col="red")
}

flowdurations(simulation1_cal1$q, Q1, log="y", ylab="Cal period 1")
flowdurations(simulation2_val1$q, Q1, log="y", ylab="Val period 1")

# DISTRIBUOVANY MODEL
# ------------------------------------------------------------------------------
data(example_TUWmodel)
parametri <- matrix(rep(c(1.02,1.70,2,0,-0.336,
                          0.934,121,2.52,
                          0.473,9.06,142,
                          50.1,2.38,10,25), 6), ncol=6)
parametri[2,] <- c(1.4, 1.7, 1.9, 2.2, 2.4, 3.0)
simDist2 <- TUWmodel(prec = P_Vils,
                     airt = T_Vils, 
                     ep = PET_Vils, 
                     area = areas_Vils/sum(areas_Vils),
                     param = parametri)
plot(as.Date(names(Q_Vils)), Q_Vils, type="l", xlab="", ylab = "Odtok [mm/den]")
lines(as.Date(rownames(T_Vils)), simDist2$q, col=2)
legend("topleft", legend = c("Pozorovane","Simulovane"), col = c(1, 2), lty = 1, bty = "n")
>>>>>>> 50919ff1cf34ebdae09da7cef60c205ed1ef6ea3
