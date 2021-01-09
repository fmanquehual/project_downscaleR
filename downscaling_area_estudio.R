library(ncdf4)
library(loadeR)
library(visualizeR)
library(downscaleR)
library(filesstrings) # para mover archivos entre carpetas
library(climate4R.climdex)
library(climate4R.value)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_identificador_de_variables_era5_CEDA.R')

# filtro de datos ----
meses <- 1
anhos <- 2010:2014
latitud <- c(-49,-36) # area de estudio CCR
longitud <- c(-75, -72) # area de estudio CCR
# latitud <- c(-48, -46) # area de estudio WRF
# longitud <- c(-74, -71) # area de estudio WRF

# fin ---




# ps ----

# Lectura de datos

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
ERA5 <- 'ERA5_2000_al_2014.nc'

# ej <- nc_open(ERA5, verbose = TRUE)

di <- dataInventory(ERA5)
di$sp

C4R.vocabulary()
ps.mean <- loadGridData(dataset = ERA5, 
                        var = "sp",
                        aggr.d = "mean",
                        aggr.m = "mean",
                        lonLim = longitud,
                        latLim= latitud, 
                        season= meses, 
                        years = anhos,
                        time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(ps.mean)

# plot 
spatialPlot(climatology(ps.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = '')

# fin ---




# tas ----

# Lectura de datos
# ta <- 'ta_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20100101-20141231.nc'

#C4R.vocabulary()
tas.mean <- loadGridData(dataset = ERA5, 
                        var = "t2m",
                        aggr.d = "mean",
                        aggr.m = "mean",
                        lonLim = longitud,#+c(-1, +1),
                        latLim= latitud, 
                        season= meses, 
                        years = anhos,
                        time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(tas.mean)

# plot 
spatialPlot(climatology(tas.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
            rev.colors = TRUE, main = '')

# fin ---




# pr ----

# Lectura de datos
# pr <- 'pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc'

# C4R.vocabulary()
pr.sum <- loadGridData(dataset = ERA5, 
                       var = "tp",
                       aggr.d = "sum",
                       aggr.m = "sum",
                       lonLim = longitud,
                       latLim= latitud, 
                       season= meses, 
                       years = anhos,
                       time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(pr.sum)

# plot 
spatialPlot(climatology(pr.sum), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = '')

# fin ---




# u10 ----

# Lectura de datos
u10.mean <- loadGridData(dataset = ERA5,
                       var = "u10",
                       aggr.d = "mean",
                       aggr.m = "mean",
                       lonLim = longitud,
                       latLim= latitud,
                       season= meses,
                       years = anhos,
                       time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(u10.mean)

# plot
spatialPlot(climatology(u10.mean), backdrop.theme = "countries", color.theme = "jet.colors",
            main = '')

# fin ---




# v10 ----

# Lectura de datos
v10.mean <- loadGridData(dataset = ERA5,
                         var = "v10",
                         aggr.d = "mean",
                         aggr.m = "mean",
                         lonLim = longitud,
                         latLim= latitud,
                         season= meses,
                         years = anhos,
                         time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(v10.mean)

# plot
spatialPlot(climatology(v10.mean), backdrop.theme = "countries", color.theme = "jet.colors",
            main = '')
# fin ---



# # Z ----
# 
# # Lectura de datos
# z.mean <- loadGridData(dataset = ERA5, 
#                          var = "z",
#                          aggr.d = "mean",
#                          aggr.m = "mean",
#                          lonLim = longitud,
#                          latLim= latitud, 
#                          season= meses, 
#                          years = anhos,
#                          time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
# str(z.mean)
# 
# # plot 
# spatialPlot(climatology(z.mean), backdrop.theme = "countries", color.theme = "BrBG",
#             main = '')
# # fin ---




# stack ----
x <- makeMultiGrid(ps.mean, tas.mean)#, u10.mean, v10.mean)#, z.mean)

# fin ---




# Estaciones metereologicas ----

# Lectura de datos
nombre.carpeta <- 'datos_transformados_a_ASCII'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
stationInfo(nombre.carpeta)

y <- loadStationData(dataset = nombre.carpeta, 
                            var="precip", 
                            years = anhos)

# Datos presencia/ausencia de precipitacion
y <- binaryGrid(y, condition = "GE", # GE: greater or equal
                threshold = 1, # umbral para discriminar entre presencia y ausencia
                partial = TRUE)

y_bin <- binaryGrid(y, condition = "GE",
                    threshold = 1)

# fin ---




# Calibracion ----

# Configuration of method M1 en Bedia et al. (2020)
vars <- c("sp", "t2m")#, "u10", "v10")#, "z")
folds <- list(2010, 2011)


# m1
spatial.pars.M1 <- list(which.combine = vars,
                        v.exp = .95, # PCs explaining the 95 % of total variance are retained (Bedia et al., 2020)
                        rot = FALSE) # non-rotated

M1cv.bin <- downscaleCV(x = x, y = y_bin,
                        method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M1,
                                                combined.only = TRUE))

# In the logistic regression model, downscaleCV returns
# a multigrid with two output prediction grids, storing 
# the variables prob and bin. The first contains the grid 
# probability of rain for every day and the second is a 
# binary prediction indicating whether it rained or not (Bedia et al., 2020).

M1cv.bin <- subsetGrid(M1cv.bin, var = "bin") # bin: grilla binaria (si llovio o no)

M1cv.cont <- downscaleCV(x = x, y = y, 
                         method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", 
                         threshold = 1,
                         folds = folds,
                         prepareData.args = list(global.vars = NULL,
                                                 local.predictors = NULL,
                                                 spatial.predictors = spatial.pars.M1,
                                                 combined.only = TRUE))

M1cv <- gridArithmetics(M1cv.bin, M1cv.cont, operator = "*")


# plot pp predicha vs observada

aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M1 <- aggregateGrid(M1cv, aggr.m = aggr.pars)

obs <- aggregateGrid(y, aggr.m = aggr.pars)

temporalPlot(pred.M1, obs)


# m2

spatial.pars.M2 <- list(which.combine
                       = NULL, 
                       v.exp = .95)
                       
M2cv.bin <- downscaleCV(x = x, y = y_bin,
                        method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M2,
                                                combined.only = TRUE))

M2cv.bin <- subsetGrid(M2cv.bin, var = "bin") # bin: grilla binaria (si llovio o no)

M2cv.cont <- downscaleCV(x = x, y = y, 
                         method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", 
                         threshold = 1,
                         folds = folds,
                         prepareData.args = list(global.vars = NULL,
                                                 local.predictors = NULL,
                                                 spatial.predictors = spatial.pars.M2,
                                                 combined.only = TRUE))

M2cv <- gridArithmetics(M2cv.bin, M2cv.cont, operator = "*")


# plot pp predicha vs observada

aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M2 <- aggregateGrid(M2cv, aggr.m = aggr.pars)

obs <- aggregateGrid(y, aggr.m = aggr.pars)

temporalPlot(pred.M2, obs)


# m3
local.pars.M3 <- list(n = 1, vars = getVarNames(x))

scaling.pars <- list(type = "standardize",
                    spatial.frame = "gridbox")

M3cv.bin <- downscaleCV(x = x, y = y_bin,
                        method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = local.pars.M3,
                                                spatial.predictors = NULL,
                                                combined.only = TRUE))

M3cv.bin <- subsetGrid(M3cv.bin, var = "bin") # bin: grilla binaria (si llovio o no)

M3cv.cont <- downscaleCV(x = x, y = y,
                         method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", threshold
                         = 1,
                         folds = folds,
                         scaleGrid.args = scaling.pars,
                         prepareData.args =
                           list(global.vars = NULL,
                                local.predictors
                                = local.pars.M3,
                                spatial.predictors
                                = NULL))

M3cv <- gridArithmetics(M3cv.bin, M3cv.cont, operator = "*")


# plot pp predicha vs observada

aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M3 <- aggregateGrid(M3cv, aggr.m = aggr.pars)

obs <- aggregateGrid(y, aggr.m = aggr.pars)

temporalPlot(pred.M3, obs)


# m4
local.pars.M4 <- list(n = 4, vars = vars)

scaling.pars <- list(type = "standardize",
                     spatial.frame = "gridbox")

M4cv.bin <- downscaleCV(x = x, y = y_bin,
                        method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = local.pars.M4,
                                                spatial.predictors = NULL,
                                                combined.only = TRUE))

M4cv.bin <- subsetGrid(M4cv.bin, var = "bin") # bin: grilla binaria (si llovio o no)

M4cv.cont <- downscaleCV(x = x, y = y,
                         method = "GLM",
                         family = Gamma(link = "log"),
                         condition = "GE", threshold
                         = 1,
                         folds = folds,
                         scaleGrid.args = scaling.pars,
                         prepareData.args =
                           list(global.vars = NULL,
                                local.predictors
                                = local.pars.M4,
                                spatial.predictors
                                = NULL))

M4cv <- gridArithmetics(M4cv.bin, M4cv.cont, operator = "*")


# plot pp predicha vs observada

aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M4 <- aggregateGrid(M4cv, aggr.m = aggr.pars)

obs <- aggregateGrid(y, aggr.m = aggr.pars)

temporalPlot(pred.M4, obs)


# m5
scaling.pars.M5 <- list(type = "standardize",
                       spatial.frame = "field")

M5cv <- downscaleCV(x = x, y = y,
                    method = "analogs", n.analogs
                    = 1,
                    folds = folds,
                    scaleGrid.args = scaling.pars.M5,
                    prepareData.args =
                      list(global.vars = vars,
                           local.predictors = NULL,
                           spatial.predictors = NULL))

# plot pp predicha vs observada

aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M5 <- aggregateGrid(M5cv, aggr.m = aggr.pars)

obs <- aggregateGrid(y, aggr.m = aggr.pars)

temporalPlot(pred.M5, obs)


# m6
spatial.pars.M6 <- list(which.combine = vars,
                        v.exp = .95, # PCs explaining the 95 % of total variance are retained (Bedia et al., 2020)
                        rot = FALSE) # non-rotated

M6cv.bin <- downscaleCV(x = x, y = y_bin,
                        method = "GLM",
                        family = binomial(link = "logit"),
                        folds = folds,
                        prepareData.args = list(global.vars = NULL,
                                                local.predictors = NULL,
                                                spatial.predictors = spatial.pars.M6,
                                                combined.only = TRUE))

M6cv.bin <- subsetGrid(M6cv.bin, var = "bin") # bin: grilla binaria (si llovio o no)

M6.cont <- downscaleCV(x = x, y = y,
                       method = "analogs", n.analogs
                       = 1,
                       folds = folds,
                       prepareData.args =
                         list(global.vars = NULL,
                              local.predictors = NULL,
                              spatial.predictors =
                                spatial.pars.M6,
                              combined.only = TRUE))

M6cv <- gridArithmetics(M6cv.bin, M6.cont, operator = "*")

# plot pp predicha vs observada

aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M6 <- aggregateGrid(M6cv, aggr.m = aggr.pars)

obs <- aggregateGrid(y, aggr.m = aggr.pars)

temporalPlot(pred.M6, obs)

# fin ---




# Validation ----

R01.ratio <- valueMeasure(y, x = M6cv,
                          measure.code = "ratio",
                          index.code = "R01")$Measure

spatialPlot(R01.ratio, backdrop.theme = "countries")

# fin ---




# Futuro ---

# Method intercomparison experiment
# M1.L <- list(local.predictors =
#                list(n = 1, vars = vars),
#              spatial.predictors =
#                list(v.exp = .95,
#                     which.combine = vars))

M6.L <- list(local.predictors = list(n = 1,
                                     vars = vars))
# Standardization
x_scale <- scaleGrid(x, type = "standardize")

# Predictor config (M6-L method)
M6.L <- prepareData(x_scale, y)#, local.predictors = M6.L)

# SDS model training
model.M6L <- downscaleTrain(M6.L, 
                            method = "analogs",
                            n.analogs = 1)

# fin ---




# psl CMIP6 ---- 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/psl/')

psl.mean.cmip6 <- loadGridData(dataset = 'psl_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
                       var = "psl",
                       aggr.d = "mean",
                       aggr.m = "mean",
                       lonLim = longitud,
                       latLim= latitud, 
                       season= meses, 
                       years = anhos,
                       time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(psl.mean.cmip6)

# plot 
spatialPlot(climatology(psl.mean.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = '')

# fin ---




# pr CMIP6 ---- 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/pr/')

pr.sum.cmip6 <- loadGridData(dataset = 'pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
                               var = "pr",
                               aggr.d = "sum",
                               aggr.m = "sum",
                               lonLim = longitud,
                               latLim= latitud, 
                               season= meses, 
                               years = anhos,
                               time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(pr.sum.cmip6)

# plot 
spatialPlot(climatology(pr.sum.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = '')

# fin ---




# tas CMIP6 ---- 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/tas/')

# C4R.vocabulary()
tas.mean.cmip6 <- loadGridData(dataset = 'tas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
                               var = "tas",
                               aggr.d = "mean",
                               aggr.m = "mean",
                               lonLim = longitud,
                               latLim= latitud, 
                               season= meses, 
                               years = anhos,
                               time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(tas.mean.cmip6)

# plot 
spatialPlot(climatology(tas.mean.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = '')

# fin ---




# # uas CMIP6 ---- 
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/uas/')
# 
# # C4R.vocabulary()
# uas.mean.cmip6 <- loadGridData(dataset = 'uas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
#                                var = "uas",
#                                aggr.d = "mean",
#                                aggr.m = "mean",
#                                lonLim = longitud,
#                                latLim= latitud, 
#                                season= meses, 
#                                years = anhos,
#                                time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
# str(uas.mean.cmip6)
# 
# # plot 
# spatialPlot(climatology(uas.mean.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = '')
# 
# # fin ---
# 
# 
# 
# 
# 
# # vas CMIP6 ---- 
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/vas/')
# 
# # C4R.vocabulary()
# vas.mean.cmip6 <- loadGridData(dataset = 'vas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
#                                var = "vas",
#                                aggr.d = "mean",
#                                aggr.m = "mean",
#                                lonLim = longitud,
#                                latLim= latitud, 
#                                season= meses, 
#                                years = anhos,
#                                time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
# str(vas.mean.cmip6)
# 
# # plot 
# spatialPlot(climatology(vas.mean.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = '')
# 
# # fin ---


# ERA 5 Y CMIP6 CSIRO, TIENEN DIFERENTES DIMENSIONES, POR ESO NO CORRE LO DE ABAJO!
# PROBAR CON EJEMPLO DE OTRO PAPER

xh <- makeMultiGrid(psl.mean.cmip6, tas.mean.cmip6)#, uas.mean.cmip6, vas.mean.cmip6)

xh <- interpGrid(xh, new.coordinates = getGrid(x))

xh <- scaleGrid(xh, base = xh, ref = x,
                type = "center",
                spatial.frame = "gridbox",
                time.frame = "monthly")

xh <- scaleGrid(xh, base = x, type =
                  "standardize")

h_analog <- prepareNewData(newdata = xh,
                           data.struc = M6L)

f_analog <- prepareNewData(newdata = xf,
                           data.struc = M6L)

hist_ocu_glm <- downscalePredict(newdata
                                 = h_analog,
                                 model = model.M6L)

# fin ---




# Apuntes ----

# Apuntes ERA 5 (para descargar datos!!!!): ERA 5 comes in two streams: the ‘oper’ stream is a single member 
# analysis at highest resolution, and the ‘enda’ stream is a 10-member ensemble at 
# lower resolution - both are supported (Fuente: https://brohan.org/IRData/subdata/data_era5.html).

# Finally (panel e), future projections from the same
# model are loaded from the RCP8.5 scenario (2071-2100)
# and bias adjusted using the biasCorrection function.


# fin ---
