library(loadeR)
library(visualizeR)
library(downscaleR)
library(climate4R.climdex)
library(climate4R.value)
library(convertR)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_identificador_de_variables_era5_CEDA.R')
source('funcion_calculo_de_resolucion.R')

# filtro de datos ----

meses <- 1:12
anhos <- 2010:2011
# latitud <- c(-49,-36) # area de estudio CCR
# longitud <- c(-75, -72) # area de estudio CCR
# latitud <- c(-48, -46) # area de estudio WRF
# longitud <- c(-74, -71) # area de estudio WRF
latitud <- c(-48, -44.5)
longitud <- c(-74, -70.5)

# fin ---




# Lectura de datos ----

# Estaciones metereologicas

nombre.carpeta <- 'datos_transformados_a_ASCII'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
stationInfo(nombre.carpeta)

y <- loadStationData(dataset = nombre.carpeta, 
                     var="precip", 
                     years = anhos)


# Predictors (ERA reanalisis) 

#setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA_LAND/') # Tiene NA por el oceano, lo que genera problemas
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
era5 <- 'ERA5_2010_2011.nc'

inventario.era5 <- dataInventory(era5)

# Precipitacion

# C4R.vocabulary()
pr.sum <- loadGridData(dataset = era5, 
                       var = "tp",
                       aggr.d = "sum",
                       #aggr.m = "sum",
                       lonLim = longitud,
                       latLim= latitud, 
                       season= meses, 
                       years = anhos,
                       time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(pr.sum)

spatialPlot(climatology(pr.sum), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Precipitacion', scales = list(draw = T))

# # Presion superficial
# ps.mean <- loadGridData(dataset = era5, 
#                         var = "sp",
#                         aggr.d = "mean",
#                         #aggr.m = "mean",
#                         lonLim = longitud,
#                         latLim= latitud, 
#                         season= meses, 
#                         years = anhos,
#                         time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
# 
# spatialPlot(climatology(ps.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = 'Presion superficial', scales = list(draw = T))

# Temperatura
tas.mean0 <- loadGridData(dataset = era5, 
                         var = "t2m",
                         aggr.d = "mean",
                         #aggr.m = "mean",
                         lonLim = longitud,#+c(-1, +1),
                         latLim= latitud, 
                         season= meses, 
                         years = anhos,
                         time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )

tas.mean <- udConvertGrid(tas.mean0, new.units = "degC")

spatialPlot(climatology(tas.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
            rev.colors = TRUE, main = 'Temperatura', scales = list(draw = T))

# Velocidad de viento U
u10.mean <- loadGridData(dataset = era5,
                         var = "u10",
                         aggr.d = "mean",
                         #aggr.m = "mean",
                         lonLim = longitud,
                         latLim= latitud,
                         season= meses,
                         years = anhos,
                         time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )

spatialPlot(climatology(u10.mean), backdrop.theme = "countries", color.theme = "jet.colors",
            main = 'u10', scales = list(draw = T))

# Velocidad de viento V
v10.mean <- loadGridData(dataset = era5,
                         var = "v10",
                         aggr.d = "mean",
                         #aggr.m = "mean",
                         lonLim = longitud,
                         latLim= latitud,
                         season= meses,
                         years = anhos,
                         time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )

spatialPlot(climatology(v10.mean), backdrop.theme = "countries", color.theme = "jet.colors",
            main = 'v10', scales = list(draw = T))


# # Z ----
# 
# # Lectura de datos
# z.mean <- loadGridData(dataset = era5,
#                          var = "z",
#                          aggr.d = "mean",
#                          #aggr.m = "mean",
#                          lonLim = longitud,
#                          latLim= latitud,
#                          season= meses,
#                          years = anhos,
#                          time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
# # plot
# spatialPlot(climatology(z.mean), backdrop.theme = "countries", color.theme = "BrBG",
#             main = 'Orografia', scales = list(draw = T))

# fin ---




# stack ----

x <- makeMultiGrid(tas.mean, u10.mean, v10.mean, skip.temporal.check=TRUE)
#PC.x <- prinComp(x, v.exp = c(.9,.9,.9,.9), imputation = 'mean', keep.orig = FALSE)

# fin ---




# Calibracion ----

# Configuration of method M1 en Bedia et al. (2020)
vars <- c("t2m", "u10", "v10")#, "sp", "z")
folds <- list(2010, 2011)

# Datos presencia/ausencia de precipitacion
y <- binaryGrid(y, condition = "GE", # GE: greater or equal
                threshold = 1, # umbral para discriminar entre presencia y ausencia
                partial = TRUE)

y_bin <- binaryGrid(y, condition = "GE",
                    threshold = 1)

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

#aggr.pars <- list(FUN = "sum", na.rm = TRUE)
pred.M6 <- aggregateGrid(M6cv)#, aggr.m = aggr.pars)

obs <- aggregateGrid(y)#, aggr.m = aggr.pars)

temporalPlot(pred.M6, obs)

# fin ---




# Validation ----

R01.ratio <- valueMeasure(y, x = M6cv,
                          measure.code = "ratio",
                          index.code = "R01")$Measure

spatialPlot(R01.ratio, backdrop.theme = "countries")

# fin ---




# Futuro ---

M6.L <- list(local.predictors = list(n = 1,
                                     vars = vars))
# Standardization
x_scale <- scaleGrid(x, type = "standardize")

# Predictor config (M6-L method)
M6.L <- prepareData(x_scale, y)#, local.predictors = M6.L)

# SDS model training
model.M6L <- downscaleTrain(M6.L, 
                            method = "analogs",
                            n.analogs = 4)

# fin ---




# Lectura de datos periodo futuro ----

# pr CMIP6 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/pr/')

pr.sum.cmip6 <- loadGridData(dataset = 'pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
                             var = "pr",
                             aggr.d = "sum",
                             # aggr.m = "sum",
                             lonLim = longitud-c(1, -1),
                             latLim= latitud-c(1, -1), 
                             season= meses, 
                             years = anhos,
                             time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(pr.sum.cmip6)

# plot 
spatialPlot(climatology(pr.sum.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Precipitacion', scales = list(draw = T))


# tas CMIP6  
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/tas/')

# C4R.vocabulary()
tas.mean.cmip6 <- loadGridData(dataset = 'tas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
                               var = "tas",
                               aggr.d = "mean",
                               #aggr.m = "mean",
                               lonLim = longitud-c(1, -1),
                               latLim= latitud-c(1, -1), 
                               season= meses, 
                               years = anhos,
                               time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )


tas.mean.cmip6 <- udConvertGrid(tas.mean.cmip6, new.units = "celsius")
# plot 
spatialPlot(climatology(tas.mean.cmip6), backdrop.theme = "countries", color.theme = "RdYlBu", rev.colors = TRUE,
            main = 'temperatura', scales = list(draw = T))


# # psl
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/psl/')
# 
# psl.mean.cmip6 <- loadGridData(dataset = 'psl_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
#                                var = "psl",
#                                aggr.d = "mean",
#                                #aggr.m = "mean",
#                                lonLim = longitud-c(1, -1),
#                                latLim= latitud-c(1, -1), 
#                                season= meses, 
#                                years = anhos,
#                                time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
# 
# # plot 
# spatialPlot(climatology(psl.mean.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = '', scales = list(draw = T))


# uas CMIP6  
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/uas/')

# C4R.vocabulary()
uas.mean.cmip6 <- loadGridData(dataset = 'uas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc',
                               var = "uas",
                               aggr.d = "mean",
                               #aggr.m = "mean",
                               lonLim = longitud-c(1, -1),
                               latLim= latitud-c(1, -1),
                               season= meses,
                               years = anhos,
                               time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(uas.mean.cmip6)

# plot
spatialPlot(climatology(uas.mean.cmip6), backdrop.theme = "countries", color.theme = "BrBG",
            main = 'uas', scales = list(draw = T))


# vas CMIP6 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/vas/')

# C4R.vocabulary()
vas.mean.cmip6 <- loadGridData(dataset = 'vas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc',
                               var = "vas",
                               aggr.d = "mean",
                               #aggr.m = "mean",
                               lonLim = longitud-c(1, -1),
                               latLim= latitud-c(1, -1),
                               season= meses,
                               years = anhos,
                               time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(vas.mean.cmip6)

# plot
spatialPlot(climatology(vas.mean.cmip6), backdrop.theme = "countries", color.theme = "BrBG",
            main = 'vas', scales = list(draw = T))

# fin ---





# Interpolacion periodo futuro ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/pr/')
inventario.cmip6 <- dataInventory('pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc')

# Precipitacion CMIP6

redondeo.i <- 3
resolucion.latitud.cmip6 <- calculo_de_resolucion(inventario.cmip6$pr, latitud = TRUE, redondeo = redondeo.i) ; resolucion.latitud.cmip6
resolucion.longitud.cmip6 <- calculo_de_resolucion(inventario.cmip6$pr, latitud = FALSE, redondeo = redondeo.i) ; resolucion.longitud.cmip6

resolucion.latitud.era5 <- calculo_de_resolucion(inventario.era5$tp, latitud = TRUE, redondeo = redondeo.i) ; resolucion.latitud.era5
resolucion.longitud.era5 <- calculo_de_resolucion(inventario.era5$tp, latitud = FALSE, redondeo = redondeo.i) ; resolucion.longitud.era5

# Precipitacion
pr.sum.cmip6.interpolado <- interpGrid(pr.sum.cmip6, 
                                       new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                              y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                       method = "bilinear",
                                       bilin.method = "fields")

spatialPlot(climatology(pr.sum.cmip6.interpolado, list(FUN = sum, na.rm = T)), backdrop.theme = "countries", 
            main = 'Precipitacion interpolada', scales = list(draw = T))

# Temperatura
tas.mean.cmip6.interpolado <- interpGrid(tas.mean.cmip6, 
                                       new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                              y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                       method = "bilinear",
                                       bilin.method = "fields")

spatialPlot(climatology(tas.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", 
            main = 'Temperatura interpolada', scales = list(draw = T))


# uas
uas.mean.cmip6.interpolado <- interpGrid(uas.mean.cmip6, 
                                         new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                                y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                         method = "bilinear",
                                         bilin.method = "fields")

spatialPlot(climatology(uas.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", 
            main = 'uas interpolada', scales = list(draw = T))


# vas
vas.mean.cmip6.interpolado <- interpGrid(vas.mean.cmip6, 
                                         new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                                y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                         method = "bilinear",
                                         bilin.method = "fields")

spatialPlot(climatology(vas.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", 
            main = 'vas interpolada', scales = list(draw = T))


# # psl
# psl.mean.cmip6.interpolado <- interpGrid(psl.mean.cmip6, 
#                                          new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
#                                                                 y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
#                                          method = "bilinear",
#                                          bilin.method = "fields")
# 
# spatialPlot(climatology(psl.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", 
#             main = 'psl interpolada', scales = list(draw = T))
# # psl a ps
# C4R.vocabulary()
# 
# getGridUnits(z.mean)
# 
# climate4R.UDG::loginUDG(username = "fmanquehual", password = "Francisco&Katherine=2")
# ej <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/ncepReanalysis1/ncepReanalysis1_4xDaily.ncml", var = 'zgs', 
#                    years = anhos, season = meses, lonLim = longitud, latLim = latitud, time = "DD")
# 
# z.mean.cmip6 <- udConvertGrid(z.mean, new.units = "m")
# ps.mean.cmip6.interpolado <- psl2ps(psl = psl.mean.cmip6.interpolado, tas = tas.mean.cmip6.interpolado, zgs = z.mean)


# fin ---



# ERA 5 Y CMIP6 CSIRO, TIENEN DIFERENTES DIMENSIONES, POR ESO NO CORRE LO DE ABAJO!
# PROBAR CON EJEMPLO DE OTRO PAPER

xh <- makeMultiGrid(tas.mean.cmip6.interpolado, uas.mean.cmip6.interpolado, vas.mean.cmip6.interpolado)

xh <- interpGrid(xh, new.coordinates = getGrid(xh)) # quedaste aqui, prueba con lineas de codigo que estan arriba

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
