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
source('funcion_identificador_de_fechas.R')
source('funcion_reemplazador_de_fechas_NA.R')
source('funcion_calculo_de_resolucion.R')

# Filtro de datos ----

# HAY PROBLEMAS CON EL MES 10, 11 Y 12!
# EL PROBLEMA SE MANTIENE SI OCUPO DE 1:9 PARA 2010:2011

# UNA ALTERNATIVA ES APLICAR (PERO PRIMERO PROBAR!) EL BIASCORRECTION DE MANERA MENSUAL ...
# Y HACER QUE LOS MESES 10:12 LOS HAGA PASAR POR LOS PRIMEROS MESES

meses <- 1 # con el año 2010, del 1 al 9no mes, corre bien todo
anhos <- 2011:2017#1979:2018 # con los años 2010:2011, del 1 al 3er mes, corre bien todo
# latitud <- c(-49,-36) # area de estudio CCR
# longitud <- c(-75, -72) # area de estudio CCR
# latitud <- c(-48, -46) # area de estudio WRF
# longitud <- c(-74, -71) # area de estudio WRF
latitud <- c(-47.3, -47)
longitud <- c(-73.3, -73)

# fin ---




# Funciones ----

loadGridData_personalizado <- function(archivo.i, variable.i, es.precipitacion=FALSE, es.cmip6=FALSE){
  
  if(es.precipitacion==TRUE){estadistico.i <- 'sum'} else(estadistico.i <- 'mean')
  if(es.cmip6==TRUE){extension.i <- c(1, -1)} else(extension.i <- c(0, 0))
  
  grilla.de.salida <- loadGridData(dataset = archivo.i, 
                                   var = variable.i,
                                   aggr.d = estadistico.i,
                                   #aggr.m = "mean",
                                   lonLim = longitud-extension.i,
                                   latLim= latitud-extension.i, 
                                   season= meses, 
                                   years = anhos,
                                   time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
  return(grilla.de.salida)
}

# fin ---




# Lectura de datos ----

# Estaciones metereologicas

# Datos observados grillados, pueden escontrarse en:
# 1. https://catalogue.ceda.ac.uk/uuid/58a8802721c94c66ae45c3baa4d814d0
# 2. CR2

nombre.carpeta <- 'datos_transformados_a_ASCII'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
estaciones <- stationInfo(nombre.carpeta) ; estaciones

estaciones <- loadStationData(dataset = nombre.carpeta, 
                     var="precip", 
                     units = 'mm',
                     #stationID = estaciones$stationID[1],
                     years = anhos,
                     season = meses)#,
                     # tz='GMT')

temporalPlot(estaciones, aggr.spatial = list(FUN = mean, na.rm = TRUE))

# y <- reemplazador_de_fechas_NA(y, tz=NULL) # activar si las fechas tienen NA
# temporalPlot(y, aggr.spatial = list(FUN = sum, na.rm = TRUE))
# 
# y$Dates$start <- as.POSIXct(y$Dates$start, format="%Y-%m-%d",
#                             origin = "1970-01-01", tz = '') # as.POSIXlt(y$Dates$start, 'GMT')
# y$Dates$end <- as.POSIXct(y$Dates$end, format="%Y-%m-%d",
#                           origin = "1970-01-01", tz = '') # as.POSIXlt(y$Dates$end, 'GMT')


# Predictors (ERA reanalisis) 

#setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA_LAND/') # Tiene NA por el oceano, lo que genera problemas
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
era5 <- 'ERA5_1979_2018_pp.nc'

inventario.era5 <- dataInventory(era5)
inventario.era5$tp$Dimensions$time$Date_range

# Precipitacion
# C4R.vocabulary()
pr.sum0 <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, es.cmip6 = FALSE)
pr.sum <- udConvertGrid(pr.sum0, new.units = "mm")

spatialPlot(climatology(pr.sum, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Precipitacion', scales = list(draw = T))

# Temperatura
# tas.mean0 <- loadGridData_personalizado(era5, "t2m", es.precipitacion = FALSE, es.cmip6 = FALSE)
# tas.mean <- udConvertGrid(tas.mean0, new.units = "degC")

# spatialPlot(climatology(tas.mean, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "RdYlBu",
#             rev.colors = TRUE, main = 'Temperatura', scales = list(draw = T))


# Velocidad de viento U
# u10.mean <- loadGridData_personalizado(era5, "u10", es.precipitacion = FALSE, es.cmip6 = FALSE)

# spatialPlot(climatology(u10.mean, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "jet.colors",
#             main = 'u10', scales = list(draw = T))


# Velocidad de viento V
# v10.mean <- loadGridData_personalizado(era5, "v10", es.precipitacion = FALSE, es.cmip6 = FALSE)

# spatialPlot(climatology(v10.mean, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "jet.colors",
#             main = 'v10', scales = list(draw = T))


# # Presion superficial
# ps.mean <- loadGridData_personalizado(era5, "sp", es.precipitacion = FALSE, es.cmip6 = FALSE)
# 
# spatialPlot(climatology(ps.mean, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = 'Presion superficial', scales = list(draw = T))


# # Z 
# z.mean <- loadGridData_personalizado(era5, "z", es.precipitacion = FALSE, es.cmip6 = FALSE)
# 
# spatialPlot(climatology(z.mean, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "BrBG",
#             main = 'Orografia', scales = list(draw = T))

# fin ---




# stack ----

# x <- makeMultiGrid(tas.mean, u10.mean, v10.mean, skip.temporal.check=TRUE)

# fin ---




# Bias correction ----

# pr.sum.corregido.delta <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
#                                    y = estaciones,
#                                    precipitation = TRUE,
#                                    method = "delta")

# pr.sum.corregido.scaling <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
#                                    y = estaciones,
#                                    precipitation = TRUE,
#                                    method = "scaling",
#                                    scaling.type = "multiplicative")

pr.sum.corregido.eqm <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
                                   y = estaciones,
                                   precipitation = TRUE,
                                   method = "eqm",
                                   wet.threshold=0.01)

pr.sum.corregido.pqm <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
                                   y = estaciones,
                                   precipitation = TRUE,
                                   method = "pqm",
                                   wet.threshold=0.01)

# pr.sum.corregido5 <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
#                                    y = estaciones,
#                                    precipitation = TRUE,
#                                    method = "gpqm",
#                                    wet.threshold=0.01)

pr.sum.corregido.loci <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
                                    y = estaciones,
                                    precipitation = TRUE,
                                    method = "loci",
                                    wet.threshold=0.01)

pr.sum.corregido.ptr <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
                                    y = estaciones,
                                    precipitation = TRUE,
                                    method = "ptr",
                                    wet.threshold=0.01)

# pr.sum.corregido8 <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
#                                     y = estaciones,
#                                     precipitation = TRUE,
#                                     method = "dqm")#,
#                                     #wet.threshold=0.01)

# pr.sum.corregido9 <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
#                                     y = estaciones,
#                                     precipitation = TRUE,
#                                     method = "qdm",
#                                     wet.threshold=0.01)
            
#pr.sum.bias.correction2 <- interpGrid(pr.sum.bias.correction, getGrid(y)) # CORDEX historical

temporalPlot(estaciones, pr.sum.corregido.eqm, lwd = c(2,1), lty = c(1,2), aggr.spatial = list(FUN = mean, na.rm = TRUE))

spatialPlot(climatology(pr.sum.corregido.pqm, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", 
            main = 'Precipitacion', scales = list(draw = T))

temporalPlot(pr.sum, aggr.spatial = list(FUN = mean, na.rm = TRUE))

pr.sum.puntos <- grid2sp(pr.sum)


# fin ---




# Calibracion ----

# Configuration of method M1 en Bedia et al. (2020)
vars <- c("t2m", "u10", "v10")#, "sp", "z")
folds <- list(2000:2010, 2011:2014)

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

M6cv$Data

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

M6.L <- list(local.predictors = list(n = 3,
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

# C4R.vocabulary()

# pr CMIP6 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/pr/')

pr.sum.cmip6 <- loadGridData_personalizado('pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc',
                                           "pr", es.precipitacion = TRUE, es.cmip6 = TRUE)

spatialPlot(climatology(pr.sum.cmip6, list(FUN=mean, na.rm=FALSE)), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Precipitacion', scales = list(draw = T))


# tas CMIP6  
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/tas/')

tas.mean.cmip6 <- loadGridData_personalizado('tas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc', 
                                             "tas", es.precipitacion = FALSE, es.cmip6 = TRUE)
tas.mean.cmip6 <- udConvertGrid(tas.mean.cmip6, new.units = "celsius")

spatialPlot(climatology(tas.mean.cmip6, list(FUN=mean, na.rm=FALSE)), backdrop.theme = "countries", color.theme = "RdYlBu", rev.colors = TRUE,
            main = 'temperatura', scales = list(draw = T))


# uas CMIP6  
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/uas/')

uas.mean.cmip6 <- loadGridData_personalizado('uas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc',
                                             "uas", es.precipitacion = FALSE, es.cmip6 = TRUE)

spatialPlot(climatology(uas.mean.cmip6, list(FUN=mean, na.rm=FALSE)), backdrop.theme = "countries", color.theme = "BrBG",
            main = 'uas', scales = list(draw = T))


# vas CMIP6 
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/vas/')

vas.mean.cmip6 <- loadGridData_personalizado('vas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc',
                                             "vas", es.precipitacion = FALSE, es.cmip6 = TRUE)

spatialPlot(climatology(vas.mean.cmip6, list(FUN=mean, na.rm=FALSE)), backdrop.theme = "countries", color.theme = "BrBG",
            main = 'vas', scales = list(draw = T))


# # psl
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/psl/')
# 
# psl.mean.cmip6 <- loadGridData_personalizado('psl_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc',
#                                              "psl", es.precipitacion = FALSE, es.cmip6 = TRUE)
# 
# spatialPlot(climatology(psl.mean.cmip6, list(FUN=mean, na.rm=FALSE)), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = '', scales = list(draw = T))

# fin ---





# Interpolacion periodo futuro ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/pr/')
inventario.cmip6 <- dataInventory('pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc')


# Calculo resolucion
redondeo.i <- 3

resolucion.latitud.cmip6 <- calculo_de_resolucion(inventario.cmip6$pr, latitud = TRUE, redondeo = redondeo.i) ; resolucion.latitud.cmip6
resolucion.longitud.cmip6 <- calculo_de_resolucion(inventario.cmip6$pr, latitud = FALSE, redondeo = redondeo.i) ; resolucion.longitud.cmip6

resolucion.latitud.era5 <- calculo_de_resolucion(inventario.era5$tp, latitud = TRUE, redondeo = redondeo.i) ; resolucion.latitud.era5
resolucion.longitud.era5 <- calculo_de_resolucion(inventario.era5$tp, latitud = FALSE, redondeo = redondeo.i) ; resolucion.longitud.era5


# Precipitacion CMIP6
pr.sum.cmip6.interpolado <- interpGrid(pr.sum.cmip6, 
                                       new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                              y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                       method = "bilinear",
                                       bilin.method = "fields")

spatialPlot(climatology(pr.sum.cmip6.interpolado, list(FUN = sum, na.rm = T)), backdrop.theme = "countries", color.theme = "YlGnBu",
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

spatialPlot(climatology(uas.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "BrBG",
            main = 'uas interpolada', scales = list(draw = T))


# vas
vas.mean.cmip6.interpolado <- interpGrid(vas.mean.cmip6, 
                                         new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                                y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                         method = "bilinear",
                                         bilin.method = "fields")

spatialPlot(climatology(vas.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "BrBG",
            main = 'vas interpolada', scales = list(draw = T))


# # psl
# psl.mean.cmip6.interpolado <- interpGrid(psl.mean.cmip6, 
#                                          new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
#                                                                 y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
#                                          method = "bilinear",
#                                          bilin.method = "fields")
# 
# spatialPlot(climatology(psl.mean.cmip6.interpolado, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", color.theme = "YlGnBu",
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




# ... ----
xh <- makeMultiGrid(tas.mean.cmip6.interpolado, uas.mean.cmip6.interpolado, vas.mean.cmip6.interpolado)

xh <- scaleGrid(xh, base = xh, ref = x,
                type = "center",
                spatial.frame = "gridbox",
                time.frame = "daily")

# xh <- scaleGrid(xh, base = x, type = "standardize", skip.season.check = TRUE)

h_analog <- prepareNewData(newdata = xh,
                           data.struc = M6.L)

f_analog <- prepareNewData(newdata = xf,
                           data.struc = M6.L)

hist_ocu_glm <- downscalePredict(newdata
                                 = h_analog,
                                 model = model.M6L)

# fin ---
