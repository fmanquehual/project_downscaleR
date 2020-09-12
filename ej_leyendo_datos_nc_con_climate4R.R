# ocupando el FTP de CEDA, ve a: /badc/cmip6/data/CMIP6/CMIP/
# para ver datos historicos, y: /badc/cmip6/data/CMIP6/ScenarioMIP
# para datos del futuro

library(ncdf4)
library(loadeR)
library(visualizeR)
library(downscaleR)
library(filesstrings) # para mover archivos entre carpetas
library(climate4R.climdex)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_identificador_de_variables_era5_CEDA.R')

# filtro de datos ----
meses <- 1:12
anhos <- 2010
latitud <- c(-49,-36)
longitud <- c(-75, -72)

# fin ---


### DESCARGA CON OTRA INSTITUCION (NO CSIRO) PQ TIENES PROBLEMAS CON ESPECIFICAR EL NIVEL (TIENE ...
### MUCHOS DECIMALES AL PARECER Y LA FUNCION NO LOS MUESTRA)

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/CEDA/2010/01/01/')
# variables <- identificador_de_variables_era5_CEDA(frase_inicial_no_numerica='ecmwf-era5_oper_an_sfc_',
#                                                   nombre_de_variables=TRUE,
#                                                   fecha_y_hora_de_datos=FALSE)
# variables
# variable.i <- '2d'
#   
# archivos.i <- list.files(pattern = variable.i)
# list.dirs()
# dir.create(variable.i)
# file.move(archivos.i, destinations = variable.i, overwrite = TRUE)

### Ahora que pudiste leer datos del ERA5, prueba con los de copernicus!




# ps ----

# Lectura de datos
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/6hrLev/ps')
# ps <- 'ps_6hrLev_ACCESS-ESM1-5_historical_r1i1p1f1_gn_201001010600-201501010000.nc'

# Lectura de datos
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
ERA5 <- 'ERA5_2010.nc'

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



# hus ----

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_era5/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/6hrLev/hus/')
# 
# # Lectura de datos
# hus <- 'hus_6hrLev_ACCESS-ESM1-5_historical_r1i1p1f1_gn_201001010600-201101010000.nc'
# 
# ej <- nc_open(hus)
# ej
# 
# variable <- ncvar_get(ej,'lev') #extracci?n valores variable
# variable[1]
# 
# C4R.vocabulary()
# hus.mean <- loadGridData(dataset = hus, 
#                         var = "hus@980.00085",
#                         aggr.d = "mean",
#                         lonLim = longitud,#+c(-1, +1),
#                         latLim= latitud, 
#                         season= meses, 
#                         years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
# str(hus.mean)
# 
# # plot 
# spatialPlot(climatology(hus.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
#             main = hus)

# fin ---




# # tas ----
# 
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_era5/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/6hrLev/ta/')
# 
# # Lectura de datos
# ta <- 'ta_6hrLev_ACCESS-ESM1-5_historical_r1i1p1f1_gn_201001010600-201101010000.nc'
# 
# 
# C4R.vocabulary()
# tas.mean <- loadGridData(dataset = ta, 
#                          var = "ta@500.000591707585",
#                          aggr.d = "mean",
#                          lonLim = longitud,
#                          latLim= latitud, 
#                          season= meses, 
#                          years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
# str(tas.mean)
# 
# # plot 
# spatialPlot(climatology(tas.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
#             rev.colors = TRUE, main = tas)
# 
# # fin ---




# ta ----

# Lectura de datos
# ta <- 'ta_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20100101-20141231.nc'

#C4R.vocabulary()
ta.mean <- loadGridData(dataset = ERA5, 
                        var = "t2m",
                        aggr.d = "mean",
                        aggr.m = "mean",
                        lonLim = longitud,#+c(-1, +1),
                        latLim= latitud, 
                        season= meses, 
                        years = anhos,
                        time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(ta.mean)

# plot 
spatialPlot(climatology(ta.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
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




# Predictores ----

predictores <- makeMultiGrid(ps.mean, ta.mean)#, skip.temporal.check=TRUE) # activalo si tienes datos horarios o diarios

# calculating predictors
data <- prepareData(x = predictores, y = pr.sum) 

# Fitting statistical downscaling methods (simple case, no cross-validation)
modelo.analog <- downscaleTrain(data, method = "analogs", n.analogs = 1)
# modelo.regression <- downscaleTrain(data, method = "GLM",family = gaussian)
# modelo.neuralnet <- downscaleTrain(data, method = "NN", output = "linear")

# Extracting the results for a particula station (Igueldo) for a single year (2000)
referencia <- subsetGrid(pr.sum, years = 2010)
analog <- subsetGrid(modelo.analog$pred, years = 2010)
# regression <- subsetGrid(modelo.regression$pred, years = 2010:2014)
# neuralnet <- subsetGrid(modelo.neuralnet$pred, years = 2010)

# plot
# temporalPlot(referencia, analog, regression, neuralnet, lty = c(1,1,2,1), lwd = c(4, 2, 2, 2), 
#              cols = c('black', 'green', 'red', 'blue'))
temporalPlot(referencia, analog)
# temporalPlot(referencia, regression, lty = c(1,2), lwd = c(4, 2))

spatialPlot(climatology(analog), backdrop.theme = "countries", color.theme = "RdYlBu",
            main = 'downscaled')

# prediccion (es lo mismo que modelo.analog$pred)
newdata <- prepareNewData(predictores,data)
pred <- downscalePredict(newdata, modelo.analog)

plot(analog$Data[,5],pred$Data[,5])

spatialPlot(climatology(analog), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Train')

spatialPlot(climatology(pred), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Predicted')

# fin ---



# Futuro ----

# Apuntes ERA 5 (para descargar datos!!!!): ERA 5 comes in two streams: the ‘oper’ stream is a single member 
# analysis at highest resolution, and the ‘enda’ stream is a 10-member ensemble at 
# lower resolution - both are supported (Fuente: https://brohan.org/IRData/subdata/data_era5.html).

# Finally (panel e), future projections from the same
# model are loaded from the RCP8.5 scenario (2071-2100)
# and bias adjusted using the biasCorrection function.

# datos observados


# presente
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/tasmax/')
archivo.j <- 'tasmax_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc'

dj <- dataInventory(archivo.j)
str(dj)
dj$tasmax

historical.tx <- loadGridData(archivo.j, 
                         var = "tasmax",
                         years = 2000:2014, 
                         aggr.d = "mean",
                         #aggr.m = "mean",
                         season = meses,
                         lonLim = longitud, 
                         latLim = latitud,
                         time = 'DD')

climdexShow() # para ver los indices disponibles
spatialPlot(climatology(historical.tx), backdrop.theme = "countries", color.theme = "RdYlBu")

historical.tx.su <- climdexGrid(tx = historical.tx, index.code = "TXx")
spatialPlot(climatology(historical.tx.su), backdrop.theme = "countries", color.theme = "RdYlBu")


# futuro
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/ssp585/r1i1p1f1/day/tasmax/')
archivo.i <- 'tasmax_day_ACCESS-ESM1-5_ssp585_r1i1p1f1_gn_20150101-20641231.nc'

di <- dataInventory(archivo.i)
str(di)
di$tasmax

rcp85.tx <- loadGridData(archivo.i, 
                         var = "tasmax",
                         years = 2015:2064, 
                         aggr.d = "mean",
                         #aggr.m = "mean",
                         season = meses,
                         lonLim = longitud, 
                         latLim = latitud,
                         time = 'DD')

spatialPlot(climatology(rcp85.tx), backdrop.theme = "countries", color.theme = "RdYlBu")

rcp85.tx.su <- climdexGrid(tx = rcp85.tx, index.code = "TXx")


# bias correction
rcp85.bc.tx <- biasCorrection(y = historical.tx, x = ...,
                              newdata = rcp85.tx, method = "eqm")

rcp85.bc.SU <- climdexGrid(tx = rcp85.bc.tx , index.code = "SU")
temporalPlot("E-OBS" = obs.SU, "SU_hist" = rcm.SU,
             "SU_rcp85" = rcp85.SU, "Adjusted" = rcp85.bc.SU,
             latLim = 41.64, lonLim = -0.89,
             cols = c("black", "red", "red", "blue"))

# fin ---
