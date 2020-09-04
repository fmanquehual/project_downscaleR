# ocupando el FTP de CEDA, ve a: /badc/cmip6/data/CMIP6/CMIP/
# para ver datos historicos, y: /badc/cmip6/data/CMIP6/ScenarioMIP
# para datos del futuro

library(ncdf4)
library(loadeR)
library(visualizeR)
library(downscaleR)


# filtro de datos ----
meses <- 1:12
anhos <- 2010
latitud <- c(-36,-49)
longitud <- c(-75, -72)

# fin ---


# DESCARGA CON OTRA INSTITUCION (NO CSIRO) PQ TIENES PROBLEMAS CON ESPECIFICAR EL NIVEL (TIENE MUCHOS DECIMALES AL PARECER Y LA FUNCION NO LOS MUESTRA)

# ps ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_era5/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/6hrLev/ps/')

# Lectura de datos
ps <- 'ps_6hrLev_ACCESS-ESM1-5_historical_r1i1p1f1_gn_201001010600-201501010000.nc'

# ej <- nc_open(ps)
# ej

di <- dataInventory(ps)
str(di)

C4R.vocabulary()
ps.mean <- loadGridData(dataset = ps, 
                   var = "ps",
                   aggr.d = "mean",
                   lonLim = longitud,
                   latLim= latitud, 
                   season= meses, 
                   years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(ps.mean)

# plot 
spatialPlot(climatology(ps.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = ps)

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




# tas ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_era5/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/6hrLev/ta/')

# Lectura de datos
ta <- 'ta_6hrLev_ACCESS-ESM1-5_historical_r1i1p1f1_gn_201001010600-201101010000.nc'

C4R.vocabulary()
tas.mean <- loadGridData(dataset = ta, 
                         var = "ta@500.000591707585",
                         aggr.d = "mean",
                         lonLim = longitud,
                         latLim= latitud, 
                         season= meses, 
                         years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(tas.mean)

# plot 
spatialPlot(climatology(tas.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
            rev.colors = TRUE, main = tas)

# fin ---




# ta ----

# Lectura de datos
ta <- 'ta_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20100101-20141231.nc'

C4R.vocabulary()
ta.mean <- loadGridData(dataset = ta, 
                         var = "ta@1000.00000001",
                         aggr.m = "mean",
                        lonLim = longitud+c(-1, +1),
                        latLim= latitud, 
                        season= meses, 
                        years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(ta.mean)

# plot 
spatialPlot(climatology(ta.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
            rev.colors = TRUE, main = ta)

# fin ---




# pr ----

# Lectura de datos
pr <- 'pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc'

C4R.vocabulary()
pr.sum <- loadGridData(dataset = pr, 
                        var = "pr",
                        aggr.m = "sum",
                       lonLim = longitud,
                       latLim= latitud, 
                       season= meses, 
                       years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(pr.sum)

# plot 
spatialPlot(climatology(pr.sum), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = pr)

# fin ---




# Predictores ----

predictores <- makeMultiGrid(hus.mean, ta.mean)

# calculating predictors
data <- prepareData(x = predictores, y = tas.mean) 

# Fitting statistical downscaling methods (simple case, no cross-validation)
modelo.analog <- downscaleTrain(data, method = "analogs", n.analogs = 1)
modelo.regression <- downscaleTrain(data, method = "GLM",family = gaussian)
modelo.neuralnet <- downscaleTrain(data, method = "NN", hidden = c(10,5), output = "linear")

# Extracting the results for a particula station (Igueldo) for a single year (2000)
referencia <- subsetGrid(tas.mean, years = 2010:2014)
analog <- subsetGrid(modelo.analog$pred, years = 2010:2014)
regression <- subsetGrid(modelo.regression$pred, years = 2010:2014)
neuralnet <- subsetGrid(modelo.neuralnet$pred, years = 2010:2014)

# plot
temporalPlot(referencia, analog, regression, neuralnet, lty = c(1,1,2,1), lwd = c(4, 2, 2, 2), 
             cols = c('black', 'green', 'red', 'blue'))
temporalPlot(referencia, analog, neuralnet)
temporalPlot(referencia, regression, lty = c(1,2), lwd = c(4, 2))

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

# f <- "EUR44.*EC-EARTH.*RCA*RCP85.*RCA4" # original
f <- "EUR44.*EC-EARTH.*RCP85.*RCA4"
fut <- UDG.datasets(pattern = f, full.info = TRUE)$CORDEX
rcp85.tx <- loadGridData(fut$name[1], var = "tasmax",
                         years = 2071:2100, season = seas,
                         lonLim = lon, latLim = lat)

rcp85.su <- climdexGrid(tx = rcp85.tx, index.code = "SU")
rcp85.SU <- interpGrid(rcp85.su, getGrid(obs.SU))

rcp85.bc.tx <- biasCorrection(y = obs.tx, x = rcm.tx,
                              newdata = rcp85.tx, method = "eqm")

rcp85.bc.SU <- climdexGrid(tx = rcp85.bc.tx , index.code = "SU")
temporalPlot("E-OBS" = obs.SU, "SU_hist" = rcm.SU,
             "SU_rcp85" = rcp85.SU, "Adjusted" = rcp85.bc.SU,
             latLim = 41.64, lonLim = -0.89,
             cols = c("black", "red", "red", "blue"))

# fin ---
