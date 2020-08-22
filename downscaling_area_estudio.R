library(loadeR)
library(visualizeR)
library(downscaleR)
library(ncdf4)
library(raster)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CR2/')

# filtro de datos ----
meses <- 1:12
anhos <- 2010:2014
latitud <- c(-36,-49)
longitud <- c(-75, -72)

# fin ---




# Lectura nc ejemplo ----
list.files()
# untar(list.files()[8])

# Lectura de datos
archivos <- list.files(pattern = '.nc') ; archivos
variable <- archivos[1]

ej <- nc_open(variable)
ej

# ej2 <- raster(variable)
# plot(ej2, 1:2)

di <- dataInventory(variable)
names(di)

C4R.vocabulary()
variable.mean <- loadGridData(dataset = variable, 
                         var = "pr",
                         # aggr.m = "mean")
                         # lonLim = longitud,
                         # latLim= latitud, 
                         season= meses,
                         years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(variable.mean)

# plot 
spatialPlot(climatology(variable.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = variable, zcol = 1:4)

# fin ---



# hus ----

# Lectura de datos
hus <- 'hus_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20100101-20141231.nc'

C4R.vocabulary()
hus.mean <- loadGridData(dataset = hus, 
                         var = "hus@1000.00000001",
                         aggr.m = "mean",
                         lonLim = longitud+c(-1, +1),
                         latLim= latitud, 
                         season= meses, 
                         years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(hus.mean)

# plot 
spatialPlot(climatology(hus.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = hus)

# fin ---




# tas ----

# Lectura de datos
tas <- 'tas_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc'

C4R.vocabulary()
tas.mean <- loadGridData(dataset = tas, 
                         var = "tas",
                         aggr.m = "mean",
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
