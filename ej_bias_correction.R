# ocupando el FTP de CEDA, ve a: /badc/cmip6/data/CMIP6/CMIP/
# para ver datos historicos, y: /badc/cmip6/data/CMIP6/ScenarioMIP
# para datos del futuro

library(loadeR)
library(visualizeR)
library(downscaleR)
library(ncdf4)
# library(devtools)

# install_github(c(#"SantanderMetGroup/loadeR",
#                  #"SantanderMetGroup/transformeR",
#                  "SantanderMetGroup/visualizeR",
#                  "SantanderMetGroup/downscaleR"))

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CR2/')


# filtro de datos ----
meses <- 1:12
anhos <- 2010:2011

# latitud <- c(-49, -36)
# longitud <- c(-75, -71)

latitud <- c(-46, -48)
longitud <- c(-74, -71)

# fin ---




# tmin ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CR2/')

# Lectura de datos
tmin <- 'CR2MET_tmin_v2.0_mon_1979_2018_005deg.nc'

ej <- nc_open(tmin)
ej

C4R.vocabulary()
tmin.mean <- loadGridData(dataset = tmin, 
                          var = "tmin",
                          aggr.m = "mean",
                          lonLim = longitud,
                          latLim= latitud, 
                          season= meses, 
                          years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(tmin.mean)

# plot 
spatialPlot(climatology(tmin.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
            rev.colors = TRUE, main = tmin)

# fin ---




# tmax ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CR2/')

# Lectura de datos
tmax <- 'CR2MET_tmax_v2.0_mon_1979_2018_005deg.nc'

ej <- nc_open(tmax)
ej

C4R.vocabulary()
tmax.mean <- loadGridData(dataset = tmax, 
                         var = "tmax",
                         aggr.m = "mean",
                         lonLim = longitud,
                         latLim= latitud, 
                         season= meses, 
                         years = anhos) # obtain daily (aggr.d) or monthly (aggr.m) data )
str(tmax.mean)

# plot 
spatialPlot(climatology(tmax.mean), backdrop.theme = "countries", color.theme = "RdYlBu",
            rev.colors = TRUE, main = tmax)

# fin ---





# pr ----
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CR2/')

# Lectura de datos
pr <- 'CR2MET_pr_v2.0_mon_1979_2018_005deg.nc'

ej <- nc_open(pr)
ej

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

predictores <- makeMultiGrid(pr.sum, tmax.mean)

# calculating predictors
data <- prepareData(x = predictores, y = tmin.mean) 

# Fitting statistical downscaling methods (simple case, no cross-validation)
modelo.analog <- downscaleTrain(data, method = "analogs", n.analogs = 1)
modelo.regression <- downscaleTrain(data, method = "GLM",family = gaussian)
modelo.neuralnet <- downscaleTrain(data, method = "NN", hidden = c(10,5), output = "linear")

# Extracting the results for a particula station (Igueldo) for a single year (2000)
referencia <- subsetGrid(tmin.mean, years = 2010:2011)
analog <- subsetGrid(modelo.analog$pred, years = 2010:2011)
regression <- subsetGrid(modelo.regression$pred, years = 2010:2011)
neuralnet <- subsetGrid(modelo.neuralnet$pred, years = 2010:2011)

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

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')

# f <- "EUR44.*EC-EARTH.*RCA*RCP85.*RCA4" # original
f <- "downscaled_WRF_referencia.asc"
# fut <- UDG.datasets(pattern = f, full.info = TRUE)$CORDEX
rcp85.tx <- loadGridData(f, var = "pr",
                         years = 2071:2100, season = seas,
                         lonLim = lon, latLim = lat)
loadStationData(f)

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
setwd('C:/Users/Usuario/')
download.file("http://meteo.unican.es/work/loadeR/data/VALUE_ECA_86_v2.tar.gz", 
              destfile = 'Downloads/ej.gz')

# Data inventory
setwd('C:/Users/Usuario/Downloads/')
value <- "ej"

stationInfo(value)
example <- loadStationData(dataset = value, 
                           var = "tmax", 
                           stationID = c("000234", "003946"), 
                           season = 6:8,
                           years = 1981:2000)