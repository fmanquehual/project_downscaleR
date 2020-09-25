library(loadeR)
library(visualizeR)
library(downscaleR)
library(climate4R.climdex)
library(climate4R.value)
library(transformeR)

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

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA_LAND/')
era5 <- 'ERA_LAND_2000_2011.nc'

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

# Presion superficial
ps.mean <- loadGridData(dataset = era5, 
                        var = "sp",
                        aggr.d = "mean",
                        #aggr.m = "mean",
                        lonLim = longitud,
                        latLim= latitud, 
                        season= meses, 
                        years = anhos,
                        time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )

spatialPlot(climatology(ps.mean), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Presion superficial', scales = list(draw = T))

# Temperatura
tas.mean <- loadGridData(dataset = era5, 
                         var = "t2m",
                         aggr.d = "mean",
                         #aggr.m = "mean",
                         lonLim = longitud,#+c(-1, +1),
                         latLim= latitud, 
                         season= meses, 
                         years = anhos,
                         time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )

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


# Predictors (CMIP6)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_CMIP6/CSIRO/ACCESS-ESM1-5/historical/r1i1p1f1/day/pr/')
cmip6 <- 'pr_day_ACCESS-ESM1-5_historical_r1i1p1f1_gn_20000101-20141231.nc'

inventario.cmip6 <- dataInventory(cmip6)

pr.sum.cmip6 <- loadGridData(dataset = cmip6, 
                             var = "pr",
                             aggr.d = "sum",
                             #aggr.m = "sum",
                             lonLim = longitud-c(1, -1),
                             latLim= latitud-c(1, -1), 
                             season= meses, 
                             years = anhos,
                             time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
str(pr.sum.cmip6)

spatialPlot(climatology(pr.sum.cmip6), backdrop.theme = "countries", color.theme = "YlGnBu",
            main = 'Precipitacion', scales = list(draw = T))

# fin ---




# Interpolacion ----

# Precipitacion CMIP6

redondeo.i <- 3
resolucion.latitud.cmip6 <- calculo_de_resolucion(inventario.cmip6$pr, latitud = TRUE, redondeo = redondeo.i) ; resolucion.latitud.cmip6
resolucion.longitud.cmip6 <- calculo_de_resolucion(inventario.cmip6$pr, latitud = FALSE, redondeo = redondeo.i) ; resolucion.longitud.cmip6

resolucion.latitud.era5 <- calculo_de_resolucion(inventario.era5$tp, latitud = TRUE, redondeo = redondeo.i) ; resolucion.latitud.era5
resolucion.longitud.era5 <- calculo_de_resolucion(inventario.era5$tp, latitud = FALSE, redondeo = redondeo.i) ; resolucion.longitud.era5

pr.sum.cmip6.interpolado <- interpGrid(pr.sum.cmip6, 
                   new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                          y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                   method = "bilinear",
                   bilin.method = "fields")

spatialPlot(climatology(pr.sum.cmip6.interpolado, list(FUN = sum, na.rm = T)), backdrop.theme = "countries", 
                main = 'Precipitacion interpolada', scales = list(draw = T))


# Presion superficial ERA-LAND
ps.mean.interpolado <- interpGrid(ps.mean, new.coordinates = getGrid(pr.sum.cmip6.interpolado),
                 method = "nearest", force.non.overlapping = TRUE)
spatialPlot(climatology(ps.mean.interpolado), backdrop.theme = "countries", scales = list(draw = T))

ps.mean.interpolado <- interpGrid(ps.mean, 
                                       new.coordinates = list(x = seq(longitud[1],longitud[2],resolucion.longitud.era5), 
                                                              y = seq(latitud[1],latitud[2],resolucion.latitud.era5)),
                                       method = "bilinear",
                                       bilin.method = "akima")
spatialPlot(climatology(ps.mean.interpolado), backdrop.theme = "countries", scales = list(draw = T))

# fin ---




# stack ----

x <- makeMultiGrid(ps.mean, tas.mean, u10.mean, v10.mean, skip.temporal.check=TRUE)
PC.x <- prinComp(x, v.exp = .90)

# fin ---




# Calibracion ----

# Configuration of method M1 en Bedia et al. (2020)
vars <- c("sp", "t2m", "u10", "v10")#, "z")
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