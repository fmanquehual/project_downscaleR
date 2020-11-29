library(loadeR)
library(loadeR.2nc)
library(convertR)
library(visualizeR)
library(raster)
library(ncdf4)

rm(list=ls())
dev.off()


# Funciones ----

loadGridData_personalizado <- function(archivo.i, variable.i, es.precipitacion=FALSE, 
                                       es.cmip6=FALSE, anhos=anhos.entrenamiento){
  
  if(es.precipitacion==TRUE){estadistico.i <- 'sum'} else(estadistico.i <- 'mean')
  if(es.cmip6==TRUE){extension.i <- c(1, -1)} else(extension.i <- c(0, 0))
  
  grilla.de.salida <- loadGridData(dataset = archivo.i, 
                                   var = variable.i,
                                   #aggr.d = estadistico.i,
                                   #aggr.m = "mean",
                                   lonLim = longitud-extension.i,
                                   latLim= latitud-extension.i, 
                                   season= meses, 
                                   years = anhos)#,
                                   #time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data )
  return(grilla.de.salida)
}

# fin ---




# Parametros ----

anhos.entrenamiento <- 1995:2010 # con los años 2010:2011, del 1 al 3er mes, corre bien todo
anhos.total <- 1995:2017
meses <- 1:12
latitud <- c(-48.5, -45.5) # area de estudio WRF
longitud <- c(-74, -71) # area de estudio WRF
tz.i <- 'GMT'

# fin ---




# Lectura de archivos ----
# era5
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
era5 <- 'ERA5_1979_2018_pp.nc'
  
dataInventory(era5)
pr.sum0.entrenamiento <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, 
                                                    es.cmip6 = FALSE, anhos = anhos.entrenamiento)
# pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")


# wrf
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
wrf <- 'RAINRATE_ERAint_Domain2_5a1_Conf10_2018_para_ejemplos.nc'
wrf.nc <- nc_open(wrf)

# fin ---




# Variable ----

pr.sum0.entrenamiento$Variable

nombre.variable <- 'pr' # 'RAINRATE'
unidad0 <- unlist(ncatt_get(wrf.nc, nombre.variable,"units"))
unidad <- as.character(unidad0[length(unidad0)])

descripcion0 <- unlist(ncatt_get(wrf.nc, nombre.variable,"description"))
descripcion <- as.character(descripcion0[length(descripcion0)])

variable <- list(varName= nombre.variable, level=NULL)
attr(variable, 'use_dictionary') <- FALSE
attr(variable, 'description') <- descripcion
attr(variable, 'units') <- unidad
attr(variable, 'longname') <- nombre.variable
attr(variable, 'daily_agg_cellfun') <- 'none'
attr(variable, 'monthly_agg_cellfun') <- 'none'
attr(variable, 'verification_time') <- 'none'

# fin ---




# Datos ----

pr.sum0.entrenamiento$Data
dim(pr.sum0.entrenamiento$Data)

array.datos <- ncvar_get(wrf.nc, nombre.variable)
dim(array.datos)

array.climate4r <- as.array(array.datos)
# array.climate4r <- array(array.datos[,,30:33], c(#1:dim(array.datos)[3]], c(
#   1, # dim(array.datos)[3],
#   dim(array.datos)[1], 
#   dim(array.datos)[2]))

# attr(array.climate4r, "dimensions") <- c('time', 'lon', 'lat')
attr(array.climate4r, "dimensions") <- c('lon', 'lat', 'time')

head(array.climate4r)
dim(array.climate4r)

# fin ---




# Tiempo ----

pr.sum0.entrenamiento$Dates

time0 <- ncvar_get(wrf.nc,"Times")
time00 <- gsub('_', ' ', time0)
time <- strptime(time00, "%Y-%m-%d %H:%M:%S", tz='GMT')

time.start <- time
time.end <- time[2:length(time)]
time.end <- c(time.end, time[length(time)]+1)
time.climate4r <- list(start=time.start, end=time.start)

# fin ---




# Longitud ----

pr.sum0.entrenamiento$xyCoords$x

# matriz.longitud <- ncvar_get(wrf.nc,"lon")[,,1] # el original me da problemas
# vector.longitud <- as.numeric(matriz.longitud)

matriz.longitud <- ncvar_get(wrf.nc,"lon")[,,1] # aqui lo simplifique!
longitud.inicio <- matriz.longitud[1,1]
longitud.fin <- matriz.longitud[nrow(matriz.longitud), ncol(matriz.longitud)]
resX <- (longitud.fin-longitud.inicio)/(dim(matriz.longitud)[2]-1)

vector.longitud <- seq(longitud.inicio, longitud.fin, by = resX)
vector.longitud

# fin ---




# Latitud ----

pr.sum0.entrenamiento$xyCoords$y

# matriz.latitud <- ncvar_get(wrf.nc,"lat")[,,1]
# vector.latitud <- as.numeric(matriz.latitud)

matriz.latitud <- ncvar_get(wrf.nc,"lat")[,,1]
latitud.inicio <- matriz.latitud[1,1]
latitud.fin <- matriz.latitud[nrow(matriz.latitud), ncol(matriz.latitud)]
resY <- (latitud.fin-latitud.inicio)/(dim(matriz.latitud)[2]-1)

vector.latitud <- seq(latitud.inicio, latitud.fin, by = resY)
vector.latitud

# fin ---




# Coordenadas ----

coordenadas.climate4r <- list(x=vector.longitud, y=vector.latitud)

# resX <- abs(vector.longitud[1]-vector.longitud[2]) # varia!
# resY <- abs(vector.latitud[1]-vector.latitud[2]) # varia!

attr(coordenadas.climate4r, 'projection') <- 'LatLonProjection'
attr(coordenadas.climate4r, 'resX') <- resX
attr(coordenadas.climate4r, 'resY') <- resY

# fin ---




# Formato climate4r ----

archivo.climate4r <- list(Variable=variable, Data=array.climate4r, 
                          xyCoords=coordenadas.climate4r,
                          Dates=time.climate4r)

attr(archivo.climate4r, 'dataset') <- wrf

# fin ---




# Plot ----

spatialPlot(climatology(archivo.climate4r), # list(FUN = sum, na.rm = T)),
            backdrop.theme = "coastline", scales = list(draw = T))

pr.sum.total <- udConvertGrid(archivo.climate4r, new.units = "mm") 

# archivo.diario <- aggregateGrid(archivo.climate4r, max.ncores = 6,
#                                 aggr.d = list(FUN = sum, na.rm = TRUE))
# spatialPlot(climatology(archivo.diario), # list(FUN = mean, na.rm = T)),
#             backdrop.theme = "coastline", scales = list(draw = T))

# fin ---