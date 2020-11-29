library(loadeR)
library(loadeR.2nc)
library(convertR)
library(raster)
library(ncdf4)
library(visualizeR)

rm(list=ls())
dev.off()


# Funciones ----

loadGridData_personalizado <- function(archivo.i, variable.i, es.precipitacion=FALSE, 
                                       es.cmip6=FALSE, anhos=anhos.entrenamiento){
  
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




# Parametros ----

anhos.entrenamiento <- 1995:2010 # con los años 2010:2011, del 1 al 3er mes, corre bien todo
anhos.total <- 1995:2017
meses <- 1:12
latitud <- c(-48.5, -45.5) # area de estudio WRF
longitud <- c(-74, -71) # area de estudio WRF
tz.i <- 'GMT'

# fin ---


# era5
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
era5 <- 'ERA5_1979_2018_pp.nc'

dataInventory(era5)
pr.sum0.entrenamiento <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, 
                                                    es.cmip6 = FALSE, anhos = anhos.entrenamiento)
pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")



# wrf
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
wrf <- 'RAINRATE_ERAint_Domain2_5a1_Conf10_2018.nc'

dataInventory(wrf)
pr.sum0.entrenamiento <- loadGridData_personalizado(wrf, "RAINRATE", es.precipitacion = TRUE, 
                                                    es.cmip6 = FALSE, anhos = anhos.entrenamiento)
# pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")

raster.i <- stack('RAINRATE_ERAint_Domain2_5a1_Conf10_2018.nc', varname='RAINRATE')
plot(raster.i, 30)


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
era5.nc <- nc_open(era5)
era5.nc

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
wrf.nc <- nc_open(wrf)
wrf.nc

# lan lon time

matriz.longitud <- ncvar_get(wrf.nc,"XLONG")[,,1] # aqui lo simplifique!
longitud.inicio <- matriz.longitud[1,1]
longitud.fin <- matriz.longitud[nrow(matriz.longitud), ncol(matriz.longitud)]
resX <- (longitud.fin-longitud.inicio)/(dim(matriz.longitud)[2]-1)

vector.longitud <- seq(longitud.inicio, longitud.fin, by = resX)
vector.longitud

# lon <- ncvar_get(wrf.nc,"XLONG")[,,1] # original
# nlon <- dim(lon)[1]*dim(lon)[2] ; nlon # original

matriz.latitud <- ncvar_get(wrf.nc,"XLAT")[,,1]
latitud.inicio <- matriz.latitud[1,1]
latitud.fin <- matriz.latitud[nrow(matriz.latitud), ncol(matriz.latitud)]
resY <- (latitud.fin-latitud.inicio)/(dim(matriz.latitud)[2]-1)

vector.latitud <- seq(latitud.inicio, latitud.fin, by = resY)
vector.latitud

# lat <- ncvar_get(wrf.nc,"XLAT")[,,1]
# nlat <- dim(lat)[1]*dim(lat)[2] ; nlat

# nlon*nlat

time0 <- ncvar_get(wrf.nc,"Times")
time00 <- gsub('_', ' ', time0)
time000 <- strptime(time00, "%Y-%m-%d %H:%M:%S", tz='GMT')

library(chron)
fechas0 <- stringi::stri_split_fixed(time00, ' ', simplify = TRUE)[,1]
fechas <- gsub('-', '/', fechas0)
fechas <- as.Date(fechas, "%Y/%m/%d")
fechas <- format(fechas, "%m/%d/%y")

tiempo <- times(stringi::stri_split_fixed(time00, ' ', simplify = TRUE)[,2])
# time <- as.array(as.numeric(julian(time000)))
# time <- as.numeric(julian(time000,  origin = as.POSIXct("1970-01-01", tz = "GMT")))
# time <- floor(as.numeric(julian(time000, origin = as.Date("1970-01-01")))) # from a date-time
# tiempo2 <- as.array(chron(dates. = fechas, times. = tiempo))
tiempo3 <- 1:length(time000)
  
# dimensiones
dim.lon <- ncdim_def('longitude', units='degrees_east', longname='Longitud E',
                     vals=as.double(vector.longitud))
dim.lat <- ncdim_def('latitude', units='degrees_north', longname='Latitud N',
                     vals=as.double(vector.latitud))
dim.time <- ncdim_def('time', units='days since 2018-01-01 00:00:00.0 -0:00', 
                      longname = 'Tiempo', vals=tiempo3)

# variable
variable <- ncvar_get(wrf.nc,"RAINRATE")
nvariable <- dim(variable)[1]*dim(variable)[2] ; nvariable

fillvalue <- -32767
def.tp <- ncvar_def(name='tp',units='mm h^-1', dim=list(dim.lon,dim.lat,dim.time),
                    missval=fillvalue,prec="float")

# creando netcdf
# nc_close(wrf.nc.output)
wrf.nc.output <- nc_create('WRF_ERAint_Domain2_5a1_Conf10_2018.nc',def.tp,
                           force_v4=TRUE, verbose=TRUE)


# anhadiendo valores
# transformacion de unidades, fuente : http://extraconversion.com/es/velocidad/milimetros-segundo/milimetros-segundo-a-milimetros-minuto.html

for (i in 1:dim(variable)[3]) {
  # i <- 30
  valores.variable0 <- as.double(variable[,,i])
  valores.variable <- valores.variable0*3600 # mm/s a mm/h
    
  ncvar_put(wrf.nc.output, def.tp, valores.variable, start=c(1,1,i), count=c(-1,-1,1) )
  
  # "-1" in the count means "the whole dimension length"
  
  mensaje <- paste('Iteracion', i, 'lista de', dim(variable)[3])
  message(mensaje)
}

nc_close(wrf.nc.output)

###

# Probando netcdf probado ---
wrf2 <- 'WRF_ERAint_Domain2_5a1_Conf10_2018.nc'
ej <- stack(wrf2)
plot(ej, 30)

dataInventory(wrf2)
ej2 <- loadGridData_personalizado(wrf2, "tp", es.precipitacion = TRUE, 
                                 es.cmip6 = FALSE, anhos = 2018)

spatialPlot(climatology(ej2), # list(FUN = sum, na.rm = T)),
            backdrop.theme = "coastline", scales = list(draw = T))
