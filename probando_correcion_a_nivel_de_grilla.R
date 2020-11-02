library(loadeR)
library(visualizeR)
library(downscaleR)
library(climate4R.climdex)
library(climate4R.value)
library(convertR)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_lista_climate4R_a_db.R')
source('funcion_grilla_a_db.R')
source('funcion_nuevas_fechas.R')
source('funcion_formato_datos_para_climate4R.R')
source('funcion_ordenar_por_fecha.R')
source('funcion_prt.R')


# Funciones ----

loadGridData_personalizado <- function(archivo.i, variable.i, es.precipitacion=FALSE, es.cmip6=FALSE, anhos=anhos.entrenamiento){
  
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

anhos.entrenamiento <- 1979:2010 # con los años 2010:2011, del 1 al 3er mes, corre bien todo
anhos.total <- 1979:2017
# latitud <- c(-49,-36) # area de estudio CCR
# longitud <- c(-75, -72) # area de estudio CCR
latitud <- c(-48.5, -45.5) # area de estudio WRF
longitud <- c(-74, -71) # area de estudio WRF
# latitud <- c(-47.3, -47)
# longitud <- c(-73.3, -73)
umbral <- 1 # The minimum value that is considered as a non-zero precipitation (default is 1 mm)
# Con el umbral=0, da problemas en gpqm (valores muy altos, al final la correcion empeora la estimacion de pp)
# Las correciones mejoran bastante cuando el umbral es 1 (default).

db.estaciones <- c()
db.eqm <- c()
db.pqm <- c()
db.gpqm <- c() # ver apunte del cuaderno!
db.loci <- c()
db.ptr <- c()
db.qdm <- c()
db.era5 <- c()

tz.i <- 'GMT'

# fin ---





# Correccion mensual ----

# for (i in 1:12) {
  i <- 1
  
  mensaje.inicio <- paste('Inicio de proceso con mes', i, '--------------------------------------------------------------')
  message(mensaje.inicio)
  
  meses <- i # con el año 2010, del 1 al 9no mes, corre bien todo
  
  
  
  # Lectura de datos ----
  
  message('Leyendo datos de estaciones')
  
  # Estaciones metereologicas
  
  nombre.carpeta <- 'datos_transformados_a_ASCII'
  
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
  # estaciones <- stationInfo(nombre.carpeta) ; estaciones
  
  
  estaciones.con.todos.los.anhos <- loadStationData(dataset = nombre.carpeta, 
                                                    var="precip", 
                                                    #units = 'mm',
                                                    #stationID = estaciones$stationID[1],
                                                    years = anhos.total,
                                                    season = meses,
                                                    tz=tz.i)
  
  estaciones <- loadStationData(dataset = nombre.carpeta, 
                                var="precip", 
                                #units = 'mm',
                                #stationID = estaciones$stationID[1],
                                years = anhos.entrenamiento,
                                season = meses,
                                tz=tz.i)
  
  estaciones.original <- estaciones
  
  if(i>1){c(
    estaciones$Dates$start <- as.vector(unlist(nuevas_fechas(estaciones, entregar_fecha_inicio = TRUE, iteracion = i, tz=tz.i)[2])),
    estaciones$Dates$end <- as.vector(unlist(nuevas_fechas(estaciones, entregar_fecha_inicio = FALSE, iteracion = i, tz=tz.i)[2])) )}
  
  # temporalPlot(estaciones, aggr.spatial = list(FUN = mean, na.rm = TRUE))
  
  
  # Predictors (ERA reanalisis) 
  
  message('Leyendo datos de ERA5')
  
  #setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA_LAND/') # Tiene NA por el oceano, lo que genera problemas
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
  era5 <- 'ERA5_1979_2018_pp.nc'
  
  
  # Precipitacion
  # C4R.vocabulary()
  
  message('Transformando unidades de variable')
  
  # Datos de entrenamiento
  
  pr.sum0.entrenamiento <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, es.cmip6 = FALSE, anhos = anhos.entrenamiento)
  pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")
  
  if(i>1){c(
    # pr.sum.entrenamiento.original <- pr.sum.entrenamiento,
    pr.sum.entrenamiento$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.entrenamiento, entregar_fecha_inicio = TRUE, 
                                                                       datos_simulados=TRUE, iteracion = i, tz=tz.i)[2])),
    pr.sum.entrenamiento$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.entrenamiento, entregar_fecha_inicio = FALSE, 
                                                                     datos_simulados=TRUE, iteracion = i, tz=tz.i)[2])) )}
  
  
  # Datos totales
  
  pr.sum0.total <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, es.cmip6 = FALSE, anhos = anhos.total)
  pr.sum.total <- udConvertGrid(pr.sum0.total, new.units = "mm")
  
  pr.sum.total.original <- pr.sum.total
  
  if(i>1){c(
    #pr.sum.total.original <- pr.sum.total,
    pr.sum.total$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total, entregar_fecha_inicio = TRUE, 
                                                               datos_simulados=TRUE, iteracion = i, tz=tz.i)[2])),
    pr.sum.total$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total, entregar_fecha_inicio = FALSE, 
                                                             datos_simulados=TRUE, iteracion = i, tz=tz.i)[2])) )}
  
  
  ############################################################################################################
  ############################################################################################################
  # EN ESTO ESTAS!!!
  
  # hacerlo para todas las estaciones (loop)
  
  # Metodo ptr
  
  message('Corrigiendo sesgo con metodo PTR')
  
  pr.sum.corregido.ptr <- biasCorrection(x=pr.sum.entrenamiento,
                                         y = estaciones,
                                         newdata = pr.sum.total,
                                         precipitation = TRUE,
                                         method = "ptr",
                                         wet.threshold=umbral)
  
  ####################
  raster.pr.sum.entrenamiento <- stack(grid2sp(pr.sum.entrenamiento))
  raster.pr.sum.entrenamiento.1 <- extract(raster.pr.sum.entrenamiento, estaciones$xyCoords[1,])
  
  raster.pr.sum.total <- stack(grid2sp(pr.sum.total))
  raster.pr.sum.total.1 <- extract(raster.pr.sum.total, estaciones$xyCoords[1,])
  
  pr.sum.corregido.ptr_2 <- ptr(o=estaciones$Data[,1], 
                                p=raster.pr.sum.entrenamiento.1, 
                                s=raster.pr.sum.total.1, 
                                precip = TRUE)
  
  length(pr.sum.corregido.ptr$Data[,1])
  dim(pr.sum.corregido.ptr_2)
  
  plot(estaciones$Data[500:540], type='l')
  lines(pr.sum.corregido.ptr$Data[500:540,1], col='blue')
  lines(pr.sum.corregido.ptr_2[500:540], col='red')
  
  a <- ptr(o=estaciones$Data[,1], p=raster.pr.sum.entrenamiento.1, s=raster.pr.sum.total.1, precip = TRUE, entregar_a = TRUE)
  b <- ptr(o=estaciones$Data[,1], p=raster.pr.sum.entrenamiento.1, s=raster.pr.sum.total.1, precip = TRUE, entregar_b = TRUE)
  
  ############################################################################################################
  ############################################################################################################
  
  
  if(i>1){c(
    pr.sum.corregido.ptr$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                       entregar_fecha_inicio = TRUE, 
                                                                       iteracion = i, tz=tz.i)[1])),
    
    pr.sum.corregido.ptr$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                     entregar_fecha_inicio = FALSE, 
                                                                     iteracion = i, tz=tz.i)[1])),
    
    pr.sum.corregido.ptr$Data <- formato_datos_para_climate4R(pr.sum.corregido.ptr, pr.sum.total.original,
                                                              entregar_fecha_inicio = TRUE,
                                                              iteracion = i, tz=tz.i) )}
  
  db.ptr.preliminar <- lista_climate4R_a_db(pr.sum.corregido.ptr)
  db.ptr <- rbind(db.ptr, db.ptr.preliminar)
  
  
  # Valores observados ----
  
  message('Valores observados')
  
  db.estaciones.preliminar <- lista_climate4R_a_db(estaciones.con.todos.los.anhos)
  db.estaciones <- rbind(db.estaciones, db.estaciones.preliminar)
  
  # fin ---
  
  
  
  
  # Extrayendo valores de los pixeles donde estan ubicados las estaciones ----
  
  message('Extrayendo valores de celda donde estan ubicados las estaciones')
  
  pr.sum.total$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                             entregar_fecha_inicio = TRUE, 
                                                             iteracion = i, tz=tz.i)[1]))
  
  pr.sum.total$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                           entregar_fecha_inicio = FALSE, 
                                                           iteracion = i, tz=tz.i)[1]))
  
  era5.en.ubicacion.de.estacion.i <- grid2sp(pr.sum.total)
  db.era5.preliminar <- grilla_a_db(era5.en.ubicacion.de.estacion.i, estaciones)
  db.era5 <- rbind(db.era5, db.era5.preliminar)
  
  # fin ---
# }