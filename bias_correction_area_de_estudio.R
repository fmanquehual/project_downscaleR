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

# probar con hacer pasar el resto de los mesecomo enero, pq con abril tambien da problemas en biascorrection!

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




# Correccion mensual ----

db.estaciones <- c()
db.eqm <- c()
db.pqm <- c()
# db.gpqm <- c() # ver apunte del cuaderno!
db.loci <- c()
db.ptr <- c()
db.qdm <- c()
db.era5 <- c()

tz.i <- 'GMT'

for (i in 1:12) {
  #i <- 1
  
  mensaje.inicio <- paste('Inicio de proceso con mes', i, '--------------------------------------------------------------')
  message(mensaje.inicio)
  
  
  # Parametros ----
  
  meses <- i # con el año 2010, del 1 al 9no mes, corre bien todo
  anhos.entrenamiento <- 2011:2015 #1979:2018 # con los años 2010:2011, del 1 al 3er mes, corre bien todo
  anhos.total <- 2011:2017
  # latitud <- c(-49,-36) # area de estudio CCR
  # longitud <- c(-75, -72) # area de estudio CCR
  # latitud <- c(-48, -46) # area de estudio WRF
  # longitud <- c(-74, -71) # area de estudio WRF
  latitud <- c(-47.3, -47)
  longitud <- c(-73.3, -73)
  
  # fin ---
  
  
  
  
  
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
  
  # inventario.era5 <- dataInventory(era5)
  # inventario.era5$tp$Dimensions$time$Date_range
  
  # Precipitacion
  # C4R.vocabulary()
  
  message('Transformando unidades de variable')
  
  # Datos de entrenamiento
  
  pr.sum0.entrenamiento <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, es.cmip6 = FALSE, anhos = anhos.entrenamiento)
  pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")
  
  if(i>1){c(
  # pr.sum.entrenamiento.original <- pr.sum.entrenamiento,
  pr.sum.entrenamiento$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.entrenamiento, entregar_fecha_inicio = TRUE, iteracion = i, tz=tz.i)[2])),
  pr.sum.entrenamiento$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.entrenamiento, entregar_fecha_inicio = FALSE, iteracion = i, tz=tz.i)[2])) )}
  
  
  # Datos totales
  
  pr.sum0.total <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, es.cmip6 = FALSE, anhos = anhos.total)
  pr.sum.total <- udConvertGrid(pr.sum0.total, new.units = "mm")
  
  pr.sum.total.original <- pr.sum.total
  
  if(i>1){c(
    #pr.sum.total.original <- pr.sum.total,
    pr.sum.total$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total, entregar_fecha_inicio = TRUE, iteracion = i, tz=tz.i)[2])),
    pr.sum.total$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total, entregar_fecha_inicio = FALSE, iteracion = i, tz=tz.i)[2])) )}
  
  # spatialPlot(climatology(pr.sum, list(FUN = mean, na.rm = T)), backdrop.theme = "coastline", scales = list(draw = T),
  #             sp.layout = list(list(SpatialPoints(getCoordinates(estaciones)), 
  #                                   pch = 17, col = "black", cex = 1.5)))
  
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
  
  
  
  
  # Bias correction ----
  
  # Metodo delta
  
  # pr.sum.corregido.delta <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
  #                                    y = estaciones,
  #                                    precipitation = TRUE,
  #                                    method = "delta")
  
  
  
  # Metodo scaling
  
  # pr.sum.corregido.scaling <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
  #                                    y = estaciones,
  #                                    precipitation = TRUE,
  #                                    method = "scaling",
  #                                    scaling.type = "multiplicative")
  
  
  
  # Metodo eqm
  
  message('Corrigiendo sesgo con metodo EQM')
  
  pr.sum.corregido.eqm <- biasCorrection(x=pr.sum.entrenamiento, # hay problemas con el mes 10, 11 y 12
                                     y = estaciones,
                                     newdata = pr.sum.total,
                                     precipitation = TRUE,
                                     method = "eqm",
                                     wet.threshold=0.01)
  
  if(i>1){c(
  pr.sum.corregido.eqm$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                     entregar_fecha_inicio = TRUE, 
                                                                     iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.eqm$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                   entregar_fecha_inicio = FALSE, 
                                                                   iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.eqm$Data <- formato_datos_para_climate4R(pr.sum.corregido.eqm) )}
  
  db.eqm.preliminar <- lista_climate4R_a_db(pr.sum.corregido.eqm)
  db.eqm <- rbind(db.eqm, db.eqm.preliminar)
  

  
  # Metodo pqm
  
  message('Corrigiendo sesgo con metodo PQM')
  
  pr.sum.corregido.pqm <- biasCorrection(x=pr.sum.entrenamiento, # hay problemas con el mes 10, 11 y 12
                                     y = estaciones,
                                     newdata = pr.sum.total,
                                     precipitation = TRUE,
                                     method = "pqm",
                                     wet.threshold=0.01)
  
  if(i>1){c(
  pr.sum.corregido.pqm$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                     entregar_fecha_inicio = TRUE, 
                                                                     iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.pqm$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                   entregar_fecha_inicio = FALSE, 
                                                                   iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.pqm$Data <- formato_datos_para_climate4R(pr.sum.corregido.pqm) )}
  
  db.pqm.preliminar <- lista_climate4R_a_db(pr.sum.corregido.pqm)
  db.pqm <- rbind(db.pqm, db.pqm.preliminar)
  
  
  
  # # Metodo gpqm
  # 
  # message('Corrigiendo sesgo con metodo GPQM')
  # 
  # pr.sum.corregido.gpqm <- biasCorrection(x=pr.sum.entrenamiento, # hay problemas con el mes 10, 11 y 12
  #                                    y = estaciones,
  #                                    newdata = pr.sum.total,
  #                                    precipitation = TRUE,
  #                                    method = "gpqm",
  #                                    wet.threshold=0.01)
  # 
  # if(i>1){c(
  #   pr.sum.corregido.gpqm$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
  #                                                                      entregar_fecha_inicio = TRUE, 
  #                                                                      iteracion = i, tz=tz.i)[1])),
  #   
  #   pr.sum.corregido.gpqm$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
  #                                                                    entregar_fecha_inicio = FALSE, 
  #                                                                    iteracion = i, tz=tz.i)[1])),
  #   
  #   pr.sum.corregido.gpqm$Data <- formato_datos_para_climate4R(pr.sum.corregido.gpqm) )}
  # 
  # db.gpqm.preliminar <- lista_climate4R_a_db(pr.sum.corregido.gpqm)
  # db.gpqm <- rbind(db.gpqm, db.gpqm.preliminar)
  
  
  
  # Metodo loci
  
  message('Corrigiendo sesgo con metodo LOCI')
  
  pr.sum.corregido.loci <- biasCorrection(x=pr.sum.entrenamiento, # hay problemas con el mes 10, 11 y 12
                                      y = estaciones,
                                      newdata = pr.sum.total,
                                      precipitation = TRUE,
                                      method = "loci",
                                      wet.threshold=0.01)
  
  if(i>1){c(
  pr.sum.corregido.loci$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                     entregar_fecha_inicio = TRUE, 
                                                                     iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.loci$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                   entregar_fecha_inicio = FALSE, 
                                                                   iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.loci$Data <- formato_datos_para_climate4R(pr.sum.corregido.loci) )}
  
  db.loci.preliminar <- lista_climate4R_a_db(pr.sum.corregido.loci)
  db.loci <- rbind(db.loci, db.loci.preliminar)
  
  
  
  # Metodo ptr
  
  message('Corrigiendo sesgo con metodo PTR')
  
  pr.sum.corregido.ptr <- biasCorrection(x=pr.sum.entrenamiento, # hay problemas con el mes 10, 11 y 12
                                      y = estaciones,
                                      newdata = pr.sum.total,
                                      precipitation = TRUE,
                                      method = "ptr",
                                      wet.threshold=0.01)
  
  if(i>1){c(
  pr.sum.corregido.ptr$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                      entregar_fecha_inicio = TRUE, 
                                                                      iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.ptr$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                    entregar_fecha_inicio = FALSE, 
                                                                    iteracion = i, tz=tz.i)[1])),
  
  pr.sum.corregido.ptr$Data <- formato_datos_para_climate4R(pr.sum.corregido.ptr) )}
  
  db.ptr.preliminar <- lista_climate4R_a_db(pr.sum.corregido.ptr)
  db.ptr <- rbind(db.ptr, db.ptr.preliminar)
  
  
  
  # Metodo dqm
  
  # pr.sum.corregido8 <- biasCorrection(x=pr.sum, # hay problemas con el mes 10, 11 y 12
  #                                     y = estaciones,
  #                                     precipitation = TRUE,
  #                                     method = "dqm",
  #                                     wet.threshold=0.01)
  
  
  
  # Metodo qdm
  
  message('Corrigiendo sesgo con metodo QDM')
  
  pr.sum.corregido.qdm <- biasCorrection(x=pr.sum.entrenamiento, # hay problemas con el mes 10, 11 y 12
                                      y = estaciones,
                                      newdata = pr.sum.total,
                                      precipitation = TRUE,
                                      method = "qdm",
                                      wet.threshold=0.01)
              
  
  if(i>1){c(
    pr.sum.corregido.qdm$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                       entregar_fecha_inicio = TRUE, 
                                                                       iteracion = i, tz=tz.i)[1])),
    
    pr.sum.corregido.qdm$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
                                                                     entregar_fecha_inicio = FALSE, 
                                                                     iteracion = i, tz=tz.i)[1])),
    
    pr.sum.corregido.qdm$Data <- formato_datos_para_climate4R(pr.sum.corregido.qdm) )}
  
  db.qdm.preliminar <- lista_climate4R_a_db(pr.sum.corregido.qdm)
  db.qdm <- rbind(db.qdm, db.qdm.preliminar)
  
  
  
  
  # Valores observados ----
  
  message('Valores observados')
  
  # if(i>1){c(
  #   estaciones$Dates$start <- as.vector(unlist(nuevas_fechas(estaciones.original, 
  #                                                            entregar_fecha_inicio = TRUE, 
  #                                                            iteracion = i, tz=tz.i)[1])),
  #   
  #   estaciones$Dates$end <- as.vector(unlist(nuevas_fechas(estaciones.original, 
  #                                                          entregar_fecha_inicio = FALSE, 
  #                                                          iteracion = i, tz=tz.i)[1])),
  #   
  #  estaciones$Data <- formato_datos_para_climate4R(estaciones) )}
  
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
  
  pr.sum.total$Data <- formato_datos_para_climate4R(pr.sum.total)
  
  era5.en.ubicacion.de.estacion.i <- grid2sp(pr.sum.total)
  db.era5.preliminar <- grilla_a_db(era5.en.ubicacion.de.estacion.i)
  db.era5 <- rbind(db.era5, db.era5.preliminar)
  
  # fin ---
  
  
  
  
  # Otros
  
  #pr.sum.bias.correction2 <- interpGrid(pr.sum.bias.correction, getGrid(y)) # CORDEX historical
  
  # temporalPlot(estaciones, pr.sum.corregido.eqm, lwd = c(2,1), lty = c(1,2), aggr.spatial = list(FUN = mean, na.rm = TRUE))
  
  # spatialPlot(climatology(pr.sum.corregido.pqm, list(FUN = mean, na.rm = T)), backdrop.theme = "countries", 
  #             main = 'Precipitacion', scales = list(draw = T))
  # 
  # temporalPlot(pr.sum, aggr.spatial = list(FUN = mean, na.rm = TRUE))


  # fin ---

  mensaje.fin <- paste('Fin del proceso con mes', i, '--------------------------------------------------------------')
  message(mensaje.fin)
  
}

# fin ---




# Ordenando las bases de datos por fecha ----

db.estaciones.ordenado <- ordenar_por_fecha(db.estaciones) ; dim(db.estaciones.ordenado)
db.eqm.ordenado <- ordenar_por_fecha(db.eqm) ; dim(db.eqm.ordenado)
db.pqm.ordenado <- ordenar_por_fecha(db.pqm) ; dim(db.pqm.ordenado)
# db.gpqm.ordenado <- ordenar_por_fecha(db.gpqm) ; dim(db.gpqm.ordenado)
db.loci.ordenado <- ordenar_por_fecha(db.loci) ; dim(db.loci.ordenado)
db.ptr.ordenado <- ordenar_por_fecha(db.ptr) ; dim(db.ptr.ordenado)
db.qdm.ordenado <- ordenar_por_fecha(db.qdm) ; dim(db.qdm.ordenado)
db.era5.ordenado <- ordenar_por_fecha(db.era5) ; dim(db.era5.ordenado)

# fin ---




# guardando dbs ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

write.csv(db.estaciones.ordenado, 'estaciones.csv', row.names = FALSE)
write.csv(db.eqm.ordenado, 'eqm.csv', row.names = FALSE)
write.csv(db.pqm.ordenado, 'pqm.csv', row.names = FALSE)
# write.csv(db.gpqm.ordenado, 'gpqm.csv', row.names = FALSE)
write.csv(db.loci.ordenado, 'loci.csv', row.names = FALSE)
write.csv(db.ptr.ordenado, 'ptr.csv', row.names = FALSE)
write.csv(db.qdm.ordenado, 'qdm.csv', row.names = FALSE)
write.csv(db.era5.ordenado, 'era5.csv', row.names = FALSE)

# fin ---
