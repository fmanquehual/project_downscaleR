library(loadeR)
library(visualizeR)
library(downscaleR)
library(convertR)
library(lattice)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_nombre_de_columnas_a_fechas.R')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_lista_climate4R_a_db.R')
source('funcion_grilla_a_db.R')
source('funcion_nuevas_fechas.R')
source('funcion_formato_datos_para_climate4R.R')
source('funcion_ordenar_por_fecha.R')
source('funcion_prt.R')
source('funcion_generador_de_fechas.R')
source('funcion_bloque_de_dias.R')
source('funcion_ptr_por_bloque.R')


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
# latitud <- c(-49,-36) # area de estudio CCR
# longitud <- c(-75, -72) # area de estudio CCR
latitud <- c(-48.5, -45.5) # area de estudio WRF
longitud <- c(-74, -71) # area de estudio WRF
# latitud <- c(-47.3, -47)
# longitud <- c(-73.3, -73)
umbral <- 1 # The minimum value that is considered as a non-zero precipitation (default is 1 mm)
# Con el umbral=0, da problemas en gpqm (valores muy altos, al final la correcion empeora la estimacion de pp)
# Las correciones mejoran bastante cuando el umbral es 1 (default).

dias_del_bloque <- 5 
buffer <- 30

db.estaciones <- c()
db.ptr <- c()
db.era5 <- c()
db.parametros <- c()
tz.i <- 'GMT'

# fin ---





# Correccion mensual ----

# for (i in 1) {
i <- 1:12
  
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
  
  numero.de.estaciones <- length(estaciones$Metadata$name)
  
  for (i in 1:numero.de.estaciones) {
    # i <- 1
    
    nombre.estacion.i <- estaciones$Metadata$name[i]
    mensaje <- paste0("Extrayendo 'a' y 'b' de la estacion ", nombre.estacion.i, 
                      ' (', i, ' de ', numero.de.estaciones, ')') 
    message(mensaje)
    
    raster.pr.sum.entrenamiento0 <- stack(grid2sp(pr.sum.entrenamiento))
    raster.pr.sum.entrenamiento <- extract(raster.pr.sum.entrenamiento0, estaciones$xyCoords[i,])
    
    raster.pr.sum.total0 <- stack(grid2sp(pr.sum.total))
    raster.pr.sum.total <- extract(raster.pr.sum.total0, estaciones$xyCoords[i,])
    
    db.fechas.por.bloque <- bloque_de_dias(estaciones, raster.pr.sum.entrenamiento, 
                                           raster.pr.sum.total, dias_del_bloque = dias_del_bloque, 
                                           buffer = buffer)
    
    db.parametros0 <- ptr_por_bloque(db.fechas.por.bloque, estaciones, raster.pr.sum.entrenamiento,
                                     raster.pr.sum.total, i)
  
    db.parametros <- rbind(db.parametros, db.parametros0)
  }
  
  
  
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




# ajustando modelos de correcion ----

bloques <- unique(db.parametros$bloque)
  
db <- c()
for (i in bloques) {
  # i <- 1    
  
  db.parametros.i <- subset(db.parametros, bloque==i)
  
  # Quitando NA
  a.original <- db.parametros.i$a ; a.original
  b.original <- db.parametros.i$b ; b.original
  H.original <- estaciones$Metadata$altitude ; H.original
  
  
  id.valores.NA <- which(is.na(a.original))
  a <- a.original[-id.valores.NA]
  b <- b.original[-id.valores.NA]
  H <- H.original[-id.valores.NA]
  
  db0 <- data.frame(a=a, b=b, H=H)
  
  
  # Ajustando modelo de poder
  
  funcion_de_poder <- function(c.i,d.i,H.i){ y <- c.i*(H.i^d.i)
                                       return(y) }
  
  modelo.a <- nls(a ~ funcion_de_poder(c, d, H),
                  data = db0,
                  start = c(c=0.1, d=0.1))
  
  c.a <- coefficients(modelo.a)[1]
  d.a <- coefficients(modelo.a)[2]
  
  
  modelo.b <- nls(b ~ funcion_de_poder(c, d, H),
                  data = db0,
                  start = c(c=0.1, d=0.1))
  
  c.b <- coefficients(modelo.b)[1]
  d.b <- coefficients(modelo.b)[2]
  
  db0$a.estimado <- funcion_de_poder(c.a, d.a, db0$H)
  db0$b.estimado <- funcion_de_poder(c.b, d.b, db0$H)
  db0$bloque <- i

  db <- rbind(db, db0)
}
  
head(db)
db$bloque <- factor(db$bloque, levels = 1:73)

xyplot(a~H | bloque, data=db, type=c('p', 'g', 'smooth'),
       main="Parametro 'a' vs altitud de cada bloque") # Show points ("p"), grids ("g") and smoothing line

xyplot(b~H | bloque, data=db, type=c('p', 'g', 'smooth'),
       main="Parametro 'b' vs altitud de cada bloque")

# fin ---




# Evaluando modelos ----

db$residual.de.a <- db$a-db$a.estimado
db$residual.de.b <- db$b-db$b.estimado

# significancia

summary(modelo.a)
summary(modelo.b)


# plots de residuales

plot(db$residual.de.a)
abline(h=0, col='red')

plot(db$residual.de.b)
abline(h=0, col='red')

histogram(~residual.de.a | bloque, data=db,
       main="Residuales parametro 'a' de cada bloque")

histogram(~residual.de.b | bloque, data=db,
       main="Residuales parametro 'b' de cada bloque")

# fin ---




# Preparando db para correcion ----

nombre.estaciones <- estaciones$Metadata$name
altitud.estaciones <- estaciones$Metadata$altitude
db.altitud <- data.frame(nombre_estacion=nombre.estaciones, altitud=altitud.estaciones)

db.estaciones$id <- paste(db.estaciones$fecha, db.estaciones$nombre_estacion, sep='_')

db.estaciones.y.era5.preliminar <- merge(db.estaciones, db.era5, by='id')
db.estaciones.y.era5 <- merge(db.estaciones.y.era5.preliminar, db.altitud, by='nombre_estacion')

head(db.estaciones.y.era5)

# fin ---




# Correccion a nivel de estacion ----

db.estaciones.y.era5$a <- funcion_de_poder(c.a, d.a, db.estaciones.y.era5$altitud)
db.estaciones.y.era5$b <- funcion_de_poder(c.b, d.b, db.estaciones.y.era5$altitud)

head(db.estaciones.y.era5)
dim(db.estaciones.y.era5)

db.estaciones.y.era5$valor.simulado.corregido <- db.estaciones.y.era5$a*(db.estaciones.y.era5$valor.simulado^db.estaciones.y.era5$b)

head(db.estaciones.y.era5)
dim(db.estaciones.y.era5)

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
# write.csv(db.estaciones.y.era5, 'ejemplo_bias_correcion_por_estacion_con_modelos_enero.csv', row.names = FALSE)

# plot

nombre.estaciones
db.subset <- subset(db.estaciones.y.era5, nombre_estacion==nombre.estaciones[1])
rango.de.valores <- 500:550

# par(mfrow=c(2,1))
valor.maximo <- max(db.subset$valor.simulado.corregido[rango.de.valores])

plot(db.subset$valor[rango.de.valores], type='l', lwd=2, ylim=c(0, valor.maximo))
lines(db.subset$valor.simulado[rango.de.valores], col='red', lwd=2)
lines(db.subset$valor.ptr[rango.de.valores], col='green', lwd=2)
lines(db.subset$valor.simulado.corregido[rango.de.valores], col='blue', lwd=2)

legend('topright', legend = c('Observado', 'ERA5', 'PTR', 'Modelo de poder'), 
       lwd=c(2,2,2,2), 
       col = c('black', 'red', 'green', 'blue'))

# plot(db.estaciones.y.era5$valor[rango.de.valores], type='l', lwd=2)
# lines(db.estaciones.y.era5$valor.simulado[rango.de.valores], col='red', lwd=2)
# lines(db.estaciones.y.era5$valor.ptr[rango.de.valores], col='green', lwd=2)
# lines(db.estaciones.y.era5$valor.simulado.ln.corregido[rango.de.valores], col='blue', lwd=2)

# fin ---




# Correcion a nivel de grilla ----

pr.sum.corregido0 <- pr.sum.total.original
pr.sum.corregido <- pr.sum.total.original

numero.de.matrices <- dim(pr.sum.corregido0$Data)[1]
pr.sum.corregido0$Data[1,,]

# pr.sum.corregido0$Data[1,,] <- pr.sum.corregido0$Data[1,,]

raster.pr.sum.corregido0 <- grid2sp(pr.sum.corregido0)
raster.pr.sum.corregido0 <- stack(raster.pr.sum.corregido0)
plot(raster.pr.sum.corregido0, 1)

setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
dem <- raster('dem3_clip_para_mapas.tif')
# plot(dem)

dem.clip <- crop(dem, raster.pr.sum.corregido0)
plot(dem.clip)

dem.clip.resemple <- resample(dem.clip, raster.pr.sum.corregido0, method='bilinear')
plot(dem.clip.resemple)

matriz.altitud <- as.matrix(dem.clip.resemple)

####


for (i in 1:numero.de.matrices) {
  
  matriz.i <- pr.sum.corregido0$Data[i,,]
  
  for (j in 1:nrow(matriz.i)) {
      
    for (k in 1:ncol(matriz.i)){
      
      precipitacion.i <- matriz.i[j,k]
      altitud.i <- matriz.altitud[j,k]
      
      a.i <- funcion_de_poder(c.a, d.a, altitud.i)
      b.i <- funcion_de_poder(c.b, d.b, altitud.i)
      
      precipitacion.corregido.i <- a.i*(precipitacion.i^b.i)
      matriz.i[j,k] <- precipitacion.corregido.i
    
    }
      
  }
  
  pr.sum.corregido0$Data[i,,] <- matriz.i
  
}

pr.sum.corregido$Data <- pr.sum.corregido0$Data
pr.sum.corregido

era5.corregido <- grid2sp(pr.sum.corregido)
db.era5.corregido <- grilla_a_db(era5.corregido, estaciones)

head(db.era5.corregido)
head(db.estaciones.y.era5)

#######

raster.era5.corregido <- stack(era5.corregido)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/plots/')

png('mapa_altitud_era5Original_era5Corregida.png', width = 780, height = 780, units = "px")
par(mfrow=c(3,3))

plot(dem.clip.resemple, main='Altitud')
plot(raster.pr.sum.corregido0, 1, main='01/01/1995 ERA5')
plot(raster.era5.corregido, 1, main='01/01/1995 ERA5-CORREGIDO')

plot(dem.clip.resemple, main='Altitud')
plot(raster.pr.sum.corregido0, 15, main='15/01/1995 ERA5')
plot(raster.era5.corregido, 15, main='15/01/1995 ERA5-CORREGIDO')

plot(dem.clip.resemple, main='Altitud')
plot(raster.pr.sum.corregido0, 30, main='30/01/1995 ERA5')
plot(raster.era5.corregido, 30, main='30/01/1995 ERA5-CORREGIDO')

dev.off()

# fin ---