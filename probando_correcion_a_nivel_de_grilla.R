library(loadeR)
library(visualizeR)
library(downscaleR)
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

db.estaciones <- c()
db.eqm <- c()
db.pqm <- c()
db.gpqm <- c() # ver apunte del cuaderno!
db.loci <- c()
db.ptr <- c()
db.qdm <- c()
db.era5 <- c()
a <- c()
b <- c()
tz.i <- 'GMT'

# fin ---





# Correccion mensual ----

for (i in 1) {
#  i <- 1
  
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
  
  numero.de.estaciones <- length(estaciones$Metadata$name)
  
  for (i in 1:numero.de.estaciones) {
    
    nombre.estacion.i <- estaciones$Metadata$name[i]
    mensaje <- paste0("Extrayendo 'a' y 'b' de la estacion ", nombre.estacion.i, ' (', i, ' de ', numero.de.estaciones, ')') 
    message(mensaje)
    
    raster.pr.sum.entrenamiento <- stack(grid2sp(pr.sum.entrenamiento))
    raster.pr.sum.entrenamiento.1 <- extract(raster.pr.sum.entrenamiento, estaciones$xyCoords[i,])
    
    raster.pr.sum.total <- stack(grid2sp(pr.sum.total))
    raster.pr.sum.total.1 <- extract(raster.pr.sum.total, estaciones$xyCoords[i,])
    
    a.i <- ptr(o=estaciones$Data[,i], p=raster.pr.sum.entrenamiento.1, s=raster.pr.sum.total.1, 
               precip = TRUE, entregar_a = TRUE)
    b.i <- ptr(o=estaciones$Data[,i], p=raster.pr.sum.entrenamiento.1, s=raster.pr.sum.total.1, 
               precip = TRUE, entregar_b = TRUE)
  
    a <- c(a, a.i)
    b <- c(b, b.i)
  }
  
  
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
}




# ajustando modelos de correcion ----


# Quitando NA
a.original <- a ; a.original
b.original <- b ; b.original
H.original <- estaciones$Metadata$altitude ; H.original


id.valores.NA <- which(is.na(a.original))
a <- a.original[-id.valores.NA]
b <- b.original[-id.valores.NA]
H <- H.original[-id.valores.NA]
log.a <- log(a)
log.b <- log(b)

db <- data.frame(a=a, b=b, H=H, log.a=log.a, log.b=log.b)


# Ajustando modelo lineal

funcion_ln <- function(c.i,d.i,H.i){ y <- log(c.i)+log(H.i)*d.i
                                     return(y) }

modelo.ln.a <- nls(log.a~funcion_ln(c, d, H),
                   data = db,
                   start = c(c=0.1, d=0.1))

summary(modelo.ln.a)
c.ln.a <- coefficients(modelo.ln.a)[1]
d.ln.a <- coefficients(modelo.ln.a)[2]


modelo.ln.b <- nls(log.b~funcion_ln(c, d, H),
                   data = db,
                   start = c(c=0.1, d=0.1))

summary(modelo.ln.b)
c.ln.b <- coefficients(modelo.ln.b)[1]
d.ln.b <- coefficients(modelo.ln.b)[2]

db$log.a.estimado <- funcion_ln(c.ln.a, d.ln.a, db$H)
db$log.b.estimado <- funcion_ln(c.ln.b, d.ln.b, db$H)

head(db)

plot(db$log.a, type='l', lwd=2)
lines(db$log.a.estimado, col='red', lwd=2)

plot(db$log.b, type='l', lwd=2)
lines(db$log.b.estimado, col='red', lwd=2)


# Ajustando modelo de poder

funcion_de_poder <- function(c.i,d.i,H.i){ y <- c.i*(H.i^d.i)
                                     return(y) }

modelo.a <- nls(a ~ funcion_de_poder(c, d, H),
                data = db,
                start = c(c=0.1, d=0.1))

summary(modelo.a)
c.a <- coefficients(modelo.a)[1]
d.a <- coefficients(modelo.a)[2]


modelo.b <- nls(b ~ funcion_de_poder(c, d, H),
                data = db,
                start = c(c=0.1, d=0.1))

summary(modelo.b)
c.b <- coefficients(modelo.b)[1]
d.b <- coefficients(modelo.b)[2]

db$a.estimado <- funcion_de_poder(c.a, d.a, db$H)
db$b.estimado <- funcion_de_poder(c.b, d.b, db$H)

head(db)

plot(db$a, type='l', lwd=2)
lines(db$a.estimado, col='red', lwd=2)

plot(db$b, type='l', lwd=2)
lines(db$b.estimado, col='red', lwd=2)

# fin ---




# Evaluando modelos ----

db$residual.de.a <- db$a-db$a.estimado
db$residual.de.log.a <- db$log.a-db$log.a.estimado

db$residual.de.b <- db$b-db$b.estimado
db$residual.de.log.b <- db$log.b-db$log.b.estimado

# significancia
summary(modelo.a)
summary(modelo.ln.a)

summary(modelo.b)
summary(modelo.ln.b)


# plots de residuales
par(mfrow=c(2,2))

plot(db$residual.de.a)
abline(h=0, col='red')

plot(db$residual.de.log.a)
abline(h=0, col='red')

plot(db$residual.de.b)
abline(h=0, col='red')

plot(db$residual.de.log.b)
abline(h=0, col='red')

# fin ---




# Preparando db para correcion ----

nombre.estaciones <- estaciones$Metadata$name
altitud.estaciones <- estaciones$Metadata$altitude
db.altitud <- data.frame(nombre_estacion=nombre.estaciones, altitud=altitud.estaciones)

db.era5$id <- paste(db.era5$fecha, db.era5$nombre_estacion, sep='_')
db.era5 <- db.era5[,c('id', 'valor')]
colnames(db.era5)[2] <- 'valor.simulado'

db.ptr$id <- paste(db.ptr$fecha, db.ptr$nombre_estacion, sep='_')
db.ptr <- db.ptr[,c('id', 'valor')]
colnames(db.ptr)[2] <- 'valor.ptr'

db.estaciones$id <- paste(db.estaciones$fecha, db.estaciones$nombre_estacion, sep='_')

db.estaciones.y.era5 <- merge(db.estaciones, db.era5, by='id')
db.estaciones.era5.y.ptr <- merge(db.estaciones.y.era5, db.ptr, by='id')
db.estaciones.era5.y.ptr <- db.estaciones.era5.y.ptr[,-1]

db.estaciones.y.era5 <- merge(db.estaciones.era5.y.ptr, db.altitud, by='nombre_estacion')

head(db.estaciones.y.era5)

# fin ---




# Correccion a nivel de estacion ----

db.estaciones.y.era5$a <- funcion_de_poder(c.a, d.a, db.estaciones.y.era5$altitud)
db.estaciones.y.era5$b <- funcion_de_poder(c.b, d.b, db.estaciones.y.era5$altitud)
db.estaciones.y.era5$ln.a <- funcion_ln(c.ln.a, d.ln.a, db.estaciones.y.era5$altitud)
db.estaciones.y.era5$ln.b <- funcion_ln(c.ln.b, d.ln.b, db.estaciones.y.era5$altitud)

head(db.estaciones.y.era5)
dim(db.estaciones.y.era5)

db.estaciones.y.era5$valor.simulado.corregido <- db.estaciones.y.era5$a*(db.estaciones.y.era5$valor.simulado^db.estaciones.y.era5$b)
db.estaciones.y.era5$valor.simulado.ln.corregido <- db.estaciones.y.era5$ln.a*(db.estaciones.y.era5$valor.simulado^db.estaciones.y.era5$ln.b)

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