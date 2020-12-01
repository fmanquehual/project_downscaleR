library(loadeR)
library(loadeR.2nc)
library(visualizeR)
library(downscaleR)
library(convertR)
library(raster)
library(rgdal)
library(rgeos)
library(gstat)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_agua/proyecto_agua_en_R/')
source('funcion_nombre_de_columnas_a_fechas.R')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_lista_climate4R_a_db.R')
source('funcion_grilla_a_db.R')
source('funcion_prt.R')
source('funcion_generador_de_fechas.R')
source('funcion_bloque_de_dias.R')
source('funcion_ptr_por_bloque.R')
source('funcion_seleccion_de_poder_optimo_para_IDW.R')




# Lectura de capas ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
cuenca <- readOGR('.', 'poligono_cuenca_baker_geo')

# fin ---




# Parametros ----

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84

valores.de.poder.IDW.a.evaluar <- seq(1, 30, by=0.5)

anhos.entrenamiento <- 1995:2010
anhos.total <- 1995:2017
meses <- 1:12
latitud <- c(-48.5, -45.5) # area de estudio WRF
longitud <- c(-74, -71) # area de estudio WRF
dias_del_bloque <- 5 
buffer <- 30

db.estaciones <- c()
db.era5 <- c()
db.parametros <- c()
tz.i <- 'GMT'

# fin ---




# Lectura de datos ----

# Estaciones metereologicas (entrenamiento)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
nombre.carpeta <- 'datos_transformados_a_ASCII' # carpeta con datos en ASCII
info.estaciones <- stationInfo(nombre.carpeta) ; info.estaciones # resumen info estaciones
info.estaciones$name

estaciones.de.interes <- c(1,3,4,5,8,12,18,19,20,23,25,27)
stationID.de.interes <- info.estaciones$stationID[estaciones.de.interes]
  
estaciones.con.todos.los.anhos <- loadStationData(dataset = nombre.carpeta, 
                                                  var="precip", 
                                                  #units = 'mm',
                                                  stationID = stationID.de.interes, # escojo estaciones especificas
                                                  years = anhos.total,
                                                  season = meses,
                                                  tz=tz.i)

# Estaciones metereologicas (total)
estaciones <- loadStationData(dataset = nombre.carpeta, 
                              var="precip", 
                              #units = 'mm',
                              stationID = stationID.de.interes, # escojo estaciones especificas
                              years = anhos.entrenamiento,
                              season = meses,
                              tz=tz.i)


# Predictors (ERA reanalisis) (entrenamiento)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
era5 <- 'ERA5_1979_2018_pp.nc'

dataInventory(era5) # resumen de informacion sobre netcdf
pr.sum0.entrenamiento <- loadGridData(dataset = era5, 
                              var = 'tp', # nombre de variable en netcdf
                              aggr.d = 'sum', # como son datos horarios, los sumo en forma diaria
                              #aggr.m = "mean",
                              lonLim = longitud,
                              latLim= latitud, 
                              season= meses, 
                              years = anhos.entrenamiento,
                              time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data

pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm") # transformando unidades


# Predictors (ERA reanalisis) (total)
pr.sum0.total <- loadGridData(dataset = era5, 
                                 var = 'tp',
                                 aggr.d = 'sum',
                                 #aggr.m = "mean",
                                 lonLim = longitud,
                                 latLim= latitud, 
                                 season= meses, 
                                 years = anhos.total,
                                 time = 'DD') # obtain daily (aggr.d) or monthly (aggr.m) data

pr.sum.total <- udConvertGrid(pr.sum0.total, new.units = "mm")

pr.sum.total.original <- pr.sum.total

# fin ---




# Obteniendo parametros a nivel de estacion de metodo ptr ----

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
  
  # Generando bloques segun fecha
  db.fechas.por.bloque <- bloque_de_dias(estaciones, raster.pr.sum.entrenamiento, 
                                         raster.pr.sum.total, dias_del_bloque = dias_del_bloque, 
                                         buffer = buffer)
  
  # Calculando parametros por bloque 'i'
  db.parametros0 <- ptr_por_bloque(db.fechas.por.bloque, estaciones, raster.pr.sum.entrenamiento,
                                   raster.pr.sum.total, i)
  
  # Generando DB con parametros a nivel de estacion
  db.parametros <- rbind(db.parametros, db.parametros0)
}

# fin ---




# Datos de estaciones a DB ----

db.estaciones <- lista_climate4R_a_db(estaciones.con.todos.los.anhos)
head(db.estaciones)

# fin ---




# Extrayendo valores de pixeles donde estan ubicados las estaciones ----

era5.en.ubicacion.de.estacion.i <- grid2sp(pr.sum.total)
db.era5 <- grilla_a_db(era5.en.ubicacion.de.estacion.i, estaciones)
head(db.era5)

# fin ---




# Estimando 'a' y 'b' con metodos H y IDW ----

nombre.estaciones <- estaciones$Metadata$name
altitud.de.estaciones <- estaciones$Metadata$altitude
db.estacion.altitud <- data.frame(nombre_estacion=nombre.estaciones, H=altitud.de.estaciones)
head(db.estacion.altitud)

x <- estaciones$xyCoords$x
y <- estaciones$xyCoords$y

# Union de DBs
db.estaciones.xy0 <- data.frame(nombre_estacion=nombre.estaciones, lon=x, lat=y)
db.parametros.xy00 <- merge(db.parametros, db.estaciones.xy0, by='nombre_estacion')
db.parametros.xy <- merge(db.parametros.xy00, db.estacion.altitud, by='nombre_estacion')
head(db.parametros.xy)

# Grillas de referencia
pr.sum.total.stack <- stack(era5.en.ubicacion.de.estacion.i)
pr.sum.total.i <- pr.sum.total.stack[[1]]
raster.referencia <- pr.sum.total.i
grilla.referencia <- rasterToPolygons(pr.sum.total.i, dissolve = TRUE)

bloques.unicos <- sort(unique(db.parametros.xy$bloque))
iteraciones <- 75 # maximo de iteraciones para nls() (default=50)
valor.estimacion.inicial <- c(0.2, 0.2) # valores iniciales (c y d) para estimar en nls()
  
# Estimacion por bloque 'i' y metodo
db <- c()
for (i in bloques.unicos) {
  # i <- 1
  
  mensaje.inicio <- paste0("Estimando 'a' y 'b' de bloque ", i, ' de ', length(bloques.unicos))
  message(mensaje.inicio)
  
  db.parametros.xy.i <- subset(db.parametros.xy, bloque==i)
  
  # Quitando NA
  
  id.valores.NA <- which(is.na(db.parametros.xy.i$a))
  
  if(length(id.valores.NA)>0){db.parametros.xy.sin.NA.i <- db.parametros.xy.i[-id.valores.NA,]
  } else(db.parametros.xy.sin.NA.i <- db.parametros.xy.i)
  
  row.names(db.parametros.xy.sin.NA.i) <- 1:nrow(db.parametros.xy.sin.NA.i)
  
  
  
  
  # Estimando 'a' y 'b' con H ----
  
  message('Metodo H')
  
  # Ajustando modelo de poder
  
  funcion_de_poder <- function(c.i,d.i,H.i){ y <- c.i*(H.i^d.i)
                                            return(y) }
  
  modelo.a <- nls(a ~ funcion_de_poder(c, d, H),
                  data = db.parametros.xy.sin.NA.i,
                  start = c(c=valor.estimacion.inicial[1], d=valor.estimacion.inicial[2]), 
                  control = list(maxiter=iteraciones, warnOnly=TRUE))
  
  c.a <- coefficients(modelo.a)[1]
  d.a <- coefficients(modelo.a)[2]
  
  
  modelo.b <- nls(b ~ funcion_de_poder(c, d, H),
                  data = db.parametros.xy.sin.NA.i,
                  start = c(c=valor.estimacion.inicial[1], d=valor.estimacion.inicial[2]),
                  control = list(maxiter=iteraciones, warnOnly=TRUE))
  
  c.b <- coefficients(modelo.b)[1]
  d.b <- coefficients(modelo.b)[2]
  
  
  db.parametros.xy.sin.NA.i$c_de_a <- c.a
  db.parametros.xy.sin.NA.i$d_de_a <- d.a
  db.parametros.xy.sin.NA.i$c_de_b <- c.b
  db.parametros.xy.sin.NA.i$d_de_b <- d.b
  
  db.parametros.xy.sin.NA.i$a.estimado.H <- funcion_de_poder(c.a, d.a, db.parametros.xy.sin.NA.i$H)
  db.parametros.xy.sin.NA.i$b.estimado.H <- funcion_de_poder(c.b, d.b, db.parametros.xy.sin.NA.i$H)
  db.parametros.xy.sin.NA.i$bloque <- i
  
  # fin ---
  
  
  
  
  # Estimando 'a' y 'b' con IDW ----
  
  message('Metodo IDW')
  
  # Transformacion a Spatial Points Data Frame
  
  puntos0 <- SpatialPoints(cbind(db.parametros.xy.sin.NA.i$lon, db.parametros.xy.sin.NA.i$lat), 
                           proj4string = CRS(wgs84))
  puntos <- SpatialPointsDataFrame(puntos0, data = db.parametros.xy.sin.NA.i, match.ID = TRUE)
  
  r.puntos.a <- rasterize(puntos, pr.sum.total.i, field="a", fun=mean, background=NA)
  r.puntos.b <- rasterize(puntos, pr.sum.total.i, field="b", fun=mean, background=NA)
  
  puntos@bbox <- grilla.referencia@bbox
  
  # Creando grilla vacia
  grd              <- as.data.frame(spsample(grilla.referencia, "regular", 
                                             n=dim(pr.sum.total.i)[1]*dim(pr.sum.total.i)[2]))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  proj4string(grd) <- crs(puntos)
  
  # Calculo de poder optimo para IDW
  poder.a <- seleccion_de_poder_optimo_para_IDW(muestra_valores_poder = valores.de.poder.IDW.a.evaluar, 
                                                capa_de_puntos = puntos, parametro = 'a', nueva_grilla = grd, 
                                                raster_de_referencia = raster.referencia, RSR=FALSE, 
                                                NSE=TRUE, PBIAS=FALSE, RMSE=FALSE, MAE=TRUE) # NSE-MAE
  
  poder.b <- seleccion_de_poder_optimo_para_IDW(muestra_valores_poder = valores.de.poder.IDW.a.evaluar, 
                                                capa_de_puntos = puntos, parametro = 'b', nueva_grilla = grd, 
                                                raster_de_referencia = raster.referencia, RSR=FALSE, 
                                                NSE=TRUE, PBIAS=FALSE, RMSE=FALSE, MAE=TRUE)
  
  mensaje <- paste0("Poder de 'a' fue ", poder.a, " y 'b' fue ", poder.b)
  message(mensaje)
  
  # Interpolacion
  puntos.a.idw <- idw(a ~ 1, puntos, newdata=grd, idp=poder.a, debug.level=0) # ~ 1, significa no hay variables independientes
  r.idw.a0 <- raster(puntos.a.idw)
  raster.referencia[] <- r.idw.a0[]
  r.idw.a <- raster.referencia
  
  puntos.b.idw <- idw(b ~ 1, puntos, newdata=grd, idp=poder.b, debug.level=0) # ~ 1, significa no hay variables independientes
  r.idw.b0 <- raster(puntos.b.idw)
  raster.referencia[] <- r.idw.b0[]
  r.idw.b <- raster.referencia
  
  # Extrayendo parametros estimados de cada estacion
  db.parametros.xy.sin.NA.i$a.estimado.idw <- extract(r.idw.a, db.parametros.xy.sin.NA.i[,c('lon', 'lat')]) 
  db.parametros.xy.sin.NA.i$b.estimado.idw <- extract(r.idw.b, db.parametros.xy.sin.NA.i[,c('lon', 'lat')]) 
  
  # Generando stack de rasters con parametros estimados a nivel de grilla
  names(r.idw.a) <- paste0('bloque_', i)
  names(r.idw.b) <- paste0('bloque_', i)
  
  if(i==1){stack.matrices.a <- r.idw.a} else(stack.matrices.a <- stack(stack.matrices.a, r.idw.a))
  if(i==1){stack.matrices.b <- r.idw.b} else(stack.matrices.b <- stack(stack.matrices.b, r.idw.b))
  
  # fin ---
  
  db <- rbind(db, db.parametros.xy.sin.NA.i)
  
}

# fin ---




# Preparando DB para correccion a nivel de estacion, usando parametros estimados ----

head(db)
head(db.estaciones)

# generando ID que compartan ambas DBs
db.estaciones$id <- paste(db.estaciones$fecha, db.estaciones$nombre_estacion, sep='_')
colnames(db.estaciones)[2] <- 'valor.estaciones'

head(db.era5)
db.era5$id <- paste(db.era5$fecha, db.era5$nombre_estacion, sep='_')
db.era5 <- db.era5[,-c(1,3)]
colnames(db.era5)[1] <- 'valor.era5'

head(db.estaciones)
head(db.era5)

# Union de DBs
db.estaciones.y.era5.preliminar <- merge(db.estaciones, db.era5, by='id')
db.estaciones.y.era5.preliminar$mes_y_dia <- paste(month(db.estaciones.y.era5.preliminar$fecha), 
                                                   day(db.estaciones.y.era5.preliminar$fecha),
                                                   sep = '-')
head(db.estaciones.y.era5.preliminar)

# Preparando DB de 'bloque sin buffer' para union
db.fechas.por.bloque.sin.buffer0 <- db.fechas.por.bloque[,c('mes_y_dia', 'bloque_sin_buffer')]
id.diferente.de.NA <- which(!is.na(db.fechas.por.bloque.sin.buffer0$bloque_sin_buffer))
db.fechas.por.bloque.sin.buffer <- db.fechas.por.bloque.sin.buffer0[id.diferente.de.NA,] # quitando NAs de la DB
head(db.fechas.por.bloque.sin.buffer)

# Union de DBs
db.estaciones.y.era5_2 <- merge(db.estaciones.y.era5.preliminar, 
                                db.fechas.por.bloque.sin.buffer, by = 'mes_y_dia', all.x=TRUE)
row.names(db.estaciones.y.era5_2) <- 1:nrow(db.estaciones.y.era5_2)

head(db)
head(db.estaciones.y.era5_2)

dim(db)
dim(db.estaciones.y.era5_2)

# Preparando DB de con parametros estimados para union
db$id <- paste(db$nombre_estacion, db$bloque, sep = '_')
db.para.merge <- db[,c('id', 'bloque', 'a', 'b', 'a.estimado.H', 'b.estimado.H', 
                       'c_de_a', 'd_de_a', 'c_de_b', 'd_de_b',
                       'a.estimado.idw', 'b.estimado.idw', 'H', 'lon', 'lat')]

db.estaciones.y.era5_2$id <- paste(db.estaciones.y.era5_2$nombre_estacion, 
                                   db.estaciones.y.era5_2$bloque_sin_buffer, sep = '_')

# Union de DBs
db.estaciones.y.era5_3 <- merge(db.estaciones.y.era5_2, db.para.merge, by = 'id', all.x=TRUE)
head(db.estaciones.y.era5_3)

dim(db.para.merge)
dim(db.estaciones.y.era5.preliminar)
dim(db.estaciones.y.era5_2)
dim(db.estaciones.y.era5_3)

# fin ---




# Correccion a nivel de estacion ----

# Correcion usando la ecuacion del metodo PRT
db.estaciones.y.era5_3$valor.era5.corregido <- db.estaciones.y.era5_3$a*(db.estaciones.y.era5_3$valor.era5^db.estaciones.y.era5_3$b)

head(db.estaciones.y.era5_3)
dim(db.estaciones.y.era5_3)


# Plot para estacion 'i', para un rango de fechas
nombre.estaciones
db.subset <- subset(db.estaciones.y.era5_3, nombre_estacion==nombre.estaciones[1])
rango.de.fechas <- 750:800
valor.maximo <- max(db.subset$valor.era5.corregido[rango.de.fechas], na.rm = TRUE)

dev.off()
plot(db.subset$valor.estaciones[rango.de.fechas], type='l', lwd=2, ylim=c(0, valor.maximo))
lines(db.subset$valor.era5[rango.de.fechas], col='red', lwd=2)
lines(db.subset$valor.era5.corregido[rango.de.fechas], col='green', lwd=2, lty=2)

legend('topright', legend = c('Observado', 'ERA5', 'PTR'), 
       lwd=c(2,2,2), 
       lty = c(1,1,2),
       col = c('black', 'red', 'green'))

# fin ---




# Preparacion de grillas ----

pr.sum.corregido0 <- pr.sum.total.original

fechas.inicio <- as.Date(pr.sum.corregido0$Dates$start)
fechas.termino <- as.Date(pr.sum.corregido0$Dates$end)
numero.de.matrices <- dim(pr.sum.corregido0$Data)[1]

# Descartando matrices con fecha 29 de febrero

db.matriz.por.fecha <- data.frame(fecha_inicio=fechas.inicio, fecha_termino=fechas.termino, 
                                  id=1:numero.de.matrices)

db.matriz.por.fecha$mes_y_dia_inicio <- paste(month(db.matriz.por.fecha$fecha_inicio), 
                                              day(db.matriz.por.fecha$fecha_inicio), sep = '-')
head(db.matriz.por.fecha)
dim(db.matriz.por.fecha)

id.a.eliminar <- which(db.matriz.por.fecha$mes_y_dia_inicio=='2-29')

pr.sum.corregido0$Dates$start <- pr.sum.corregido0$Dates$start[-id.a.eliminar]
pr.sum.corregido0$Dates$end <- pr.sum.corregido0$Dates$end[-id.a.eliminar]
pr.sum.corregido0$Data <- pr.sum.corregido0$Data[-id.a.eliminar,,]  

attr(pr.sum.corregido0$Data, "dimensions") <- c('time', 'lat', 'lon')

dim(pr.sum.corregido0$Data)
length(pr.sum.corregido0$Dates$start)
length(pr.sum.corregido0$Dates$end)

# fin ---




# Correcion a nivel de grilla ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
dem.clip.resemple <- raster('dem_regrillado_ngb_era5.tif')
matriz.altitud <- as.matrix(dem.clip.resemple)

pr.sum.corregido.idw <- pr.sum.corregido0
pr.sum.corregido.H <- pr.sum.corregido0
numero.de.matrices.2 <- dim(pr.sum.corregido.idw$Data)[1]

for (i in 1:numero.de.matrices.2) {
  # i <- 146

  # Seleccion de matriz con fecha 'i'
  
  matriz.i <- pr.sum.corregido.idw$Data[i,,]
  fecha.i <- as.Date(pr.sum.corregido.idw$Dates$start[i])
  mes_y_dia_i <- paste(month(fecha.i), day(fecha.i), sep = '-')
  
  
  
  
  # Correccion con metodo IDW ----
  message('Correccion con metodo IDW')
  
  # Seleccion de parametros 'a' y 'b' segun bloque 'i'
  
  bloque.i0 <- db.estaciones.y.era5_3$bloque_sin_buffer[db.estaciones.y.era5_3$mes_y_dia==mes_y_dia_i]
  bloque.i <- unique(bloque.i0)
  
  stack.matrices.a.i <- stack.matrices.a[[bloque.i]]
  stack.matrices.b.i <- stack.matrices.b[[bloque.i]]
  
  # Correccion de cada pixel de precipitacion 'j,k' segun altitud correspondiente
  
  for (j in 1:nrow(matriz.i)) {
    #  j <- 74
    
    for (k in 1:ncol(matriz.i)){
      #  k <- 74
      
      a.i <- stack.matrices.a.i[j,k]
      b.i <- stack.matrices.b.i[j,k]
      
      precipitacion.i <- matriz.i[j,k]
      
      precipitacion.corregido.i <- a.i*(precipitacion.i^b.i)
      matriz.i[j,k] <- precipitacion.corregido.i
      
    }
    
  }
  
  
  # reemplazando matriz de precipitacion 'i' por matriz corregida
  
  pr.sum.corregido.idw$Data[i,,] <- matriz.i
  
  # fin ---
  
  
  
  
  # Correccion con metodo H ----
  message('Correccion con metodo H')
  
  # Seleccion de matriz con fecha 'i'
  
  matriz.i <- pr.sum.corregido.H$Data[i,,]
  fecha.i <- as.Date(pr.sum.corregido.H$Dates$start[i])
  
  # Seleccion de parametros 'c' y 'd' segun bloque 'i'
  
  bloque.i0 <- db.estaciones.y.era5_3$bloque_sin_buffer[db.estaciones.y.era5_3$mes_y_dia==mes_y_dia_i]
  bloque.i <- bloque.i0
  
  c.a.preliminar <- db.estaciones.y.era5_3$c_de_a[db.estaciones.y.era5_3$bloque_sin_buffer%in%bloque.i]
  c.a.preliminar2 <- unique(c.a.preliminar)
  c.a <- c.a.preliminar2[!is.na(c.a.preliminar2)]
  
  d.a.preliminar <- db.estaciones.y.era5_3$d_de_a[db.estaciones.y.era5_3$mes_y_dia==mes_y_dia_i]
  d.a.preliminar2 <- unique(d.a.preliminar)
  d.a <- d.a.preliminar2[!is.na(d.a.preliminar2)]
  
  c.b.preliminar <- db.estaciones.y.era5_3$c_de_b[db.estaciones.y.era5_3$mes_y_dia==mes_y_dia_i]
  c.b.preliminar2 <- unique(c.b.preliminar)
  c.b <- c.b.preliminar2[!is.na(c.b.preliminar2)]
  
  d.b.preliminar <- db.estaciones.y.era5_3$d_de_b[db.estaciones.y.era5_3$mes_y_dia==mes_y_dia_i]
  d.b.preliminar2 <- unique(d.b.preliminar)
  d.b <- d.b.preliminar2[!is.na(d.b.preliminar2)]
  
  
  # Correccion de cada pixel de precipitacion 'j,k' segun altitud correspondiente
  
  for (j in 1:nrow(matriz.i)) {
    #  j <- 13
    
    for (k in 1:ncol(matriz.i)){
      #  k <- 13
      precipitacion.i <- matriz.i[j,k]
      altitud.i <- matriz.altitud[j,k]
      
      a.i <- funcion_de_poder(c.a, d.a, altitud.i)
      b.i <- funcion_de_poder(c.b, d.b, altitud.i)
      
      precipitacion.corregido.i <- a.i*(precipitacion.i^b.i)
      matriz.i[j,k] <- precipitacion.corregido.i
      
    }
    
  }
  
  
  # reemplazando matriz de precipitacion 'i' por matriz corregida
  
  pr.sum.corregido.H$Data[i,,] <- matriz.i
  
  mensaje.salida <- paste('Matriz', i, 'lista de', numero.de.matrices.2)
  message(mensaje.salida)
}

# fin ---




# Guardando grilla corregida ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

# Name of output file:
fileName.idw <- "pr_era5_corregido_idw.nc4"
fileName.H <- "pr_era5_corregido_H.nc4"

# Including a global attribute:
globalAttributeList <- list("institution" = "Centro Butamallin - Investigación en Cambio Climático")

# Including variable attributes:
varAttributeList <- list(var_attr1 = "Precipitación (mm)")

# Create file:
grid2nc(data = pr.sum.corregido.idw,
        NetCDFOutFile = fileName.idw,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList)

grid2nc(data = pr.sum.corregido.H,
        NetCDFOutFile = fileName.H,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList)

# fin ---




# Generando db con valores crudos y corregidos ----

# Cambiando formato de grilla desde 'climate4R' a una DB
era5.corregido.idw <- grid2sp(pr.sum.corregido.idw)
era5.corregido.H <- grid2sp(pr.sum.corregido.H)

db.era5.corregido.idw <- grilla_a_db(era5.corregido.idw, estaciones)
db.era5.corregido.H <- grilla_a_db(era5.corregido.H, estaciones)

# generando ID que compartan ambas DBs
db.era5.corregido.idw$id <- paste(db.era5.corregido.idw$fecha, db.era5.corregido.idw$nombre_estacion, sep = '_')
colnames(db.era5.corregido.idw)[2] <- 'valor.corregido.idw.en.grilla'
db.era5.corregido.idw <- db.era5.corregido.idw[,c('id', 'valor.corregido.idw.en.grilla')]
head(db.era5.corregido.idw)

db.era5.corregido.H$id <- paste(db.era5.corregido.H$fecha, db.era5.corregido.H$nombre_estacion, sep = '_')
colnames(db.era5.corregido.H)[2] <- 'valor.corregido.H.en.grilla'
db.era5.corregido.H <- db.era5.corregido.H[,c('id', 'valor.corregido.H.en.grilla')]
head(db.era5.corregido.H)

# Union de DBs
db.era5.corregido <- merge(db.era5.corregido.H, db.era5.corregido.idw, by='id')
head(db.era5.corregido)

dim(db.era5.corregido.H)
dim(db.era5.corregido.idw)
dim(db.era5.corregido)

# Preparacion de DB para union
head(db.estaciones.y.era5_3)
db.estaciones.y.era5_4 <- db.estaciones.y.era5_3[,c('fecha', 'nombre_estacion', 'H', 
                                                    'lon', 'lat', 'valor.estaciones', 
                                                    'valor.era5', 'valor.era5.corregido', 
                                                    'a', 'b', 'a.estimado.H', 'b.estimado.H',
                                                    'a.estimado.idw', 'b.estimado.idw')]

db.estaciones.y.era5_4$id <- paste(db.estaciones.y.era5_4$fecha, 
                                   db.estaciones.y.era5_4$nombre_estacion, sep = '_')

head(db.estaciones.y.era5_4)

# Union
db.con.valores.corregidos.preliminar <- merge(db.estaciones.y.era5_4, db.era5.corregido, by='id')
head(db.con.valores.corregidos.preliminar)

# Conservando variables de interes
db.con.valores.corregidos <- db.con.valores.corregidos.preliminar[,-1]

head(db.con.valores.corregidos)
dim(db.con.valores.corregidos)
dim(db.estaciones.y.era5_4)

# Guardando DB
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
write.csv(db.con.valores.corregidos, 'bias_correcion_por_estacion.csv', row.names = FALSE)

# fin ---




# Plots grillas no corregidas vs corregidas ----

raster.era5.corregido.H <- stack(grid2sp(pr.sum.corregido.H))
raster.era5.corregido.idw <- stack(grid2sp(pr.sum.corregido.idw))

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/plots/')
# png('mapa_altitud_era5Original_era5Corregida.png', width = 780, height = 780, units = "px")

par(mfrow=c(1,3))

plot(pr.sum.total.stack, 1) # '01/01/1995 ERA5'
plot(raster.era5.corregido.H, 1) # '01/01/1995 ERA5-CORREGIDO H'
plot(raster.era5.corregido.idw, 1) # '01/01/1995 ERA5-CORREGIDO IDW'

plot(pr.sum.total.stack, 15) # '01/01/1995 ERA5'
plot(raster.era5.corregido.H, 15) # '01/01/1995 ERA5-CORREGIDO H'
plot(raster.era5.corregido.idw, 15) # '01/01/1995 ERA5-CORREGIDO IDW'

plot(pr.sum.total.stack, 30) # '01/01/1995 ERA5'
plot(raster.era5.corregido.H, 30) # '01/01/1995 ERA5-CORREGIDO H'
plot(raster.era5.corregido.idw, 30) # '01/01/1995 ERA5-CORREGIDO IDW'

# dev.off()

# fin ---
