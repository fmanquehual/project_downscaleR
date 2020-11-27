library(loadeR)
library(loadeR.2nc)
library(visualizeR)
library(downscaleR)
library(convertR)
library(lattice)
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
source('funcion_nuevas_fechas.R')
source('funcion_formato_datos_para_climate4R.R')
source('funcion_ordenar_por_fecha.R')
source('funcion_prt.R')
source('funcion_generador_de_fechas.R')
source('funcion_bloque_de_dias.R')
source('funcion_ptr_por_bloque.R')
source('funcion_calculo_de_resolucion.R')
source('funcion_seleccion_de_poder_optimo_para_IDW.R')

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




# Lectura de capas ----
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
cuenca <- readOGR('.', 'poligono_cuenca_baker_geo')

# fin ---




# Parametros ----

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84

valores.de.poder.IDW.a.evaluar <- seq(1, 30, by=0.5)

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
db.era5 <- c()
db.parametros <- c()
tz.i <- 'GMT'

# fin ---




# Correccion mensual ----

for (j in 1) {
  
  mensaje.inicio <- paste('Inicio de proceso ', '--------------------------------------------------------------')
  message(mensaje.inicio)
  
  meses <- 1:12
  
  
  
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
  
  
  # Predictors (ERA reanalisis) 
  
  # message('Leyendo datos de ERA5')
  
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
  era5 <- 'ERA5_1979_2018_pp.nc'
  
  # Precipitacion
  # C4R.vocabulary()
  
  message('Transformando unidades de variable')
  
  # Datos de entrenamiento
  # dataInventory(era5)
  pr.sum0.entrenamiento <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, 
                                                      es.cmip6 = FALSE, anhos = anhos.entrenamiento)
  pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")
  
  
  # Datos totales
  
  pr.sum0.total <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, es.cmip6 = FALSE, anhos = anhos.total)
  pr.sum.total <- udConvertGrid(pr.sum0.total, new.units = "mm")
  
  pr.sum.total.original <- pr.sum.total
  
  
  # Obteniendo parametros de metodo ptr ----
  
  message("Obteniendo parametro 'a' y 'b' de metodo PTR")
  
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
  
  # fin ---
  
  
  
  
  # Valores observados ----
  
  message('Valores observados')
  
  db.estaciones <- lista_climate4R_a_db(estaciones.con.todos.los.anhos)
  
  # fin ---
  
  
  
  
  # Extrayendo valores de los pixeles donde estan ubicados las estaciones ----
  
  message('Extrayendo valores de celda donde estan ubicados las estaciones')
  
  era5.en.ubicacion.de.estacion.i <- grid2sp(pr.sum.total)
  db.era5 <- grilla_a_db(era5.en.ubicacion.de.estacion.i, estaciones)
  
}

# fin ---




# Estimando 'a' y 'b' con IDW ----

nombre.estaciones <- estaciones$Metadata$name
altitud.de.estaciones <- estaciones$Metadata$altitude
x <- estaciones$xyCoords$x
y <- estaciones$xyCoords$y

db.estaciones.xy <- data.frame(nombre_estacion=nombre.estaciones, lon=x, lat=y)
db.parametros.xy <- merge(db.parametros, db.estaciones.xy, by='nombre_estacion')

head(db.parametros.xy)

pr.sum.total.stack <- stack(era5.en.ubicacion.de.estacion.i)
pr.sum.total.i <- pr.sum.total.stack[[1]]
raster.referencia <- pr.sum.total.i
p.puntos <- rasterToPolygons(pr.sum.total.i, dissolve = TRUE)

bloques.unicos <- sort(unique(db.parametros.xy$bloque))
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
  
  
  # Transformacion a Spatial Points Data Frame
  
  puntos0 <- SpatialPoints(cbind(db.parametros.xy.sin.NA.i$lon, db.parametros.xy.sin.NA.i$lat), 
                           proj4string = CRS(wgs84))
  puntos <- SpatialPointsDataFrame(puntos0, data = db.parametros.xy.sin.NA.i, match.ID = TRUE)
  
  r.puntos.a <- rasterize(puntos, pr.sum.total.i, field="a", fun=mean, background=NA)
  r.puntos.b <- rasterize(puntos, pr.sum.total.i, field="b", fun=mean, background=NA)
  
  puntos@bbox <- p.puntos@bbox
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(p.puntos, "regular", n=dim(pr.sum.total.i)[1]*dim(pr.sum.total.i)[2]))
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
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  puntos.a.idw <- idw(a ~ 1, puntos, newdata=grd, idp=poder.a, debug.level=0) # ~ 1, significa no hay variables independientes
  r.idw.a0 <- raster(puntos.a.idw)
  raster.referencia[] <- r.idw.a0[]
  r.idw.a <- raster.referencia
  
  puntos.b.idw <- idw(b ~ 1, puntos, newdata=grd, idp=poder.b, debug.level=0) # ~ 1, significa no hay variables independientes
  r.idw.b0 <- raster(puntos.b.idw)
  raster.referencia[] <- r.idw.b0[]
  r.idw.b <- raster.referencia
  
  # setwd('C:/Users/Usuario/Desktop/')
  # png('IDW_parametro_a_peso_de_5.png', width = 720, height = 720, units = "px", pointsize = 15)
  # plot(r.idw.a)
  # plot(cuenca, add=TRUE, border='cyan')
  # text(r.idw.a, round(r.idw.a[],1), cex=0.7)
  # text(puntos, round(puntos$a,1), col='red', cex=0.7)
  # dev.off()
  
  db.parametros.xy.i$a_estimado <- extract(r.idw.a, db.parametros.xy.i[,c('lon', 'lat')]) 
  db.parametros.xy.i$b_estimado <- extract(r.idw.b, db.parametros.xy.i[,c('lon', 'lat')]) 
  
  par(mfrow=c(2,1))
  plot(db.parametros.xy.i$a-db.parametros.xy.i$a_estimado, xlab="Estacion", ylab="Observado-Estimado (a)")
  abline(h=0, col='red')
  plot(db.parametros.xy.i$b-db.parametros.xy.i$b_estimado, xlab="Estacion", ylab="Observado-Estimado (b)")
  abline(h=0, col='red')
  
  names(r.idw.a) <- paste0('bloque_', i)
  names(r.idw.b) <- paste0('bloque_', i)
  
  if(i==1){stack.matrices.a <- r.idw.a} else(stack.matrices.a <- stack(stack.matrices.a, r.idw.a))
  if(i==1){stack.matrices.b <- r.idw.b} else(stack.matrices.b <- stack(stack.matrices.b, r.idw.b))
  
  db <- rbind(db, db.parametros.xy.i)

}

# fin ---




# Exportando puntos de bloque i ----

# puntos0 <- SpatialPoints(cbind(db.parametros.xy.i$lon, db.parametros.xy.i$lat), 
#                          proj4string = CRS(wgs84))
# row.names(db.parametros.xy.i) <- 1:nrow(db.parametros.xy.i)
# puntos <- SpatialPointsDataFrame(puntos0, data = db.parametros.xy.i, match.ID = TRUE)
# 
# # setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
# # writeOGR(puntos, ".", "puntos_bloque_i", driver="ESRI Shapefile",
# #         overwrite_layer = TRUE)

# fin ---




# Preparando db para correccion a nivel de estacion ----

head(db)
head(db.estaciones)

db.estaciones$id <- paste(db.estaciones$fecha, db.estaciones$nombre_estacion, sep='_')
colnames(db.estaciones)[2] <- 'valor.estaciones'

head(db.era5)
db.era5$id <- paste(db.era5$fecha, db.era5$nombre_estacion, sep='_')
db.era5 <- db.era5[,-c(1,3)]
colnames(db.era5)[1] <- 'valor.era5'

head(db.estaciones)
head(db.era5)

db.estaciones.y.era5.preliminar <- merge(db.estaciones, db.era5, by='id')
head(db.estaciones.y.era5.preliminar)

altitud.de.estaciones <- estaciones$Metadata$altitude
db.estacion.altitud <- data.frame(nombre_estacion=nombre.estaciones, H=altitud.de.estaciones)
head(db.estacion.altitud)

db.estaciones.y.era5_1 <- merge(db.estaciones.y.era5.preliminar, db.estacion.altitud, by='nombre_estacion', all.x=TRUE)
db.estaciones.y.era5_1$mes_y_dia <- paste(month(db.estaciones.y.era5_1$fecha), day(db.estaciones.y.era5_1$fecha),
                                          sep = '-')
head(db.estaciones.y.era5_1) 

db.fechas.por.bloque.sin.buffer0 <- db.fechas.por.bloque[,c('mes_y_dia', 'bloque_sin_buffer')]
id.diferente.de.NA <- which(!is.na(db.fechas.por.bloque.sin.buffer0$bloque_sin_buffer))
db.fechas.por.bloque.sin.buffer <- db.fechas.por.bloque.sin.buffer0[id.diferente.de.NA,]
head(db.fechas.por.bloque.sin.buffer)

db.estaciones.y.era5_2 <- merge(db.estaciones.y.era5_1, db.fechas.por.bloque.sin.buffer, by = 'mes_y_dia', all.x=TRUE)
row.names(db.estaciones.y.era5_2) <- 1:nrow(db.estaciones.y.era5_2)

head(db)
head(db.estaciones.y.era5_2)

dim(db)
dim(db.estaciones.y.era5_2)

db$id <- paste(db$nombre_estacion, db$bloque, sep = '_')
db.para.merge <- db[,c('id', 'a', 'b', 'a_estimado', 'b_estimado', 'lon', 'lat')]

db.estaciones.y.era5_2$id <- paste(db.estaciones.y.era5_2$nombre_estacion, 
                                   db.estaciones.y.era5_2$bloque_sin_buffer, sep = '_')

db.estaciones.y.era5_3 <- merge(db.estaciones.y.era5_2, db.para.merge, by = 'id', all.x=TRUE)
head(db.estaciones.y.era5_3)

dim(db.para.merge)
dim(db.estaciones.y.era5_1)
dim(db.estaciones.y.era5_2)
dim(db.estaciones.y.era5_3)

# fin ---




# Correccion a nivel de estacion ----

db.estaciones.y.era5_3$valor.era5.corregido <- db.estaciones.y.era5_3$a_estimado*(db.estaciones.y.era5_3$valor.era5^db.estaciones.y.era5_3$b_estimado)

head(db.estaciones.y.era5_3)
dim(db.estaciones.y.era5_3)


# Plot

nombre.estaciones
db.subset <- subset(db.estaciones.y.era5_3, nombre_estacion==nombre.estaciones[1])
rango.de.valores <- 850:900
valor.maximo <- max(db.subset$valor.era5.corregido[rango.de.valores], na.rm = TRUE)

dev.off()
plot(db.subset$valor.estaciones[rango.de.valores], type='l', lwd=2, ylim=c(0, valor.maximo))
lines(db.subset$valor.era5[rango.de.valores], col='red', lwd=2)
lines(db.subset$valor.era5.corregido[rango.de.valores], col='blue', lwd=2)

legend('topright', legend = c('Observado', 'ERA5', 'PTR'), 
       lwd=c(2,2,2), 
       col = c('black', 'red', 'blue'))

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

pr.sum.corregido <- pr.sum.corregido0
numero.de.matrices.2 <- dim(pr.sum.corregido$Data)[1]

for (i in 1:numero.de.matrices.2) {
  #  i <- 1
  
  mensaje.inicio <- paste('Matriz', i, 'lista de', numero.de.matrices.2)
  message(mensaje.inicio)
  
  # Seleccion de matriz con fecha 'i'
  
  matriz.i <- pr.sum.corregido$Data[i,,]
  fecha.i <- as.Date(pr.sum.corregido$Dates$start[i])
  mes_y_dia_i <- paste(month(fecha.i), day(fecha.i), sep = '-')
  
  
  # Seleccion de parametros 'a' y 'b' segun bloque 'i'
  
  bloque.i <- db.fechas.por.bloque.sin.buffer$bloque_sin_buffer[db.fechas.por.bloque.sin.buffer$mes_y_dia==mes_y_dia_i]
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
  
  pr.sum.corregido$Data[i,,] <- matriz.i
  
}

# fin ---




# Guardando grilla corregida ----

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
# 
# # Name of output file:
# fileName <- "pr_era5_corregido.nc4"
# 
# # Including a global attribute:
# globalAttributeList <- list("institution" = "Centro Butamallin - Investigación en Cambio Climático")
# 
# # Including variable attributes:
# varAttributeList <- list(var_attr1 = "Precipitación (mm)")
# 
# # Create file:
# grid2nc(data = pr.sum.corregido,
#         NetCDFOutFile = fileName,
#         missval = 1e20,
#         prec = "float",
#         globalAttributes = globalAttributeList,
#         varAttributes = varAttributeList)
# 
# # library(ncdf4)
# # ej <- nc_open('pr_era5_corregido_2.nc4')
# # ej
# 
# 
# # Create file:
# crs(stack.matrices.a) <- wgs84
# writeRaster(stack.matrices.a[[nlayers(stack.matrices.a)]], filename='pr_era5_parametro_a.tiff', 
#             format="GTiff", overwrite=TRUE)

# fin ---




# Generando db con valores crudos y corregidos ----

era5.corregido <- grid2sp(pr.sum.corregido)
db.era5.corregido <- grilla_a_db(era5.corregido, estaciones)
db.era5.corregido$id <- paste(db.era5.corregido$fecha, db.era5.corregido$nombre_estacion, sep = '_')
colnames(db.era5.corregido)[2] <- 'valor.corregido.en.grilla'
db.era5.corregido <- db.era5.corregido[,c('id', 'valor.corregido.en.grilla')]
head(db.era5.corregido)

head(db.estaciones.y.era5_3)
db.estaciones.y.era5_4 <- db.estaciones.y.era5_3[,c('fecha', 'nombre_estacion', 'H', 
                                                    'lon', 'lat', 'valor.estaciones', 
                                                    'valor.era5', 'valor.era5.corregido', 
                                                    'a', 'b', 'a_estimado', 'b_estimado')]

db.estaciones.y.era5_4$id <- paste(db.estaciones.y.era5_4$fecha, 
                                   db.estaciones.y.era5_4$nombre_estacion, sep = '_')

db.con.valores.corregidos.preliminar <- merge(db.estaciones.y.era5_4, db.era5.corregido, by='id')
head(db.con.valores.corregidos.preliminar)

db.con.valores.corregidos <- db.con.valores.corregidos.preliminar[,c(
  'fecha', 'nombre_estacion', 'H', 
  'lon', 'lat', 'valor.estaciones', 
  'valor.era5', 'valor.era5.corregido',
  'valor.corregido.en.grilla',
  'a', 'b', 'a_estimado', 'b_estimado')]

head(db.con.valores.corregidos)
dim(db.con.valores.corregidos)
dim(db.estaciones.y.era5_4)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
# write.csv(db.con.valores.corregidos, 'bias_correcion_por_estacion.csv', row.names = FALSE)

# fin ---




# Plots grillas no corregidas vs corregidas ----

raster.era5.corregido <- stack(grid2sp(pr.sum.corregido))

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/plots/')
# png('mapa_altitud_era5Original_era5Corregida.png', width = 780, height = 780, units = "px")

par(mfrow=c(1,2))

plot(pr.sum.total.stack, 1) # '01/01/1995 ERA5'
plot(raster.era5.corregido, 1) # '01/01/1995 ERA5-CORREGIDO'

plot(pr.sum.total.stack, 15) # '15/01/1995 ERA5'
plot(raster.era5.corregido, 15) # '15/01/1995 ERA5-CORREGIDO'

plot(pr.sum.total.stack, 30) # '30/01/1995 ERA5'
plot(raster.era5.corregido, 30) # '30/01/1995 ERA5-CORREGIDO'

# dev.off()

# fin ---
