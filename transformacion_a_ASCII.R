library(PerformanceAnalytics)
library(raster)
library(rgdal)

rm(list=ls())
dev.off()

# Llamada de funciones ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR')
source('funcion_anhadiendo_coordenadas.R')
source('funcion_verificacion_anhos_de_interes.R')
source('funcion_coincidencia_entre_nombres_de_archivos.R')
source('funcion_identificador_de_archivo.R')
source('funcion_anhadiendo_altitud.R')
source('funcion_datos_observados_en_formato_ts.R')

setwd('C:/Users/Usuario/Documents/Francisco/WRF/proyecto_WRF/proyecto_WRF_complementario/')
source('funcion_nombre_estacion.R')
source('funcion_db_a_serie_de_tiempo.R')
source('funcion_preparacion_db_para_serie_de_tiempo.R')


# fin ---



# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
db.estaciones <- read.csv('db_todas_las_instituciones.csv', sep = ';')
tail(db.estaciones)

# fin ---



# Preparacion de db ----

anhos.interes <- 2008:2017
variable.de.interes <- 'tmin'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/ParaTaylorTmin/')
archivos.i <- list.files()
db.todos <- c()

for (i in 1:length(archivos.i)) {
 #i <- 2
  print(i)
  
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/ParaTaylorTmin/')
  # Lectura de archivos
  db.i <- read.csv(archivos.i[i])
  
  # Testeo y omicion de archivos sin datos para los anhos de interes
  anhos.observados.i <- verificacion_anhos_de_interes(db.i)
  match.i <- anhos.interes[anhos.interes%in%anhos.observados.i]
  if( length(match.i)==0 ){c(message( paste(archivos.i[i], 'no tiene datos para los anhos de interes!', sep = ' ') ), 
                             next)}
  
  # Preparacion de archivo final
  db2.i <- datos_observados_en_formato_ts(db.i, anhos.interes, variable = variable.de.interes)
  db3.i <- anhadiendo_coordenadas(db2.i, archivos.i[i], variable = variable.de.interes)
  db.todos <- rbind(db.todos, db3.i)
  
}

head(db.todos)
tail(db.todos)

table(db.todos$archivo.con.datos.climaticos)
table(db.todos$anho)
dim(db.todos)

# fin ---




# Referencia archivos ASCII ----

# estaciones
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
dir()

estaciones <- read.table("stations.txt", sep = ",", header = TRUE)
head(estaciones)
head(db.todos)

estaciones.db.todos0 <- data.frame(station_id = db.todos$archivo.con.coordenadas, name = db.todos$archivo.con.coordenadas,
                                  longitude = db.todos$lon, latitude = db.todos$lat, altitude = NaN, 
                                  source = db.todos$archivo.con.datos.climaticos)
estaciones.db.todos <- estaciones.db.todos0[!duplicated(estaciones.db.todos0$name), ]
estaciones.db.todos

estaciones.db.todos <- anhadiendo_altitud(estaciones.db.todos, sistema_de_coordenadas = 'wgs84')
head(estaciones.db.todos)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
# write.table(estaciones.db.todos, file='stations.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')




# informacion de variables
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
variables <- read.table("variables.txt", sep = ",", header = TRUE)
variables$missing_code <- NaN

variables1 <- variables
id <- which(variables1$variable_id%in%variable.de.interes)

variables2 <- variables1[id,]
variables2

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
# write.table(variables2, file='variables.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')


# variable
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
variable.i <- read.table("tmin.txt", sep = ",", header = TRUE)
head(variable.i)

head(db.todos)
anho.mes <- gsub('-', '', db.todos$Date) ; head(anho.mes)
anho.mes.dia <- paste(anho.mes, '01', sep = '') ; anho.mes.dia

db.todos$YYYYMMDD <- anho.mes.dia
head(db.todos)

db.todos.valores <- db.todos[, c("YYYYMMDD", "archivo.con.coordenadas", "valor.observado")]
head(db.todos.valores)

nombre.archivos.i <- unique( as.character(db.todos.valores$archivo.con.coordenadas) )

for (i in 1:length(nombre.archivos.i)) {
  # i <- 2
  db.todos.valores.i <- subset(db.todos.valores, archivo.con.coordenadas==nombre.archivos.i[i])
  db.todos.valores.i2 <- db.todos.valores.i[, c("YYYYMMDD", "valor.observado")]
  colnames(db.todos.valores.i2)[2] <- nombre.archivos.i[i]
  db.todos.valores.i2
  
  if(i==1){matriz.de.valores.por.estacion0 <- db.todos.valores.i2
  } else(matriz.de.valores.por.estacion0 <- merge(matriz.de.valores.por.estacion0, db.todos.valores.i2, by='YYYYMMDD', all=TRUE))
  
}

matriz.de.valores.por.estacion <- matriz.de.valores.por.estacion0

for (j in 2:ncol(matriz.de.valores.por.estacion0)) {
  matriz.de.valores.por.estacion[,j][matriz.de.valores.por.estacion[,j]%in%NA] <- NaN
}

matriz.de.valores.por.estacion

##
# Hay que darle este formato!!

# YYYYMMDD, estacion_1, estacion_2, estacion_3 
# 20100101, valor,      valor,      valor
# 20100201, valor,      valor,      valor
# 20100301, valor,      valor,      valor

##

# ---

# fecha.i <- unique(db.todos.valores$YYYYMMDD)[1]
# id <- which(db.todos.valores$YYYYMMDD%in%fecha.i)
# db.todos.valores.i <- db.todos.valores[id, c("archivo.con.coordenadas", "valor.observado")]
# db.todos.valores.i$archivo.con.coordenadas <- as.character(db.todos.valores.i$archivo.con.coordenadas)
# str(db.todos.valores.i)

# db.todos.valores.i.2 <- t( db.todos.valores.i[, "valor.observado"] )
# colnames(db.todos.valores.i.2) <- db.todos.valores.i$archivo.con.coordenadas
# db.todos.valores.i.2


# ---

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
# write.table(matriz.de.valores.por.estacion0, file='tmin.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')

# fin ---




# a raster ASCII ----
# setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# area.de.estudio <- raster('dem_clip.tif')
# plot(area.de.estudio)
# 
# setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/CCSM4/rcp45/pp/2011')
# raster.referencia <- raster('NASA_NEX_GDDP_CCSM4__RCP45_pp_ver_2011_04.tif')
# plot(raster.referencia)
# 
# subset.i <- subset(db.todos, anho==2010 & mes==1)
# row.names(subset.i) <- 1:nrow(subset.i)
# plot(subset.i$lon, subset.i$lat)
# 
# # Transformacion a Spatial Points Data Frame
# subset.i.sp <- SpatialPoints(list(subset.i$lon, subset.i$lat), proj4string = crs(raster.referencia))
# subset.i.spd <- SpatialPointsDataFrame(subset.i.sp, data = subset.i, match.ID = TRUE)
# 
# plot(subset.i.spd, pch = 16, col = 'red', axes=TRUE)
# text(subset.i.spd, subset.i.spd$archivo.con.datos.climaticos, pos = 3, cex=0.7)
# 
# # Rasterizacion
# plot(raster.referencia)
# raster.referencia.clip <- crop(raster.referencia, area.de.estudio)
# plot(raster.referencia.clip)
# 
# r.subset.i <- rasterize(subset.i.spd, raster.referencia.clip, field="valor.observado", fun="last", background=NA)
# plot(r.subset.i)
# # zoom(r.subset.i, ext=drawExtent())
# r.subset.i
# 
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
# # writeRaster(r.subset.i,'downscaled_WRF_referencia.asc', format='ascii', overwrite=TRUE)
# 
# 
# r <- raster('downscaled_WRF_referencia.asc')
# plot(r)
# summary(r)
