library(PerformanceAnalytics)
library(rgdal)
library(raster)

rm(list=ls())
dev.off()

# Llamada de funciones ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR')
source('funcion_anhadiendo_altitud.R')
source('funcion_localizacion_directorio_de_trabajo.R')
source('funcion_generador_de_matriz.R')
source('funcion_union_db_valores_observados_y_coordenadas.R')

# fin ---



# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
db.estaciones <- read.csv('db_todas_las_instituciones.csv', sep = ';')
tail(db.estaciones)

# fin ---



# Preparacion de db ----

anhos.interes <- 2008:2017

# tmin
variable.de.interes <- 'tmin'

directorio.de.trabajo <- localizacion_directorio_de_trabajo(variable.de.interes)
setwd(directorio.de.trabajo)
archivos.tmin <- list.files()

db.tmin <- union_db_valores_observados_y_coordenadas(archivos.tmin, variable.de.interes, directorio.de.trabajo)

head(db.tmin)
tail(db.tmin)
dim(db.tmin)


# tmax
variable.de.interes <- 'tmax'

directorio.de.trabajo <- localizacion_directorio_de_trabajo(variable.de.interes)
setwd(directorio.de.trabajo)
archivos.tmax <- list.files()

db.tmax <- union_db_valores_observados_y_coordenadas(archivos.tmax, variable.de.interes, directorio.de.trabajo)

head(db.tmax)
tail(db.tmax)
dim(db.tmax)


# pp
variable.de.interes <- 'pp'

directorio.de.trabajo <- localizacion_directorio_de_trabajo(variable.de.interes)
setwd(directorio.de.trabajo)
archivos.pp <- list.files()

db.pp <- union_db_valores_observados_y_coordenadas(archivos.pp, variable.de.interes, directorio.de.trabajo)
db.pp$variable <- 'precip'

head(db.pp)
tail(db.pp)
dim(db.pp)


# union de db's

db.todos <- rbind(db.pp)#, db.tmax)#, db.pp)

head(db.todos)
tail(db.todos)
table(db.todos$variable)

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
id <- which(variables1$variable_id%in%unique(db.todos$variable))

variables2 <- variables1[id,]
variables2

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
# write.table(variables2, file='variables.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')


# variable
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
variable.i <- read.table("tmin.txt", sep = ",", header = TRUE)
head(variable.i)


# matriz.tmin <- generador_de_matriz(db.tmin) ; head(matriz.tmin)
# matriz.tmax <- generador_de_matriz(db.tmax) ; head(matriz.tmax)
matriz.pp <- generador_de_matriz(db.pp) ; head(matriz.pp)

# save

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')

# write.table(matriz.tmin, file='tmin.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')
# 
# write.table(matriz.tmax, file='tmax.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')
# 
# write.table(matriz.pp, file='precip.txt', row.names=FALSE, col.names=TRUE, sep=", ",
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
