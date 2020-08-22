library(PerformanceAnalytics)
library(ncdf4)
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

setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
area.de.estudio <- raster('dem_clip.tif')
plot(area.de.estudio)

setwd('C:/Users/Usuario/Documents/Francisco/var_predictoras/CCSM4/rcp45/pp/2011')
raster.referencia <- raster('NASA_NEX_GDDP_CCSM4__RCP45_pp_ver_2011_04.tif')
plot(raster.referencia)

# fin ---




# Preparacion de db ----

anhos.interes <- 2008:2017
variable.de.interes <- 'tmin'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/ParaTaylorTmin/')
archivos.i <- list.files()
db.todos <- c()

# Loop funciona en R version 4.0.2!

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





subset.i <- subset(db.todos, anho==2010 & mes==1)
row.names(subset.i) <- 1:nrow(subset.i)
plot(subset.i$lon, subset.i$lat)

# Transformacion a Spatial Points Data Frame
subset.i.sp <- SpatialPoints(list(subset.i$lon, subset.i$lat), proj4string = crs(raster.referencia))
subset.i.spd <- SpatialPointsDataFrame(subset.i.sp, data = subset.i, match.ID = TRUE)

plot(subset.i.spd, pch = 16, col = 'red', axes=TRUE)
text(subset.i.spd, subset.i.spd$archivo.con.datos.climaticos, pos = 3, cex=0.7)

# Rasterizacion
plot(raster.referencia)
raster.referencia.clip <- crop(raster.referencia, area.de.estudio)
plot(raster.referencia.clip)

r.subset.i <- rasterize(subset.i.spd, raster.referencia.clip, field="valor.observado", fun="last", background=NA)
plot(r.subset.i)
# zoom(r.subset.i, ext=drawExtent())
r.subset.i

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
# writeRaster(r.subset.i,'downscaled_WRF_referencia.asc', format='ascii', overwrite=TRUE)


r <- raster('downscaled_WRF_referencia.asc')
plot(r)
summary(r)
