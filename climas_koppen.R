library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(dplyr)

rm(list=ls())
dev.off()


# Lectura de datos ---

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/climas_koppen/')
db0 <- read.csv('codificacion_koppen_para_join.csv', sep=';')
db0

db <- db0[,c('id_principal', 'clase_principal')]
db <- db[!duplicated(db$id_principal),]
db

# fin --




# lectura coberturas ----

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

setwd('C:/Users/Usuario/Documents/Francisco/coberturas/climas_koppen/rasters/')
clima.presente <- raster('Beck_KG_V1_present_0p0083.tif')
plot(clima.presente)

setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
marco <- readOGR('.', 'marco_area_estudio')
plot(marco, axes=TRUE)

# fin ---




# Clip de capa Climas-Koppen ----

clima.presente.clip <- crop(clima.presente, marco)
plot(clima.presente.clip)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
# writeRaster(clima.presente.clip, 
# filename='climas_koppen_sin_reclasificar_marco_cuenca_Baker_geo.tif', format="GTiff", overwrite=TRUE)

# fin ---




# reclasificacion ----

m1 = c(0, 0, NA,
       1, 3, 1,
       4, 7, 2,
       8, 16, 3,
       17, 28, 4,
       28, 30, 5)

matriz.reclasificacion = matrix(m1, ncol=3, byrow=TRUE)
clima.presente.reclasificado <- reclassify(clima.presente.clip, matriz.reclasificacion, 
                                           include.lowest=TRUE)

unique(values(clima.presente.reclasificado))
plot(clima.presente.reclasificado, col=bpy.colors(5))

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')

# writeRaster(clima.presente.reclasificado, 
# filename='climas_koppen_marco_cuenca_Baker_geo.tif', format="GTiff", overwrite=TRUE)

# fin ---




# Raster a poligono ----

clima.presente.poligono <- rasterToPolygons(clima.presente.reclasificado, 
                                fun=function(x){x>0}, dissolve = TRUE)
plot(clima.presente.poligono, col=terrain.colors(5), border=terrain.colors(5))

head(clima.presente.poligono@data)
names(clima.presente.poligono@data) <- 'id_principal'

head(clima.presente.poligono@data)
head(db)


clima.presente.poligono@data <- left_join(clima.presente.poligono@data, db, 
                                          by.y='id_principal')

clima.presente.poligono.utm18s <- spTransform(clima.presente.poligono, utm18)

plot(clima.presente.poligono.utm18s, axes=TRUE,
     col=terrain.colors(5), border=terrain.colors(5))

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
# writeOGR(clima.presente.poligono, ".", "poligono_climas_koppen_marco_cuenca_Baker_geo", driver="ESRI Shapefile", overwrite_layer = TRUE)
# writeOGR(clima.presente.poligono.utm18s, ".", "poligono_climas_koppen_marco_cuenca_Baker_utm18s", driver="ESRI Shapefile", overwrite_layer = TRUE)

# fin ---