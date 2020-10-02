rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_generador_de_matriz_archivos_DGA.R')
source('funcion_anhadiendo_altitud.R')

setwd('C:/Users/Usuario/Documents/Francisco/WRF/proyecto_WRF/proyecto_WRF_complementario/')
source('funcion_nombre_estacion.R')

# Matriz de valores ----
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/dga/')
lista.de.archivos <- list.files() ; lista.de.archivos
variable <- 'precip' # precip # tmean # tmin # tmax

matriz <- generador_de_matriz_archivos_DGA(lista.de.archivos)
head(matriz)

# save

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')

# write.table(matriz.tmin, file='tmin.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')
# 
# write.table(matriz.tmax, file='tmax.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')
# 
write.table(matriz, file='precip.txt', row.names=FALSE, col.names=TRUE, sep=", ",
            append=FALSE, quote=FALSE, na = 'NaN')


# fin ---




# # Tranformacion unidades de coordenadas ----
# 
# library(biogeo)
# 
# latitud <- dms2dd(46, 50, 33, 'S') ; latitud
# longitud <- dms2dd(72, 42, 21, 'W') ; longitud
# 
# # fin ---




# Referencia archivos ASCII ----

# estaciones

setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
db.estaciones <- read.csv('db_todas_las_instituciones.csv', sep = ';')
head(db.estaciones)

db.estaciones$nombre_estacion_depurado <- nombre_estacion(db.estaciones$names_modi)
db.estaciones2 <- db.estaciones[,c('nombre_estacion_depurado', 'lat', 'lon')]

match <- which(db.estaciones2$nombre_estacion_depurado%in%nombre_estacion(lista.de.archivos))
estaciones.lista.de.archivos0 <- db.estaciones2[match,]
estaciones.lista.de.archivos <- estaciones.lista.de.archivos0[!duplicated(estaciones.lista.de.archivos0$nombre_estacion_depurado), ]

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
dir()

estaciones <- read.table("stations.txt", sep = ",", header = TRUE)
head(estaciones)
head(db.estaciones)

estaciones.db.todos <- data.frame(station_id = lista.de.archivos, name = lista.de.archivos,
                                   longitude = estaciones.lista.de.archivos$lon, latitude = estaciones.lista.de.archivos$lat,
                                   altitude = NaN, source = 'DGA')
estaciones.db.todos

estaciones.db.todos <- anhadiendo_altitud(estaciones.db.todos, sistema_de_coordenadas = 'wgs84')
head(estaciones.db.todos)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
write.table(estaciones.db.todos, file='stations.txt', row.names=FALSE, col.names=TRUE, sep=", ",
            append=FALSE, quote=FALSE, na = 'NaN')




# informacion de variables
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
variables <- read.table("variables.txt", sep = ",", header = TRUE)
variables$missing_code <- NaN

variables1 <- variables
id <- which(variables1$variable_id%in%variable)

variables2 <- variables1[id,]
variables2

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
write.table(variables2, file='variables.txt', row.names=FALSE, col.names=TRUE, sep=", ",
            append=FALSE, quote=FALSE, na = 'NaN')

# fin ---

