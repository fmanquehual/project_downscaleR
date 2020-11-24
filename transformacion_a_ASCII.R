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
source('funcion_db_a_matriz.R')
source('funcion_identificador_de_nombres_similares.R')
source('funcion_depurador_nombre_archivos_csv.R')
source('funcion_union_db_valores_observados_y_coordenadas.R')


# fin ---



# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/WRF/Estadisticos/puntajes_finales_y_estaciones_para_mapas/')
db.estaciones <- read.csv('db_todas_las_instituciones.csv', sep = ';')
tail(db.estaciones)

# fin ---



# Preparacion de db ----

anhos.interes <- 1950:2018

# tmin
# carpeta.madre <- 'C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/'
carpeta.madre <- 'C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/DatosAysen_Baker/'
variable.de.interes <- 'tmin'

localizacion_directorio_de_trabajo(carpeta.madre)
archivos.tmin <- list.files()
directorio.de.trabajo <- getwd()

db.tmin <- union_db_valores_observados_y_coordenadas(archivos.tmin, variable.de.interes, directorio.de.trabajo)

head(db.tmin)
tail(db.tmin)
dim(db.tmin)


# tmax
carpeta.madre <- 'C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/DatosAysen_Baker/'
variable.de.interes <- 'tmax'

localizacion_directorio_de_trabajo(carpeta.madre)
archivos.tmax <- list.files()
directorio.de.trabajo <- getwd()

db.tmax <- union_db_valores_observados_y_coordenadas(archivos.tmax, variable.de.interes, directorio.de.trabajo)

head(db.tmax)
tail(db.tmax)
dim(db.tmax)


# pp
carpeta.madre <- 'C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/DatosAysen_Baker/'
variable.de.interes <- 'pp'

localizacion_directorio_de_trabajo(carpeta.madre)
archivos.pp <- list.files()
directorio.de.trabajo <- getwd()

db.pp <- union_db_valores_observados_y_coordenadas(archivos.pp, variable.de.interes, directorio.de.trabajo)
db.pp$variable <- 'precip'

unique(db.pp$archivo.con.coordenadas)
# estaciones.de.interes <- c('Rio.Nef.Antes.Junta.Estero.el.Revalse_DGA')
# db.pp <- db.pp[db.pp$archivo.con.coordenadas%in%estaciones.de.interes,]

head(db.pp)
tail(db.pp)
dim(db.pp)


# hr
carpeta.madre <- 'C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/DatosAysen_Baker/'
variable.de.interes <- 'rh'

localizacion_directorio_de_trabajo(carpeta.madre)
archivos.rh <- list.files()
directorio.de.trabajo <- getwd()
  
db.rh <- union_db_valores_observados_y_coordenadas(archivos.rh, variable.de.interes, directorio.de.trabajo)
db.rh$variable <- 'relativeHumidity'

head(db.rh)
tail(db.rh)
dim(db.rh)


# union de db's

db.todos <- rbind(db.pp)#, db.tmin, db.tmax, db.pp, db.rh)

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

estaciones.db.todos0 <- data.frame(station_id = NA, name = db.todos$archivo.con.coordenadas,
                                  longitude = db.todos$lon, latitude = db.todos$lat, altitude = NaN, 
                                  source = db.todos$archivo.con.datos.climaticos)

estaciones.db.todos <- estaciones.db.todos0[!duplicated(estaciones.db.todos0$name), ]

del_1_al_9 <- paste0('00', 1:9)
del_10_a_n <- paste0('0', 10:nrow(estaciones.db.todos))
estaciones.db.todos$station_id <- c(del_1_al_9, del_10_a_n)

estaciones.db.todos <- anhadiendo_altitud(estaciones.db.todos, sistema_de_coordenadas = 'wgs84')
estaciones.db.todos$altitude[estaciones.db.todos$altitude==0] <- 1
head(estaciones.db.todos)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
write.table(estaciones.db.todos, file='stations.txt', row.names=FALSE, col.names=TRUE, sep=", ",
            append=FALSE, quote=FALSE, na = 'NaN')


# informacion de variables
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
variables <- read.table("variables.txt", sep = ",", header = TRUE)
variables$missing_code <- NaN

variables1 <- variables
id <- which(variables1$variable_id%in%unique(db.todos$variable))

variables2 <- variables1[id,]
variables2

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')
write.table(variables2, file='variables.txt', row.names=FALSE, col.names=TRUE, sep=", ",
            append=FALSE, quote=FALSE, na = 'NaN')



# variable
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
variable.i <- read.table("tmin.txt", sep = ",", header = TRUE)
head(variable.i)


# matriz.tmin <- generador_de_matriz(db.tmin) ; head(matriz.tmin)
# matriz.tmax <- generador_de_matriz(db.tmax) ; head(matriz.tmax)
matriz.pp <- generador_de_matriz(db.pp) ; head(matriz.pp) ; dim(matriz.pp)
colnames(matriz.pp) <- c('YYYYMMDD', del_1_al_9, del_10_a_n)
matriz.pp

# save

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')

# write.table(matriz.tmin, file='tmin.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')
# 
# write.table(matriz.tmax, file='tmax.txt', row.names=FALSE, col.names=TRUE, sep=", ",
#             append=FALSE, quote=FALSE, na = 'NaN')
# 
write.table(matriz.pp, file='precip.txt', row.names=FALSE, col.names=TRUE, sep=", ",
            append=FALSE, quote=FALSE, na = 'NaN')
  
# fin ---