library(loadeR)
library(loadeR.2nc)
library(visualizeR)
library(downscaleR)
library(convertR)
library(lattice)
library(raster)

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

wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #coordenadas geograficas WGS84

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
  
  # if(i>1){c(
  #   estaciones$Dates$start <- as.vector(unlist(nuevas_fechas(estaciones, entregar_fecha_inicio = TRUE, iteracion = i, tz=tz.i)[2])),
  #   estaciones$Dates$end <- as.vector(unlist(nuevas_fechas(estaciones, entregar_fecha_inicio = FALSE, iteracion = i, tz=tz.i)[2])) )}
  
  # temporalPlot(estaciones, aggr.spatial = list(FUN = mean, na.rm = TRUE))
  
  
  
  
  # Predictors (ERA reanalisis) 
  
  # message('Leyendo datos de ERA5')
  
  # setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA_LAND/') # Tiene NA por el oceano, lo que genera problemas
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/descargas_ERA5/copernicus/')
  # setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
  # era5 <- 'RAINRATE_ERAint_Domain2_5a1_Conf10_2018_para_ejemplos.nc'
  era5 <- 'ERA5_1979_2018_pp.nc'
  
  # library(ncdf4)
  # ej <- nc_open(era5, write = TRUE)
  # 
  # ej <- ncvar_rename(ej, old_varname = 'tp', new_varname = 'pr')
  # ej <- ncvar_rename(ej, old_varname = 'XLONG', new_varname = 'lon'v)
  # ej <- ncvar_rename(ej, old_varname = 'XLAT', new_varname = 'lat')
  # nc_close(ej)
  
  # Precipitacion
  # C4R.vocabulary()
  
  message('Transformando unidades de variable')
  
  # Datos de entrenamiento
  # dataInventory(era5)
  pr.sum0.entrenamiento <- loadGridData_personalizado(era5, "tp", es.precipitacion = TRUE, 
                                                      es.cmip6 = FALSE, anhos = anhos.entrenamiento)
  pr.sum.entrenamiento <- udConvertGrid(pr.sum0.entrenamiento, new.units = "mm")
  
  # if(i>1){c(
  #   # pr.sum.entrenamiento.original <- pr.sum.entrenamiento,
  #   pr.sum.entrenamiento$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.entrenamiento, entregar_fecha_inicio = TRUE, 
  #                                                                      datos_simulados=TRUE, iteracion = i, tz=tz.i)[2])),
  #   pr.sum.entrenamiento$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.entrenamiento, entregar_fecha_inicio = FALSE, 
  #                                                                    datos_simulados=TRUE, iteracion = i, tz=tz.i)[2])) )}
  
  
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
  
  db.estaciones.preliminar <- lista_climate4R_a_db(estaciones.con.todos.los.anhos)
  db.estaciones <- rbind(db.estaciones, db.estaciones.preliminar)
  
  # fin ---
  
  
  
  
  # Extrayendo valores de los pixeles donde estan ubicados las estaciones ----
  
  message('Extrayendo valores de celda donde estan ubicados las estaciones')
  
  # pr.sum.total$Dates$start <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
  #                                                            entregar_fecha_inicio = TRUE, 
  #                                                            iteracion = i, tz=tz.i)[1]))
  # 
  # pr.sum.total$Dates$end <- as.vector(unlist(nuevas_fechas(pr.sum.total.original, 
  #                                                          entregar_fecha_inicio = FALSE, 
  #                                                          iteracion = i, tz=tz.i)[1]))
  
  era5.en.ubicacion.de.estacion.i <- grid2sp(pr.sum.total)
  db.era5.preliminar <- grilla_a_db(era5.en.ubicacion.de.estacion.i, estaciones)
  db.era5 <- rbind(db.era5, db.era5.preliminar)
  
}

# fin ---




# ajustando modelos de correcion ----

nombre.estaciones <- estaciones$Metadata$name
altitud.de.estaciones <- estaciones$Metadata$altitude
db.estacion.altitud <- data.frame(nombre_estacion=nombre.estaciones, H=altitud.de.estaciones)

db.parametros.con.altitud <- merge(db.parametros, db.estacion.altitud, by = 'nombre_estacion')
head(db.parametros.con.altitud)

bloques <- unique(db.parametros.con.altitud$bloque)
  
iteraciones <- 75 # default es 50
db <- c()

for (i in bloques) {
  # i <- 29
  
  db.parametros.con.altitud.i <- subset(db.parametros.con.altitud, bloque==i)
  
  # Quitando NA
  
  id.valores.NA <- which(is.na(db.parametros.con.altitud.i$a))
  db.parametros.con.altitud.sin.NA.i <- db.parametros.con.altitud.i[-id.valores.NA,]
  
  # Ajustando modelo de poder
  
  funcion_de_poder <- function(c.i,d.i,H.i){ y <- c.i*(H.i^d.i)
                                       return(y) }
  
  modelo.a <- nls(a ~ funcion_de_poder(c, d, H),
                  data = db.parametros.con.altitud.sin.NA.i,
                  start = c(c=0.1, d=0.1),
                  control = list(maxiter=iteraciones))
  
  c.a <- coefficients(modelo.a)[1]
  d.a <- coefficients(modelo.a)[2]
  
  
  modelo.b <- nls(b ~ funcion_de_poder(c, d, H),
                  data = db.parametros.con.altitud.sin.NA.i,
                  start = c(c=0.1, d=0.1),
                  control = list(maxiter=iteraciones))
  
  c.b <- coefficients(modelo.b)[1]
  d.b <- coefficients(modelo.b)[2]
  
  
  db.parametros.con.altitud.sin.NA.i$c_de_a <- c.a
  db.parametros.con.altitud.sin.NA.i$d_de_a <- d.a
  db.parametros.con.altitud.sin.NA.i$c_de_b <- c.b
  db.parametros.con.altitud.sin.NA.i$d_de_b <- d.b
  
  db.parametros.con.altitud.sin.NA.i$a.estimado <- funcion_de_poder(c.a, d.a, 
                                                                    db.parametros.con.altitud.sin.NA.i$H)
  db.parametros.con.altitud.sin.NA.i$b.estimado <- funcion_de_poder(c.b, d.b, 
                                                                    db.parametros.con.altitud.sin.NA.i$H)
  db.parametros.con.altitud.sin.NA.i$bloque <- i

  db <- rbind(db, db.parametros.con.altitud.sin.NA.i)
}

row.names(db) <- 1:nrow(db)
head(db)
db$bloque <- factor(db$bloque, levels = 1:73)


# Plots

xyplot(a~H | bloque, data=db, type=c('p', 'g', 'smooth'),
       main="Parametro 'a' vs altitud de cada bloque") # Show points ("p"), grids ("g") and smoothing line

xyplot(b~H | bloque, data=db, type=c('p', 'g', 'smooth'),
       main="Parametro 'b' vs altitud de cada bloque")

# fin ---




# Residuales ----

db$residual.de.a <- db$a-db$a.estimado
db$residual.de.b <- db$b-db$b.estimado


# plots de residuales

histogram(~residual.de.a | bloque, data=db,
       main="Residuales parametro 'a' de cada bloque")

histogram(~residual.de.b | bloque, data=db,
       main="Residuales parametro 'b' de cada bloque")

# fin ---




# Preparando db para correccion ----

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
head(db.estacion.altitud)

db.estaciones.y.era5_1 <- merge(db.estaciones.y.era5.preliminar, db.estacion.altitud, by='nombre_estacion')
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
db.para.merge <- db[,c('id', 'a', 'b', 'c_de_a', 'd_de_a', 'c_de_b', 'd_de_b')]

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

db.estaciones.y.era5_3$valor.era5.corregido <- db.estaciones.y.era5_3$a*(db.estaciones.y.era5_3$valor.era5^db.estaciones.y.era5_3$b)

head(db.estaciones.y.era5_3)
dim(db.estaciones.y.era5_3)

# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
# write.csv(db.estaciones.y.era5, 'ejemplo_bias_correcion_por_estacion_con_modelos_enero.csv', row.names = FALSE)


# Plot

nombre.estaciones
db.subset <- subset(db.estaciones.y.era5_3, nombre_estacion==nombre.estaciones[1])
rango.de.valores <- 850:900

# par(mfrow=c(2,1))
valor.maximo <- max(db.subset$valor.era5.corregido[rango.de.valores], na.rm = TRUE)

plot(db.subset$valor.estaciones[rango.de.valores], type='l', lwd=2, ylim=c(0, valor.maximo))
lines(db.subset$valor.era5[rango.de.valores], col='red', lwd=2)
lines(db.subset$valor.era5.corregido[rango.de.valores], col='blue', lwd=2)

legend('topright', legend = c('Observado', 'ERA5', 'Modelo de poder'), 
       lwd=c(2,2,2), 
       col = c('black', 'red', 'blue'))

# fin ---




# Preparacion de grillas ----

pr.sum.corregido0 <- pr.sum.total.original

fechas.inicio <- as.Date(pr.sum.corregido0$Dates$start)
fechas.termino <- as.Date(pr.sum.corregido0$Dates$end)
numero.de.matrices <- dim(pr.sum.corregido0$Data)[1]
pr.sum.corregido0$Data[1,,]


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

pr.sum.corregido <- pr.sum.corregido0


# Generando matriz de elevacion

raster.pr.sum.corregido0 <- grid2sp(pr.sum.corregido)
raster.pr.sum.corregido <- stack(raster.pr.sum.corregido0)
plot(raster.pr.sum.corregido, 1)


# setwd('C:/Users/Usuario/Documents/Francisco/WRF/coberturas/coberturas_ok/')
# dem <- raster('dem3_clip_para_mapas.tif')
# 
# dem.clip <- crop(dem, raster.pr.sum.corregido)
# plot(dem.clip, colNA='red')
# 
# dem.clip.resemple <- resample(dem.clip, raster.pr.sum.corregido, method='bilinear')
# crs(dem.clip.resemple) <- wgs84
# plot(dem.clip.resemple)
# 
# matriz.altitud <- as.matrix(dem.clip.resemple)
# 
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/capas/')
# writeRaster(dem.clip.resemple, filename="dem_regrillado_bilinear_era5.tif",
#             format="GTiff", overwrite=TRUE)

dem.clip.resemple <- raster('dem_regrillado_bilinear_era5.tif')

# Generando db con elevacion de estaciones

db.estaciones <- estaciones$xyCoords
db.estaciones$nombre_estacion <- estaciones$Metadata$name
db.estaciones$H.resample <- extract(dem.clip.resemple, db.estaciones[,c('x', 'y')], sp=TRUE)
db.estaciones

# fin ---




# Correcion a nivel de grilla ----

numero.de.matrices.2 <- dim(pr.sum.corregido$Data)[1]

for (i in 1:numero.de.matrices.2) {
#  i <- 1
  
  mensaje.inicio <- paste('Matriz', i, 'lista de', numero.de.matrices.2)
  message(mensaje.inicio)
  
  # Seleccion de matriz con fecha 'i'
  
  matriz.i <- pr.sum.corregido$Data[i,,]
  fecha.i <- as.Date(pr.sum.corregido$Dates$start[i])
  mes_y_dia_i <- paste(month(fecha.i), day(fecha.i), sep = '-')
  
  
  # Seleccion de parametros 'c' y 'd' segun bloque 'i'
  
  bloque.i <- db.fechas.por.bloque.sin.buffer$bloque_sin_buffer[db.fechas.por.bloque.sin.buffer$mes_y_dia==mes_y_dia_i]
  
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
  #  j <- 1
    
   for (k in 1:ncol(matriz.i)){
   #  k <- 1    
      precipitacion.i <- matriz.i[j,k]
      altitud.i <- matriz.altitud[j,k]
      
      a.i <- funcion_de_poder(c.a, d.a, altitud.i)
      b.i <- funcion_de_poder(c.b, d.b, altitud.i)
      
      precipitacion.corregido.i <- a.i*(precipitacion.i^b.i)
      matriz.i[j,k] <- precipitacion.corregido.i
    
    }
      
  }
  
  
  # reemplazando matriz de precipitacion 'i' por matriz corregida
  
  pr.sum.corregido$Data[i,,] <- matriz.i
  
}

# fin ---




# Guardando grilla corregida ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

# Name of output file:
fileName <- "pr_era5_corregido_2.nc4"

# Including a global attribute:
globalAttributeList <- list("institution" = "Centro Butamallin - Investigación en Cambio Climático")

# Including variable attributes:
varAttributeList <- list(var_attr1 = "Precipitación (mm)")

# Create file:
grid2nc(data = pr.sum.corregido,
        NetCDFOutFile = fileName,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList)

# library(ncdf4)
# ej <- nc_open('pr_era5_corregido_2.nc4')
# ej

# fin ---




# Generando db con valores crudos y corregidos ----

era5.corregido <- grid2sp(pr.sum.corregido)
db.era5.corregido <- grilla_a_db(era5.corregido, estaciones)
db.era5.corregido$id <- paste(db.era5.corregido$fecha, db.era5.corregido$nombre_estacion, 
                              sep = '_')
colnames(db.era5.corregido)[2] <- 'valor.corregido.en.grilla'
head(db.era5.corregido)

head(db.estaciones.y.era5_3)
db.estaciones.y.era5_4 <- db.estaciones.y.era5_3[,c('nombre_estacion', 'fecha', 
                                                    'valor.estaciones', 'valor.era5',
                                                    'valor.era5.corregido', 'H')]

db.estaciones.y.era5_4$id <- paste(db.estaciones.y.era5_4$fecha, 
                                   db.estaciones.y.era5_4$nombre_estacion, sep = '_')
head(db.estaciones.y.era5_4)

db.estaciones.y.era5_5 <- db.estaciones.y.era5_4[,c('id', 'valor.estaciones',
                                                    'valor.era5', 'valor.era5.corregido',
                                                    'H')]
head

db.con.valores.corregidos.preliminar <- merge(db.era5.corregido, db.estaciones.y.era5_5, by='id')
db.con.valores.corregidos.preliminar.2 <- db.con.valores.corregidos.preliminar[,c(
                                                           'fecha', 'nombre_estacion', 'H',
                                                           'valor.estaciones', 'valor.era5',
                                                           'valor.era5.corregido',
                                                           'valor.corregido.en.grilla')]

db.con.valores.corregidos <- merge(db.con.valores.corregidos.preliminar.2, db.estaciones,
                                   by='nombre_estacion')

db.con.valores.corregidos <- db.con.valores.corregidos[,c('fecha', 'nombre_estacion', 'x', 'y',
                                                          'H', 'H.resample', 'valor.estaciones', 
                                                          'valor.era5', 'valor.era5.corregido',
                                                          'valor.corregido.en.grilla')]
head(db.con.valores.corregidos)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
# write.csv(db.con.valores.corregidos, 'bias_correcion_por_estacion.csv', row.names = FALSE)

# fin ---




# Plots grillas no corregidas vs corregidas ----

raster.era5.corregido <- stack(era5.corregido)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/plots/')

# png('mapa_altitud_era5Original_era5Corregida.png', width = 780, height = 780, units = "px")
par(mfrow=c(3,3))

plot(dem.clip.resemple, main='Altitud')
plot(raster.pr.sum.corregido, 1, main='01/01/1995 ERA5')
plot(raster.era5.corregido, 1, main='01/01/1995 ERA5-CORREGIDO')

plot(dem.clip.resemple, main='Altitud')
plot(raster.pr.sum.corregido, 15, main='15/01/1995 ERA5')
plot(raster.era5.corregido, 15, main='15/01/1995 ERA5-CORREGIDO')

plot(dem.clip.resemple, main='Altitud')
plot(raster.pr.sum.corregido, 30, main='30/01/1995 ERA5')
plot(raster.era5.corregido, 30, main='30/01/1995 ERA5-CORREGIDO')

# dev.off()

# fin ---