# Los valores por debajo del percentil 25 menos tres veces el 
# rango intercuartílico y por encima del percentil 75 más tres
# veces el rango intercuartílico se consideraron valores atípicos
# de temperatura ([P25 - 3*(P75 - P25), P75+3*(P75 - P25)]).
# Los mismos criterios se aplicaron a la precipitación, aunque
# cambiando los percentiles de referencia y considerando sólo
# las distribuciones de los días húmedos 
# ([P1 - 3*(P90 - P1), P90 + 3*(P90 - P1)]). 
# Los valores atípicos se indicaron como datos que faltaban y
# las estaciones que registraron datos sobre menos del 50% del
# período de 35 años fueron eliminados (Araya‑Osses et al., 2020)

library(lubridate)
library(xts)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_plot_series_temporales.R')


# Lectura de archivos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
db <- read.csv('estaciones_precipitacion_bruto.csv')
head(db)

# fin ---




# Filtro 1: Al menos 1 anho de entrenamiento presente ----
periodo.entrenamiento <- 1995:2010

db$Date <- as.Date(db$Date)
db$anho <- year(db$Date)
head(db)

estaciones <- unique(db$archivo.con.coordenadas)
estacion_a_eliminar <- c()

for (i in 1:length(estaciones)) {
  # i <- 1
  
  estacion.i <- estaciones[i]
  db.i <- subset(db, archivo.con.coordenadas == estacion.i)
  
  anhos.unicos <- unique(db.i$anho)
  match.i <- which(anhos.unicos%in%periodo.entrenamiento)
  
  if(length(match.i) == 0){estacion_a_eliminar <- c(estacion_a_eliminar, estacion.i)}
  
}

filas_a_eliminar <- which(db$archivo.con.coordenadas%in%estacion_a_eliminar)
db1 <- db[-filas_a_eliminar,]
row.names(db1) <- 1:nrow(db1)
head(db1)

dim(db)
dim(db1)

# fin ---




# Series de tiempo 1 ----

estaciones1 <- unique(db1$archivo.con.coordenadas)
plot_series_temporales(db1, estaciones1)

# fin ---




# Valores atipicos a NA ----

# Fuente de metodologia: Araya-Osses et al. (2020)

db2 <- c()

for (i in 1:length(estaciones1)) {
  # i <- 1
  
  estacion.i <- estaciones1[i]
  db.i <- subset(db1, archivo.con.coordenadas == estacion.i)
  
  fechas.i <- db.i$Date
  valores.i <- db.i$valor.observado
  
  db.xts.i <- xts(valores.i, fechas.i)
  
  for (j in 1:length(periodo.entrenamiento)) {
    # j <- 2
    
    fecha.inicio.ij <- paste0(periodo.entrenamiento[j], '-04-01')
    fecha.fin.ij <- paste0(periodo.entrenamiento[j], '-09-30')
    
    db.entrenamiento.xts.ij <- window(db.xts.i, start = fecha.inicio.ij, end = fecha.fin.ij)
    valores.ij <- as.numeric(db.entrenamiento.xts.ij)
    
    p1 <- as.numeric(quantile(valores.ij, probs=c(0.01), na.rm=TRUE))
    p90 <- as.numeric(quantile(valores.ij, probs=c(0.9), na.rm=TRUE))
    
    umbral.minimo <- p1-3*(p90-p1)
    umbral.maximo <- p90+3*(p90-p1)
    
    id.atipicos.minimos <- which(db.i$valor.observado < umbral.minimo)
    id.atipicos.maximos <- which(db.i$valor.observado > umbral.maximo)
    id.valores.atipicos <- c(id.atipicos.minimos, id.atipicos.maximos)
    
    db.i$valor.observado[id.valores.atipicos] <- NA
    
  }
  
  db2 <- rbind(db2, db.i)
  
}

head(db2)

dim(db1)
dim(db2)

length(which(is.na(db1$valor.observado)))
length(which(is.na(db2$valor.observado)))

# fin --- 




# Filtro 2: por x% de datos ausentes ----

estaciones2 <- unique(db2$archivo.con.coordenadas)
porcentaje_con_NA_aceptado <- 50

fecha.inicio <- paste0(periodo.entrenamiento[1], '-01-01')
fecha.fin <- paste0(periodo.entrenamiento[length(periodo.entrenamiento)], '-12-31')

estaciones_que_no_cumplen_umbral <- c()

for (i in 1:length(estaciones2)) {
  # i <- 11
  # print(i)    
  
  estacion2.i <- estaciones2[i]
  db2.i <- subset(db2, archivo.con.coordenadas == estacion2.i)
  
  fechas.i <- db2.i$Date
  valores.i <- db2.i$valor.observado
  
  db2.xts.i <- xts(valores.i, fechas.i)
  
  db2.entrenamiento.xts.i <- window(db2.xts.i, start = fecha.inicio, end = fecha.fin)
  total.de.datos <- nrow(db2.entrenamiento.xts.i)
  
  if(total.de.datos == 0){c(
    estaciones_que_no_cumplen_umbral <- c(estaciones_que_no_cumplen_umbral, estacion2.i),
    next)}
  
  id_elementos_con_NA <- which(is.na(db2.entrenamiento.xts.i))
  cantidad_de_NA <- length(id_elementos_con_NA)
  porcentaje.de.datos.NA <- round( (cantidad_de_NA*100)/total.de.datos, 2)
  
  if(porcentaje.de.datos.NA > porcentaje_con_NA_aceptado){
    estaciones_que_no_cumplen_umbral <- c(estaciones_que_no_cumplen_umbral, estacion2.i)}

}

estaciones_que_no_cumplen_umbral

filas.a.eliminar <- which(db2$archivo.con.coordenadas%in%estaciones_que_no_cumplen_umbral)
db3 <- db2[-filas.a.eliminar,]
row.names(db3) <- 1:nrow(db3)

dim(db2)
dim(db3)

# fin ---




# Series de tiempo 2 ----

estaciones3 <- unique(db3$archivo.con.coordenadas)
plot_series_temporales(db3, estaciones3)

# fin ---




# Filtro 3: Descarte manual ----

estaciones3

id.filas.a.eliminar <- which(db3$archivo.con.coordenadas==estaciones2[5])
db4 <- db3[-id.filas.a.eliminar,]
head(db4)

db.depurado <- db4[,-ncol(db4)]
head(db.depurado)

# fin ---




# Series de tiempo 3 ----

estaciones4 <- unique(db.depurado$archivo.con.coordenadas)
plot_series_temporales(db.depurado, estaciones4)

# fin ---




# Guardando db ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')
# write.csv(db.depurado, 'estaciones_precipitacion_depurado.csv', row.names = FALSE)