library(lubridate)
library(Metrics)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_db_a_formato_ts.R')
source('funcion_calculo_de_metricas_de_desempenho.R')


# Lectura de datos ----

metodo <- 'IDW' # 'IDW' # 'H'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

db <- read.csv('bias_correcion_por_estacion.csv')
db$fecha <- as.Date(db$fecha)
db$anho <- year(db$fecha)

str(db)
head(db)
sort(unique(db$anho))

# fin ---




# Segregando datos por tipo de valor ----

db.observado <- db[,c('fecha', 'nombre_estacion', 'valor.estaciones')]
colnames(db.observado)[3] <- 'valor'

db.era5.raw <- db[,c('fecha', 'nombre_estacion', 'valor.era5')]
colnames(db.era5.raw)[3] <- 'valor'

db.era5.corregido <- db[,c('fecha', 'nombre_estacion', 'valor.era5.corregido')]
colnames(db.era5.corregido)[3] <- 'valor'

db.corregido.nivel.grilla <- db[,c('fecha', 'nombre_estacion', 'valor.corregido.en.grilla')]
colnames(db.corregido.nivel.grilla)[3] <- 'valor'

# fin ---




# Calculando precipitacion anual ----
# ERA 5 corregido a nivel de estacion, no mostrara informacion en algunos meses para
# ciertas estaciones debido a la ausencia o falta de datos para un bloque en particular.
# Esto hace que no se pueda calcular los parametros a y b, por ende, no se realiza la correcion.
for (i in 1:15) {
  
nombre.estacion <- unique(db$nombre_estacion)
estacion.de.interes <- nombre.estacion[i] ; estacion.de.interes

observado.anual <- db_a_formato_ts(db.observado, estacion = estacion.de.interes)
era5.raw.anual <- db_a_formato_ts(db.era5.raw, estacion = estacion.de.interes)
era5.corregido.anual <- db_a_formato_ts(db.era5.corregido, estacion = estacion.de.interes)
corregido.nivel.grilla.anual <- db_a_formato_ts(db.corregido.nivel.grilla, estacion = estacion.de.interes)

# fin ---




# metricas de desempenho ---

metricas.era5.raw <- calculo_de_metricas_de_desempenho(db.observado, db.era5.raw, estacion.de.interes)
metricas.era5.corregido <- calculo_de_metricas_de_desempenho(db.observado, db.era5.corregido, estacion.de.interes)
metricas.corregido.nivel.grilla <- calculo_de_metricas_de_desempenho(db.observado, db.corregido.nivel.grilla, estacion.de.interes)

metricas.era5.raw
metricas.era5.corregido
metricas.corregido.nivel.grilla

# fin ---




# Plot serie de tiempo ----

valor.maximo <- round(max(observado.anual, era5.raw.anual, era5.corregido.anual, 
                          corregido.nivel.grilla.anual, na.rm=TRUE), 0)+1
nombre.plot <- paste0(estacion.de.interes, '_', metodo, '.png')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/plots/series_de_tiempo/')

png(nombre.plot, width = 720, height = 480, units = "px")

plot(observado.anual, ylim=c(0, valor.maximo), col = c('black'), lty=1, lwd=2, 
     main= estacion.de.interes, sub=paste('Metodo', metodo), 
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
# abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.raw.anual, lty=1, lwd=2, col='blue')
lines(era5.corregido.anual, lty=1, lwd=2, col='red')
lines(corregido.nivel.grilla.anual, lty=1, lwd=2, col='orange')

legend('topleft', legend = c('Observado', 'ERA5 sin corregir', 'ERA5 corregido', 'Corregido a nivel de grilla'), 
       lty = c(1,1,1,1), lwd=c(2,2,2,2), col = c('black', 'blue', 'red', 'orange'), bty='n', cex=0.8)

dev.off()

}


# fin ---
