rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_db_a_formato_ts.R')
source('funcion_homogenizacion_entre_db_observado_y_simulado.R')


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

estaciones <- read.csv('estaciones.csv') ; summary(estaciones$valor)
eqm <- read.csv('eqm.csv') ; summary(eqm$valor)
pqm <- read.csv('pqm.csv') ; summary(pqm$valor)
gpqm <- read.csv('gpqm.csv') ; summary(gpqm$valor)
loci <- read.csv('loci.csv') ; summary(loci$valor)
ptr <- read.csv('ptr.csv') ; summary(ptr$valor)
qdm <- read.csv('qdm.csv') ; summary(qdm$valor)
era5 <- read.csv('era5.csv') ; summary(era5$valor)

unique(eqm$nombre_estacion)
estacion.de.interes <- unique(eqm$nombre_estacion)[3] ; estacion.de.interes
        
estaciones.ts <- db_a_formato_ts(estaciones, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

eqm.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, eqm, estacion = estacion.de.interes)
eqm.ts <- db_a_formato_ts(eqm.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

pqm.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, pqm, estacion = estacion.de.interes)
pqm.ts <- db_a_formato_ts(pqm.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

gpqm.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, gpqm, estacion = estacion.de.interes)
gpqm.ts <- db_a_formato_ts(gpqm.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

loci.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, loci, estacion = estacion.de.interes)
loci.ts <- db_a_formato_ts(loci.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

ptr.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, ptr, estacion = estacion.de.interes)
ptr.ts <- db_a_formato_ts(ptr.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

qdm.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, qdm, estacion = estacion.de.interes)
qdm.ts <- db_a_formato_ts(qdm.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

era5.pre.ts <- homogenizacion_entre_db_observado_y_simulado(estaciones, era5, estacion = estacion.de.interes)
era5.ts <- db_a_formato_ts(era5.pre.ts, estacion.de.interes, calculo_diario = sum, calculo_mensual = mean)

valor.maximo <- round(max(estaciones.ts, eqm.ts, pqm.ts, loci.ts, ptr.ts, qdm.ts, era5.ts, na.rm=TRUE), 0)+1
valor.maximo


setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/plots/')

# todos ----

# png('todos_los_metodos_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, valor.maximo), col = c('black'), lty=1, lwd=2, 
     main= 'Observado vs distintas correciones de ERA5 (2011-2017)',
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(eqm.ts, lty=1, col='blue')
lines(pqm.ts, lty=1, col='red')
lines(gpqm.ts, lty=1, col='purple')
lines(loci.ts, lty=1, col='green')
lines(ptr.ts, lty=1, col='orange')
lines(qdm.ts, lty=1, col='yellow')
lines(era5.ts, lty=1, lwd=2, col='red')

legend('topright', legend = c('Observado', 'ERA5', 'EQM', 'LOCI', 'PQM', 'GPQM', 'PTR', 'QDM'), 
       lty = c(1,1,1,1,1,1), lwd=c(2,2,1,1,1,1,1), 
       col = c('black', 'red', 'blue', 'green', 'red', 'purple', 'orange', 'yellow'))

dev.off()

# fin ---




# eqm ----

# png('eqm_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, max(valor.maximo)), col = c('black'), lty=1, lwd=2, 
     main= 'Observado vs correcion EQM de ERA5 (2011-2017)', ylab=expression('mm mensual total'^-1), 
     xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.ts, lty=1, lwd=2, col='red')
lines(eqm.ts, lty=2, lwd=2, col='green')

legend('topright', legend = c('Observado', 'ERA5', 'EQM'), lty = c(1,1,2), lwd=c(2,2,2), 
       col = c('black', 'red', 'green'))

dev.off()

# fin ---




# loci ----

# png('loci_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, max(valor.maximo)), col = c('black'), lty=1, lwd=2, 
     main= 'Observado vs correcion LOCI de ERA5 (2011-2017)', 
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.ts, lty=1, lwd=2, col='red')
lines(loci.ts, lty=2, lwd=2, col='green')

legend('topright', legend = c('Observado', 'ERA5', 'LOCI'), lty = c(1,1,2), lwd=c(2,2,2), 
       col = c('black', 'red', 'green'))

dev.off()

# fin ---




# pqm ----

# png('pqm_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, max(valor.maximo)), col = c('black'), lty=1, lwd=2, 
     main= 'Observado vs correcion PQM de ERA5 (2011-2017)', 
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.ts, lty=1, lwd=2, col='red')
lines(pqm.ts, lty=2, lwd=2, col='green')

legend('topright', legend = c('Observado', 'ERA5', 'PQM'), lty = c(1,1,2), lwd=c(2,2,2),
       col = c('black', 'red', 'green'))

dev.off()

# fin ---




# gpqm ----

# png('gpqm_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, max(valor.maximo)), col = c('black'), lty=1, lwd=2,
     main= 'Observado vs correcion GPQM de ERA5 (2011-2017)',
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.ts, lty=1, lwd=2, col='red')
lines(gpqm.ts, lty=2, lwd=2, col='green')

legend('topright', legend = c('Observado', 'ERA5', 'GPQM'), lty = c(1,1,2), lwd=c(2,2,2),
       col = c('black', 'red', 'green'))

dev.off()

# fin ---




# ptr ----

# png('ptr_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, max(valor.maximo)), col = c('black'), lty=1, lwd=2, 
     main= 'Observado vs correcion PTR de ERA5 (2011-2017)', 
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.ts, lty=1, lwd=2, col='red')
lines(ptr.ts, lty=2, lwd=2, col='green')

legend('topright', legend = c('Observado', 'ERA5', 'PTR'), lty = c(1,1,2), lwd=c(2,2,2),
       col = c('black', 'red', 'green'))

dev.off()

# fin ---




# qdm ----

# png('qdm_con_datos_de_entrenamiento.png', width = 720, height = 480, units = "px")

plot(estaciones.ts, ylim=c(0, max(valor.maximo)), col = c('black'), lty=1, lwd=2, 
     main= 'Observado vs correcion QDM de ERA5 (2011-2017)', 
     ylab=expression('mm mensual total'^-1), xlab='Mes', xaxt = 'n')

axis(1, at = 1:12, labels = 1:12)
abline(v=c(1:12), h=c(seq(0, valor.maximo, 25)), col='gray')

lines(era5.ts, lty=1, lwd=2, col='red')
lines(qdm.ts, lty=2, lwd=2, col='green')

legend('topright', legend = c('Observado', 'ERA5', 'QDM'), lty = c(1,1,2), lwd=c(2,2,2),
       col = c('black', 'red', 'green'))

dev.off()

# fin ---

