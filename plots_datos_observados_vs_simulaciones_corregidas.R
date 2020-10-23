setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_db_a_formato_ts.R')
source('funcion_grilla_a_formato_ts.R')

pr.observado <- db_a_formato_ts(estaciones, funcion = mean)
era5 <- grilla_a_formato_ts(pr.sum.puntos)

# pr.simulado.delta <- db_a_formato_ts(pr.sum.corregido.delta, funcion = mean)
pr.simulado.eqm <- db_a_formato_ts(pr.sum.corregido.eqm, funcion = mean)
pr.simulado.loci <- db_a_formato_ts(pr.sum.corregido.loci, funcion = mean)
pr.simulado.pqm <- db_a_formato_ts(pr.sum.corregido.pqm, funcion = mean)
pr.simulado.ptr <- db_a_formato_ts(pr.sum.corregido.ptr, funcion = mean)
# pr.simulado.scaling <- db_a_formato_ts(pr.sum.corregido.scaling, funcion = mean)


# todos ----

plot(pr.observado, col = c('black'), lty=1, lwd=2, main= 'observado vs distintas correciones de ERA5', 
     ylab=expression('mm dia'^-1), xlab='Dias de enero')
lines(era5, lty=1, lwd=2, col='red')
lines(pr.simulado.eqm, lty=1, col='blue')
lines(pr.simulado.loci, lty=1, col='green')
lines(pr.simulado.pqm, lty=1, col='red')
lines(pr.simulado.ptr, lty=1, col='orange')

grid(col='gray')

legend('top', legend = c('Observado', 'ERA5', 'EQM', 'LOCI', 'PQM', 'PTR'), lty = c(1,1,1,1,1,1), lwd=c(2,2,1,1,1,1,1), 
       col = c('black', 'red', 'blue', 'green', 'red', 'orange'))

# fin ---




# eqm ----

plot(pr.observado, col = c('black'), lty=1, lwd=2, main= 'observado vs correcion EQM de ERA5', 
     ylab=expression('mm dia'^-1), xlab='Dias de enero')
lines(era5, lty=1, lwd=2, col='red')
lines(pr.simulado.eqm, lty=1, col='blue')

legend('top', legend = c('Observado', 'ERA5', 'EQM'), lty = c(1,1,1), lwd=c(2,2,1), 
       col = c('black', 'red', 'blue'))

# fin ---




# loci ----

plot(pr.observado, col = c('black'), lty=1, lwd=2, main= 'observado vs correcion LOCI de ERA5', 
     ylab=expression('mm dia'^-1), xlab='Dias de enero')
lines(era5, lty=1, lwd=2, col='red')
lines(pr.simulado.loci, lty=1, col='blue')

legend('top', legend = c('Observado', 'ERA5', 'LOCI'), lty = c(1,1,1), lwd=c(2,2,1), 
       col = c('black', 'red', 'blue'))

# fin ---




# pqm ----

plot(pr.observado, col = c('black'), lty=1, lwd=2, main= 'observado vs correcion PQM de ERA5', 
     ylab=expression('mm dia'^-1), xlab='Dias de enero')
lines(era5, lty=1, lwd=2, col='red')
lines(pr.simulado.pqm, lty=1, col='blue')

legend('top', legend = c('Observado', 'ERA5', 'PQM'), lty = c(1,1,1), lwd=c(2,2,1), 
       col = c('black', 'red', 'blue'))

# fin ---




# ptr ----

plot(pr.observado, col = c('black'), lty=1, lwd=2, main= 'observado vs correcion PTR de ERA5', 
     ylab=expression('mm dia'^-1), xlab='Dias de enero')
lines(era5, lty=1, lwd=2, col='red')
lines(pr.simulado.ptr, lty=1, col='blue')

legend('top', legend = c('Observado', 'ERA5', 'PTR'), lty = c(1,1,1), lwd=c(2,2,1), 
       col = c('black', 'red', 'blue'))

# fin ---