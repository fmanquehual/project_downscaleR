rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_calculo_de_metricas_de_desempenho.R')

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
estacion.de.interes <- unique(eqm$nombre_estacion)[3]

metricas.eqm <- calculo_de_metricas_de_desempenho(estaciones, eqm, estacion = estacion.de.interes)
metricas.pqm <- calculo_de_metricas_de_desempenho(estaciones, pqm, estacion = estacion.de.interes)
metricas.gpqm <- calculo_de_metricas_de_desempenho(estaciones, gpqm, estacion = estacion.de.interes)
metricas.loci <- calculo_de_metricas_de_desempenho(estaciones, loci, estacion = estacion.de.interes)
metricas.ptr <- calculo_de_metricas_de_desempenho(estaciones, ptr, estacion = estacion.de.interes)
metricas.qdm <- calculo_de_metricas_de_desempenho(estaciones, qdm, estacion = estacion.de.interes)
metricas.era5 <- calculo_de_metricas_de_desempenho(estaciones, era5, estacion = estacion.de.interes)

metricas.eqm
metricas.pqm
metricas.gpqm
metricas.loci
metricas.ptr
metricas.qdm
metricas.era5

