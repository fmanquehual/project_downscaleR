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


metricas.eqm <- calculo_de_metricas_de_desempenho(estaciones, eqm)
metricas.pqm <- calculo_de_metricas_de_desempenho(estaciones, pqm)
metricas.gpqm <- calculo_de_metricas_de_desempenho(estaciones, gpqm)
metricas.loci <- calculo_de_metricas_de_desempenho(estaciones, loci)
metricas.ptr <- calculo_de_metricas_de_desempenho(estaciones, ptr)
metricas.qdm <- calculo_de_metricas_de_desempenho(estaciones, qdm)
metricas.era5 <- calculo_de_metricas_de_desempenho(estaciones, era5)

metricas.eqm
metricas.pqm
metricas.gpqm
metricas.loci
metricas.ptr
metricas.qdm
metricas.era5
