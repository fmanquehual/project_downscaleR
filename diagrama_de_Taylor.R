library(plotrix)
library(corrplot)
library(scales)

rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_homogenizacion_entre_db_observado_y_simulado.R')

setwd('C:/Users/Usuario/Documents/Francisco/WRF/proyecto_WRF/proyecto_WRF_complementario/')
source('funcion_diagrama_de_Taylor_modificado_2.R')
source('funcion_SD_diagrama_de_Taylor_modificado.R')
source('funcion_calculo_de_maximos_y_minimos_para_leyenda.R')
source('funcion_xy_diagrama_de_Taylor_modificado_2.R')

# Leyendo archivos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

estaciones <- read.csv('estaciones.csv')
eqm <- read.csv('eqm.csv')
pqm <- read.csv('pqm.csv')
gpqm <- read.csv('gpqm.csv')
loci <- read.csv('loci.csv')
ptr <- read.csv('ptr.csv')
qdm <- read.csv('qdm.csv')
era5 <- read.csv('era5.csv')

# fin ---




# Preparacion de db's ----

unique(eqm$nombre_estacion)
estacion.de.interes <- unique(eqm$nombre_estacion)[3]

estaciones_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, estacion = estacion.de.interes, 
                                                                             solo_observado = TRUE)
# eqm_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, eqm, estacion = estacion.de.interes)
# pqm_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, pqm, estacion = estacion.de.interes)
# gpqm_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, gpqm, estacion = estacion.de.interes)
# loci_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, loci, estacion = estacion.de.interes)
# ptr_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, ptr, estacion = estacion.de.interes)
# qdm_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, qdm, estacion = estacion.de.interes)
# era5_depurado_completo <- homogenizacion_entre_db_observado_y_simulado(estaciones, era5, estacion = estacion.de.interes)

eqm_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, eqm, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
pqm_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, pqm, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
gpqm_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, gpqm, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
loci_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, loci, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
ptr_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, ptr, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
qdm_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, qdm, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
era5_depurado <- homogenizacion_entre_db_observado_y_simulado(estaciones, era5, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)

# fin ---




# Uniendo db's ----

# oldpar<-taylor.diagram(estaciones_depurado_completo$valor, eqm_depurado_completo$valor, col = 'blue', normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(eqm_depurado$r, eqm_depurado$sd.observado, eqm_depurado$sd.simulado, add=TRUE)
# 
# taylor.diagram(estaciones_depurado_completo$valor, pqm_depurado_completo$valor,add=TRUE,col="red", normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(pqm_depurado$r, pqm_depurado$sd.observado, pqm_depurado$sd.simulado, add=TRUE)
# 
# taylor.diagram(estaciones_depurado_completo$valor, gpqm_depurado_completo$valor,add=TRUE,col="purple", normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(gpqm_depurado$r, gpqm_depurado$sd.observado, gpqm_depurado$sd.simulado, add=TRUE)
# 
# taylor.diagram(estaciones_depurado_completo$valor, loci_depurado_completo$valor,add=TRUE,col="green", normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(loci_depurado$r, loci_depurado$sd.observado, loci_depurado$sd.simulado, add=TRUE)
# 
# taylor.diagram(estaciones_depurado_completo$valor, ptr_depurado_completo$valor,add=TRUE,col="orange", normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(ptr_depurado$r, ptr_depurado$sd.observado, ptr_depurado$sd.simulado, add=TRUE)
# 
# taylor.diagram(estaciones_depurado_completo$valor, qdm_depurado_completo$valor,add=TRUE,col="gray", normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(qdm_depurado$r, qdm_depurado$sd.observado, qdm_depurado$sd.simulado, add=TRUE)
# 
# taylor.diagram(estaciones_depurado_completo$valor, era5_depurado_completo$valor,add=TRUE,col="cyan", normalize = FALSE, pcex = 1)
# diagrama.de.Taylor.modificado.2(era5_depurado$r, era5_depurado$sd.observado, era5_depurado$sd.simulado, add=TRUE)

eqm_depurado$metodo <- 'EQM'
pqm_depurado$metodo <- 'PQM'
gpqm_depurado$metodo <- 'GPQM'
loci_depurado$metodo <- 'LOCI'
ptr_depurado$metodo <- 'PTR'
qdm_depurado$metodo <- 'QDM'
era5_depurado$metodo <- 'ERA5'

metodos <- rbind(eqm_depurado, pqm_depurado, gpqm_depurado, loci_depurado, ptr_depurado, qdm_depurado, era5_depurado)
metodos$etiqueta <- 1:nrow(metodos)

# fin ---




# detalles de plot ----

metodos$bias <- round(metodos$bias, 0) # 0 en pp, 1 decimal para t2 multipleforcing
min.bias <-  round(min(metodos$bias), 0) ; min.bias
max.bias <-  round(max(metodos$bias), 0) ; max.bias

intervalo <- 0.1 # 10 en pp, 0.25 (t2 multipleforcing)

minimo.leyenda <- calculo.maximo.y.minimo.de.leyenda(min.bias, intervalo, 'minimo') ; minimo.leyenda
maximo.leyenda <- calculo.maximo.y.minimo.de.leyenda(max.bias, intervalo, 'maximo') ; maximo.leyenda
intervalos.para.leyenda <- seq(minimo.leyenda, maximo.leyenda, by=intervalo) ; intervalos.para.leyenda
longitud.intervalo <- length(intervalos.para.leyenda) ; longitud.intervalo
n <- as.character( seq(minimo.leyenda, maximo.leyenda, by=0.1) ) # 1 en pp, 0.1 (t2 multipleforcing)

db.bias.y.color <- data.frame( n.bias=n, color.bias=viridis_pal()(length(n)) )
db.bias.y.color$color.bias <- as.character(db.bias.y.color$color.bias)

pch.bias <- 16
tamanho.pch.bias.leyenda <- 3

color.i <- db.bias.y.color$color.bias[as.character( db.bias.y.color$n.bias )%in% as.character( metodos$bias[1] )]

# fin ---




# Plot ----

diagrama.de.Taylor.modificado.2(R.i = metodos$r[1], sd.ref.i = metodos$sd.observado[1], sd.model.i = metodos$sd.simulado[1],
                                tamanho.punto.de.referencia=1, pos.cor=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, 
                                pch = pch.bias, show.gamma = TRUE, ancho.linea.de.pch = 0.5, main = '', xlab = 'Standard Deviation (Normalized)')
text(eqm_depurado$x[1], eqm_depurado$y[1], metodos$etiqueta[1], cex=0.9, font=2, col='white')


for (i in 2:nrow(metodos)) {
  # i <- 5
  color.i <- db.bias.y.color$color.bias[as.character( db.bias.y.color$n.bias )%in%as.character( metodos$bias[i] )]
  
  diagrama.de.Taylor.modificado.2(R.i = metodos$r[i], sd.ref.i = metodos$sd.observado[i], sd.model.i = metodos$sd.simulado[i],
                                  add=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, pch = pch.bias, ancho.linea.de.pch = 0.5)
  text(metodos$x[i], metodos$y[i], metodos$etiqueta[i], cex=0.9, font=2, col='white')
}

leyenda.i <- paste('(', metodos$etiqueta, ')', ' ', metodos$metodo, sep = '') ; leyenda.i

colorlegend(db.bias.y.color$color.bias, intervalos.para.leyenda, xlim = c(1.55, 1.71), ylim = c(0.5, 1.5))
text(1.585, 1.55, 'BIAS', srt = 0, font = 2)
legend(0.5, 1.8, legend=leyenda.i, horiz=FALSE, cex = 0.71, ncol = 4, lty = NULL, text.font = 2)
#text(1.72, 1.7, '(b)', font = 2) # '(a)' en t2, '(b)' en pp

# fin ---
