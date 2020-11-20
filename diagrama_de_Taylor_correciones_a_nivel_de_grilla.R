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



# Lectura de datos ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

db <- read.csv('bias_correcion_por_estacion.csv')
db$fecha <- as.Date(db$fecha)
db$anho <- year(db$fecha)

str(db)
head(db)
sort(unique(db$anho))

boxplot(unique(db$H), unique(db$H.resample))

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




# Preparacion de db's ----

unique(db.observado$nombre_estacion)
estacion.de.interes <- unique(db.observado$nombre_estacion)[1] ; estacion.de.interes

db.observado_depurado <- homogenizacion_entre_db_observado_y_simulado(db.observado, estacion = estacion.de.interes, solo_observado = TRUE)
db.era5.raw_depurado <- homogenizacion_entre_db_observado_y_simulado(db.observado, db.era5.raw, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
db.era5.corregido_depurado <- homogenizacion_entre_db_observado_y_simulado(db.observado, db.era5.corregido, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)
db.corregido.nivel.grilla_depurado <- homogenizacion_entre_db_observado_y_simulado(db.observado, db.corregido.nivel.grilla, para_Taylor = TRUE, pbias = FALSE, estacion = estacion.de.interes, normalizado = TRUE)

# fin ---




# Uniendo db's ----

db.observado_depurado$tipo <- 'observado'
db.era5.raw_depurado$tipo <- 'raw'
db.era5.corregido_depurado$tipo <- 'correccion-estacion'
db.corregido.nivel.grilla_depurado$tipo <- 'correccion-grilla'

tipos <- rbind(db.era5.raw_depurado, db.era5.corregido_depurado, db.corregido.nivel.grilla_depurado)
tipos$etiqueta <- 1:nrow(tipos)

# fin ---




# detalles de plot ----

tipos$bias <- round(tipos$bias, 0) # 0 en pp, 1 decimal para t2 multipleforcing
min.bias <-  round(min(tipos$bias), 0) ; min.bias
max.bias <-  round(max(tipos$bias), 0) ; max.bias

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

color.i <- db.bias.y.color$color.bias[as.character( db.bias.y.color$n.bias )%in% as.character( tipos$bias[1] )]

# fin ---




# Plot ----

diagrama.de.Taylor.modificado.2(R.i = tipos$r[1], sd.ref.i = tipos$sd.observado[1], sd.model.i = tipos$sd.simulado[1],
                                tamanho.punto.de.referencia=1, pos.cor=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, 
                                pch = pch.bias, show.gamma = TRUE, ancho.linea.de.pch = 0.5, main = '', xlab = 'Standard Deviation (Normalized)')
text(db.era5.raw_depurado$x[1], db.era5.raw_depurado$y[1], tipos$etiqueta[1], cex=0.9, font=2, col='white')


for (i in 2:nrow(tipos)) {
  # i <- 5
  color.i <- db.bias.y.color$color.bias[as.character( db.bias.y.color$n.bias )%in%as.character( tipos$bias[i] )]
  
  diagrama.de.Taylor.modificado.2(R.i = tipos$r[i], sd.ref.i = tipos$sd.observado[i], sd.model.i = tipos$sd.simulado[i],
                                  add=TRUE, col = color.i, pcex = tamanho.pch.bias.leyenda, pch = pch.bias, ancho.linea.de.pch = 0.5)
  text(tipos$x[i], tipos$y[i], tipos$etiqueta[i], cex=0.9, font=2, col='white')
}

leyenda.i <- paste('(', tipos$etiqueta, ')', ' ', tipos$tipo, sep = '') ; leyenda.i

colorlegend(db.bias.y.color$color.bias, intervalos.para.leyenda, xlim = c(1.55, 1.71), ylim = c(0.5, 1.5))
text(1.585, 1.55, 'BIAS', srt = 0, font = 2)
legend(0.5, 1.8, legend=leyenda.i, horiz=FALSE, cex = 0.71, ncol = 3, lty = NULL, text.font = 2)
#text(1.72, 1.7, '(b)', font = 2) # '(a)' en t2, '(b)' en pp

# fin ---
