library(lubridate)

db_a_formato_ts <- function(base_de_datos, estacion=NULL, calculo_diario=sum, calculo_mensual=mean){
  
  # base_de_datos <- eqm.pre.ts
  # estacion <- estacion.de.interes
  
  if(is.null(estacion)){warning('Debes indicar la estacion')}
  
  # Filtro estacion de interes
  base_de_datos <- subset(base_de_datos, nombre_estacion==estacion)
  
  # eliminando NA's
  id_elementos_con_NA <- which(is.na(base_de_datos$valor))
  cantidad_de_NA <- length(id_elementos_con_NA)
  mensaje_cantidad_de_NA <- paste('La base de datos tiene', cantidad_de_NA, 'NA', sep = ' ')
  
  if(cantidad_de_NA > 0){base_de_datos <- base_de_datos[-id_elementos_con_NA,]}
  if(cantidad_de_NA > 0){message(mensaje_cantidad_de_NA)}
  
  # calculo a nivel diario
  base_de_datos$fecha <- as.Date(base_de_datos$fecha)
  anho <- year(base_de_datos$fecha)
  mes <- month(base_de_datos$fecha)
  
  base_de_datos$anho_mes <- paste(anho, mes, sep = '-')
  
  # NO OCUPAR "na.rm=TRUE" EN tapply() PORQUE DA PROBLEMAS
  calculo.diario <- tapply(base_de_datos$valor, base_de_datos$anho_mes, calculo_diario) 
  mes.2 <- names(calculo.diario)
  valor.mensual <- as.vector(calculo.diario) # aqui
  
  
  # calculo a nivel mensual
  base_de_datos_2 <- data.frame(fecha=mes.2, valor=valor.mensual)
  base_de_datos_2$fecha <- paste0(base_de_datos_2$fecha, '-01')
  base_de_datos_2$mes <- month(as.Date(base_de_datos_2$fecha, '%Y-%m-%d'))
  base_de_datos_2 <- base_de_datos_2[order(base_de_datos_2$mes),]
  
  calculo.mensual <- tapply(base_de_datos_2$valor, base_de_datos_2$mes, calculo_mensual)
  mes.3 <- names(calculo.mensual)
  valor.anual <- as.vector(calculo.mensual)
  valor.i <- as.vector(valor.anual)
  
  
  # preparacion de db en caso de que meses no tengan datos
  menores.a.10 <- which(mes.3%in%(1:9))
  mes.3[menores.a.10] <- paste0('0', mes.3[menores.a.10])
  base_de_datos_3 <- data.frame(mes=mes.3, valor=valor.i)
  
  mes.complementario <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:12)
  base_de_datos_3_complementario <- data.frame(mes=mes.complementario, valor=NA)
  
  base_de_datos_4 <- merge(base_de_datos_3, base_de_datos_3_complementario, by='mes', all.y=TRUE)[,c(1,2)]
  colnames(base_de_datos_4) <- colnames(base_de_datos_3)
  
  
  # pasando a formato ts
  matriz <- matrix(base_de_datos_4$valor, nrow = nrow(base_de_datos_4), ncol = 1, byrow = TRUE)
  series.de.tiempo <- ts(data = matriz, frequency = 1)
  
  # str(ej, '%m')
  # library(zoo)
  # library(lubridate)
  # 
  # z <- read.zoo(base_de_datos_3, FUN = as.yearmon, format = '%m', aggregate = function(x) tail(x, 1))
  # ej <- as.ts(z)
  # class(ej)
  
  message('Listo')
  
  return(series.de.tiempo)
}