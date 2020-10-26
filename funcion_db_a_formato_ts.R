library(lubridate)

db_a_formato_ts <- function(base_de_datos, calculo_diario=sum, calculo_mensual=mean){
  
  # base_de_datos <- eqm
  
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
  valor.mensual <- as.vector(calculo.diario)
  
  
  # calculo a nivel mensual
  base_de_datos_2 <- data.frame(fecha=mes.2, valor=valor.mensual)
  base_de_datos_2$fecha <- paste0(base_de_datos_2$fecha, '-01')
  base_de_datos_2$mes <- month(as.Date(base_de_datos_2$fecha, '%Y-%m-%d'))
  
  base_de_datos_2 <- base_de_datos_2[order(base_de_datos_2$mes),]
  calculo.mensual <- tapply(base_de_datos_2$valor, base_de_datos_2$mes, calculo_mensual)
  mes.3 <- names(calculo.mensual)
  valor.anual <- as.vector(calculo.mensual)
  
  valor.i <- as.vector(valor.anual)
  
  matriz <- matrix(valor.i, nrow = length(valor.i), ncol = 1, byrow = TRUE)
  
  series.de.tiempo <- ts(data = matriz, frequency = 1)
  
  message('Listo')
  
  return(series.de.tiempo)
}