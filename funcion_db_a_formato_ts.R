library(lubridate)

db_a_formato_ts <- function(base_de_datos, calculo_diario=sum, calculo_mensual=mean){
  
  # base_de_datos <- era5
  
  # calculo a nivel diario
  anho <- year(base_de_datos$fecha)
  mes <- month(base_de_datos$fecha)
  
  base_de_datos$anho_mes <- paste(anho, mes, sep = '-')
  
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