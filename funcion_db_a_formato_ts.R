db_a_formato_ts <- function(lista.climate4R, tipo.variable='pp', funcion=mean){
  
  if(tipo.variable=='pp'){ message('Preparando la db para precipitacion') } else(
    message('Preparando la db para temperatura') )
  
  datos.i <- lista.climate4R$Data
  fecha.i <- lista.climate4R$Dates$start

  dia <- day(as.Date(fecha.i, '%Y-%m-%d'))
  
  calculo <- tapply(datos.i, dia, funcion)
  valor.i <- as.vector(calculo)
  
  matriz <- matrix(valor.i, nrow = length(valor.i), ncol = 1, byrow = TRUE)
  
  series.de.tiempo <- ts(data = matriz, frequency = 1)

  message('Listo')
  
  return(series.de.tiempo)
}


