library(raster)
library(stringi)

grilla_a_formato_ts <- function(SpatialGridDataFrame, tipo_variable='pp', funcion=mean){
    
      # SpatialGridDataFrame <- pr.sum.puntos
    
  if(tipo_variable=='pp'){ message('Preparando la db para precipitacion') 
    } else( message('Preparando la db para temperatura') )
    
  pixel_de_estaciones <- stack(SpatialGridDataFrame)
  valores_de_pixel <- as.vector(extract(pixel_de_estaciones, estaciones$xyCoords))
  fechas_de_valores <- colnames(extract(pixel_de_estaciones, estaciones$xyCoords))
  
  fechas_de_valores2 <- gsub('GMT', '', fechas_de_valores)
  fechas_de_valores3 <- gsub('X', '', fechas_de_valores2)
  fechas_de_valores4 <- gsub('\\.', '-', fechas_de_valores3)
  fechas_de_valores5 <- sub('-$', '', fechas_de_valores4)
  
  dia <- day(as.Date(fechas_de_valores5, '%Y-%m-%d'))
  
  
  calculo <- tapply(valores_de_pixel, dia, funcion)
  valor_i <- as.vector(calculo)
  
  matriz <- matrix(valor_i, nrow = length(valor_i), ncol = 1, byrow = TRUE)
  
  series.de.tiempo <- ts(data = matriz, frequency = 1)
  
  message('Listo')
  
  return(series.de.tiempo)

}