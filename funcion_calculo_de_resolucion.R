calculo_de_resolucion <- function(resultado_variable_dataInventory, latitud=TRUE, redondeo=1){

  # resultado_variable_dataInventory <- inventario.CMIP6$pr
  # latitud = TRUE
  # redondeo = 1
  
  if(latitud==TRUE){coordenada <- resultado_variable_dataInventory$Dimensions$lat$Values} else(
                    coordenada <- resultado_variable_dataInventory$Dimensions$lon$Values)
  
  coordenada1 <- coordenada[1]
  coordenada2 <- coordenada[2]
  
  diferencia.absoluta <- abs(coordenada2-coordenada1)
  diferencia.redondeada <- round(diferencia.absoluta, redondeo)
  
  return(diferencia.redondeada)
}
