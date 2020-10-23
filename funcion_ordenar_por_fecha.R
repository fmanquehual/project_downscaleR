ordenar_por_fecha <- function(base_de_datos){

  base_de_datos_ordenado <- base_de_datos[order(base_de_datos$fecha),]
  row.names(base_de_datos_ordenado) <- 1:nrow(base_de_datos_ordenado)
  
  return(base_de_datos_ordenado)
  
}