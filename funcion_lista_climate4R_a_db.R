lista_climate4R_a_db <- function(lista.climate4R){
  
  # lista.climate4R <- estaciones.con.todos.los.anhos
  
  valores <- lista.climate4R$Data
  fecha <- lista.climate4R$Dates$start
  fecha2 <- as.Date(fecha, '%Y-%m-%d')
  db <- data.frame(fecha=fecha2, valor=valores)
  
  return(db)
  
}