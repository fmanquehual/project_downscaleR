lista_climate4R_a_db <- function(lista.climate4R){
  
  # lista.climate4R <- pr.sum.corregido.eqm
  
  db <- c()
  
  for (i in 1:ncol(lista.climate4R$Data)) {
    
    valores <- lista.climate4R$Data[,i]
    nombre_estacion <- lista.climate4R$Metadata$name[i]
    
    fecha <- lista.climate4R$Dates$start
    fecha2 <- as.Date(fecha, '%Y-%m-%d')
    
    db0 <- data.frame(fecha=fecha2, valor=valores, nombre_estacion=nombre_estacion)
    db <- rbind(db, db0)  
  }
  
  return(db)
  
}
