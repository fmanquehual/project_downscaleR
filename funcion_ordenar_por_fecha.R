ordenar_por_fecha <- function(base_de_datos){

  db.eqm.ordenado <- db.eqm[order(db.eqm$fecha),]
  row.names(db.eqm.ordenado) <- 1:nrow(db.eqm.ordenado)
  
  return(db.eqm.ordenado)
  
}