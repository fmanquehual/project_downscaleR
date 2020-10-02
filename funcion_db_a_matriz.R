db_a_matriz <- function(data_base, filas_son_fechas=FALSE, separador_fecha='-'){
  
  # data_base <- generador_de_matriz(db.tmin)
  # filas_son_fechas=TRUE
  # separador_fecha='-'
  
  col.j <- colnames(data_base)[-1]
  row.i <- as.character(data_base[,1])
  
  if(separador_fecha=='-'){separador_fecha <- ''}
  
  if(filas_son_fechas==TRUE){c(
      formato.fechas <- paste('%Y', '%m', '%d', sep = separador_fecha),
      row.j <- as.character(as.Date(row.i, formato.fechas))
      )
    }
  
  values.i <- t(as.matrix(data_base[,-1]))
  m <- matrix(values.i, nrow = nrow(values.i), ncol = ncol(values.i), byrow = FALSE, 
              dimnames = list(col.j, row.j))
  
  return(m)
}