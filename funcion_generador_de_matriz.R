generador_de_matriz <- function(base_de_datos){
  # base_de_datos <- db.pp
  anho.mes.dia <- gsub('-', '', base_de_datos$Date) ; head(anho.mes.dia)
  
  base_de_datos$YYYYMMDD <- anho.mes.dia
  base_de_datos.valores <- base_de_datos[, c("YYYYMMDD", "archivo.con.coordenadas", "valor.observado")]
  # head(base_de_datos.valores)
  
  nombre.archivos.i <- unique( as.character(base_de_datos.valores$archivo.con.coordenadas) )
  
  # Matriz
  for (i in 1:length(nombre.archivos.i)) {
    # i <- 1
    base_de_datos.valores.i <- subset(base_de_datos.valores, archivo.con.coordenadas==nombre.archivos.i[i])
    base_de_datos.valores.i2 <- base_de_datos.valores.i[, c("YYYYMMDD", "valor.observado")]
    colnames(base_de_datos.valores.i2)[2] <- nombre.archivos.i[i]
    # head(base_de_datos.valores.i2)
    
    if(i==1){matriz.de.valores.por.estacion0 <- base_de_datos.valores.i2
    } else(matriz.de.valores.por.estacion0 <- merge(matriz.de.valores.por.estacion0, base_de_datos.valores.i2, by='YYYYMMDD', all=TRUE))
  }
  
  matriz.de.valores.por.estacion <- matriz.de.valores.por.estacion0
  
  return(matriz.de.valores.por.estacion)
  
}
