plot_series_temporales <- function(base_de_datos, nombre_estaciones){
  
  # base_de_datos <- db
  # nombre_estaciones <- estaciones
  
  for (i in 1:length(nombre_estaciones)) {
    # i <- 1
    
    estacion.i <- nombre_estaciones[i] # ; estacion.i
    
    db.i <- subset(base_de_datos, archivo.con.coordenadas == estacion.i)
    
    db.i$Date <- as.Date(db.i$Date)
    fechas.i <- db.i$Date
    valores.i <- db.i$valor.observado
    
    db.xts.i <- xts(valores.i, fechas.i)
    
    # Mensual
    final.de.mes <- endpoints(db.xts.i, on='months')
    db.mensual.xts.i <- period.apply(db.xts.i, INDEX = final.de.mes, 
                                     FUN = function(x) sum(x, na.rm = TRUE)) # pp mensual
    
    # Anual
    final.anual <- endpoints(db.mensual.xts.i, on='years')
    db.anual.xts.i <- period.apply(db.mensual.xts.i, INDEX = final.anual, 
                                   FUN = function(x) mean(x, na.rm = TRUE)) # pp mensual
    
    # Plots
    print(plot.xts(db.anual.xts.i, main = estacion.i))
    
  }
  
}