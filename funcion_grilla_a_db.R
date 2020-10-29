library(raster)

grilla_a_db <- function(SpatialGridDataFrame, lista_climate4R_referencia){
  
  # SpatialGridDataFrame <- era5.en.ubicacion.de.estacion.i
  # lista_climate4R_referencia <- estaciones
  
  pixel_de_estaciones <- stack(SpatialGridDataFrame)
  nombre_estaciones_metereologicas <- lista_climate4R_referencia$Metadata$name
  db <- c()
  
  for (i in 1:length(nombre_estaciones_metereologicas)) {
  # i <- 2
  nombre_de_estacion <- nombre_estaciones_metereologicas[i]
  valores_de_pixel <- as.vector(extract(pixel_de_estaciones, lista_climate4R_referencia$xyCoords[i,]))
  fechas_de_valores <- colnames(extract(pixel_de_estaciones, lista_climate4R_referencia$xyCoords[i,]))
  
  fechas_de_valores2 <- gsub('GMT', '', fechas_de_valores)
  fechas_de_valores3 <- gsub('X', '', fechas_de_valores2)
  fechas_de_valores4 <- gsub('\\.', '-', fechas_de_valores3)
  fechas_de_valores5 <- sub('-$', '', fechas_de_valores4)
  
  db0 <- data.frame(fecha=fechas_de_valores5, valor=valores_de_pixel, 
                    nombre_estacion=nombre_de_estacion)
  
  db <- rbind(db, db0)
  
  }
  
  message('Listo')
  
  return(db)
  
}