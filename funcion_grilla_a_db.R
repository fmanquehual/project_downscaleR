library(raster)

grilla_a_db <- function(SpatialGridDataFrame){
  
  pixel_de_estaciones <- stack(SpatialGridDataFrame)
  valores_de_pixel <- as.vector(extract(pixel_de_estaciones, estaciones$xyCoords))
  fechas_de_valores <- colnames(extract(pixel_de_estaciones, estaciones$xyCoords))
  
  fechas_de_valores2 <- gsub('GMT', '', fechas_de_valores)
  fechas_de_valores3 <- gsub('X', '', fechas_de_valores2)
  fechas_de_valores4 <- gsub('\\.', '-', fechas_de_valores3)
  fechas_de_valores5 <- sub('-$', '', fechas_de_valores4)
  
  db <- data.frame(fecha=fechas_de_valores5, valor=valores_de_pixel)
  
  message('Listo')
  
  return(db)
  
}