anhadiendo_altitud <- function(data_base, sistema_de_coordenadas='wgs84'){

  data_base <- estaciones.db.todos
  sistema_de_coordenadas <- 'wgs84'
  
  wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  if(sistema_de_coordenadas=='wgs84'){sistema_de_coordenadas_i <- wgs84
    } else if(sistema_de_coordenadas=='utm18'){sistema_de_coordenadas_i <- utm18
      } else if(sistema_de_coordenadas=='utm19'){sistema_de_coordenadas_i <- utm19
        } else(sistema_de_coordenadas_i <- sistema_de_coordenadas)
  
  estaciones.shp0 <- SpatialPoints(data_base[,c('longitude', 'latitude')], proj4string = CRS(sistema_de_coordenadas_i))
  estaciones.shp <- SpatialPointsDataFrame(estaciones.shp0, data = data_base, match.ID = TRUE)
  
  # plot(estaciones.shp, pch = 16, col = 'red')
  # text(estaciones.shp, data_base$name, pos = 3, cex=0.5)
  
  setwd('C:/Users/Usuario/Documents/Francisco/coberturas/')
  dem.area.de.estudio <- raster('clip_dem_sudamerica.tif')
  data_base$altitude <- as.character(data_base$altitude)
  data_base$altitude <- extract(dem.area.de.estudio, estaciones.shp)
  
  return(data_base)

}
