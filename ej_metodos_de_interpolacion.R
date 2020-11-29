setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

db.parametros <- read.csv('parametros_de_correcion_1995_2010.csv')
db.estaciones <- read.csv('valores_observados_1995_2017.csv')
db.era5 <- read.csv('valores_era5_1995_2017.csv')



# Estimando 'a' y 'b' con IDW ----

nombre.estaciones <- estaciones$Metadata$name
altitud.de.estaciones <- estaciones$Metadata$altitude
x <- estaciones$xyCoords$x
y <- estaciones$xyCoords$y

db.estaciones.xy <- data.frame(nombre_estacion=nombre.estaciones, lon=x, lat=y)
db.parametros.xy <- merge(db.parametros, db.estaciones.xy, by='nombre_estacion')

head(db.parametros.xy)

pr.sum.total.stack <- stack(era5.en.ubicacion.de.estacion.i)
pr.sum.total.i <- pr.sum.total.stack[[1]]
raster.referencia <- pr.sum.total.i
p.puntos <- rasterToPolygons(pr.sum.total.i, dissolve = TRUE)

bloques.unicos <- sort(unique(db.parametros.xy$bloque))
db <- c()
#  for (i in bloques.unicos) {
  i <- 1
  
  mensaje.inicio <- paste0("Estimando 'a' y 'b' de bloque ", i, ' de ', length(bloques.unicos))
  message(mensaje.inicio)
  
  db.parametros.xy.i <- subset(db.parametros.xy, bloque==i)
  
  # Quitando NA
  
  id.valores.NA <- which(is.na(db.parametros.xy.i$a))
  
  if(length(id.valores.NA)>0){db.parametros.xy.sin.NA.i <- db.parametros.xy.i[-id.valores.NA,]
  } else(db.parametros.xy.sin.NA.i <- db.parametros.xy.i)
  
  row.names(db.parametros.xy.sin.NA.i) <- 1:nrow(db.parametros.xy.sin.NA.i)
  
  
  # Transformacion a Spatial Points Data Frame
  
  puntos0 <- SpatialPoints(cbind(db.parametros.xy.sin.NA.i$lon, db.parametros.xy.sin.NA.i$lat), 
                           proj4string = CRS(wgs84))
  puntos <- SpatialPointsDataFrame(puntos0, data = db.parametros.xy.sin.NA.i, match.ID = TRUE)
  
  r.puntos.a <- rasterize(puntos, pr.sum.total.i, field="a", fun=mean, background=NA)
  r.puntos.b <- rasterize(puntos, pr.sum.total.i, field="b", fun=mean, background=NA)
  
  puntos@bbox <- p.puntos@bbox
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(p.puntos, "regular", n=dim(pr.sum.total.i)[1]*dim(pr.sum.total.i)[2]))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  proj4string(grd) <- crs(puntos)
  
  
  
  # Nearest Neighbor
  fit_NN <- gstat::gstat( # using package {gstat} 
    formula = a ~ 1,    # The column `NH4` is what we are interested in
    data = as(puntos, "Spatial"), # using {sf} and converting to {sp}, which is expected
    nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
  )
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = a ~ 1,
    data = as(puntos, "Spatial"),
    nmax = 10, nmin = 3,
    set = list(idp = 0.5) # inverse distance power
  )
  
  # Thin Plate Spline Regression
  fit_TPS <- fields::Tps( # using {fields}
    x = as.matrix(puntos@data[, c("lon", "lat")]), # accepts points but expects them as matrix
    Y = puntos@data$a,  # the dependent variable
    miles = FALSE     # EPSG 25833 is based in meters
  )
  
  # Generalized Additive Model
  fit_GAM <- mgcv::gam( # using {mgcv}
    a ~ s(lon, lat, k=4),      # here come our X/Y/Z data - straightforward enough
    data = puntos      # specify in which object the data is stored
  )
  
  # Next we use a couple of functions that have a slightly different modus
  # operandi as they in fact already return interpolated Z values.
  
  # Triangular Irregular Surface
  fit_TIN <- interp::interp( # using {interp}
    x = puntos@data$lon,           # the function actually accepts coordinate vectors
    y = puntos@data$lat,
    z = puntos@data$a,
    xo = coordinates(grd)[,'X'] ,     # here we already define the target grid
    yo = coordinates(grd)[,'Y'],
    output = "points"
  ) %>% bind_cols()
  
  
  
  wgs84 <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  puntos_utm18 <- spTransform(puntos, utm18)
  
  sf_puntos <- st_as_sf(puntos_utm18, coords = coordinates(puntos_utm18), crs = 32718)
  
  # Automatized Kriging  
  fit_KRIG <- automap::autoKrige(      # using {automap}
    formula = a ~ 1,                 # The interface is similar to {gstat} but
    input_data = as(sf_puntos, "Spatial") # {automap} makes a lot of assumptions for you
  ) %>% 
    .$krige_output %>%  # the function returns a complex object with lot's of metainfo
    as.data.frame() %>% # we keep only the data we are interested in
    dplyr::select(X = x1, Y = x2, Z = var1.pred) 
  
  # Triangular Irregular Surface
  interp_TIN <- raster::rasterFromXYZ(fit_TIN, crs = utm18)
  interp_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = utm18)
  
  plot(interp_TIN)
  plot(interp_KRIG)
  
  interp_NN <- interpolate(grd_template_raster, fit_NN)
  interp_IDW <- interpolate(grd_template_raster, fit_IDW)
  interp_TPS <- interpolate(grd_template_raster, fit_TPS)

  plot(interp_NN)
  plot(interp_IDW)
  plot(interp_TPS)

  
#   # Calculo de poder optimo para IDW
#   poder.a <- seleccion_de_poder_optimo_para_IDW(muestra_valores_poder = valores.de.poder.IDW.a.evaluar, 
#                                                 capa_de_puntos = puntos, parametro = 'a', nueva_grilla = grd, 
#                                                 raster_de_referencia = raster.referencia, RSR=FALSE, 
#                                                 NSE=TRUE, PBIAS=FALSE, RMSE=FALSE, MAE=TRUE) # NSE-MAE
#   
#   poder.b <- seleccion_de_poder_optimo_para_IDW(muestra_valores_poder = valores.de.poder.IDW.a.evaluar, 
#                                                 capa_de_puntos = puntos, parametro = 'b', nueva_grilla = grd, 
#                                                 raster_de_referencia = raster.referencia, RSR=FALSE, 
#                                                 NSE=TRUE, PBIAS=FALSE, RMSE=FALSE, MAE=TRUE)
#   mensaje <- paste0("Poder de 'a' fue ", poder.a, " y 'b' fue ", poder.b)
#   message(mensaje)
#   
#   # Interpolate the grid cells using a power value of 2 (idp=2.0)
#   puntos.a.idw <- idw(a ~ 1, puntos, newdata=grd, idp=poder.a, debug.level=0) # ~ 1, significa no hay variables independientes
#   r.idw.a0 <- raster(puntos.a.idw)
#   raster.referencia[] <- r.idw.a0[]
#   r.idw.a <- raster.referencia
#   
#   puntos.b.idw <- idw(b ~ 1, puntos, newdata=grd, idp=poder.b, debug.level=0) # ~ 1, significa no hay variables independientes
#   r.idw.b0 <- raster(puntos.b.idw)
#   raster.referencia[] <- r.idw.b0[]
#   r.idw.b <- raster.referencia
#   
#   # setwd('C:/Users/Usuario/Desktop/')
#   # png('IDW_parametro_a_peso_de_5.png', width = 720, height = 720, units = "px", pointsize = 15)
#   # plot(r.idw.a)
#   # plot(cuenca, add=TRUE, border='cyan')
#   # text(r.idw.a, round(r.idw.a[],1), cex=0.7)
#   # text(puntos, round(puntos$a,1), col='red', cex=0.7)
#   # dev.off()
#   
#   db.parametros.xy.i$a_estimado <- extract(r.idw.a, db.parametros.xy.i[,c('lon', 'lat')]) 
#   db.parametros.xy.i$b_estimado <- extract(r.idw.b, db.parametros.xy.i[,c('lon', 'lat')]) 
#   
#   # par(mfrow=c(2,1))
#   # plot(db.parametros.xy.i$a-db.parametros.xy.i$a_estimado, xlab="Estacion", ylab="Observado-Estimado (a)")
#   # abline(h=0, col='red')
#   # plot(db.parametros.xy.i$b-db.parametros.xy.i$b_estimado, xlab="Estacion", ylab="Observado-Estimado (b)")
#   # abline(h=0, col='red')
#   
#   names(r.idw.a) <- paste0('bloque_', i)
#   names(r.idw.b) <- paste0('bloque_', i)
#   
#   if(i==1){stack.matrices.a <- r.idw.a} else(stack.matrices.a <- stack(stack.matrices.a, r.idw.a))
#   if(i==1){stack.matrices.b <- r.idw.b} else(stack.matrices.b <- stack(stack.matrices.b, r.idw.b))
#   
#   db <- rbind(db, db.parametros.xy.i)
#   
# }
# 
