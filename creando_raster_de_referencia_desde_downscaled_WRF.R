library(ncdf4)
library(raster)
library(rgdal)

rm(list=ls())
dev.off()

# preparacion netcdf referencia ----

lambert <- CRS("+proj=lcc +lat_1=12.190 +lat_0=40 +lon_0=-97 +lat_2=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/downscaled_WRF/')
nc.stack.referencia0 <- stack('RAINRATE_ERAint_Domain2_5a1_Conf10_2018.nc')
plot(nc.stack.referencia0, 50:55)

nc.stack.referencia <- nc.stack.referencia0[[54]]
plot(nc.stack.referencia)
nc.stack.referencia

# writeRaster(nc.stack.referencia, filename="downscaled_WRF_referencia.tif", format="GTiff", overwrite=TRUE)

nc.referencia <- nc_open('RAINRATE_ERAint_Domain2_5a1_Conf10_2018.nc')
nc.referencia

lon <- ncvar_get(nc.referencia, 'XLAT')
lon

lat <- ncvar_get(nc.referencia, 'XLONG')
lat

variable <- ncvar_get(nc.referencia, 'RAINRATE')
variable

dimensiones.netcdf.referencia <- dim(variable) ; dimensiones.netcdf.referencia

# creacion raster referencia

r.referencia.proyectado <- raster(nrow=dimensiones.netcdf.referencia[2], ncol=dimensiones.netcdf.referencia[1], 
                                  xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=lambert)
r.referencia.proyectado[] <- variable[,,54]
plot(r.referencia.proyectado)

r.referencia.proyectado <- t(flip(r.referencia.proyectado, 1))
plot(r.referencia.proyectado)
r.referencia.proyectado

par(mfrow=c(1,2))
plot(nc.stack.referencia, legend=FALSE)
plot(r.referencia.proyectado, legend=FALSE)

# writeRaster(r.referencia.proyectado, filename="downscaled_WRF_referencia.tif", format="GTiff", overwrite=TRUE)

# fin ---



# apuntes de reproyeccion ----

# Para saber las proyecciones que entrega WRF: https://www.ncl.ucar.edu/Applications/wrflc.shtml

# Lambert Conformal Conic (lcc)
# crs.lcc <- CRS("+proj=lcc +lat_1=12.190 +lat_0=40 
#                +lon_0=-97 +lat_2=45
#                +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# 
# # reproject to lcc
# rast.ext <- projectExtent(rast.no.na, crs.lcc)
# rast.lcc <- projectRaster(rast.no.na, rast.ext)

# Fuente: https://weiming-hu.github.io/projection-in-R/
