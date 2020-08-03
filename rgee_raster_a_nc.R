# Load the libraries ----
library(ncdf4)
library(raster)
library(lattice)
library(zoo)


rm(list=ls())
dev.off()


# De tif a nc ----
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/raster_descargados_con_rgee/')

lista.de.archivos.raster <- list.files(pattern = '.tif') ; lista.de.archivos.raster
raster.i <- stack(lista.de.archivos.raster)
plot(raster.i, 1:4)
raster.i

nombre.variable <- 'mean_2m_air_temperature'
nombre.variable.en.nc <- 'tas'
anhos <- 2010:2015
meses <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

xy <- coordinates(raster.i[[1]])

lon.valor <- unique(xy[,'x'])
lat.valor <- unique(xy[,'y'])
valores.variable <- as.matrix(raster.i[[1]])
dim(valores.variable)

db.out <- c()
for (i in 1:length(anhos)) {
  db <- data.frame(anho = anhos[i], mes = meses)
  db.out <- rbind(db.out, db)
}

db.out$nombre.de.banda <- paste(db.out$anho, db.out$mes, sep = '')
head(db.out)
table(db.out$anho)

fecha.de.img <- gsub('X_', '', names(raster.i))
nuevo.nombre.de.banda <- paste(fecha.de.img, nombre.variable, sep = '_')

anho.raster.i <- db.out$anho[db.out$nombre.de.banda%in%fecha.de.img]
mes.raster.i <- db.out$mes[db.out$nombre.de.banda%in%fecha.de.img]
fecha.raster.i00 <- paste(anho.raster.i, mes.raster.i, sep = '-')
fecha.raster.i0 <- as.yearmon(fecha.raster.i00)
fecha.raster.i <- as.Date(fecha.raster.i0)
fecha.raster.dias.julianos <- as.vector(julian(fecha.raster.i))

dim.variable <- ncdim_def(nombre.variable.en.nc, units='K', vals=as.double(valores.variable))
dim.time <- ncdim_def('t', units='mes', longname = 'dia_juliano', calendar="julian", 
                      vals=fecha.raster.dias.julianos, unlim = TRUE)

dim.lon <- ncdim_def('lon', units='grados_este', longname='Longitud E', vals=as.double(lon.valor))
dim.lat <- ncdim_def('lat', units='grados_norte', longname='Latitud N', vals=as.double(lat.valor))


def.var <- ncvar_def(name=nombre.variable.en.nc, units='K', dim=list(dim.time, dim.lon, dim.lat), 
                     missval=NA, prec = 'float')

def.lon <- ncvar_def(name='lon_bnds', units='grados_este', dim=list(dim.lon),
                     longname='Longitud E', prec='double')

def.lat <- ncvar_def(name='lat_bnds', units='grados_norte', dim=list(dim.lat),
                     longname='Latitud N', prec='double')

# we'll create a list of all variables so we can add them all at once
var.i <- list(def.var, def.lon, def.lat)

# We got the headers done, now let's make the file
nombre.archivo.nc <- paste(nombre.variable.en.nc, '.nc', sep = '')

# Create a new empty netcdf file.
archivo.nc <- nc_create(nombre.archivo.nc, var.i, force_v4=TRUE, verbose = FALSE)

# put variables

for (i in 1:nlayers(raster.i)) {
  # i <- 1
  valores.variable <- as.vector(raster.i[[i]])
  ncvar_put(archivo.nc, def.var, valores.variable, start=c(i,1,1), count=c(1,
                                                                    length(lon.valor),
                                                                    length(lat.valor)) )
  mensaje <- paste('Iteracion', i, 'lista de', nlayers(raster.i))
  message(mensaje)
  }

ncvar_put(archivo.nc, def.lon, lon.valor, start = 1, count = length(lon.valor))
ncvar_put(archivo.nc, def.lat, lat.valor, start = 1, count = length(lat.valor))

# Add some extra attributes
ncatt_put(archivo.nc,"lat","axis","Latitud")
ncatt_put(archivo.nc,"lon","axis","Longitud")
#ncatt_put(archivo.nc,"t","dia_juliano", fecha.raster.dias.julianos)
ncatt_put(archivo.nc,"t","dia_juliano_inicio", fecha.raster.dias.julianos[1])

# This variable was implicitly created by the dimension, so we'll just specify it by name
ncatt_put(archivo.nc, nombre.variable.en.nc, 'coordinates', 'lat lon')

# add global attributes
ncatt_put(archivo.nc,0,"Datos",'ERA 5 (mensual)')
ncatt_put(archivo.nc,0,"Institucion", 'ECMWF')
ncatt_put(archivo.nc,0,"EPSG de la capa original", '4326')
ncatt_put(archivo.nc,0,"Capa original procesada por", 'Francisco F. Manquehual Cheuque')
ncatt_put(archivo.nc,0,"Fecha de creacion", format(Sys.time(), "%d-%m-%Y"))
ncatt_put(archivo.nc,0,"Otros", 'A traves de R, se utilizo el paquete rgee para trabajar con las herramientas y datos disponibles de Google Engine')

archivo.nc

nc_close(archivo.nc)

# fin ---




# Probando el nc (resultado) ----

nc.i <- list.files(pattern = '.nc')[2]
ncin <- nc_open(nc.i)
ncin

valores <- ncvar_get(ncin, nombre.variable.en.nc)
lon <- ncvar_get(ncin, 'lon')
lat <- ncvar_get(ncin, 'lat')
t <- ncvar_get(ncin, 't')

head(valores)
head(lon)
head(lat)
head(t)

nc_close(ncin)

valores.i <- valores[4,,] 
dim(valores.i) # debe tener las dimensiones de la matriz del area de estudio (25*20)

r <- raster(t(valores.i), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
# r <- flip(r, direction='y')
plot(r)

# fin ---