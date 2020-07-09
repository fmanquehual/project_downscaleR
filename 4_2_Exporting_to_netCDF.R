# instalacion de 'loadeR.2nc' ----

# library(devtools)
# install_github("SantanderMetGroup/loadeR.2nc")

# fin ---


library(raster)
library(loadeR.2nc)

# Lectura de datos ----

data(tx) # A climate4R grid

# fin ---


# Creacion netcdf ----

# Name of output file:
fileName <- "tasmax_WFDEI_JJA_W2001_2010.nc4"

# Including a global attribute:
globalAttributeList <- list("institution" = "SantanderMetGroup, http://www.meteo.unican.es/")

# Including two variable attributes:
varAttributeList <- list(var_attr1 = "one_attribute", var_attr2 = "another_attribute")

# Create file:
grid2nc(data = tx,
        NetCDFOutFile = fileName,
        missval = 1e20,
        prec = "float",
        globalAttributes = globalAttributeList,
        varAttributes = varAttributeList)
# fin ---


# archivo netcdf creado ----

r <- raster('tasmax_WFDEI_JJA_W2001_2010.nc4')
plot(r)

# fin ---


# fuente 
# https://github.com/SantanderMetGroup/loadeR/wiki/Exporting-to-netCDF