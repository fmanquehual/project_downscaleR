library(loadeR)
library(visualizeR)

# NO FUNCIONA !

# Acerca de los datos ----

# In the previous sections of the Wiki (2. Model Data), it has been 
# shown how to access and load model data. In the cases where observation 
# data are also gridded, the way to load this data is the same. In 
# this example the E-OBS gridded dataset will be used, we can access 
# the NetCDF files stored in the OPeNDAP catalog http://opendap.knmi.nl/knmi/thredds/catalog.html 
# through the corresponding NcML file. The NcML and dic files can be downloaded as follows:

# fin ---

setwd('/home/msomos/Documentos/proyecto_DownscaleR/proyecto_DownscaleR/mydirectory/')

download.file("http://meteo.unican.es/work/loadeR/data/OPeNDAP_EOBS/Grid_0.25deg_reg_v12/Grid_0.25deg_reg_v12.ncml", 
              destfile = "eobs.ncml")

download.file("http://meteo.unican.es/work/loadeR/data/OPeNDAP_EOBS/Grid_0.25deg_reg_v12/Grid_0.25deg_reg_v12.dic", 
              destfile = "eobs.dic")

dir.eobs <- 'eobs.ncml'
di <- dataInventory(dir.eobs)
names(di)

example1.grid <- loadGridData(dataset = dir.eobs, 
                              var = "tas", 
                              lonLim = c(-12, 5), 
                              latLim=c(35,45), 
                              season=c(6:8), 
                              years = 1981:2000)
# fuente
# https://github.com/SantanderMetGroup/loadeR/wiki/Accessing-and-loading-grid-data