library(loadeR)
library(visualizeR)

# Acerca de los datos ----

# This may contain just one file per variable!

# This dataset comes from the NCEP/NCAR Reanalysis 1...
# encompassing the period 1961-2010 for the Iberian...
# Peninsula domain and is available in a tar.gz file...
# that can be downloaded and stored in a local directory as follows:

# fin ---

  
# descargando datos ----

# download.file('http://meteo.unican.es/work/loadeR/data/Iberia_NCEP.tar.gz', 
#               destfile = "mydirectory/Iberia_NCEP.tar.gz")

# fin ---

# Lectura de datos ----

setwd('/home/msomos/Documentos/proyecto_DownscaleR/proyecto_DownscaleR/')
# Extract files from the tar.gz file
untar('mydirectory/Iberia_NCEP.tar.gz', exdir = "mydirectory")

setwd('/home/msomos/Documentos/proyecto_DownscaleR/proyecto_DownscaleR/mydirectory/Iberia_NCEP/')
list.files(pattern = "\\.nc$")
di <- dataInventory("NCEP_pr.nc")
str(di)

pr <- loadGridData(dataset = "NCEP_pr.nc", var = "pr")
str(pr)
monthlyPr <- loadGridData(dataset = "NCEP_pr.nc", 
                          var = "pr",
                          aggr.m = "sum") # obtain daily (aggr.d) or monthly (aggr.m) data 
# fin ---

# plot ---

spatialPlot(climatology(pr), backdrop.theme = "countries", color.theme = "YlGnBu")

# fin ---








# Acerca de los datos ----

# Imagine that we want to load several variables, or that...
# the NetCDF files in the directory are of the same variable...
# but for different time periods and we want to load a large period.

# fin ---


# Lectura de datos ----

makeAggregatedDataset(source.dir = '.', ncml.file = "Iberia_NCEP.ncml", verbose = TRUE)
system("cat Iberia_NCEP.ncml")
ncep.local <- "Iberia_NCEP.ncml"

di <- dataInventory(ncep.local)
names(di)
str(di$T)

T1000 <- loadGridData(dataset = ncep.local, 
                      var = "T@1000", 
                      lonLim = c(-12, 5), 
                      latLim= c(35,45), 
                      season= 6:8, 
                      years = 1981:2000)

str(T1000)
print(sessionInfo())

# plot ----
spatialPlot(climatology(T1000), backdrop.theme = "countries", color.theme = "YlOrRd")

# fin ---



# fuente 
# https://github.com/SantanderMetGroup/loadeR/wiki/Dataset-definition-and-loading-local-grid-data
