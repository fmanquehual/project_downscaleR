library(loadeR)
library(visualizeR)

# Acerca de los datos ----

# Datasets provided through an OPeNDAP (remote data access protocols)...
# service, can be remotely accessed and loaded. Here, we illustrate an... 
# example for the NCEP/NCAR Reanalysis 1 data, that is available trough... 
# the NOAA thredds data server

# fin ---

# Leyendo datos remotos ----
di <- dataInventory("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis/surface/air.sig995.1948.nc")
air <- loadGridData("http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis/surface/air.sig995.1948.nc", 
                    var = "air")
# fin ---

download.file("http://meteo.unican.es/work/loadeR/data/OPeNDAP_NCEP/ncepReanalysis1_4xDaily.ncml", 
              destfile = "ncep.ncml")

ncep.remote <- "ncep.ncml"
system("cat ncep.ncml")

di <- dataInventory(ncep.remote)
names(di)

tx <- loadGridData(dataset = ncep.remote, 
                   var = "tmax",
                   season = 6, # junio
                   years = 1991)

tx.mean <- loadGridData(dataset = ncep.remote,
                   var = "tmax",
                   season = 6,
                   years = 1991,
                   time = "DD",
                   aggr.d = "mean")

spatialPlot(climatology(tx), backdrop.theme = "countries", color.theme = "YlOrRd")
spatialPlot(climatology(tx.mean), backdrop.theme = "countries", color.theme = "YlOrRd")

# fin ---






# Acerca de los datos ----

# The EOBS database can be remotely accessed via OPeNDAP using...
# loadeR. The different datasets are available can be accessed here:

# fin ---

# Precipitation dataset URL
ds <- "http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.25regular/rr_0.25deg_reg_v17.0.nc"
# Load daily spring precipitation (MAM) for the period 1991-2000 over the Iberian Peninsula:
precip.MAM <- loadGridData(dataset = ds,
                           var = "rr",
                           lonLim = c(-10,3),
                           latLim = c(36,44),
                           years = 1991:2000,
                           season = 3:5)

spatialPlot(climatology(precip.MAM), backdrop.theme = "countries", color.theme = "YlGnBu")

# fin ---





# Acerca de los datos ----

# It provides open online access to different CMIP5 downscaled climate... 
# scenarios for the entire globe from the The NASA Earth Exchange Global... 
# Daily Downscaled Projections (NEX-GDDP)

# fin ---

help("loadGridData")

## Example of data load from a remote repository via OPeNDAP (NASA dataserver)
ds <- "http://dataserver3.nccs.nasa.gov/thredds/dodsC/bypass/NEX-GDDP/bcsd/rcp85/r1i1p1/tasmax/MIROC-ESM.ncml"
# Monthly mean maximum summer 2m temperature at 12:00 UTC over the Iberian Peninsula:
# (CMIP5 MIROC-ESM model, RCP 8.5)
tasmax <- loadGridData(dataset = ds,
                       var = "tasmax",
                       lonLim = c(-10,5),
                       latLim = c(35,44),
                       season = 6:8,
                       years = 2021,
                       time = "12",
                       aggr.m = "mean")
str(tasmax)
spatialPlot(climatology(tasmax), backdrop.theme = "countries", color.theme = "YlOrRd")

# fin ---

# fuente
# https://github.com/SantanderMetGroup/loadeR/wiki/Loading-remote-grid-data