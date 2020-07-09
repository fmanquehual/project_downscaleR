library(loadeR)
library(transformeR)
library(visualizeR)

# Acerca de los datos ----

# Weather station data are most often stored in the form
# of text/csv files instead NetCDF. In the following, we 
# describe the standard format for observational datasets 
# considered in loadeR, which is the same defined within the 
# COST Action VALUE. Then, the VALUE ECA&D dataset is going to 
# be used as example, which contains weather data of 86 stations
# spread over Europe, and is available for download:

# fin ---


# Lectura de datos ----

value <- tempfile(fileext = ".zip")
download.file("www.value-cost.eu/sites/default/files/VALUE_ECA_86_v2.zip", 
              destfile = value)
# Data inventory
di <- dataInventory(dataset = value)

valuefiles <- tempdir()
unzip(value, exdir = valuefiles)
head(read.table(paste0(valuefiles, "/VALUE_ECA_86_v2/stations.txt"), sep = ",", header = TRUE))
head(read.table(paste0(valuefiles, "/VALUE_ECA_86_v2/variables.txt"), sep = ",", header = TRUE))
head(read.table(paste0(valuefiles, "/VALUE_ECA_86_v2/tmin.txt"), sep = ",", header = TRUE))
print(sessionInfo())

stationInfo(value)



example1 <- loadStationData(dataset = value, 
                            var="tmax", 
                            stationID = c("000234", "003946"), 
                            season = 6:8, 
                            years = 1981:2000)

example2 <- loadStationData(dataset = value, 
                            var="tmax", 
                            lonLim = -2.03, 
                            latLim = 43.3, 
                            season = 6:8, 
                            years = 1981:2000)

example3 <- loadStationData(dataset = value, 
                            var="tmax", 
                            lonLim = c(-5,10), 
                            latLim = c(37,45), 
                            season = 6:8, 
                            years = 1981:2000)

example4 <- loadStationData(dataset = value, 
                            var="tmax", 
                            season = 6:8, 
                            years = 1981:2000)


Madrid <- subsetGrid(example1, station.id = "003946")
Donostia <- subsetGrid(example1, station.id = "000234")

temporalPlot(Madrid, Donostia, xyplot.custom = list(ylab = "Tasmax ºC"))
spatialPlot(climatology(example3), backdrop.theme = "countries", colorkey = T)

# fin ---




# Acerca de los datos ----

# The GSN dataset contains data for a World station network. A subset 
# containing the stations with at least the 75% of the data in the period 
# 1979-2012 (374 stations) can be downloaded as follows:

# fin ---

# Lectura de datos ----

gsn <- tempfile(fileext = "GSN_World.zip")
download.file("http://meteo.unican.es/work/loadeR/data/GSN_World.zip", 
              destfile = gsn)
gsnload <- loadStationData(gsn, var = "tmean")

spatialPlot(climatology(gsnload), backdrop.theme = "coastline", colorkey = T)

# fin ---





# Acerca de los datos ----

# The VALUE ECA&D dataset contains weather data of 86 stations 
# spread over Europe, and is available for download:

# fin ---

value <- tempfile(fileext = "VALUE_ECA_86_v2.zip")
download.file("www.value-cost.eu/sites/default/files/VALUE_ECA_86_v2.zip", 
              destfile = value)

valueload <- loadStationData(value, var = "tmean")
spatialPlot(climatology(valueload), backdrop.theme = "coastline", colorkey = T)
print(sessionInfo())

# fin ---

# fuente
# https://github.com/SantanderMetGroup/loadeR/wiki/Accessing-and-loading-station-data