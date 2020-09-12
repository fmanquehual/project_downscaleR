library(loadeR)
library(visualizeR)
library(downscaleR)
library(transformeR)

rm(list=ls())
dev.off()

# Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_261/")  #Windows
# Sys.getenv("JAVA_HOME")



# descargando datos ----

# No funciono! copie el link y lo descargue a traves del navegador

# value <- tempfile(fileext = ".zip")
# download.file("www.value-cost.eu/sites/default/files/VALUE_ECA_86_v2.zip", 
#               destfile = value)

# fin ---




# Descomprimiendo el archivo ----

# setwd('C:/Users/Usuario/Downloads/')
# dir()
# untar(dir()[24]) # o unzip()

# fin ---




# Explorando contenido de la carpeta ----

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/VALUE_ECA_86_v2/')
dir()

read.table("stations.txt", sep = ",", header = TRUE)
read.table("variables.txt", sep = ",", header = TRUE)
ej1 <- read.table("tmin.txt", sep = ",", header = TRUE)
ej2 <- read.table("tmax.txt", sep = ",", header = TRUE)
ej3 <- read.table("precip.txt", sep = ",", header = TRUE)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_transformados_a_ASCII/')

read.table("variables.txt", sep = ",", header = TRUE)
ej4 <- read.table("tmin.txt", sep = ",", header = TRUE)
ej5 <- read.table("tmax.txt", sep = ",", header = TRUE)
ej6 <- read.table("precip.txt", sep = ",", header = TRUE)

# fin ---




# cargando datos ----

nombre.carpeta <- 'VALUE_ECA_86_v2'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
di <- dataInventory(dataset = nombre.carpeta)
di

str(di)

di2 <- dataInventory(dataset = nombre.carpeta, return.stats= TRUE)
di2$Summary.stats

stationInfo(nombre.carpeta)

# fin ---

nombre.carpeta2 <- 'datos_transformados_a_ASCII'

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
dj <- dataInventory(dataset = nombre.carpeta2, return.stats = TRUE)
dj$Summary.stats
dj

stationInfo(nombre.carpeta2)


# Time series ----
setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')

example1 <- loadStationData(dataset = nombre.carpeta2, 
                            var="tmin", 
                            stationID = c('Bahia.Murta_DGA', 'BalmacedaAd_DMC', 'Chile.ChicoAd_DMC', 'Cochrane_INIA'),
                            years = 2010)

balmaceda <- subsetGrid(example1, station.id = "BalmacedaAd_DMC")
cochrane <- subsetGrid(example1, station.id = "Cochrane_INIA")

temporalPlot(balmaceda, cochrane, xyplot.custom = list(ylab = "Minimum Temperature ºC"))
spatialPlot(climatology(example1), backdrop.theme = "countries", colorkey = T)

# fin ---





# estaciones a nivel mundial ----
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
# dir.create('mydirectory')
# download.file("http://meteo.unican.es/work/loadeR/data/GSN_World.tar.gz",
#               destfile = "mydirectory/GSN_World.tar.gz")

# Extract files
# untar("mydirectory/GSN_World.tar.gz", exdir = "mydirectory")

# Define the path to the data directory

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')

gsn <- "mydirectory/GSN_World/"
dir()
di <- dataInventory(dataset = gsn)

gsn <- "mydirectory/GSN_World"
gsnload <- loadStationData(gsn, var = "tmean")
spatialPlot(climatology(gsnload), backdrop.theme = "coastline", colorkey = T)
# fin ---


# fuente: https://github.com/SantanderMetGroup/loadeR/wiki/Accessing-and-loading-station-data