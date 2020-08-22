library(loadeR)
library(visualizeR)
library(downscaleR)

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

setwd('C:/Users/Usuario/Downloads/VALUE_ECA_86_v2/')
dir()

read.table("stations.txt", sep = ",", header = TRUE)
read.table("variables.txt", sep = ",", header = TRUE)
read.table("tmin.txt", sep = ",", header = TRUE)

# fin ---




# cargando datos ----

nombre.carpeta <- 'VALUE_ECA_86_v2'

setwd('C:/Users/Usuario/Downloads/')
di <- dataInventory(dataset = nombre.carpeta)
di

str(di)

di2 <- dataInventory(dataset = nombre.carpeta, return.stats= TRUE)
di2$Summary.stats

stationInfo(nombre.carpeta)

# fin ---




# Time series ----
setwd('C:/Users/Usuario/Downloads/VALUE_ECA_86_v2/')

example1 <- loadStationData(dataset = 'tmax.txt', 
                            var="tmax", 
                            stationID = c("000234", "003946"), 
                            season = 6:8, 
                            years = 1981:2000)
# fin ---




# ----



# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
# dir.create('mydirectory')
# download.file("http://meteo.unican.es/work/loadeR/data/GSN_World.tar.gz",
#               destfile = "mydirectory/GSN_World.tar.gz")

# Extract files
# untar("mydirectory/GSN_World.tar.gz", exdir = "mydirectory")

# Define the path to the data directory

library(rJava)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_261")  #Windows
Sys.getenv("JAVA_HOME")

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')

gsn <- "mydirectory/GSN_World/"
dir()
di <- dataInventory(dataset = gsn)

gsn <- "mydirectory/GSN_World"
gsnload <- loadStationData(gsn, var = "tmean")
