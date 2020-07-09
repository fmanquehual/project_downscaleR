library(loadeR)
library(visualizeR)

# Harmonization ----

# The dictionary file is the tool used in loadeR to harmonize the 
# variables according to the climate4R vocabulary

# fin ---


# Acerca de los datos ----

# In this worked example we will use the same local dataset as that used
# in section 2.1. Dataset definition and loading local grid data. This 
# dataset comes from the NCEP/NCAR Reanalysis 1 encompassing the period 
# 1961-2010 for the Iberian Peninsula domain and is available in a tar.gz 
# file that can be downloaded and stored in a local directory as follows:

# fin ---


setwd('/home/msomos/Documentos/proyecto_DownscaleR/proyecto_DownscaleR/mydirectory/')

# download.file("http://meteo.unican.es/work/loadeR/data/Iberia_NCEP.tar.gz", 
#               destfile = "Iberia_NCEP.tar.gz")

# Extract files from the tar.gz file
untar("Iberia_NCEP.tar.gz", exdir = "mydirectory")

# First, the path to the ncml file is defined:
ncep.local <- "Iberia_NCEP/Iberia_NCEP.ncml"

di <- dataInventory(ncep.local)
# e.g. temperature
str(di$`2T`)


# The corresponding variable name of 2m air temperature is "2T" and 
# the units are Kelvin. We can load this data with `loadGridData`as follows:
  
tas <- loadGridData(ncep.local, 
                    var = "2T",
                    lonLim = c(-12, 5), 
                    latLim= c(35,45), 
                    season= 6:8, 
                    years = 1981:2000)
C4R.vocabulary()

dictionary <- "Iberia_NCEP/Iberia_NCEP.dic"
read.table(dictionary, header = TRUE, sep = ",")

tas2 <- loadGridData(ncep.local, var = "tas", dictionary = TRUE)

str(tas$Variable)
str(tas2$Variable)

# fuente
# https://github.com/SantanderMetGroup/loadeR/wiki/Harmonization
