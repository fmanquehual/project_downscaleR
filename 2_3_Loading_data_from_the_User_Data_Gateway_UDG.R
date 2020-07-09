library(visualizeR)
library(climate4R.UDG)

# NO FUNCIONA!!!!

# Acerca de los datos ----

# The UDG builds on the THREDDS Access Portal (UDG-TAP) which is 
# the entry point for authentication and data access (information 
# for registration). Authorization is organized in thematic 
# groups, according to the data policies of different projects and 
# international initiatives. There is also public data available 
# that is open for everyone, for instance, the NCEP/NCAR Reanalysis 
# 1 data, that is used in this worked example.

# fin ---

climate4R.UDG::loginUDG(username = "fmanquehual", password = "Francisco&Katherine=2")

tx <- loadGridData(dataset = "http://meteo.unican.es/tds5/dodsC/ncepReanalysis1/ncepReanalysis1_4xDaily.ncml",
                   var = "tmax",
                   season = 6,
                   years = 1991,
                   time = "DD",
                   aggr.d = "mean")

# fuente
# https://github.com/SantanderMetGroup/loadeR/wiki/Loading-data-from-the-User-Data-Gateway-(UDG)