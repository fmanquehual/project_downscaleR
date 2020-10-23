# Pasos para instalar: https://github.com/SantanderMetGroup/loadeR/wiki/Installation

# En la consola de Ubuntu ----

# sudo apt-get install -y default-jre
# sudo apt-get install -y default-jdk
# sudo R CMD javareconf

# instalando Java ----

# install.packages("rJava")

library(rJava)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_261/")  #Windows
Sys.getenv("JAVA_HOME")


# En R ----
# Verificar si ya tienes instalado 'devtools' para descargar desde github

library(devtools)

install_github(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/climate4R.UDG@devel",
                 #"SantanderMetGroup/climate4R.UDG",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/visualizeR",
                 "SantanderMetGroup/downscaleR"))

# si tienes problemas con 'rlang', 'ellipsis', 'fansi', instalalo manualmente y vuelve a instalar visualizeR
# install_github("SantanderMetGroup/visualizeR")
# install_github("SantanderMetGroup/downscaleR")

install_github('SantanderMetGroup/climate4R.datasets')
install_github('SantanderMetGroup/climate4R.climdex')
install_github('SantanderMetGroup/VALUE')
install_github('SantanderMetGroup/climate4R.value')
install_github(#"SantanderMetGroup/drought4R@v0.1.0",
                         "SantanderMetGroup/convertR@v0.1.2") # convert, para convertir Kelvin a Celcius




# ----
library(devtools)

# install_github("SantanderMetGroup/loadeR.java")
# install_github("SantanderMetGroup/climate4R.UDG@devel")
# install_github("SantanderMetGroup/loadeR@v1.4.5")
# install_github("SantanderMetGroup/transformeR") # no se pudo instalar 'transformeR@v1.4.3'
# install_github("SantanderMetGroup/visualizeR@v1.2.1")
# install_github("SantanderMetGroup/downscaleR") # no se pudo instalar 'downscaleR@v3.0.1'

# install_github("SantanderMetGroup/drought4R@v0.1.0")
# install_github("SantanderMetGroup/convertR@v0.1.2")

# fuente: http://www.meteo.unican.es/work/climate4r/drought4R/drought4R_notebook.html

library(installr)
# uninstall.packages('glue')
# 
# uninstall.packages(c(
#                      'climate4R.UDG',
#                      'convertR',
#                      'climate4R.value',
#                      'VALUE',
#                      "loadeR",
#                      'loadeR.java',
#                      'downscaleR',
#                      'visualizeR',
#                      'climate4R.datasets',
#                      'climate4R.climdex',
#                      'transformeR'))

# install_github(c(
#                  
#                  "SantanderMetGroup/transformeR",
#                  "SantanderMetGroup/downscaleR",
#                  "SantanderMetGroup/visualizeR"
#                  
#                  # "SantanderMetGroup/climate4R.UDG",
#                  # "SantanderMetGroup/loadeR.java",
#                  # "SantanderMetGroup/loadeR",
#                  ))



# install_github("SantanderMetGroup/climate4R.UDG")
# install_github("SantanderMetGroup/loadeR")
# 
# usethis::browse_github_pat(scope = c("fmanquehual", "R:GITHUB_PAT"))
# 
# browse_github_pat(
#   scopes = c("repo", "gist", "user:fcomanquehual@gmail.com"),
#   description = "R:GITHUB_PAT",
#   host = "https://github.com"
# )
# 
# usethis::edit_r_environ()
# 
# devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/climate4R.UDG", "SantanderMetGroup/loadeR"),
#                          force = TRUE)
# 
# devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/climate4R.UDG", "SantanderMetGroup/loadeR"),
#                          force = TRUE)


# En la consola de Ubuntu ----
# R CMD javareconf
# sudo apt install openjdk-X-jdk

# Si no sale bien ----
# export LD_LIBRARY_PATH=/home/msomos/jre-8u251-linux-x64/jre1.8.0_251/lib/amd64
# sudo R CMD javareconf
