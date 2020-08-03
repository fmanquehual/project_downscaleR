# Pasos para instalar: https://github.com/SantanderMetGroup/loadeR/wiki/Installation

# En la consola de Ubuntu ----

# sudo apt-get install -y default-jre
# sudo apt-get install -y default-jdk
# sudo R CMD javareconf

# instalando Java ----

# install.packages("rJava")

library(rJava)

Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk1.8.0_261")  #Windows
Sys.getenv("JAVA_HOME")

# devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/loadeR"))
# 
# devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/climate4R.UDG", "SantanderMetGroup/loadeR"))

# En R ----
# Verificar si ya tienes instalado 'devtools' para descargar desde github

library(devtools)

install_github(c('SantanderMetGroup/climate4R.UDG@devel', 'SantanderMetGroup/loadeR'))

# library(installr)
# 
# uninstall.packages(c("visualizeR",
#   "transformeR",
#   "loadeR-devel",
#   "downscaleR",
#                      'loadeR.2nc',
#   "loadeR",
#                      "loadeR.java",
#                      
#                      "climate4R.UDG"))

install_github(c(
                 
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/downscaleR",
                 "SantanderMetGroup/visualizeR"
                 
                 # "SantanderMetGroup/climate4R.UDG",
                 # "SantanderMetGroup/loadeR.java",
                 # "SantanderMetGroup/loadeR",
                 ))



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
