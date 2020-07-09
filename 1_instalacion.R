# Pasos para instalar: https://github.com/SantanderMetGroup/loadeR/wiki/Installation

# En la consola de Ubuntu ----

# sudo apt-get install -y default-jre
# sudo apt-get install -y default-jdk
# sudo R CMD javareconf

# instalando Java ----
Sys.setenv(JAVA_HOME='/home/msomos/jre-8u251-linux-x64/jre1.8.0_251/')
Sys.getenv("JAVA_HOME")

# En R ----
# install.packages("rJava")
# Verificar si ya tienes instalado 'devtools' para descargar desde github

library(devtools)

install_github(c("SantanderMetGroup/loadeR.java",
                 "SantanderMetGroup/climate4R.UDG",
                 "SantanderMetGroup/loadeR",
                 "SantanderMetGroup/transformeR",
                 "SantanderMetGroup/visualizeR",
                 "SantanderMetGroup/downscaleR"))
# En la consola de Ubuntu ----
# R CMD javareconf
# sudo apt install openjdk-X-jdk

# Si no sale bien ----
# export LD_LIBRARY_PATH=/home/msomos/jre-8u251-linux-x64/jre1.8.0_251/lib/amd64
# sudo R CMD javareconf

library(rJava)