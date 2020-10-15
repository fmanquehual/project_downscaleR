rm(list=ls())
dev.off()

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_generador_de_matriz_archivos_DGA.R')

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/dga/')
lista.de.archivos <- list.files() ; lista.de.archivos

for (i in 1:length(lista.de.archivos)) {

  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/dga/')
  nombre.archivo <- lista.de.archivos[i]
  db.depurado <- generador_de_matriz_archivos_DGA(nombre.archivo, entregar_db_depurada = TRUE)
  
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/dga_depurados/')
  write.csv(db.depurado, nombre.archivo, row.names = FALSE)
  
}

