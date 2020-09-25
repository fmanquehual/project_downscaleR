library(stringi)

localizacion_directorio_de_trabajo <- function(variable_de_interes){
  
  # variable_de_interes <- 'pp'
  
  carpeta.madre <- 'C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/datos_observados/'
  setwd(carpeta.madre)
  
  carpetas1 <- gsub('./', '', list.dirs())
  carpetas2 <- gsub('ParaTaylor', '', carpetas1)
  
  if(variable_de_interes == 'pp'){
    variable.de.interes.modificada <- 'Rain'} else(
    variable.de.interes.modificada <- gsub('t', 'T', variable_de_interes))
  
  match <- stri_detect_fixed(carpetas2, variable.de.interes.modificada)
  carpeta.de.interes <- carpetas1[match]
  directorio.de.trabajo <- paste(carpeta.madre, carpeta.de.interes, sep = '')
  
  return(directorio.de.trabajo)
}
