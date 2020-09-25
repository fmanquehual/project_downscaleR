setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR')
source('funcion_anhadiendo_coordenadas.R')
source('funcion_verificacion_anhos_de_interes.R')
source('funcion_coincidencia_entre_nombres_de_archivos.R')
source('funcion_identificador_de_archivo.R')
source('funcion_subset_anhos_de_interes.R')
source('funcion_verificacion_anhos_de_interes.R')
source('funcion_datos_observados_en_formato_ts.R')

setwd('C:/Users/Usuario/Documents/Francisco/WRF/proyecto_WRF/proyecto_WRF_complementario/')
source('funcion_nombre_estacion.R')

union_db_valores_observados_y_coordenadas <- function(lista_de_archivos, variable_de_interes, directorio_de_trabajo){
  
  # variable_de_interes <- variable.de.interes
  # lista_de_archivos <- archivos.pp
  # directorio_de_trabajo <- directorio.de.trabajo
  
  db.todos <- c()
  
  for (i in 1:length(lista_de_archivos)) {
    # i <- 6
    
    setwd(directorio_de_trabajo)
    # Lectura de archivos
    db.i <- read.csv(lista_de_archivos[i])
    
    # Testeo y omicion de archivos sin datos para los anhos de interes
    anhos.observados.i <- verificacion_anhos_de_interes(db.i)
    match.i <- anhos.interes[anhos.interes%in%anhos.observados.i]
    if( length(match.i)==0 ){c(message( paste(lista_de_archivos[i], 'no tiene datos para los anhos de interes!', sep = ' ') ), 
                               next)}
    
    # Preparacion de archivo final
    db2.i <- datos_observados_por_anhos_de_interes(db.i, anhos.interes, variable = variable_de_interes)
    db3.i <- anhadiendo_coordenadas(db2.i, lista_de_archivos[i], variable = variable_de_interes)
    db.todos <- rbind(db.todos, db3.i)
  }
  
  return(db.todos)
}
