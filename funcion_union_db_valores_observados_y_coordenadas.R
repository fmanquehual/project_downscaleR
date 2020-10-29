library(stringi)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR')
source('funcion_anhadiendo_coordenadas.R')
source('funcion_verificacion_anhos_de_interes.R')
source('funcion_coincidencia_entre_nombres_de_archivos.R')
source('funcion_identificador_de_archivo.R')
source('funcion_subset_anhos_de_interes.R')
source('funcion_verificacion_anhos_de_interes.R')
source('funcion_fechas_del_anho.R')
source('funcion_datos_observados_en_formato_ts.R')
source('funcion_incorporacion_de_fechas_perdidas.R')
source('funcion_identificador_formato_fecha.R')

setwd('C:/Users/Usuario/Documents/Francisco/WRF/proyecto_WRF/proyecto_WRF_complementario/')
source('funcion_nombre_estacion.R')

union_db_valores_observados_y_coordenadas <- function(lista_de_archivos, variable_de_interes, directorio_de_trabajo){
  
  # variable_de_interes <- variable.de.interes
  # lista_de_archivos <- archivos.pp
  # directorio_de_trabajo <- directorio.de.trabajo
  
  db.todos <- c()
  
  for (i in 1:length(lista_de_archivos)) {
    # i <- 7
    
    # Lectura de archivos
    setwd(directorio_de_trabajo)
    db.i <- read.csv(lista_de_archivos[i])
    
    # nuevo (19 DE OCTUBRE DEL 2020)----
    
      if(ncol(db.i)==1){db.i <- read.csv2(lista_de_archivos[i])}
      
      db.i_split <- stri_split_fixed(db.i[,1], ' ')
      elementos <- unlist(db.i_split[1])
      numero_de_elementos <- length(elementos)

      if(numero_de_elementos==2){
        db.i[,1] <- stri_replace_all_fixed(db.i[,1], paste0(' ', elementos[2]), '') }
      
      anho <- stri_split_fixed(db.i[1,1], '-')[[1]][1]
      
      numero_de_elementos_de_anho <- length( unlist(strsplit(anho, "")) )
      
      if(numero_de_elementos_de_anho<4){db.i[,1] <- as.Date(db.i[,1], "%d-%m-%Y")} 
    
    # nuevo (25 DE OCTUBRE DEL 2020)----  
    
      if(numero_de_elementos_de_anho==7){ c(
        formato_de_fecha <- identificador_formato_fecha(db.i, 'y'),
        db.i <- incorporacion_de_fechas_perdidas(db.i, formato_de_fecha) ) }
      
      if(numero_de_elementos_de_anho!=7){c(
        formato_de_fecha <- '%Y-%m-%d',
        db.i <- incorporacion_de_fechas_perdidas(db.i, formato_de_fecha) ) } 
              
    # fin ---
    
    
    # Testeo y omicion de archivos sin datos para los anhos de interes
    anhos.observados.i <- verificacion_anhos_de_interes(db.i)
    match.i <- anhos.interes[anhos.interes%in%anhos.observados.i]
    if( length(match.i)==0 ){c(message( paste(lista_de_archivos[i], 'no tiene datos para los anhos de interes!', sep = ' ') ), 
                               next)}
    
    # Preparacion de archivo final
    db2.i <- datos_observados_por_anhos_de_interes(db.i, anhos.interes, variable = variable_de_interes)
    db3.i <- db2.i[!duplicated(db2.i$Date),]
    db4.i <- anhadiendo_coordenadas(db3.i, lista_de_archivos[i], variable = variable_de_interes)
    db.todos <- rbind(db.todos, db4.i)
  }
  
  return(db.todos)
}
