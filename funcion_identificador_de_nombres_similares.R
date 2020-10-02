directorio_de_trabajo_original <- getwd()

setwd('C:/Users/Usuario/Documents/Francisco/WRF/proyecto_WRF/proyecto_WRF_complementario/')
source('funcion_nombre_estacion.R')

setwd(directorio_de_trabajo_original)

identificador_de_nombres_similares <- function(vector_con_nombres_de_referencia, vector_con_nombres_a_evaluar, 
                                               porcentaje_de_aciertos=75, es_precipitacion=FALSE){

  # vector_con_nombres_de_referencia = db.pp$archivo.con.datos.climaticos
  # vector_con_nombres_a_evaluar = db.rh$archivo.con.datos.climaticos
  # porcentaje_de_aciertos = 75
  
  # preparacion de los datos
  
  vector_con_nombres_de_referencia_unicos <- unique(vector_con_nombres_de_referencia)
  vector_con_nombres_a_evaluar_unicos <- unique(vector_con_nombres_a_evaluar)
  
  vector_con_nombres_de_referencia_depurado <- nombre_estacion(vector_con_nombres_de_referencia_unicos)
  vector_con_nombres_a_evaluar_depurado <- nombre_estacion(vector_con_nombres_a_evaluar_unicos)
  
  
  # calculo del porcentaje de coincidencias
  
  estaciones.compartidas <- c()
  
  for (k in 1:length(vector_con_nombres_de_referencia_depurado)) {
    #  k <- 1
    vector_con_nombres_de_referencia_depurado_k <- vector_con_nombres_de_referencia_depurado[k]
    
    if(stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'DGA') | stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'DG')){institucion1 <- 'DG'}
    if(stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'DMC')){institucion1 <- 'DMC'}
    if(stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'GDGA')){institucion1 <- 'GDGA'}
    if(stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'INIA')){institucion1 <- 'INIA'}
    if(stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'GHCN')){institucion1 <- 'GHCN'}
    if(stri_detect_fixed(vector_con_nombres_de_referencia_depurado_k, 'CDOM')){institucion1 <- 'CDOM'}
    
      for (j in 1:length(vector_con_nombres_a_evaluar_depurado)) {
      #j <- 1
      
        vector_con_nombres_a_evaluar_depurado_j <- vector_con_nombres_a_evaluar_depurado[j]
        
        if(stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'DGA') | stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'DG')){institucion2 <- 'DG'}
        if(stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'DMC')){institucion2 <- 'DMC'}
        if(stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'GDGA')){institucion2 <- 'GDGA'}
        if(stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'INIA')){institucion2 <- 'INIA'}
        if(stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'GHCN')){institucion2 <- 'GHCN'}
        if(stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, 'CDOM')){institucion2 <- 'CDOM'}
        
        nombre1.segregado <- unlist(strsplit(vector_con_nombres_a_evaluar_depurado_j, ""))
        nombre2.segregado <- unlist(strsplit(vector_con_nombres_de_referencia_depurado_k, ""))
        
        longitud.nombre1.segregado <- length(nombre1.segregado)
        longitud.nombre2.segregado <- length(nombre2.segregado)
        
        min.i <- min(longitud.nombre1.segregado, longitud.nombre2.segregado)
        
        numero.de.verdaderos <- c()
        numero.de.falsos <- c()
        i.letras.match <- c()
        
        for (i in 1:min.i) {
          if(nombre1.segregado[i] == nombre2.segregado[i]){
            numero.de.verdaderos <- c(numero.de.verdaderos, 1)
            i.letras.match <- c(i.letras.match, i)
          } 
          else(numero.de.falsos <- c(numero.de.falsos, 1))  
        }
        
        if( is.null(i.letras.match) ){i.letras.match <- 0}
        
        nombre1.segregado2 <- nombre1.segregado[-i.letras.match]
        nombre2.segregado2 <- nombre2.segregado[-i.letras.match]
        
        longitud.nombre1.segregado2 <- length(nombre1.segregado2)
        longitud.nombre2.segregado2 <- length(nombre2.segregado2)
        
        min.j <- min(longitud.nombre1.segregado2, longitud.nombre2.segregado2)
        
        for (i in min.j:1) {
          if(min.j==0){break}
          if(nombre1.segregado2[i] == nombre2.segregado2[i]){numero.de.verdaderos <- c(numero.de.verdaderos, 1)} 
          #if(!stri_detect_fixed(vector_con_nombres_a_evaluar_depurado_j, institucion)){numero.de.verdaderos <- c()}
          else(numero.de.falsos <- c(numero.de.falsos, 1))  
        }
        
        numero.de.coincidencias <- length(numero.de.verdaderos)
        porcentaje.de.aciertos <- (numero.de.coincidencias*100)/min.i
        
        if(institucion1!=institucion2){porcentaje.de.aciertos <- 0}
        
        if(porcentaje.de.aciertos>=porcentaje_de_aciertos & es_precipitacion==TRUE){
           estaciones.compartidas <- c(estaciones.compartidas, vector_con_nombres_de_referencia_unicos[k])
           } else if(porcentaje.de.aciertos>=porcentaje_de_aciertos & es_precipitacion==FALSE){
                     estaciones.compartidas <- c(estaciones.compartidas, vector_con_nombres_a_evaluar_unicos[j]) }
    }
  }
  
  return(estaciones.compartidas)
}

