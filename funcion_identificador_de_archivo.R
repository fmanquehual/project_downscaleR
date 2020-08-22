identificador_de_archivo <- function(nombre.archivo.de.referencia, nombre.de.archivos.a.evaluar){
  
  # nombre.archivo.de.referencia <- nombre.estacion.referencia
  # nombre.de.archivos.a.evaluar <- nombre.estaciones.a.evaluar

  nombre1 <- nombre_estacion(nombre.de.archivos.a.evaluar)
  nombre2 <- nombre_estacion(nombre.archivo.de.referencia)
  
  coincidencia.nombre.de.archivos <- data.frame(nombre.archivo=nombre1, porcentaje.de.coincidencia=0)
  
  for(i in 1:nrow(coincidencia.nombre.de.archivos)){
     # i <- 50
    nombre1.i <- coincidencia.nombre.de.archivos$nombre.archivo[i]
    
    ###
        #porcentaje.de.coincidencia <- coincidencia_entre_nombres_de_archivos(nombre2, nombre1.i) ; porcentaje.de.coincidencia
        # nombre_archivo_de_referencia <- nombre2
        # nombre_de_archivo_a_evaluar <- nombre1.i
        
        if(stri_detect_fixed(nombre1.i, 'DGA') | stri_detect_fixed(nombre1.i, 'DG')){institucion <- 'DG'}
        if(stri_detect_fixed(nombre1.i, 'DMC')){institucion <- 'DMC'}
        if(stri_detect_fixed(nombre1.i, 'GDGA')){institucion <- 'GDGA'}
        if(stri_detect_fixed(nombre1.i, 'INIA')){institucion <- 'INIA'}
        if(stri_detect_fixed(nombre1.i, 'GHCN')){institucion <- 'GHCN'}
        if(stri_detect_fixed(nombre1.i, 'CDOM')){institucion <- 'CDOM'}
        
        nombre1.segregado <- unlist(strsplit(nombre1.i, ""))
        nombre2.segregado <- unlist(strsplit(nombre2, ""))
        
        longitud.nombre1.segregado <- length(nombre1.segregado)
        longitud.nombre2.segregado <- length(nombre2.segregado)
        
        min.i <- min(longitud.nombre1.segregado, longitud.nombre2.segregado)
        
        numero.de.verdaderos <- c()
        numero.de.falsos <- c()
        i.letras.match <- c()
        
        for (j in 1:min.i) {
          if(nombre1.segregado[j] == nombre2.segregado[j]){
            numero.de.verdaderos <- c(numero.de.verdaderos, 1)
            i.letras.match <- c(i.letras.match, j)
          } 
          else(numero.de.falsos <- c(numero.de.falsos, 1))  
        }
        
        if( is.null(i.letras.match) ){i.letras.match <- 0}
        
        nombre1.segregado2 <- nombre1.segregado[-i.letras.match]
        nombre2.segregado2 <- nombre2.segregado[-i.letras.match]
        
        longitud.nombre1.segregado2 <- length(nombre1.segregado2)
        longitud.nombre2.segregado2 <- length(nombre2.segregado2)
        
        min.j <- min(longitud.nombre1.segregado2, longitud.nombre2.segregado2)
        
        for (k in min.j:1) {
          if(min.j==0){break}
          if(nombre1.segregado2[k] == nombre2.segregado2[k]){numero.de.verdaderos <- c(numero.de.verdaderos, 1)} 
          if(!stri_detect_fixed(nombre2, institucion)){numero.de.verdaderos <- c()}
          else(numero.de.falsos <- c(numero.de.falsos, 1))  
        }
        
        numero.de.coincidencias <- length(numero.de.verdaderos)
        porcentaje.de.coincidencia <- (numero.de.coincidencias*100)/min.i
    
    ###
        
    coincidencia.nombre.de.archivos$porcentaje.de.coincidencia[i] <- porcentaje.de.coincidencia
    
    }
  
  id.max.coincidencia <- which.max(coincidencia.nombre.de.archivos$porcentaje.de.coincidencia)
  nombre.con.mayor.coincidencia <- coincidencia.nombre.de.archivos$nombre.archivo[id.max.coincidencia]
  
  return(nombre.con.mayor.coincidencia)
  
}