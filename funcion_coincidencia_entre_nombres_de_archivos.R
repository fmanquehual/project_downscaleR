library(stringi)

coincidencia_entre_nombres_de_archivos <- function(nombre_archivo_de_referencia, nombre_de_archivo_a_evaluar){
  
  # nombre_archivo_de_referencia <- nombre2
  # nombre_de_archivo_a_evaluar <- nombre1.i
  
  if(stri_detect_fixed(nombre_de_archivo_a_evaluar, 'DGA') | stri_detect_fixed(nombre_de_archivo_a_evaluar, 'DG')){institucion <- 'DG'}
  if(stri_detect_fixed(nombre_de_archivo_a_evaluar, 'DMC')){institucion <- 'DMC'}
  if(stri_detect_fixed(nombre_de_archivo_a_evaluar, 'GDGA')){institucion <- 'GDGA'}
  if(stri_detect_fixed(nombre_de_archivo_a_evaluar, 'INIA')){institucion <- 'INIA'}
  if(stri_detect_fixed(nombre_de_archivo_a_evaluar, 'GHCN')){institucion <- 'GHCN'}
  if(stri_detect_fixed(nombre_de_archivo_a_evaluar, 'CDOM')){institucion <- 'CDOM'}
  
    nombre1.segregado <- unlist(strsplit(nombre_de_archivo_a_evaluar, ""))
    nombre2.segregado <- unlist(strsplit(nombre_archivo_de_referencia, ""))
    
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
      if(!stri_detect_fixed(nombre2, institucion)){numero.de.verdaderos <- c()}
      else(numero.de.falsos <- c(numero.de.falsos, 1))  
    }
    
    numero.de.coincidencias <- length(numero.de.verdaderos)
    porcentaje.de.aciertos <- (numero.de.coincidencias*100)/min.i ; porcentaje.de.aciertos
    
    return(porcentaje.de.aciertos)
}