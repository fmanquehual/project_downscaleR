depurador_nombre_archivos_csv <- function(lista_de_archivos){

  # lista_de_archivos <- archivos.tmin
  lista_de_archivos_depurada <- c()
    
  for (i in 1:length(lista_de_archivos)) {
  
    nombre_i <- unlist(stri_split_fixed(lista_de_archivos[i], '_'))[1]
    institucion_i <- unlist(stri_split_fixed(lista_de_archivos[i], '_'))[2]
    formato <- '.csv'
    
    archivo_i_depurado <- paste0(nombre_i, institucion_i, formato)
    lista_de_archivos_depurada <- c(lista_de_archivos_depurada, archivo_i_depurado)
  
  }
  
  return(lista_de_archivos_depurada)

}