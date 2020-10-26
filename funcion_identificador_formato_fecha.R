identificador_formato_fecha <- function(base_de_datos, formato_de_anho='Y'){
  # base_de_datos <- db.i
  
  # Preparando datos
  
  if( unique(str_detect(base_de_datos[,1], '-')) ){
    separador_de_fecha <- '-'
    } else if( unique(str_detect(base_de_datos[,1], '/')) ){
      separador_de_fecha <- '/' 
      } else(warning('No se pudo identificar el separador de fecha'))
  
  fecha_split1 <- stri_split_fixed(base_de_datos[1,1], separador_de_fecha)[[1]]
  fecha_split2 <- stri_split_fixed(base_de_datos[16,1], separador_de_fecha)[[1]]
  fecha_split3 <- stri_split_fixed(base_de_datos[32,1], separador_de_fecha)[[1]]
  fecha_split4 <- stri_split_fixed(base_de_datos[47,1], separador_de_fecha)[[1]]
  
  fecha_split1_numerico <- as.numeric(fecha_split1)
  fecha_split2_numerico <- as.numeric(fecha_split2)
  fecha_split3_numerico <- as.numeric(fecha_split3)
  fecha_split4_numerico <- as.numeric(fecha_split4)
  
  
  # Evaluando
  e1 <- c(fecha_split1_numerico[1], fecha_split2_numerico[1], fecha_split3_numerico[1], fecha_split4_numerico[1])
  n_elementos_unicos_e1 <- length(unique(e1))
  
  e2 <- c(fecha_split1_numerico[2], fecha_split2_numerico[2], fecha_split3_numerico[2], fecha_split4_numerico[2])
  n_elementos_unicos_e2 <- length(unique(e2))
  
  e3 <- c(fecha_split1_numerico[3], fecha_split2_numerico[3], fecha_split3_numerico[3], fecha_split4_numerico[3])
  n_elementos_unicos_e3 <- length(unique(e3))
  
  
  # Resultado
  
  if(n_elementos_unicos_e1 < n_elementos_unicos_e2 & n_elementos_unicos_e2 > n_elementos_unicos_e3){
      formato_de_fecha <- paste0('%m/', '%d/', '%', formato_de_anho)
       } else if(n_elementos_unicos_e1 < n_elementos_unicos_e2 & n_elementos_unicos_e2 < n_elementos_unicos_e3
               ) {formato_de_fecha <- paste0('%', formato_de_anho, '/', '%m/', '%d')
                } else(warning('No se pudo identificar el formato de fecha'))
  
  
  # Salida
  
  return(formato_de_fecha)

}