library(lubridate)

formato_datos_para_climate4R <- function(lista_climate4R_a_depurar, lista_climate4R_referencia, 
                                         entregar_fecha_inicio=TRUE, iteracion, tz=NULL){

  # lista_climate4R_referencia <- pr.sum.total.original
  # lista_climate4R_a_depurar <- pr.sum.corregido.eqm
  # iteracion <- i
  # tz=tz.i
  
  datos_originales <- lista_climate4R_a_depurar$Data

  for (j in 1:ncol(datos_originales)) {
    # j <- 3
    
    nombre_de_estacion <- lista_climate4R_a_depurar$Metadata$name[j]
    mensaje_de_inicio <- paste0('Depurando datos de la estacion', ' ', nombre_de_estacion, ' ', '(', j, ')')
    message(mensaje_de_inicio)
    
    datos_a_depurar <- lista_climate4R_a_depurar$Data[,j]
    fechas_originales <- as.vector(unlist(nuevas_fechas(lista_climate4R_referencia,
                                                        entregar_fecha_inicio = entregar_fecha_inicio, 
                                                        iteracion = iteracion, tz=tz)[1]))
    
    db_fecha_original <- data.frame(fecha=fechas_originales)
    db_fecha_original$anho <- year(db_fecha_original$fecha)
    
    anhos_unicos <- unique(db_fecha_original$anho)
    numero_de_dias_enero <- 31
    datos_correctos <- c()
    
    if( (length(datos_a_depurar)/2) == length(fechas_originales) ){
    
    id_elementos_NA <- which(is.na(datos_a_depurar))
    datos_correctos <- datos_a_depurar[-id_elementos_NA]
    
    } else(
        for (i in 1:length(anhos_unicos)) {
          # i <- 1
          
          anho.i <- anhos_unicos[i]
          
          db_fecha_original_subset <- subset(db_fecha_original, anho==anho.i)
          numero_de_filas_a_conservar <- nrow(db_fecha_original_subset)
          elemento_sobrante_a_eliminar <- numero_de_dias_enero-numero_de_filas_a_conservar
          
          if(i==1){datos_restantes.i <- datos_a_depurar[-(1:numero_de_filas_a_conservar)]
            } else( c(
              datos_correctos.i <- datos_restantes.i[1:numero_de_filas_a_conservar],
              datos_restantes.i <- datos_restantes.i[-(1:numero_de_filas_a_conservar)]) )
          
          if(elemento_sobrante_a_eliminar!=0){datos_restantes.i <- datos_restantes.i[-c(1:elemento_sobrante_a_eliminar)]}
          
          if(i==1){datos_correctos.i <- datos_a_depurar[1:numero_de_filas_a_conservar]}
          
          datos_correctos <- c(datos_correctos, datos_correctos.i)
          
        }
      )
    
    if(j==1){matriz_con_datos_depurados <- matrix(datos_correctos, 
                                                  nrow = length(datos_correctos), 
                                                  ncol = 1, byrow = TRUE)
      } else(matriz_con_datos_depurados <- cbind(matriz_con_datos_depurados, datos_correctos))
  }
 
  attr(matriz_con_datos_depurados, "dimensions") <- "time"
  
  return(matriz_con_datos_depurados)
  
  }