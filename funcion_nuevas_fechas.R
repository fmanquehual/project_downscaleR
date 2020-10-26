library(stringr)
library(lubridate)

nuevas_fechas <- function(lista.climate4R, entregar_fecha_inicio=TRUE, 
                          datos_simulados=FALSE, iteracion, tz=NULL){
  
  # lista.climate4R <- pr.sum.total.original
  # iteracion <- i
  
  fecha_inicio <- lista.climate4R$Dates$start
  fecha_fin <- lista.climate4R$Dates$end
  
  if(iteracion<=9){mes.i <- paste0('-', 0, iteracion,'-')} else(mes.i <- paste0('-', iteracion,'-'))
  
  nueva_fecha_inicio <- gsub(mes.i, '-01-', fecha_inicio)
  id <- str_detect(nueva_fecha_inicio, '01-31')
  
  if(iteracion==2){nuevos_anhos <- year(nueva_fecha_inicio[id])-1} else(nuevos_anhos <- year(nueva_fecha_inicio[id]))

  if(is.null(tz)){nueva_fecha_inicio[id] <- paste0(nuevos_anhos, '-12-31 00:00:00')
  } else(nueva_fecha_inicio[id] <- paste0(nuevos_anhos, '-12-31', ' ', tz)) 
    
  # if(datos_simulados==FALSE){
  #   
  #     if(is.null(tz)){nueva_fecha_inicio[id] <- paste0(nuevos_anhos, '-12-31 00:00:00')
  #     } else(nueva_fecha_inicio[id] <- paste0(nuevos_anhos, '-12-31 00:00:00', ' ', tz)) 
  #   
  # } else(nueva_fecha_inicio[id] <- paste0(nuevos_anhos, '-12-31', ' ', tz))
  
  nueva_fecha_fin_preliminar <- gsub(mes.i, '-01-', fecha_fin)
  
  mes.siguiente.i <- iteracion+1
  if(mes.siguiente.i<=8){mes.j <- paste0('-', 0, (iteracion+1),'-')} else(mes.j <- paste0('-', (iteracion+1),'-'))
  
  nueva_fecha_fin <- gsub(mes.j, '-02-', nueva_fecha_fin_preliminar)
  
  fecha_inicio_output <- list(start=fecha_inicio, start=nueva_fecha_inicio)
  fecha_fin_output <- list(end=fecha_fin, end=nueva_fecha_fin)
  
  if(entregar_fecha_inicio){return(fecha_inicio_output)} else(return(fecha_fin_output))
  
}