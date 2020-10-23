formato_datos_para_climate4R <- function(lista.climate4R){

  # lista.climate4R <- pr.sum.corregido.eqm
  
  datos <- lista.climate4R$Data
  
  id.valores.NA <- which(is.na(datos))
  
  if(length(id.valores.NA)==0){datos_depurados <- datos
    } else(c(datos_depurados <- datos[-id.valores.NA],
             attr(datos_depurados, "dimensions") <- "time"))
  
  return(datos_depurados)
  
  }