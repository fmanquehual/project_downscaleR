library(stringr)
library(stringi)

identificador_de_variables_era5_CEDA <- function(frase_inicial_no_numerica='ecmwf-era5_oper_an_sfc_',
                                                 nombre_de_variables=TRUE,
                                                 fecha_y_hora_de_datos=FALSE){
  
  archivos <- list.files()
  archivos2 <- str_remove_all(archivos, frase_inicial_no_numerica)
  archivos3 <- str_remove_all(archivos2, 'nc')
  
  fecha_y_variable <- unlist(stri_split_fixed(archivos3, ".", omit_empty = TRUE))
  db <- data.frame(nombre=fecha_y_variable, es.numerico=NA)
  
  for (i in 1:nrow(db)) {
    x <- as.numeric(db$nombre[i])
    if(is.na(x)){db$es.numerico[i] <- 0} else(db$es.numerico[i] <- 1)
  }
  
  db.nombre.variable <- subset(db, es.numerico == 0)
  nombre.de.variables.y.cantidad <- table(db.nombre.variable$nombre)
  nombre.variables.unicas <- names(nombre.de.variables.y.cantidad)
  cantidad.de.obsrvaciones.por.nombre <- as.vector(nombre.de.variables.y.cantidad)
  
  db.fecha.y.hora <- subset(db, es.numerico == 1)
  db.fecha.y.hora.unicos <- unique(db.fecha.y.hora$nombre)
  fecha.y.hora <- strptime(db.fecha.y.hora.unicos, format='%Y%M%d%H%M')
  
  setwd.original <- getwd()
  setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
  nombre.y.abreviaciones.de.variables.ERA5 <- read.csv2('nombre_y_abreviaciones_variables_ERA5.csv')
  setwd(setwd.original)
    
  db.nombre.variable.out0 <- data.frame(n=1:length(nombre.variables.unicas), 
                                        abreviacion_variable=nombre.variables.unicas,
                                        numero_de_observaciones=cantidad.de.obsrvaciones.por.nombre)
  
  db.nombre.variable.out <- merge(db.nombre.variable.out0, nombre.y.abreviaciones.de.variables.ERA5,
        by.x = 'abreviacion_variable', by.y = 'abreviacion')
  
  db.nombre.variable.out <- db.nombre.variable.out[,c('n', "abreviacion_variable", "nombre_completo_variable", "unidades", "numero_de_observaciones")]
  
  db.fecha.y.hora.out <- data.frame(n=1:length(fecha.y.hora),
                                    fecha_y_hora_de_datos=fecha.y.hora)
  
  lista.out <- list(db.nombre.variable.out, db.fecha.y.hora.out)
  
  if(nombre_de_variables == TRUE & fecha_y_hora_de_datos == FALSE){return(db.nombre.variable.out)
    } else if(nombre_de_variables == FALSE & fecha_y_hora_de_datos == TRUE){return(fecha_y_hora_de_datos)
      } else if(nombre_de_variables == TRUE & fecha_y_hora_de_datos == TRUE){return(lista.out)}
}