library(lubridate)

bloque_de_dias <- function(datos_de_estaciones=NULL, datos_simulados_entrenamiento=NULL, 
                           datos_simulados_total=NULL, dias_del_bloque=5, buffer=30,
                           data_frame=FALSE){
  
  # datos_de_estaciones <- estaciones
  # datos_simulados_entrenamiento <- raster.pr.sum.entrenamiento
  # datos_simulados_total <- raster.pr.sum.total
  
  # datos_de_estaciones <- db.estaciones.y.era5
  # datos_simulados_entrenamiento <- db.estaciones.y.era5
  # datos_simulados_total <- db.estaciones.y.era5
  
  
  # Preparando datos de entrada ----
  
  if(data_frame==FALSE){c(
    
    fechas.estaciones <- as.Date(datos_de_estaciones$Dates$start),
    fechas.simulado.entrenamiento <- nombre_de_columnas_a_fechas(datos_simulados_entrenamiento),
    fechas.simulado.total <- nombre_de_columnas_a_fechas(datos_simulados_total)
    
      )
  
    } else(c(
      
      fechas.estaciones <- as.Date(datos_de_estaciones$fecha),
      fechas.simulado.entrenamiento <- as.Date(datos_de_estaciones$fecha),
      fechas.simulado.total <- as.Date(datos_de_estaciones$fecha)
  
        )
      
      )
  
  dias.del.bloque <- dias_del_bloque
  
  # fin ---
  
  
  
  
  # Verificando cantidad de fechas ----
  
  n.fechas.estaciones <- length(fechas.estaciones)
  n.fechas.simulado.entrenamiento <- length(fechas.simulado.entrenamiento)
  
  if(n.fechas.estaciones != n.fechas.simulado.entrenamiento){
    warning('La longitud de fechas entre las estaciones y simulados es diferente')}
  
  # fin ---
  
  
  
  
  # Descartando a 29 de febrero de las db's ----
  
  febrero.29 <- '2-29'
  
  mes.y.dia <- paste(month(fechas.estaciones), day(fechas.estaciones), sep = '-')
  id.febrero.29 <- which(mes.y.dia%in%febrero.29)
  
  fechas.estaciones.sin.febrero.29 <- fechas.estaciones[-id.febrero.29]
  fechas.simulado.entrenamiento.sin.febrero.29 <- fechas.simulado.entrenamiento[-id.febrero.29]
  
  db <- data.frame(fecha=fechas.estaciones.sin.febrero.29)
  db$anho <- year(db$fecha)
  db$mes.y.dia <- paste(month(fechas.estaciones.sin.febrero.29), 
                        day(fechas.estaciones.sin.febrero.29), sep = '-')
  
  # fin ---
  
  
  
  
  # Generando una db de referencia con los 365 dias ----
  
  # Importante en caso de que las db's originales no empiecen el 1 de enero y/o ...
  # terminen el 31 de diciembre
  
  anhos.unicos <- unique(db$anho)
  db.anho.completo <- generador_de_fechas(anhos.unicos[1], incluir.febrero.29 = FALSE)
  db.anho.completo$mes.y.dia <- paste(month(db.anho.completo$fecha), 
                                        day(db.anho.completo$fecha), sep = '-')
  
  # fin ---
  
  
  
  
  # Generando db donde se obtendran las fechas segun bloque, a partir de un id ----
  
  cantidad.bloques.anual <- 365/dias.del.bloque
  db.anho.completo$bloque <- sort(rep(1:cantidad.bloques.anual, dias.del.bloque))
  
  mes.y.dia.preliminar.1 <- db.anho.completo$mes.y.dia
  mes.y.dia.1 <- mes.y.dia.preliminar.1[335:365]
  
  mes.y.dia.preliminar.2 <- db.anho.completo$mes.y.dia
  mes.y.dia.2 <- mes.y.dia.preliminar.2[-(335:365)]
  
  mes.y.dia <- c(mes.y.dia.1, mes.y.dia.2)
  
  db.mes.y.dia_preliminar <- data.frame(mes_y_dia=mes.y.dia, id=1:length(mes.y.dia))
  db.mes.y.dia_complementario.1 <- data.frame(mes_y_dia=mes.y.dia.2[305:length(mes.y.dia.2)], id=-29:0)
  db.mes.y.dia_complementario.2 <- data.frame(mes_y_dia=mes.y.dia.1, 
                                              id=366:(365+length(mes.y.dia.1)))
  db.mes.y.dia <- rbind(db.mes.y.dia_complementario.1, db.mes.y.dia_preliminar, db.mes.y.dia_complementario.2)
  
  # fin ---
  
  
  
  
  # Generando db con fechas segun bloque ---- 
  
  for (i in 1:cantidad.bloques.anual) {
    #i <- 1
    
    bloque.i <- i
    
    fila.bloque <- which(db.anho.completo$bloque==bloque.i)
    fila.bloque.inicio <- which(db.anho.completo$bloque==bloque.i)[1]
    fila.bloque.termino <- which(db.anho.completo$bloque==bloque.i)[dias.del.bloque]
    
    mes.y.dia_bloque <- db.anho.completo$mes.y.dia[fila.bloque]
    mes.y.dia_bloque.inicio <- db.anho.completo$mes.y.dia[fila.bloque.inicio]
    mes.y.dia_bloque.termino <- db.anho.completo$mes.y.dia[fila.bloque.termino]
    
    
    id_mes.y.dia_bloque.inicio <- which(mes.y.dia==mes.y.dia_bloque.inicio)
    id_mes.y.dia_bloque.termino <- which(mes.y.dia==mes.y.dia_bloque.termino)
    
    buffer.inicio <- db.mes.y.dia$mes_y_dia[db.mes.y.dia$id%in%(id_mes.y.dia_bloque.inicio-30):(id_mes.y.dia_bloque.inicio-1)]
    buffer.termino <- db.mes.y.dia$mes_y_dia[db.mes.y.dia$id%in%(id_mes.y.dia_bloque.termino+1):(id_mes.y.dia_bloque.termino+30)]
    
    mes.y.dia_bloque.i <- c(buffer.inicio, mes.y.dia_bloque, buffer.termino)
    
    if(i==1){c(
      
      db.bloques <- data.frame(mes_y_dia=mes.y.dia_bloque.i, bloque=i, bloque_sin_buffer=NA),
      db.bloques$bloque_sin_buffer[db.bloques$mes_y_dia%in%mes.y.dia_bloque] <- i
      
    )
      } else(c(
                db.bloques0 <- data.frame(mes_y_dia=mes.y.dia_bloque.i, bloque=i, bloque_sin_buffer=NA),
                db.bloques0$bloque_sin_buffer[db.bloques0$mes_y_dia%in%mes.y.dia_bloque] <- i,
                db.bloques <- rbind(db.bloques, db.bloques0)
        )
      )
    
  }
  
  # fin ---
  
  return(db.bloques)
  
}