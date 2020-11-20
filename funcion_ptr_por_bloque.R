library(lubridate)

ptr_por_bloque <- function(base_de_datos_fechas_segun_bloque, datos_de_estaciones,
                           datos_simulados_entrenamiento, datos_simulados_total, 
                           estacion){
  
  # base_de_datos_fechas_segun_bloque <- db.fechas.por.bloque
  # datos_de_estaciones <- estaciones
  # datos_simulados_entrenamiento <- raster.pr.sum.entrenamiento
  # datos_simulados_total <- raster.pr.sum.total
  # estacion <- 1
  
  bloques <- unique(base_de_datos_fechas_segun_bloque$bloque)
  db <- c()
  for (i in bloques) {
    
    # i <- 1
    
    mensaje.inicio <- paste0('Bloque ', i, ' de ', bloques[length(bloques)])
    message(mensaje.inicio)
    
    # Filtro de fechas por bloque
    
    db.bloque.i <- subset(base_de_datos_fechas_segun_bloque, bloque==i)
    fechas.bloque.i <- db.bloque.i$mes_y_dia
    
    
    # Estaciones, extrayendo valores segun bloque 'i'
    fechas.i <- as.Date(datos_de_estaciones$Dates$start)
    db.estaciones <- data.frame(fecha=fechas.i)
    db.estaciones$mes_y_dia <-  paste(month(db.estaciones$fecha), 
                                      day(db.estaciones$fecha), sep = '-')
    
    fechas.estaciones.i <- db.estaciones$fecha[db.estaciones$mes_y_dia%in%fechas.bloque.i]
    id.fechas.estaciones.i <- which(fechas.i%in%fechas.estaciones.i)
    estaciones.valores.bloque.i <- estaciones$Data[id.fechas.estaciones.i, estacion]
    
    
    # Simulados entrenamiento, extrayendo valores segun bloque 'i'
    fechas.i <- nombre_de_columnas_a_fechas(datos_simulados_entrenamiento)
    db.entrenamiento <- data.frame(fecha=fechas.i)
    db.entrenamiento$mes_y_dia <-  paste(month(db.entrenamiento$fecha), 
                                      day(db.entrenamiento$fecha), sep = '-')
    
    fechas.entrenamiento.i <- db.entrenamiento$fecha[db.entrenamiento$mes_y_dia%in%fechas.bloque.i]
    id.fechas.entrenamiento.i <- which(fechas.i%in%fechas.entrenamiento.i)
    entrenamiento.valores.bloque.i <- as.vector(datos_simulados_entrenamiento[,id.fechas.entrenamiento.i])
    
    
    # Simulados total, extrayendo valores segun bloque 'i'
    fechas.i <- nombre_de_columnas_a_fechas(datos_simulados_total)
    db.total <- data.frame(fecha=fechas.i)
    db.total$mes_y_dia <-  paste(month(db.total$fecha), 
                                         day(db.total$fecha), sep = '-')
    
    fechas.total.i <- db.total$fecha[db.total$mes_y_dia%in%fechas.bloque.i]
    id.fechas.total.i <- which(fechas.i%in%fechas.total.i)
    total.valores.bloque.i <- as.vector(datos_simulados_total[,id.fechas.total.i])
    
    
    # Calculo del parametro 'a' y 'b'
    
    a.i <- ptr(o=estaciones.valores.bloque.i, p=entrenamiento.valores.bloque.i, s=total.valores.bloque.i, 
               precip = TRUE, entregar_a = TRUE)
    
    b.i <- ptr(o=estaciones.valores.bloque.i, p=entrenamiento.valores.bloque.i, s=total.valores.bloque.i, 
               precip = TRUE, entregar_b = TRUE)
    
    
    # Generando db de salida
    
    nombre.estacion.i <- datos_de_estaciones$Metadata$name[estacion]
    
    db0 <- data.frame(nombre_estacion=nombre.estacion.i, bloque=i, a=a.i, b=b.i)
    db <- rbind(db, db0)
    
  }

  return(db)
  
}