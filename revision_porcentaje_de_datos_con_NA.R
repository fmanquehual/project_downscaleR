library(xlsx)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/bias_correction/')

porcentaje_con_NA_aceptado <- 25
db_resumen <- c()
lista_de_archivos <- list.files(pattern = '.csv')

for (i in 1:length(lista_de_archivos)) {
  # i <- 2
  
  db <- read.csv(lista_de_archivos[i])
  nombre.archivo <- lista_de_archivos[i]
  
  estaciones.i <- unique(db$nombre_estacion)
  
    for (j in 1:length(estaciones.i)) {
      # j <- 1
      
      db.subset <- subset(db, nombre_estacion==estaciones.i[j])
      nombre.estacion <- unique(db.subset$nombre_estacion)
      fecha.inicio.con.datos <- min(db.subset$fecha)
      fecha.fin.con.datos <- max(db.subset$fecha)
      total.de.datos <- nrow(db.subset)
      
      id_elementos_con_NA <- which(is.na(db.subset$valor))
      cantidad_de_NA <- length(id_elementos_con_NA)
      porcentaje.de.datos.NA <- round( (cantidad_de_NA*100)/total.de.datos, 2)
      
      db_resumen0 <- data.frame(nombre_archivo=nombre.archivo, 
                                nombre_estacion=nombre.estacion, 
                                fecha_inicio_con_datos=fecha.inicio.con.datos, 
                                fecha_fin_con_datos=fecha.fin.con.datos, 
                                total_de_datos=total.de.datos, 
                                porcentaje_de_datos_NA=porcentaje.de.datos.NA)
      
      db_resumen <- rbind(db_resumen, db_resumen0)
      
    }
  
}

id_match <- which(db_resumen$porcentaje_de_datos_NA>porcentaje_con_NA_aceptado)
estaciones_que_no_cumplen_la_condicion <- unique(db_resumen$nombre_estacion[id_match])

id_match_2 <- which(!db_resumen$nombre_estacion%in%estaciones_que_no_cumplen_la_condicion)
db_resumen_2 <- db_resumen[id_match_2,]

# write.xlsx(db_resumen, file = 'revision_porcentaje_de_datos_con_NA.xlsx',
#            sheetName = 'Resultados', row.names = FALSE)
# write.csv(db_resumen, 'revision_porcentaje_de_datos_con_NA.csv', row.names = FALSE)

# write.xlsx(db_resumen_2, file = 'estaciones_que_cumplen_porcentaje_de_NA_aceptado.xlsx',
#            sheetName = 'Resultados', row.names = FALSE)
# write.csv(db_resumen_2, 'estaciones_que_cumplen_porcentaje_de_NA_aceptado.csv', row.names = FALSE)
