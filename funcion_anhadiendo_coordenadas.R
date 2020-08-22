library(stringr)

anhadiendo_coordenadas <- function(base_de_datos, nombre_del_archivo, variable = 'pp'){
  
  # base_de_datos <- db2.i
  # nombre_del_archivo <- archivos.i[i]
  # variable <- variable.de.interes

  message('Iniciando')
  
  fecha.segregada <- unlist(str_extract_all(base_de_datos$Date, "[0-9]+"))
  fecha.segregada.numerica <- as.numeric(fecha.segregada)
  
  meses.de.la.db <- fecha.segregada.numerica[fecha.segregada.numerica <= 12]
  anhos.de.la.db <- fecha.segregada.numerica[fecha.segregada.numerica > 12]
  
  base_de_datos_nueva <- base_de_datos
  colnames(base_de_datos_nueva)[2] <- 'valor.observado'
  base_de_datos_nueva$valor.observado <- as.numeric(base_de_datos_nueva$valor.observado)
  base_de_datos_nueva$anho <- anhos.de.la.db
  base_de_datos_nueva$mes <- meses.de.la.db
  
  nombre.estaciones.a.evaluar <- nombre_estacion(db.estaciones$names_modi)
  nombre.estacion.referencia <- nombre_estacion(nombre_del_archivo)
  
  nombre.archivo.con.coordenadas <- identificador_de_archivo(nombre.estacion.referencia, nombre.estaciones.a.evaluar)
  
  latitud <- unique( db.estaciones$lat[nombre.estaciones.a.evaluar%in%nombre.archivo.con.coordenadas] )
  longitud <- unique( db.estaciones$lon[nombre.estaciones.a.evaluar%in%nombre.archivo.con.coordenadas] )
  
  nombre_del_archivo_con_coordenadas <- db.estaciones$names.ori[nombre.estaciones.a.evaluar%in%nombre.archivo.con.coordenadas]
  
  base_de_datos_nueva$variable <- variable
  base_de_datos_nueva$lon <- longitud
  base_de_datos_nueva$lat <- latitud
  base_de_datos_nueva$archivo.con.datos.climaticos <- nombre_del_archivo
  base_de_datos_nueva$archivo.con.coordenadas <- nombre_del_archivo_con_coordenadas[length(nombre_del_archivo_con_coordenadas)]
  
  message('Proceso terminado con exito!')
  
  return(base_de_datos_nueva)
}
