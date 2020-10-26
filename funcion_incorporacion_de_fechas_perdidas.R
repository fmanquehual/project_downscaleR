library(lubridate)

incorporacion_de_fechas_perdidas <- function(base_de_datos, formato_fecha="%Y-%m-%d"){
  # base_de_datos <- read.csv("Cochrane_INIA_prec_47.2417_72.5822_246.csv")
  # base_de_datos <- db.i
  # formato_fecha <- formato_de_fecha
  
  message('Incorporando fechas perdidas...')
  
  colnames(base_de_datos)[1] <- 'fecha'
  base_de_datos$fecha <- as.Date(base_de_datos$fecha, formato_fecha)
  
  base_de_datos$anho <- year(base_de_datos$fecha)
  base_de_datos$mes <- month(base_de_datos$fecha)
  
  base_de_datos$anho_mes <- paste(base_de_datos$anho, base_de_datos$mes, sep = '_')
  
  anho_minimo <- year(min(base_de_datos$fecha))
  anho_maximo <- year(max(base_de_datos$fecha))
  rango_de_anhos <- anho_minimo:anho_maximo
  
  anhos_correctos <- sort(rep(rango_de_anhos, 12))
  meses_correctos <- rep(1:12, (anho_maximo-anho_minimo+1))
  
  periodo_correcto_preliminar <- data.frame(anho_2=anhos_correctos, mes_2=meses_correctos)
  periodo_correcto_preliminar$anho_mes <- paste(periodo_correcto_preliminar$anho_2, periodo_correcto_preliminar$mes_2, sep = '_')
  
  base_de_datos_2 <- base_de_datos
  periodo_completo <- merge(base_de_datos_2, periodo_correcto_preliminar, by='anho_mes', all=TRUE)
  anho_mes_periodo_completo <- unique(periodo_completo$anho_mes)
  
  base_de_datos_depurado <- c()
  
  for (i in 1:length(periodo_completo$anho_mes)) {
   
    base_de_datos_i <- subset(periodo_completo, anho_mes==anho_mes_periodo_completo[i])
    if(nrow(base_de_datos_i)==0){next}
      
    fecha_correcta <- fechas_del_anho(base_de_datos_i)
    
    base_de_datos_depurado0 <- merge(base_de_datos_i, fecha_correcta, by='fecha', all=TRUE)
    
    base_de_datos_depurado <- rbind(base_de_datos_depurado0, base_de_datos_depurado)
   
  }
  
  base_de_datos_depurado <- base_de_datos_depurado[order(base_de_datos_depurado$fecha),]
  row.names(base_de_datos_depurado) <- 1:nrow(base_de_datos_depurado)
  
  columnas_a_eliminar0 <- c('anho_mes', 'anho', 'mes', 'anho_2', 'mes_2')
  columnas_a_eliminar <- which(colnames(base_de_datos_depurado)%in%columnas_a_eliminar0)

  id_fecha_inicio <- which(base_de_datos_depurado$fecha==min(base_de_datos$fecha))
  id_fecha_fin <-  which(base_de_datos_depurado$fecha==max(base_de_datos$fecha))
  id_fechas_originales <- id_fecha_inicio:id_fecha_fin
  
  base_de_datos_depurado_output <- base_de_datos_depurado[id_fechas_originales,-columnas_a_eliminar]
  row.names(base_de_datos_depurado_output) <- 1:nrow(base_de_datos_depurado_output)
  
  return(base_de_datos_depurado_output)
  
}