library(Metrics)

homogenizacion_entre_db_observado_y_simulado <- function(db_observado, db_simulado, solo_observado=FALSE, estacion=NULL, 
                                                         para_Taylor=FALSE, pbias=FALSE, normalizado=FALSE){
  # db_observado <- estaciones
  # db_simulado <- eqm
  # estacion <- estacion.de.interes 
  
  if(is.null(estacion)){warning('Debes indicar la estacion')}
  
  # Filtro estacion de interes
  db_observado <- subset(db_observado, nombre_estacion==estacion)
  if(solo_observado==FALSE){db_simulado <- subset(db_simulado, nombre_estacion==estacion)}
  
  
  # Preparacion de base de datos 
  message('Descartando valores NA de ambas bases de datos')  
  
  id_elementos_con_NA <- which(is.na(db_observado$valor))
  if(length(id_elementos_con_NA)==0){db_observado_depurado <- db_observado
  } else(db_observado_depurado <- db_observado[-id_elementos_con_NA,])
  
  fechas.a.evaluar <- db_observado_depurado$fecha
  
  
  if(solo_observado==FALSE){ c(
    
  # Homogenizacion entre db's
    message('Conservando fechas compartidas entre observado y simulado'),
    db_simulado_depurado <- db_simulado[db_simulado$fecha%in%fechas.a.evaluar,],
    
    # simulado
    id_elementos_con_NA <- which(is.na(db_simulado_depurado$valor)),
    
    if(length(id_elementos_con_NA)==0){db_simulado_depurado <- db_simulado_depurado
    } else(c(
      db_simulado_depurado <- db_simulado_depurado[-id_elementos_con_NA,],
      fechas.a.evaluar <- db_simulado_depurado$fecha,
      db_observado_depurado <- db_observado_depurado[db_observado_depurado$fecha%in%fechas.a.evaluar,] ))
    
  )
  } else(db_simulado_depurado <- db_observado_depurado)
  
  se_conservaron_todas_las_fechas_1 <- length(db_simulado_depurado$fecha)==length(fechas.a.evaluar)
  se_conservaron_todas_las_fechas_2 <- setequal(unique(db_simulado_depurado$fecha), unique(fechas.a.evaluar))
  
  if(se_conservaron_todas_las_fechas_1==FALSE & se_conservaron_todas_las_fechas_2 == FALSE){
     warning('No se pudo conservar a todas las fechas debido a los NA')}
  
  # Calculo de metricas
  
  if(solo_observado==FALSE & para_Taylor==TRUE){ c(
  
    # Desviacion estandar
    message('Calculando desviacion estandar de datos observados y simulados'),
    db_simulado_depurado$sd.observado <- sd.diagrama.de.Taylor.modificado(db_observado_depurado$valor, 
                                                                          db_simulado_depurado$valor,
                                                                          type = 'observado',
                                                                          normalize = normalizado),
    
    db_simulado_depurado$sd.simulado <- sd.diagrama.de.Taylor.modificado(db_observado_depurado$valor, 
                                                                         db_simulado_depurado$valor,
                                                                         type = 'model',
                                                                         normalize = normalizado),
    
    # Sesgo
    if(pbias==FALSE){c(db_simulado_depurado$bias <- bias(db_observado_depurado$valor, db_simulado_depurado$valor),
                                             message('Calculando sesgo'))
      } else{c(db_simulado_depurado$bias <- percent_bias(db_observado_depurado$valor, db_simulado_depurado$valor),
                                              message('Calculando sesgo en porcentaje'))},
  
    # R
    valores_observados <- db_observado_depurado$valor,
    valores_simulados <- db_simulado_depurado$valor,
    
    if(length(valores_observados) > 5000 | length(valores_simulados) > 5000){
      message('Numero de datos excede el limite maximo para test de Shapiro-Wilk (>5000), se tomara una muestra aleatoria (=5000)')},
    
    if(length(valores_observados) > 5000){muestra_valores_observados <- sample(valores_observados, 5000)
    } else(muestra_valores_observados <- valores_observados),
    
    if(length(valores_simulados) > 5000){muestra_valores_simulados <- sample(valores_simulados, 5000)
    } else(muestra_valores_simulados <- valores_simulados),
    
    p.value.db_observado_depurado <- shapiro.test(muestra_valores_observados)[2], # Ho = ~N
    p.value.db_simulado_depurado <- shapiro.test(muestra_valores_simulados)[2],
    
    if(p.value.db_observado_depurado < 0.05 | p.value.db_simulado_depurado < 0.05
    ){c(db_simulado_depurado$r <- cor(valores_observados, valores_simulados, method = "spearman"),
      message('Calculando coeficiente de correlacion de Spearman'))
    } else(c(db_simulado_depurado$r <- cor(valores_observados, valores_simulados, method = "pearson"),
           message('Calculando coeficiente de correlacion de Pearson'))),
    
    # Preparando base de datos
    db_simulado_depurado <- db_simulado_depurado[1, c('bias', 'r', 'sd.observado', 'sd.simulado')],
    
    # Coordenadas 
    db_simulado_depurado$x <- xy.diagrama.de.Taylor.modificado.2(db_simulado_depurado$r, db_simulado_depurado$sd.observado, 
                                                                 db_simulado_depurado$sd.simulado)[1,1],
    
    db_simulado_depurado$y <- xy.diagrama.de.Taylor.modificado.2(db_simulado_depurado$r, db_simulado_depurado$sd.observado, 
                                                                 db_simulado_depurado$sd.simulado)[1,2]
    
  ) }
  
  
  # Salida
  message('Listo!')
  return(db_simulado_depurado)
  
}
