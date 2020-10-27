library(Metrics)

homogenizacion_entre_db_observado_y_simulado <- function(db_observado, db_simulado, solo_observado=FALSE, 
                                                         para_Taylor=FALSE, pbias=FALSE, normalizado=FALSE){
  # db_observado <- estaciones
  # db_simulado <- eqm
  
  # Preparacion de base de datos 
  message('Descartando valores NA')  
  
  id_elementos_con_NA <- which(is.na(db_observado$valor))
  db_observado_depurado <- db_observado[-id_elementos_con_NA,]
  fechas.a.evaluar <- db_observado_depurado$fecha
  
  
  if(solo_observado==FALSE){ c(
    
  # Homogenizacion entre db's
    message('Conservando fechas compartidas entre observado y simulado'),
    db_simulado_depurado <- db_simulado[db_simulado$fecha%in%fechas.a.evaluar,]
  )
  } else(db_simulado_depurado <- db_observado_depurado)
  
  
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
    p.value.db_observado_depurado <- shapiro.test(db_observado_depurado$valor)[2], # Ho = ~N
    p.value.db_simulado_depurado <- shapiro.test(db_simulado_depurado$valor)[2],
    
    if(p.value.db_observado_depurado < 0.05 | p.value.db_simulado_depurado < 0.05
    ){c(db_simulado_depurado$r <- cor(db_observado_depurado$valor, db_simulado_depurado$valor, method = "spearman"),
      message('Calculando coeficiente de correlacion de Spearman'))
    } else(c(db_simulado_depurado$r <- cor(db_observado_depurado$valor, db_simulado_depurado$valor, method = "pearson"),
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
