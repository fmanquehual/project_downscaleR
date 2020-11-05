library(Metrics)

calculo_de_metricas_de_desempenho <- function(db_observado, db_simulado, estacion=NULL, precision=2){
  
  # db_observado <- estaciones
  # db_simulado <- ptr
  # estacion <- estacion.de.interes
  
  if(is.null(estacion)){warning('Debes indicar la estacion')}
  
  # Filtro estacion de interes
  db_observado <- subset(db_observado, nombre_estacion==estacion)
  db_simulado <- subset(db_simulado, nombre_estacion==estacion)
  
  
  # Preparacion de base de datos 
  message('Descartando valores NA de ambas bases de datos')  
  
  # observado
  id_elementos_con_NA <- which(is.na(db_observado$valor))
  
  if(length(id_elementos_con_NA)==0){db_observado_depurado <- db_observado
    } else(db_observado_depurado <- db_observado[-id_elementos_con_NA,])
  
  fechas.a.evaluar <- db_observado_depurado$fecha
  db_simulado_depurado0 <- db_simulado[db_simulado$fecha%in%fechas.a.evaluar,]
  
  # simulado
  id_elementos_con_NA <- which(is.na(db_simulado_depurado0$valor))
  
  if(length(id_elementos_con_NA)==0){db_simulado_depurado <- db_simulado_depurado0
  } else(c(
    db_simulado_depurado <- db_simulado_depurado0[-id_elementos_con_NA,],
    fechas.a.evaluar <- db_simulado_depurado$fecha,
    db_observado_depurado <- db_observado_depurado[db_observado_depurado$fecha%in%fechas.a.evaluar,] ))
  
  # Calculo de metricas de desempeño
  message('Calculo de metricas utilizados en "Mendez et al. (2020)"')
  
  valores_observados <- db_observado_depurado$valor
  valores_simulados <- db_simulado_depurado$valor
  
  length(valores_observados)
  length(valores_simulados)
  
  rmse <- rmse(actual = valores_observados, predicted = valores_simulados)
  mae <- mae(actual = valores_observados, predicted = valores_simulados)
  bias <- bias(actual = valores_observados, predicted = valores_simulados)
  pbias <- (sum(valores_observados-valores_simulados)/sum(valores_observados))*100 # Mendez et al. (2020)
  
  # Si p-value es > 0.05, entonces la distribucion es normal. Ocupamos a Pearson.
  # Si p-value es < 0.05, entonces la distribucion es asimetricas. Necesitamos a Spearman o Kendall.
  # Mendez et al. (2020) ocupo la de Pearson.
  
  if(length(valores_observados) > 5000 | length(valores_simulados) > 5000){
      message('Numero de datos excede el limite maximo para test de Shapiro-Wilk (>5000), se tomara una muestra aleatoria (=5000)')}
  
  if(length(valores_observados) > 5000){muestra_valores_observados <- sample(valores_observados, 5000)
    } else(muestra_valores_observados <- valores_observados)
  
  if(length(valores_simulados) > 5000){muestra_valores_simulados <- sample(valores_simulados, 5000)
  } else(muestra_valores_simulados <- valores_simulados)
  
  p.value.db_observado_depurado <- shapiro.test(muestra_valores_observados)[2] # Ho = ~N
  p.value.db_simulado_depurado <- shapiro.test(muestra_valores_simulados)[2]
  
  if(p.value.db_observado_depurado < 0.05 | p.value.db_simulado_depurado < 0.05
  ){r <- cor(valores_observados, valores_simulados, method = "spearman")
  } else(r <- cor(valores_observados, valores_simulados, method = "pearson"))
  
  
  
  # Preparando base de datos
  nombre.de.metricas <- c('RMSE', 'MAE', 'BIAS', 'PBIAS', 'R')
  valores.de.metricas <- c(rmse, mae, bias, pbias, r)
  
  db <- data.frame(metrica = nombre.de.metricas, valor = round(valores.de.metricas, precision))
  
  
  
  # Salida
  return(db)
  
}

