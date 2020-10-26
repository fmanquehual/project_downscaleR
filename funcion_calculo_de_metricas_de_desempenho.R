library(Metrics)

calculo_de_metricas_de_desempenho <- function(db_observado, db_simulado, precision=2){
  
  # Preparacion de base de datos 
  message('Descartando valores NA')  
  
  id_elementos_con_NA <- which(is.na(db_observado$valor))
  db_observado_depurado <- db_observado[-id_elementos_con_NA,]
  fechas.a.evaluar <- db_observado_depurado$fecha
  
  db_simulado_depurado <- db_simulado[db_simulado$fecha%in%fechas.a.evaluar,]
  
  
  
  # Calculo de metricas de desempeño
  message('Calculo de metricas utilizados en "Mendez et al. (2020)"')
  
  rmse <- rmse(actual = db_observado_depurado$valor, predicted = db_simulado_depurado$valor)
  mae <- mae(actual = db_observado_depurado$valor, predicted = db_simulado_depurado$valor)
  bias <- bias(actual = db_observado_depurado$valor, predicted = db_simulado_depurado$valor)
  pbias <- (sum(db_observado_depurado$valor-db_simulado_depurado$valor)/sum(db_observado_depurado$valor))*100 # Mendez et al. (2020)
  
  # Si p-value es > 0.05, entonces la distribucion es normal. Ocupamos a Pearson.
  # Si p-value es < 0.05, entonces la distribucion es asimetricas. Necesitamos a Spearman o Kendall.
  # Mendez et al. (2020) ocupo la de Pearson.
  
  p.value.db_observado_depurado <- shapiro.test(db_observado_depurado$valor)[2] # Ho = ~N
  p.value.db_simulado_depurado <- shapiro.test(db_simulado_depurado$valor)[2]
  
  if(p.value.db_observado_depurado < 0.05 | p.value.db_simulado_depurado < 0.05
  ){r <- cor(db_observado_depurado$valor, db_simulado_depurado$valor, method = "spearman")
  } else(r <- cor(db_observado_depurado$valor, db_simulado_depurado$valor, method = "pearson"))
  
  
  
  # Preparando base de datos
  nombre.de.metricas <- c('RMSE', 'MAE', 'BIAS', 'PBIAS', 'R')
  valores.de.metricas <- c(rmse, mae, bias, pbias, r)
  
  db <- data.frame(metrica = nombre.de.metricas, valor = round(valores.de.metricas, precision))
  
  
  
  # Salida
  return(db)
  
}