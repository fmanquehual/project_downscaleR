db_a_serie_de_tiempo <- function(data_base, tipo.variable='pp', conf.especificas=NULL, serie.de.tiempo=TRUE){
  
  if(tipo.variable=='pp'){ message('Preparando la db para precipitacion') } else(
    message('Preparando la db para temperatura') )
  
  # data_base <- base_de_datos2
  # tipo.variable <- variable
  # serie.de.tiempo <- FALSE
  # conf.especificas <- NULL
  # 
  anho_mes.i <- unique(data_base$anho_mes)
  
  # observado ----
  
  for (i in 1:length(anho_mes.i)) {
    #i <- 1
    id.i <- which(data_base$anho_mes%in%anho_mes.i[i])
    data_base2 <- data_base[id.i,]
    
    # En pp, quedaria mm/day, por que los datos son pp acumulada en 5 dias
    if(tipo.variable=='pp'){conf.acum.i <- mean(data_base2[,'Obs'])/5} else(
      conf.acum.i <- mean(data_base2[,'Obs']) # t2, tiene datos diarios
    ) 
    
    if(i==1){data_base.i <- data.frame(Date=anho_mes.i[i], Obs=conf.acum.i)} 
    if(i>1){data_base.j <- data.frame(Date=anho_mes.i[i], Obs=conf.acum.i)
    data_base.i <- rbind(data_base.i, data_base.j)}
  }
  
  data_base.obs <- data_base.i ; data_base.obs
  
  # fin ---
  
  
  
  # pronostico (promedio mensual) ----
  columnas.de.configuraciones <- 3:(ncol(data_base)-1)
  conf.acum.i <- c()
  numero.de.conf <- c(NA, NA, 1:24)
  
  for (j in columnas.de.configuraciones) {
    # j <- 3
    for (i in 1:length(anho_mes.i)) {
      # i <- 1
      id.i <- which(data_base$anho_mes%in%anho_mes.i[i])
      data_base2 <- data_base[id.i,]
      
      # En pp, quedaria mm/day, por que los datos son pp acumulada en 5 dias
      if(tipo.variable=='pp'){conf.acum.i <- mean(data_base2[,j])/5} else(
        conf.acum.i <- mean(data_base2[,j]) # t2, tiene datos diarios
      ) 
      
      if(i==1){data_base.i <- data.frame(Date=anho_mes.i[i], Conf.i=conf.acum.i)} 
      if(i>1){data_base.j <- data.frame(Date=anho_mes.i[i], Conf.i=conf.acum.i)
      data_base.i <- rbind(data_base.i, data_base.j)}
    }
    
    nombre.columna <- paste('Conf', numero.de.conf[j], sep = '-')
    colnames(data_base.i) <- c('Date', nombre.columna)
    if(j==3){data_base.conf.i <- data_base.i} else(data_base.conf.i <- merge(data_base.conf.i, data_base.i, by='Date'))
  }
  
  data_base3 <- data_base[,c('Date', 'anho_mes')]
  data_base4 <- data_base3[!duplicated(data_base3$anho_mes), ]
  
  data_base.obs.conf.i <- merge(data_base.obs, data_base.conf.i, by='Date')
  data_base5 <- merge(data_base.obs.conf.i, data_base4, by.x='Date', by.y='anho_mes')
  
  anho.i <- as.numeric(format(data_base5$Date.y[1], '%Y'))
  mes.i <- as.numeric(format(data_base5$Date.y[1], '%m'))
  
  numero.de.filas <- nrow(data_base5)
  anho.f <- as.numeric(format(data_base5$Date.y[numero.de.filas], '%Y'))
  mes.f <- as.numeric(format(data_base5$Date.y[numero.de.filas], '%m'))
  
  matriz <- as.matrix(data_base5[,c(2:26)])
  db.con.fechas <- data_base5[,c(1:26)]
  
  conf.1.al.24 <- c(0:24)
  conf.especificas.i <- which(conf.1.al.24%in%conf.especificas)
  
  if(is.null(conf.especificas)){conf.especificas.i <- 2:25}
  
  subset.matriz <- matriz[,c(1, conf.especificas.i)] ; subset.matriz
  series.de.tiempo <- ts(data = subset.matriz, start = c(anho.i, mes.i), frequency = 12) ; series.de.tiempo
  
  message('Listo')
  
  if(serie.de.tiempo==TRUE){return(series.de.tiempo)} else(return(db.con.fechas))
  
}
