seleccion_de_poder_optimo_para_IDW <- function(muestra_valores_poder, capa_de_puntos,
                                               parametro, nueva_grilla, raster_de_referencia,
                                               RSR=FALSE, NSE=TRUE, PBIAS=FALSE, RMSE=FALSE, 
                                               MAE=TRUE){
  
  # muestra_valores_poder <- valores.de.poder.IDW.a.evaluar
  # capa_de_puntos <- puntos
  # parametro <- 'a'
  # nueva_grilla <- grd
  # raster_de_referencia <- raster.referencia
  
  rsr.calculados <- c()
  nse.calculados <- c()
  pbias.calculados <- c()
  rmse.calculados <- c()
  mae.calculados <- c()
  
  for (j in muestra_valores_poder) {
    # j <- 1
    
    capa_de_puntos.parametro.idw <- idw(eval(parse(text=parametro)) ~ 1, capa_de_puntos, 
                                        newdata=nueva_grilla, debug.level=0, idp=j)
    r.idw.parametro0 <- raster(capa_de_puntos.parametro.idw)
    raster_de_referencia[] <- r.idw.parametro0[]
    r.idw.parametro <- raster_de_referencia
  
    parametro.estimado.i0 <- extract(r.idw.parametro, capa_de_puntos, sp=TRUE) 
    parametro.estimado.i <- parametro.estimado.i0@data[,ncol(parametro.estimado.i0@data)]
    parametro.observado.i <- parametro.estimado.i0@data[,parametro]
    
    rsr.j <- hydroGOF::rsr(sim = parametro.estimado.i, obs=parametro.observado.i)
    nse.j <- hydroGOF::NSE(sim = parametro.estimado.i, obs=parametro.observado.i)
    pbias.j <- hydroGOF::pbias(sim = parametro.estimado.i, obs=parametro.observado.i)
    rmse.j <- Metrics::rmse(actual = parametro.observado.i, predicted = parametro.estimado.i)
    mae.j <- Metrics::mae(actual = parametro.observado.i, predicted = parametro.estimado.i)
    
    rsr.calculados <- c(rsr.calculados, rsr.j)
    nse.calculados <- c(nse.calculados, nse.j)
    pbias.calculados <- c(pbias.calculados, pbias.j)
    rmse.calculados <- c(rmse.calculados, rmse.j)
    mae.calculados <- c(mae.calculados, mae.j)
    
  }
  
  db.rsr <- data.frame(poder=muestra_valores_poder, rsr=rsr.calculados)
  db.rsr <- db.rsr[order(db.rsr$rsr, decreasing = FALSE),]
  db.rsr$puntaje_rsr <- 1:nrow(db.rsr)
  
  db.nse <- data.frame(poder=muestra_valores_poder, nse=nse.calculados)
  db.nse <- db.nse[order(db.nse$nse, decreasing = TRUE),]
  db.nse$puntaje_nse <- 1:nrow(db.nse)
  
  db.pbias <- data.frame(poder=muestra_valores_poder, pbias=pbias.calculados)
  db.pbias <- db.pbias[order(db.pbias$pbias, decreasing = FALSE),]
  db.pbias$puntaje_pbias <- 1:nrow(db.pbias)
  
  db.rmse <- data.frame(poder=muestra_valores_poder, rmse=rmse.calculados)
  db.rmse <- db.rmse[order(db.rmse$rmse, decreasing = FALSE),]
  db.rmse$puntaje_rmse <- 1:nrow(db.rmse)
  
  db.mae <- data.frame(poder=muestra_valores_poder, mae=mae.calculados)
  db.mae <- db.mae[order(db.mae$mae, decreasing = FALSE),]
  db.mae$puntaje_mae <- 1:nrow(db.mae)
  
  db.metricas1 <- merge(db.rsr, db.nse, by='poder')
  db.metricas2 <- merge(db.metricas1, db.pbias, by='poder')
  db.metricas3 <- merge(db.metricas2, db.rmse, by='poder')
  db.metricas4 <- merge(db.metricas3, db.mae, by='poder')
  db.metricas <- db.metricas4[,c('poder', 'puntaje_rsr', 'puntaje_nse', 'puntaje_pbias',
                                 'puntaje_rmse', 'puntaje_mae')]
  
  estadistico <- c()
  if(RSR==TRUE){estadistico <- c(estadistico, 2)}
  if(NSE==TRUE){estadistico <- c(estadistico, 3)}
  if(PBIAS==TRUE){estadistico <- c(estadistico, 4)}
  if(RMSE==TRUE){estadistico <- c(estadistico, 5)}
  if(MAE==TRUE){estadistico <- c(estadistico, 6)}
  
  if(length(estadistico)==1){db.metricas$puntaje_total <- rowSums(cbind(db.metricas[,estadistico]))
    } else(db.metricas$puntaje_total <- rowSums(db.metricas[,estadistico]))
  
  db.metricas <- db.metricas[order(db.metricas$puntaje_total, db.metricas$poder, 
                                   decreasing = FALSE),]
  
  poder.optimo <- db.metricas$poder[1]
  
  return(poder.optimo)

}