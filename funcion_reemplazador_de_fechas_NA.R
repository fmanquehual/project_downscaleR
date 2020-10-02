library(lubridate)

setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
source('funcion_identificador_de_fechas.R')

reemplazador_de_fechas_NA <- function(objeto_loadStationData, tz = "GMT", incluir_tiempo=TRUE){
  
  # objeto_loadStationData <- y

  if(incluir_tiempo==TRUE){objeto_loadStationData$Dates$start <- objeto_loadStationData$Dates$start} else(c(
  objeto_loadStationData$Dates$start <- stri_split_fixed(objeto_loadStationData$Dates$start, ' ', simplify=TRUE),
  objeto_loadStationData$Dates$start <- objeto_loadStationData$Dates$start[,1]) )
  
  fechas.i.NA <- which(objeto_loadStationData$Dates$start%in%NA)
  
  for (i in 1:length(fechas.i.NA)) {
    # i <- 1
    fecha.referencia <- objeto_loadStationData$Dates$start[(fechas.i.NA[i]+1)]
    fecha.identificada <- identificador_de_fechas(fecha.referencia, es_fecha_inicio=TRUE, incluir_tiempo=incluir_tiempo)
    
    objeto_loadStationData$Dates$start[fechas.i.NA[i]] <- fecha.identificada
  }
  
  # objeto_loadStationData$Dates$start <- as.POSIXct(strptime(objeto_loadStationData$Dates$start,
  #                                                           "%Y-%m-%d"))
  # objeto_loadStationData$Dates$start <- as.POSIXlt(objeto_loadStationData$Dates$start, "%Y-%m-%d", tz='GMT')
  # objeto_loadStationData$Dates$start <- as.POSIXct(objeto_loadStationData$Dates$start, format="%Y-%m-%d",
  #                                                  origin = objeto_loadStationData$Dates$start[1], tz = tz)    # in UTC
  # objeto_loadStationData$Dates$start <- ymd(objeto_loadStationData$Dates$start)
  # objeto_loadStationData$Dates$start <- as.POSIXlt(objeto_loadStationData$Dates$start, "%Y-%m-%d %H:%M:%OS", tz='')
  
  
  if(incluir_tiempo==TRUE){objeto_loadStationData$Dates$end <- objeto_loadStationData$Dates$end} else(c(
  objeto_loadStationData$Dates$end <- stri_split_fixed(objeto_loadStationData$Dates$end, ' ', simplify=TRUE),
  objeto_loadStationData$Dates$end <- objeto_loadStationData$Dates$end[,1]) )

  fechas.j.NA <- which(objeto_loadStationData$Dates$end%in%NA)
  
  for (j in 1:length(fechas.j.NA)) {
    
    fecha.referencia <- objeto_loadStationData$Dates$end[(fechas.j.NA[j]+1)]
    fecha.identificada <- identificador_de_fechas(fecha.referencia, es_fecha_inicio=FALSE, incluir_tiempo=incluir_tiempo)
    
    objeto_loadStationData$Dates$end[fechas.j.NA[j]] <- fecha.identificada
  }
  
  # objeto_loadStationData$Dates$end <- as.POSIXlt(objeto_loadStationData$Dates$end, "%Y-%m-%d", tz='GMT')
  # objeto_loadStationData$Dates$end <- as.POSIXct(objeto_loadStationData$Dates$end, format="%Y-%m-%d",
  #                                                  origin = objeto_loadStationData$Dates$end[1], tz = tz)    # in UTC
  # objeto_loadStationData$Dates$end <- ymd(objeto_loadStationData$Dates$end)
  # objeto_loadStationData$Dates$end <- as.POSIXlt(objeto_loadStationData$Dates$end, "%Y-%m-%d %H:%M:%OS", tz='')
  
  return(objeto_loadStationData)
}
