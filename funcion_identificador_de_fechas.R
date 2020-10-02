library(stringi)

identificador_de_fechas <- function(fecha_de_referencia, es_fecha_inicio=TRUE, incluir_tiempo=TRUE){
  
  # Segregacion de la informacion
  segregacion <- stri_split_fixed(fecha_de_referencia, ' ', simplify=TRUE) ; segregacion
  
  hora.segregada <- unlist(segregacion)[2] ; hora.segregada
  fecha.segregada <- unlist(segregacion)[1] ; fecha.segregada
  
  fecha.segregada.2 <- stri_split_fixed(fecha.segregada, '-', simplify=TRUE) ; fecha.segregada.2
  dia.segregada <- unlist(fecha.segregada.2)[3] ; dia.segregada
  mes.segregada <- unlist(fecha.segregada.2)[2] ; mes.segregada
  anho.segregada <- unlist(fecha.segregada.2)[1] ; anho.segregada
  
  dia.i <- dia.segregada
  mes.i <- mes.segregada
  anho.i <- anho.segregada
  
  
  # Creando db con los dias totales por mes y anho bisiesto
  anhos.bisiestos <- seq(0, 2100, by=4)
  match.anho.bisiesto <- anhos.bisiestos%in%anho.i
  anho.bisiesto <- which(match.anho.bisiesto)
  
  if(length(anho.bisiesto)==0){feb.i <- 28} else(feb.i <- 29)
  
  numero.de.dias.por.mes <- data.frame(mes_nombre=c('ene', 'feb', 'mar', 'abr', 'may', 'jun', 'jul', 'ago', 'sep', 'oct', 'nov', 'dic'),
                                       mes_numero=c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'),
                                       numero_de_dias=c(31, feb.i, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))
  
  numero.de.dias.por.mes$mes_numero <- as.character(numero.de.dias.por.mes$mes_numero)
  
  
  # Identificacion de la fecha desconocida a parir de la referencia (el dia despues)
  numero.de.fila.i <- which(numero.de.dias.por.mes$mes_numero==mes.i)
  
  if(numero.de.fila.i==1){numero.de.fila.j <- 12} else(numero.de.fila.j <- numero.de.fila.i)
  
  if(dia.i=='01'){dia.j <- numero.de.dias.por.mes$numero_de_dias[numero.de.fila.j]} else(
                  dia.j <- as.numeric(dia.i)-1)
  
  if(dia.j<=9){dia.j <- paste('0', dia.j, sep = '')}
  
  mes.j <- numero.de.dias.por.mes$mes_numero[numero.de.fila.j]
  
  if(mes.i=='01' & dia.i=='01'){anho.j <- anho.i-1} else(anho.j <- anho.i)
  
  fecha.identificada.preliminar <- paste(anho.i, mes.j, dia.j, sep = '-')
  
  if(incluir_tiempo==TRUE){fecha.identificada <- paste(fecha.identificada.preliminar, hora.segregada, sep = ' ')} else(
                           fecha.identificada <- fecha.identificada.preliminar)
  
  
  # Salida
  if(es_fecha_inicio==TRUE){periodo_de_tiempo <- '...$Dates$start'} else(periodo_de_tiempo <- '...$Dates$end')
  
  mensaje <- paste('La fecha identificada ', '(en ', periodo_de_tiempo, ')',' fue ', fecha.identificada, 
                   ' (', 'referencia: ', fecha_de_referencia, ')', sep = '')
  
  message(mensaje)
  
  return(fecha.identificada)
}
