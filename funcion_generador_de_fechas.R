# ESTA FUNCION GENERA FECHAS A NIVEL ANUAL (SOLO 1 ANHO)

generador_de_fechas <- function(anho, incluir.febrero.29=TRUE){
  
  # anho <- 1995
  # incluir.febrero.29 <- FALSE
 
  # Anhos bisiestos
  anhos.bisiestos <- seq(0, 2100, by=4)
  
  
  # Es anho bisiesto?
  match.anho.bisiesto <- anhos.bisiestos==anho
  anho.bisiesto <- which(match.anho.bisiesto)
  
  if(incluir.febrero.29==TRUE){
    if(length(anho.bisiesto)==0){feb.i <- 28} else(feb.i <- 29)
      } else(feb.i <- 28)
  
  
  # Creando dbs con los dias del mes correspondiente
  
  enero <- data.frame(anho=anho,
                      mes='01',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  febrero <- data.frame(anho=anho,
                        mes='02',
                        dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:feb.i))
  
  marzo <- data.frame(anho=anho,
                      mes='03',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  abril <- data.frame(anho=anho,
                      mes='04',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  mayo <- data.frame(anho=anho,
                     mes='05',
                     dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  junio <- data.frame(anho=anho,
                      mes='06',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  julio <- data.frame(anho=anho,
                      mes='07',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  agosto <- data.frame(anho=anho,
                       mes='08',
                       dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  septiembre <- data.frame(anho=anho,
                           mes='09',
                           dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  octubre <- data.frame(anho=anho,
                        mes='10',
                        dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  noviembre <- data.frame(anho=anho,
                          mes='11',
                          dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  diciembre <- data.frame(anho=anho,
                          mes='12',
                          dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  
  # Creando db con todas las fechas del anho
  
  db_fechas_anho <- rbind(enero, febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, 
                          octubre, noviembre, diciembre)
  
  fechas <- paste(db_fechas_anho$anho, db_fechas_anho$mes, db_fechas_anho$dia, sep = '-')
  fechas <- as.Date(fechas, "%Y-%m-%d")
  db_fechas <- data.frame(fecha=fechas)
  
  # Salida
  
  return(db_fechas)
  
}