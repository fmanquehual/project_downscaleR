fechas_del_anho <- function(base_de_datos){

  # base_de_datos <- base_de_datos_i
  # base_de_datos <- db.i
  # base_de_datos <- db.fecha.original
  
  if( is.na(unique(base_de_datos$mes)) ){
    if(unique(base_de_datos$mes_2)<=9){base_de_datos$mes <- paste0('0', base_de_datos$mes_2)}
    } else( 
        if(unique(base_de_datos$mes)<=9){base_de_datos$mes <- paste0('0', base_de_datos$mes)} 
        )
  
  if( is.na(unique(base_de_datos$anho)) ){ base_de_datos$anho <- base_de_datos$anho_2  }
    
  # Anhos bisiestos
  anhos.bisiestos <- seq(0, 2100, by=4)
  
  
  # Es anho bisiesto?
  match.anho.bisiesto <- anhos.bisiestos%in%base_de_datos$anho
  anho.bisiesto <- which(match.anho.bisiesto)
  if(length(anho.bisiesto)==0){feb.i <- 28} else(feb.i <- 29)
  
  
  # Creando dbs con los dias del mes correspondiente

  enero <- data.frame(anho=unique(base_de_datos$anho),
                      mes='01',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  febrero <- data.frame(anho=unique(base_de_datos$anho),
                        mes='02',
                        dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:feb.i))
  
  marzo <- data.frame(anho=unique(base_de_datos$anho),
                      mes='03',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  abril <- data.frame(anho=unique(base_de_datos$anho),
                      mes='04',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  mayo <- data.frame(anho=unique(base_de_datos$anho),
                     mes='05',
                     dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  junio <- data.frame(anho=unique(base_de_datos$anho),
                      mes='06',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  julio <- data.frame(anho=unique(base_de_datos$anho),
                      mes='07',
                      dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  agosto <- data.frame(anho=unique(base_de_datos$anho),
                       mes='08',
                       dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  septiembre <- data.frame(anho=unique(base_de_datos$anho),
                           mes='09',
                           dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  octubre <- data.frame(anho=unique(base_de_datos$anho),
                        mes='10',
                        dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  noviembre <- data.frame(anho=unique(base_de_datos$anho),
                          mes='11',
                          dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:30))
  
  diciembre <- data.frame(anho=unique(base_de_datos$anho),
                          mes='12',
                          dia=c('01', '02', '03', '04', '05', '06', '07', '08', '09', 10:31))
  
  
  # Creando db con todas las fechas del anho
  
  db_fechas_anho <- rbind(enero, febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, octubre, noviembre, diciembre)
  db_fechas_anho_subset <- subset(db_fechas_anho, mes==unique(base_de_datos$mes))
    
  fechas_mes <- paste(db_fechas_anho_subset$anho, db_fechas_anho_subset$mes, db_fechas_anho_subset$dia, sep = '-')
  fechas_mes <- as.Date(fechas_mes, "%Y-%m-%d")
  db_fechas_mes <- data.frame(fecha=fechas_mes)
  
  # Salida
  
  return(db_fechas_mes)
  
}