library(stringi)

generador_de_matriz_archivos_DGA <- function(lista_de_archivos, entregar_db_depurada=FALSE,
                                             separador_de_columna=';', decimal='.'){
  
  # lista_de_archivos <- lista.de.archivos
  
  for (l in 1:length(lista_de_archivos)) {
    # l <- 1
    
    # Lectura archivo(s) ----
    nombre.archivo <- lista_de_archivos[l]
    db <- read.csv(nombre.archivo, sep = separador_de_columna, dec = decimal)
    
    # fin ----  
    
    
    
    # Depurando la base de datos---
  
    nombre.columnas <- colnames(db)
    
    # quito las columnas sin datos (X, X.1, X.2 ..., X.n)
    columnas.a.eliminar.identificadas0 <- stri_detect_fixed(nombre.columnas, 'X')
    columnas.a.eliminar.identificadas <- nombre.columnas[columnas.a.eliminar.identificadas0]
    columnas.a.eliminar <- which(nombre.columnas%in%columnas.a.eliminar.identificadas)
    db2 <- db[,-columnas.a.eliminar]
    
    # quito las filas sin datos
    filas.a.eliminar <- which(db$anho%in%NA)
    
    if(length(filas.a.eliminar)==0){db3 <- db2} else(
       db3 <- db2[-filas.a.eliminar,])
    
    data_base_depurada <- db3
    
    # fin ---
    
    
    
    
    # Generando una DB con formato 'YYYYMMDD' y 'CALOR OBSERVADO'---
    
    data_base_lista <- c()
    anhos.unicos <- unique(db3$anho)
    
    for (anho.i in anhos.unicos) {
      
      anhos.bisiestos <- seq(0, 2100, by=4)
      db3.anho.i <- db3[db3$anho%in%anho.i,]
      
      match.anho.bisiesto <- anhos.bisiestos%in%anho.i
      anho.bisiesto <- which(match.anho.bisiesto)
      
      if(length(anho.bisiesto)==0){feb.i <- 28} else(feb.i <- 29)
      numero.de.dias.por.mes <- data.frame(ene=31,
                                          feb=feb.i,
                                          mar=31,
                                          abr=30,
                                          may=31,
                                          jun=30,
                                          jul=31,
                                          ago=31,
                                          sep=30,
                                          oct=31,
                                          nov=30,
                                          dic=31)
      
      for (columna.mes.i in 3:ncol(db3)) {
        
        #columna.mes.i <- 3
        
        mes.i <- columna.mes.i-2
        numero.de.dias.del.mes.i <- numero.de.dias.por.mes[,mes.i]
        
        obs.i.preliminar <- as.character(db3.anho.i[,columna.mes.i])
        obs.i.preliminar <- as.numeric(obs.i.preliminar)
        obs.i <- obs.i.preliminar[1:numero.de.dias.del.mes.i]
        obs.i
        
        if(mes.i<=9){mes.i <- paste('0', mes.i, sep = '')}
        
        dias.desde.el.1.al.9.numerico <- 1:9
        dias.desde.el.1.al.9 <- paste(0, dias.desde.el.1.al.9.numerico, sep = '')
        dias.desde.el.10 <- 10:numero.de.dias.del.mes.i
        dias <- c(dias.desde.el.1.al.9, dias.desde.el.10)
        
        fecha.i <- paste(anho.i, mes.i, dias, sep = '')
        db4 <- data.frame(YYYYMMDD=fecha.i, obs=obs.i)
        
        if(mes.i=='01'){db5 <- db4} else(db5 <- rbind(db5, db4))
        if(mes.i=='12'){colnames(db5)[ncol(db5)] <- nombre.archivo}
      }
      
      data_base_lista <- rbind(data_base_lista, db5)
      
      # fin ---
    }
    
    
    # Elaboracion de matriz ----
    
    if(l==1){matriz <- data_base_lista} else(matriz <- merge(matriz, data_base_lista, by='YYYYMMDD', all=TRUE))
    
    for (m in 2:ncol(matriz)) {
      matriz[,m][matriz[,m]%in%NA] <- NaN
    }
    
    # fin ---
    
  }
  
  if(entregar_db_depurada==FALSE){return(matriz)} else(return(data_base_depurada))
}