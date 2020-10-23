localizacion_directorio_de_trabajo <- function(nombre_carpeta){

    setwd(nombre_carpeta)
    
    opciones <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
    opciones_numerico1 <- 1:length(list.dirs(path = ".", full.names = FALSE, recursive = FALSE))
    opciones_numerico2 <- paste0(opciones_numerico1,  ': ')
    
    opciones_matriz <- as.matrix(opciones)
    colnames(opciones_matriz) <- 'Carpetas:'
    row.names(opciones_matriz) <- opciones_numerico2
    
    print(opciones_matriz)
    carpeta_especifica <- readline(prompt='Que carpeta escoges? (Escribe el numero): ')
    
    directorio_de_trabajo <- paste0(nombre_carpeta, opciones[as.numeric(carpeta_especifica)], '/')
    setwd(directorio_de_trabajo)
    
    mas.carpetas <- list.dirs(path = ".", full.names = FALSE, recursive = FALSE)
    
    if(length(mas.carpetas)>0){
      seguir.buscando <- readline(prompt='Hay mas carpetas, quieres seguir buscando?\n1: Si\n2: No ')
      
      if(seguir.buscando=='1'){ localizacion_directorio_de_trabajo(directorio_de_trabajo) }
    }
    
    message('Hecho!')

  }

