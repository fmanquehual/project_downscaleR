verificacion_anhos_de_interes <- function(base_de_datos){

    # base_de_datos <- read.csv(archivos.i[i])
    
    colnames(base_de_datos)[1] <- 'Dates'
    base_de_datos$Dates <- as.Date(base_de_datos$Dates, "%Y-%m-%d")
    anhos.observados <- unique(format(base_de_datos$Dates, "%Y"))
    
    return(anhos.observados)
}