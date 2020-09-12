datos_observados_por_anhos_de_interes <- function(base_de_datos, anhos_de_interes, variable = 'pp'){
    
    # base_de_datos <- read.csv(archivos.i[i])
    # anhos_de_interes <- anhos.interes
    # variable <- variable.de.interes
    
    colnames(base_de_datos)[1] <- 'Dates'
    base_de_datos$Dates <- as.Date(base_de_datos$Dates, "%Y-%m-%d")
    base_de_datos2 <- subset_anhos_de_interes(base_de_datos, anho_para_filtro = anhos_de_interes)
    base_de_datos3 <- base_de_datos2[, c('Date', 'Obs')]
    
    anhos.despues.del.filtro <- unique(format(base_de_datos3$Date, '%Y'))
    anhos.match <- setequal(anhos_de_interes, anhos.despues.del.filtro)
    
    if(anhos.match==TRUE){message('Se encontraron todos los anhos de interes')} else( c(
        
        message('Solo se hallaron datos para:'),
    
        for (i in 1:length(anhos.despues.del.filtro)) {
            message(anhos.despues.del.filtro[i])
        }
        )
    )
    
    return(base_de_datos3)
}
