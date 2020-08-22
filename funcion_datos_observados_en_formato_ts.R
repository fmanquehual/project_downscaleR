datos_observados_en_formato_ts <- function(base_de_datos, anhos_de_interes, variable = 'pp'){
    
    # base_de_datos <- read.csv(archivos.i[i])
    # anhos_de_interes <- anhos.interes
    # variable <- variable.de.interes
    
    colnames(base_de_datos)[1] <- 'Dates'
    base_de_datos$Dates <- as.Date(base_de_datos$Dates, "%Y-%m-%d")
    base_de_datos2 <- preparacion.db.para.serie.de.tiempo(base_de_datos, anho_para_filtro = anhos_de_interes)
    base_de_datos3 <- db.a.serie.de.tiempo(base_de_datos2, tipo.variable = variable, serie.de.tiempo = FALSE)
    base_de_datos4 <- base_de_datos3[, c('Date', 'Obs')]
    
    return(base_de_datos4)
}