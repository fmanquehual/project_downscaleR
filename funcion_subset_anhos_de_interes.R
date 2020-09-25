subset_anhos_de_interes <- function(data_base, anho_para_filtro=NULL){
  # data_base <- base_de_datos
  # anho_para_filtro <- anhos_de_interes
  
  colnames(data_base)[1] <- 'Date'
  data_base[,1] <- as.character(data_base[,1])
  data_base[,1] <- gsub('-', '/', data_base[,1])
  data_base[,1] <- as.Date(data_base[,1])
  data_base$anho <- format(data_base[,1],'%Y')
  
  if(is.null(anho_para_filtro)){db.out0 <- data_base} else(c(id <- which(data_base$anho%in%anho_para_filtro), 
                                                             db.out0 <- data_base[id,]))
  db.out <- db.out0[,-ncol(db.out0)]
  return(db.out)
}
