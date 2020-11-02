varCoeficient <- function(delta,data,cv){
  y <- cv - sd((data^delta), na.rm = TRUE)/mean((data^delta), na.rm = TRUE)
  return(y)
}

ptr <- function(o, p, s, precip, entregar_a=FALSE, entregar_b=FALSE){
  
  if (precip == FALSE) {
    stop("method power transformation is only applied to precipitation data")
  } else( c(
    # o <- observado
    # p <- simulados
    # s <- simulados
    
    b <- NaN,
    cvO <- sd(o,na.rm = TRUE) / mean(o, na.rm = TRUE),
    
    if (!is.na(cvO)) {
      
      bi <- try(uniroot(function(x)
        varCoeficient(x, abs(p), cvO), c(0,1), extendInt = "yes"), silent = TRUE)
      
      if ("try-error" %in% class(bi)) {  # an error occurred
        b <- NA
      } else(
        b <- bi$root
      )
    },
    
    p[p < 0] <-  0,
    s[s < 0] <-  0,
    
    aux_c <- p^rep(b,length(p),1),
    aux <- s^rep(b,length(s),1),
    a <- (mean(o, na.rm = TRUE)/mean(aux_c, na.rm = TRUE)),
    prC <- aux * rep((mean(o, na.rm = TRUE) / mean(aux_c, na.rm = TRUE)), length(s), 1),
    aux <- aux_c <- NULL,
  
    if(entregar_a==TRUE){return(a)} else if(entregar_b==TRUE){return(b)} else(return(prC))
    
    )
  )
  
}

# Fuente: https://rdrr.io/github/SantanderMetGroup/downscaleR/src/R/biasCorrection.R
