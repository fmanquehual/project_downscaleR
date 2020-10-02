# # Depende de:
# setwd('C:/Users/Usuario/Documents/Francisco/proyecto_DownscaleR/')
# source('funcion_fillGridDates_modificado.R')

# x <- pr.sum
# y <- y

biasCorrection_modificado <- function (y, x, newdata = NULL, precipitation = FALSE, method = c("delta", 
                                                                  "scaling", "eqm", "pqm", "gpqm", "loci", "dqm", "qdm", "isimip3"), 
          cross.val = c("none", "loo", "kfold"), folds = NULL, window = NULL, 
          scaling.type = c("additive", "multiplicative"), fitdistr.args = list(densfun = "normal"), 
          wet.threshold = 1, n.quantiles = NULL, extrapolation = c("none", 
                                                                   "constant"), theta = c(0.95, 0.05), detrend = TRUE, 
          isimip3.args = NULL, join.members = FALSE, parallel = FALSE, 
          max.ncores = 16, ncores = NULL) 
{
  if (method == "gqm") 
    stop("'gqm' is not a valid choice anymore. Use method = 'pqm' instead and set fitdistr.args = list(densfun = 'gamma')")
  method <- match.arg(method, choices = c("delta", "scaling", 
                                          "eqm", "pqm", "gpqm", "mva", "loci", "ptr", "variance", 
                                          "dqm", "qdm", "isimip3"))
  cross.val <- match.arg(cross.val, choices = c("none", "loo", 
                                                "kfold"))
  scaling.type <- match.arg(scaling.type, choices = c("additive", 
                                                      "multiplicative"))
  extrapolation <- match.arg(extrapolation, choices = c("none", 
                                                        "constant"))
  stopifnot(is.logical(join.members))
  nwdatamssg <- TRUE
  if (is.null(newdata)) {
    newdata <- x
    nwdatamssg <- FALSE
  }
  seas <- getSeason(y)
  
  # Aqui modificaste ----
  
  #y <- fillGridDates(y) # original
  y <- fillGridDates_modificado(y, tz = "GMT", estaciones_metereologicas=TRUE) # nuevo
  
  #x <- fillGridDates(x) # original
  x <- fillGridDates_modificado(x, tz = "GMT", estaciones_metereologicas=FALSE) # nuevo
  
  #newdata <- fillGridDates(newdata)
  newdata <- fillGridDates_modificado(newdata, tz = "GMT", estaciones_metereologicas=FALSE) # nuevo
  
  # fin ---
  
  yx <- intersectGrid(y, x, type = "temporal", which.return = 1:2)
  y <- yx[[1]]
  x <- yx[[2]]
  if (cross.val == "none") {
    output <- biasCorrectionXD(y = y, x = x, newdata = newdata, 
                               precipitation = precipitation, method = method, 
                               window = window, scaling.type = scaling.type, fitdistr.args = fitdistr.args, 
                               pr.threshold = wet.threshold, n.quantiles = n.quantiles, 
                               extrapolation = extrapolation, theta = theta, join.members = join.members, 
                               detrend = detrend, isimip3.args = isimip3.args, 
                               parallel = parallel, max.ncores = max.ncores, ncores = ncores)
  }
  else {
    if (nwdatamssg) {
      message("'newdata' will be ignored for cross-validation")
    }
    if (cross.val == "loo") {
      years <- as.list(unique(getYearsAsINDEX(x)))
    }
    else if (cross.val == "kfold" & !is.null(folds)) {
      years <- folds
    }
    else if (cross.val == "kfold" & is.null(folds)) {
      stop("Fold specification is missing, with no default")
    }
    output.list <- lapply(1:length(years), function(i) {
      target.year <- years[[i]]
      rest.years <- setdiff(unlist(years), target.year)
      station <- FALSE
      if ("loc" %in% getDim(y)) 
        station <- TRUE
      yy <- redim(y, member = FALSE)
      yy <- if (method == "delta") {
        subsetGrid(yy, years = target.year, drop = FALSE)
      }
      else {
        subsetGrid(yy, years = rest.years, drop = FALSE)
      }
      if (isTRUE(station)) {
        yy$Data <- adrop(yy$Data, drop = 3)
        attr(yy$Data, "dimensions") <- c(setdiff(getDim(yy), 
                                                 c("lat", "lon")), "loc")
      }
      else {
        yy <- redim(yy, drop = TRUE)
      }
      newdata2 <- subsetGrid(x, years = target.year, drop = F)
      xx <- subsetGrid(x, years = rest.years, drop = F)
      message("Validation ", i, ", ", length(unique(years)) - 
                i, " remaining")
      biasCorrectionXD(y = yy, x = xx, newdata = newdata2, 
                       precipitation = precipitation, method = method, 
                       window = window, scaling.type = scaling.type, 
                       fitdistr.args = fitdistr.args, pr.threshold = wet.threshold, 
                       n.quantiles = n.quantiles, extrapolation = extrapolation, 
                       theta = theta, join.members = join.members, 
                       detrend = detrend, isimip3.args = isimip3.args, 
                       parallel = parallel, max.ncores = max.ncores, 
                       ncores = ncores)
    })
    al <- which(getDim(x) == "time")
    Data <- sapply(output.list, function(n) unname(n$Data), 
                   simplify = FALSE)
    bindata <- unname(do.call("abind", c(Data, along = al)))
    output <- output.list[[1]]
    dimNames <- attr(output$Data, "dimensions")
    output$Data <- bindata
    attr(output$Data, "dimensions") <- dimNames
    output$Dates <- x$Dates
    output$Data[which(is.infinite(output$Data))] <- NA
  }
  output <- subsetGrid(output, season = seas)
  return(output)
}
