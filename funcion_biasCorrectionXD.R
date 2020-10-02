biasCorrectionXD <- function(y, x, newdata, 
                             precipitation, 
                             method,
                             window,
                             scaling.type,
                             fitdistr.args,
                             pr.threshold, 
                             n.quantiles, 
                             extrapolation, 
                             theta,
                             join.members,
                             detrend,
                             isimip3.args,
                             return.raw = FALSE,
                             interpGrid.args = list(),
                             parallel = FALSE,
                             max.ncores = 16,
                             ncores = NULL) {
  if (method == "isimip3") {
    window <- NULL
    # warning("Only parameter isimip3.args is considered")
    if (is.null(isimip3.args)) isimip3.args <- list()
    isimip3.args[["dates"]] <- list(obs_hist = y[["Dates"]][["start"]],
                                    sim_hist = x[["Dates"]][["start"]],
                                    sim_fut = newdata[["Dates"]][["start"]])
  }
  station <- FALSE
  if ("loc" %in% getDim(y)) station <- TRUE
  xy <- y$xyCoords
  # suppressWarnings(suppressMessages(pred <- interpGrid(x, getGrid(y), force.non.overlapping = TRUE)))
  # suppressWarnings(suppressMessages(sim <- interpGrid(newdata, getGrid(y), force.non.overlapping = TRUE)))
  interpGrid.args[["new.coordinates"]] <- getGrid(y)
  interpGrid.args[["grid"]] <- x
  suppressWarnings(suppressMessages(pred <- do.call("interpGrid", interpGrid.args)))
  interpGrid.args[["grid"]] <- newdata
  suppressWarnings(suppressMessages(sim <- do.call("interpGrid", interpGrid.args)))
  delta.method <- method == "delta"
  precip <- precipitation
  message("[", Sys.time(), "] Argument precipitation is set as ", precip, ", please ensure that this matches your data.")
  bc <- y
  if (isTRUE(join.members) & getShape(redim(sim))["member"] > 1) {
    n.mem.aux <- getShape(sim)["member"]
    pred <- flatMemberDim(pred, station)
    pred <- redim(pred, drop = T)
    sim <- flatMemberDim(sim, station)
    sim <- redim(sim, drop = T)
    y <- bindGrid(rep(list(y), n.mem.aux), dimension = "time")
  } else if (isTRUE(join.members) & !getShape(redim(sim))["member"] > 1) {
    warning("There is only one member, argument join.members ignored.")
    join.members <- FALSE
  }
  y <- redim(y, drop = TRUE)
  y <- redim(y, member = FALSE, runtime = FALSE)
  pred <- redim(pred, member = TRUE, runtime = TRUE)
  sim <- redim(sim, member = TRUE, runtime = TRUE)
  dimNames <- attr(y$Data, "dimensions")
  n.run <- getShape(sim)["runtime"]
  n.mem <- getShape(sim)["member"]
  if (join.members & !is.null(window)) {
    message("[", Sys.time(), "] Window option is currently not supported for joined members and will be ignored")
    window <- NULL
  }
  if (!is.null(window)) {
    win <- getWindowIndex(y = y, x = pred, newdata = sim, window = window, delta.method = delta.method)
  } else {
    win <- list()
    
    # Aqui modificaste ----
    
    indobservations <- match(as.POSIXct(pred$Dates$start, 'GMT'), as.POSIXct(y$Dates$start, 'GMT'))
    
    # fin ---
    
    ## esto no mola, es para el caso especial de join members...hay que mirarlo
    if (length(indobservations) > length(unique(indobservations))) indobservations <- 1:length(indobservations) 
    win[["Window1"]] <- list("obsWindow" = indobservations, "window" = 1:getShape(pred)["time"], "step" = 1:getShape(sim)["time"])
    if (delta.method) win[["Window1"]][["deltaind"]] <- indobservations
  }
  message("[", Sys.time(), "] Number of windows considered: ", length(win), "...")
  winarr <- array(dim = dim(sim$Data))
  if (delta.method) winarr <- array(dim = c(n.run, n.mem, getShape(y)))
  for (j in 1:length(win)) {
    yind <- win[[j]]$obsWindow
    outind <- win[[j]]$step
    if (delta.method) {
      yind <- win[[j]]$deltaind
      outind <- win[[j]]$deltaind
    } 
    yw <- y$Data[yind,,, drop = FALSE]
    pw <- pred$Data[,,win[[j]]$window,,, drop = FALSE]
    sw <- sim$Data[,,win[[j]]$step,,, drop = FALSE]
    runarr <- lapply(1:n.run, function(l){
      memarr <- lapply(1:n.mem, function(m){
        #join members message
        if (j == 1 & m == 1) {
          if (!isTRUE(join.members)) {
            message("[", Sys.time(), "] Bias-correcting ", n.mem, " members separately...")
          } else {
            message("[", Sys.time(), "] Bias-correcting ", attr(pred, "orig.mem.shape"), " members considering their joint distribution...")
          }
        }
        o = yw[, , , drop = FALSE]
        p = adrop(pw[l, m, , , , drop = FALSE], drop = c(T, T, F, F, F))
        s = adrop(sw[l, m, , , , drop = FALSE], drop = c(T, T, F, F, F))
        data <- list(o, p, s)
        if (!station) {
          data <- lapply(1:length(data), function(x) {
            attr(data[[x]], "dimensions") <- dimNames
            abind(array3Dto2Dmat(data[[x]]), along = 3)
          }) 
        }
        o <- lapply(seq_len(ncol(data[[1]])), function(i) data[[1]][,i,1])
        p <- lapply(seq_len(ncol(data[[2]])), function(i) data[[2]][,i,1])
        s <- lapply(seq_len(ncol(data[[3]])), function(i) data[[3]][,i,1])
        data <- NULL
        mat <- biasCorrection1D(o, p, s,
                                method = method,
                                scaling.type = scaling.type,
                                fitdistr.args = fitdistr.args,
                                precip = precip,
                                pr.threshold = pr.threshold,
                                n.quantiles = n.quantiles,
                                extrapolation = extrapolation,
                                theta = theta,
                                detrend = detrend,
                                isimip3.args = isimip3.args,
                                parallel = parallel,
                                max.ncores = max.ncores,
                                ncores = ncores)  
        if (!station) mat <- mat2Dto3Darray(mat, xy$x, xy$y)
        mat
      })
      unname(do.call("abind", list(memarr, along = 0)))
    })
    yw <- pw <- sw <- NULL
    winarr[,,outind,,] <- unname(do.call("abind", list(runarr, along = 0))) 
    runarr <- NULL
  }
  bc$Data <- unname(do.call("abind", list(winarr, along = 3)))
  winarr <- NULL
  attr(bc$Data, "dimensions") <- attr(sim$Data, "dimensions")
  if (station) bc <- redim(bc, loc = TRUE)
  bc$Dates <- sim$Dates
  ## Recover the member dimension when join.members=TRUE:
  if (isTRUE(join.members)) {
    if (method == "delta") {
      bc <- recoverMemberDim(plain.grid = pred, bc.grid = bc, newdata = newdata)
    }else{
      bc <- recoverMemberDim(plain.grid = sim, bc.grid = bc, newdata = newdata)      
    }
  } else {
    bc$InitializationDates <- sim$InitializationDates
    bc$Members <- sim$Members
  }
  if (return.raw) {
    sim[["Variable"]][["varName"]] <- paste0(bc[["Variable"]][["varName"]], "_raw")
    bc <- makeMultiGrid(bc, sim)
    if (station){
      bc <- redim(bc, loc = TRUE)
    }
  }
  pred <- newdata <- sim <- y <- NULL
  attr(bc$Variable, "correction") <- method
  bc <- redim(bc, drop = TRUE)
  message("[", Sys.time(), "] Done.")
  return(bc)
}
