biasCorrection1D <- function(o, p, s,
                             method, 
                             scaling.type,
                             fitdistr.args,
                             precip, 
                             pr.threshold,
                             n.quantiles,
                             extrapolation, 
                             theta,
                             detrend,
                             isimip3.args,
                             parallel = FALSE,
                             max.ncores = 16,
                             ncores = NULL) {
  parallel.pars <- parallelCheck(parallel, max.ncores, ncores)
  mapply_fun <- selectPar.pplyFun(parallel.pars, .pplyFUN = "mapply")
  if (parallel.pars$hasparallel) on.exit(parallel::stopCluster(parallel.pars$cl))
  if (method == "delta") {
    mapply_fun(delta, o, p, s)
  } else if (method == "scaling") {
    mapply_fun(scaling, o, p, s, MoreArgs = list(scaling.type = scaling.type))
  } else if (method == "eqm") {
    suppressWarnings(
      mapply_fun(eqm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles, extrapolation))
    )
  } else if (method == "pqm") {
    suppressWarnings(
      mapply_fun(pqm, o, p, s, MoreArgs = list(fitdistr.args, precip, pr.threshold))
    )
  } else if (method == "gpqm") {
    mapply_fun(gpqm, o, p, s, MoreArgs = list(precip, pr.threshold, theta))
  } else if (method == "mva") {
    mapply_fun(mva, o, p, s) 
  } else if (method == "variance") {
    mapply_fun(variance, o, p, s, MoreArgs = list(precip))
  } else if (method == "loci") {
    mapply_fun(loci, o, p, s, MoreArgs = list(precip, pr.threshold))
  } else if (method == "ptr") {
    mapply_fun(ptr, o, p, s, MoreArgs = list(precip))
  } else if (method == "dqm") {
    mapply_fun(dqm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles, detrend))
  } else if (method == "qdm") {
    mapply_fun(qdm, o, p, s, MoreArgs = list(precip, pr.threshold, n.quantiles))
  } else if (method == "isimip3") {
    mapply_fun(isimip3, o, p, s, MoreArgs = isimip3.args) #this method is in a separate file
  }
  #INCLUIR AQUI METODOS NUEVOS
}
