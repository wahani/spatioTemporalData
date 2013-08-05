# Class and method Definition for simSetup
setClass(Class="simSetup", representation(n = "numeric",
                                          nDomains = "numeric",
                                          nTime = "numeric",
                                          arCorr = "numeric",
                                          sarCorr = "numeric",
                                          sarVar = "function",
                                          arVar = "function",
                                          sigma1 = "numeric",
                                          sigma2 = "numeric",
                                          sigma = "matrix",
                                          neighbourHood = "matrix",
                                          beta = "numeric",
                                          xdt = "function",
                                          data = "list"))

simRunner <- function(setup) {
  # Run simulation for given setup object
  gc()
  omegaList <- reSarArVar(sarVar = setup@sigma1 * setup@sarVar(W0 = setup@neighbourHood, sarCorr = setup@sarCorr), 
                          arVar = setup@sigma2 * setup@arVar(arCorr = setup@arCorr, nTime = setup@nTime), 
                          outputAsList = TRUE)
  
  Z <- reZ(nDomains=setup@nDomains, nTime=setup@nTime)
  xdt <- spGenerator(nDomains=setup@nDomains, nTime=setup@nTime)
  re <- reGeneratorMC(omega=omegaList, n = setup@n)
  se <- seGenerator(nDomains=setup@nDomains, nTime=setup@nTime, n=setup@n)
  
  y <- setup@beta[1] + setup@beta[2] * xdt + Z %*% re + se
  
  slot(setup, "sigma") <- se
  slot(setup, "data") <- lapply(data.frame(y), 
                                function(y, xdt, nDomains, nTime) {
                                  data.frame(y = y, x = xdt, Domain = rep(1:nDomains, each = nTime), Time = rep(1:nTime, nDomains))
                                }, 
                                xdt = xdt, setup@nDomains, setup@nTime)
  setup
}        

simRun <- function(..., mc = F) {
  # Start simulation with given specification
  gc()
  setupList <- as.list(...)
  if (mc) {
    require(parallel)
    numberOfCores <- getNumberOfCores()
    setupList <- mclapply(X=setupList, FUN=simRunner, mc.cores = numberOfCores)
  } else {
    setupList <- lapply(setupList, simRunner)
  }
  
  if(length(setupList) == 1) {
    return(unlist(setupList))
  } else {
    return(setupList)
  }
}