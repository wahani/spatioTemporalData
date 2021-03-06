#' Class for sumulation setups
#'
#' @name simSetup
#' @aliases simSetup-class
#' @exportClass simSetup
setClass(Class="simSetup", representation(n = "numeric",
                                          nDomains = "numeric",
                                          nTime = "numeric",
                                          arCorr = "numeric",
                                          sarCorr = "numeric",
                                          sarVar = "function",
                                          arVar = "function",
                                          seVar = "function",
                                          sigma1 = "numeric",
                                          sigma2 = "numeric",
                                          sigma = "matrix", # Bezeichnung
                                          sigmaSE = "numeric",
                                          neighbourHood = "matrix",
                                          beta = "numeric",
                                          xdt = "function",
                                          data = "list",
                                          scenarioName = "character"))

simRunner <- function(setup) {
  # Run simulation for given setup object
  omegaList <- reSarArVar(sarVar = setup@sigma1 * setup@sarVar(W0 = setup@neighbourHood, sarCorr = setup@sarCorr), 
                          arVar = setup@sigma2 * setup@arVar(arCorr = setup@arCorr, nTime = setup@nTime), 
                          outputAsList = TRUE)
  
  Z <- reZ(nDomains=setup@nDomains, nTime=setup@nTime)
  xdt <- setup@xdt(nDomains=setup@nDomains, nTime=setup@nTime)
  re <- reGeneratorMC(omega=omegaList, n = setup@n)
  se <- seGenerator(nDomains=setup@nDomains, nTime=setup@nTime, n=setup@n, sigmaFun = setup@seVar)
  
  y <- setup@beta[1] + setup@beta[2] * xdt + Z %*% re + se
  
  slot(setup, "sigma") <- se
  slot(setup, "data") <- lapply(data.frame(y), 
                                function(y, xdt, nDomains, nTime) {
                                  data.frame(y = y, x = xdt, Domain = rep(1:nDomains, each = nTime), Time = rep(1:nTime, nDomains))
                                }, 
                                xdt = xdt, setup@nDomains, setup@nTime)
  slot(setup, "sigmaSE") <- setup@seVar()
  setup <- setTrueY(setup)
  setup
}     

setTrueY <- function(simSetup) {
  # funciton-definition
  trueY <- function(dat, sigmaE) {
    dat$trueY <- dat$y - sigmaE 
    dat
  }
  
  #
  sigmaE <- as.data.frame(slot(simSetup, "sigma"))
  dat <- slot(simSetup, "data")
  dataList <- mapply("trueY", dat, sigmaE, SIMPLIFY = FALSE)
  slot(simSetup, "data") <- dataList
  simSetup
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