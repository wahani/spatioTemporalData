# Functions needed for the simulation of Marhuenda (2013) Data
simConstructSetupSets <- function(nDomains, nTime, sarCorr, arCorr) {
  # Creates a list with all possible combinations in setup
  counter <- counterClosure()
  setupSets <- list()
  for (d in seq_along(nDomains)) {
    for (t in seq_along(nTime)) {
      for (i in seq_along(sarCorr)) {
        for (j in seq_along(arCorr)) {
          setupSets[[counter()]] <- list("nDomains" = nDomains[d], "nTime" = nTime[t], "sarCorr" = sarCorr[i], "arCorr" = arCorr[j])
        }
      }
    }  
  }
  setupSets
}

#' simRun
#' 
#' @description Run simulation for given simSetup
#' 
#' @param setup is of class simSetup and best be supplied by simSetupMarhuenda.
#' @param mc logical indicator if multicore functionality should be used. On windows, the statement is irrelevant.
#' 
#' @return Returns a list containing as many simSetup objects as possible scenarios are supplied by the input. The slot 'data' will be filled with simulated data.
#' 
#' @examples
#' setup <- simSetupMarhuenda(nDomains=100, nTime=c(10, 20), sarCorr=0.5, arCorr=c(0, 0.5))
#' output <- simRun(setup)
#' length(slot(output[[1]], "data"))
#' dat <- slot(output[[1]], "data")[[1]]
#' summary(dat)
#' @export
simRunSetup <- function(setup, mc = F) {
  # Run simulation
  setupSets <- simConstructSetupSets(slot(setup, "nDomains"), 
                                     slot(setup, "nTime"), 
                                     slot(setup, "sarCorr"), 
                                     slot(setup, "arCorr"))
  setups <- lapply(setupSets, 
                   function(set, setup) {
                     slot(setup, "nDomains") <- set$nDomains
                     slot(setup, "nTime") <- set$nTime
                     slot(setup, "sarCorr") <- set$sarCorr
                     slot(setup, "arCorr") <- set$arCorr
                     setup
                   }, 
                   setup)
 
  simRun(setups, mc = mc)
}

#' simSetupMarhuenda
#'  
#' @description Setup the properties of data to be simulated. Based on the simulation in Marhuenda (2013).
#' 
#' @param nDomains numeric. Number of domains, can be a vector.
#' @param nTime numeric. Number of time periods, can be a vector.
#' @param sarCorr numeric. Autocorrelation for spatial process, can be a vector.
#' @param arCorr numeric. Autocorrelation for AR(1) process, can be a vector.
#' @param n numeric. Number of data to be simulated, is a scalar. 
#' 
#' @return an object of class simSetup. Needs no further modification.
#' @examples
#' setup <- simSetupMarhuenda(nDomains=100, nTime=20, sarCorr=0.5, arCorr=0.5, n = 50)
#' output <- simRunMarhuenda(setup)
#' @export
simSetupMarhuenda <- function(nDomains, nTime, sarCorr, arCorr, n = 1000) {
  #simMarhuendaCheckInput(nDomains, nTime, sarCorr, arCorr, n)
  setup <- new("simSetup", 
               n = n,
               nDomains = nDomains,
               nTime = nTime,
               sarCorr = sarCorr,
               arCorr = arCorr,
               sarVar = reSar1Var,
               arVar = reAr1Var,
               seVar = seSigmaClosure(nDomains, nTime),
               sigma1 = 1,
               sigma2 = 1,
               #               sigma = seSigmaClosure(nDomains, nTime),
               neighbourHood = w0Matrix(nDomains),
               beta = c(0,1)
               #                xdt = spGenerator(nDomains, nTime)
  )
  setup
}

simMarhuendaCheckInput <- function(nDomains, nTime, sarCorr, arCorr, n) {
  if (any(!(nDomains > 1))) stop("nDomains must be larger than 1 - as in 'spatial' data!?")
  if (any(!(nTime > 1))) stop("nTime must be larger than 1 - as in 'temporal' data!?")
  #if (any(!(sarCorr >= 0 & sarCorr < 1))) stop("Correlation is defined between 0 and 1. But nice try.")
  #if (any(!(arCorr >= 0 & arCorr < 1))) stop("Correlation is defined between 0 and 1. But nice try.")
  if (any(!(n > 1))) stop("At this time the number of simulations need to be larger than 1, sorry.")
}