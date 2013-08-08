#' simSetupContamination
#'  
#' @description Setup the properties of data to be simulated. Based on the simulation in Marhuenda (2013) with the possibility 
#' to add contaminated data.
#' 
#' @param nDomains numeric. Number of domains, can be a vector.
#' @param nTime numeric. Number of time periods, can be a vector.
#' @param sarCorr numeric. Autocorrelation for spatial process, can be a vector.
#' @param arCorr numeric. Autocorrelation for AR(1) process, can be a vector.
#' @param n numeric. Number of data to be simulated, is a scalar.
#' @param seVar Function which returns the contaminated variances.
#' 
#' @return an object of class simSetup. Needs no further modification.
#' @examples
#' setup <- simSetupContamination(nDomains=100, nTime=20, sarCorr=0.5, arCorr=0.5, n = 50)
#' output <- simRun(setup)
#' @export
simSetupContamination <- function(nDomains, nTime, sarCorr, arCorr, 
                                  seVar = seSigmaContClosure(nDomains, 
                                                             nTime, 
                                                             nDomainsCont = 1, 
                                                             contFactor = 2, 
                                                             randomDomains = FALSE), 
                                  n = 200) {
  
  setup <- new("simSetup", 
               n = n,
               nDomains = nDomains,
               nTime = nTime,
               sarCorr = sarCorr,
               arCorr = arCorr,
               sarVar = reSar1Var,
               arVar = reAr1Var,
               seVar = seVar,
               sigma1 = 1,
               sigma2 = 1,
               #               sigma = seSigmaClosure(nDomains, nTime),
               neighbourHood = w0Matrix(nDomains),
               beta = c(0,1)
               #                xdt = spGenerator(nDomains, nTime)
  )
  
  setup
}

