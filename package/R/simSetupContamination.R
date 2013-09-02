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
                                                             nDomainsCont = 0, 
                                                             contFactor = 0, 
                                                             randomDomains = FALSE),
                                  spatialCont = list(sigma1 = 1, sigma1Cont = 10, nDomainsCont = 2),
                                  temporalCont = list(sigma2 = 1, sigma2Cont = 10, nDomainsCont = 2),
                                  spatioTemporalMessup = FALSE,
                                  n = 200) {
  
  # setup without cont
  nDomainsNonC <- nDomains - spatialCont$nDomainsCont - temporalCont$nDomainsCont
  setup <- new("simSetup", 
               n = n,
               nDomains = nDomainsNonC,
               nTime = nTime,
               sarCorr = sarCorr,
               arCorr = arCorr,
               sarVar = reSar1Var,
               arVar = reAr1Var,
               seVar = seSigmaContClosure(nDomainsNonC, nTime),
               sigma1 = spatialCont$sigma1,
               sigma2 = temporalCont$sigma2,
               #               sigma = seSigmaClosure(nDomains, nTime),
               neighbourHood = w0Matrix(nDomainsNonC),
               beta = c(0,1)
               #                xdt = spGenerator(nDomains, nTime)
  )
  
  if(spatialCont$nDomainsCont == 0 & temporalCont$nDomainsCont == 0) 
    # Setup without Contamination
    return(simRunSetup(setup))
  
  outputNonC <- simRunSetup(setup)
  setupsCont <- list()
  
  if(spatioTemporalMessup) {
    setupsCont$Messup <- setup
    setupsCont$Messup@nDomains <- spatialCont$nDomainsCont + temporalCont$nDomainsCont
    setupsCont$Messup@sigma1 <- spatialCont$sigma1Cont
    setupsCont$Messup@sigma2 <- temporalCont$sigma2Cont
    setupsCont$Messup@seVar <- seSigmaContClosure(setupsCont$Messup@nDomains, nTime)
    setupsCont$Messup@neighbourHood <- w0Matrix(setupsCont$Messup@nDomains)
  }
  
  lapply(setupsCont, simRunSetup)

}

