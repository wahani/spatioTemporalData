#' simRunContamination
#'  
#' @description Setup the properties of data to be simulated. Based on the simulation in Marhuenda (2013) with the possibility 
#' to add contaminated data.
#' 
#' @param nDomains numeric. Number of domains, can be a vector.
#' @param nTime numeric. Number of time periods, can be a vector.
#' @param sarCorr numeric. Autocorrelation for spatial process, can be a vector.
#' @param arCorr numeric. Autocorrelation for AR(1) process, can be a vector.
#' @param n numeric. Number of data to be simulated, is a scalar.
# #' @param seVar Function which returns the contaminated variances.
#' @param spatialCont list. Settings for spatial contaminated Data - see defaults
#' @param temporalCont list. Settings for temporal contaminated Data - see defaults
#' 
#' @return an object of class simSetup. Needs no further modification.
#' @examples
#' output <- simRunContamination(100, 10, c(0, 0.5), c(0, 0.5), n = 200, spatioTemporalMessup=F,
#'                              spatialCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 0),
#'                              temporalCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2))
#' @export
simRunContamination <- function(nDomains, nTime, sarCorr, arCorr, 
#                                    seVar = seSigmaContClosure(nDomains, 
#                                                               nTime, 
#                                                               nDomainsCont = 0, 
#                                                               contFactor = 0, 
#                                                               randomDomains = FALSE),
                                   spatialCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
                                   temporalCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
                                   spatioTemporalMessup = FALSE,
                                   n = 200) {
  
  settingList <- strsplit(levels(interaction(nDomains, nTime, sarCorr, arCorr, sep = "-")), split = "-", fixed = T)
  settingList <- lapply(settingList, as.numeric)
  setupList <- lapply(settingList, 
                      function(setting) 
                        list(nDomains = setting[1], 
                             nTime = setting[2], 
                             sarCorr = setting[3], 
                             arCorr = setting[4]
                             #seVar = seVar,
#                              spatialCont = spatialCont,
#                              temporalCont = temporalCont,
#                              spatioTemporalMessup = spatioTemporalMessup,
#                              n = n))
                        ))
  
  lapply(setupList, function(setup) simRunnerContamination(setup$nDomains, setup$nTime, setup$sarCorr, setup$arCorr, 
                                                           #seVar = setup$seVar,
                                                           spatialCont = spatialCont,
                                                           temporalCont = temporalCont,
                                                           spatioTemporalMessup = spatioTemporalMessup,
                                                           n = n))
}


simRunnerContamination <- function(nDomains, nTime, sarCorr, arCorr, 
                                  seVar = seSigmaContClosure(nDomains, 
                                                             nTime, 
                                                             nDomainsCont = 0, 
                                                             contFactor = 0, 
                                                             randomDomains = FALSE),
                                  spatialCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
                                  temporalCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
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
               sigma1 = spatialCont$sigma,
               sigma2 = temporalCont$sigma,
               #               sigma = seSigmaClosure(nDomains, nTime),
               neighbourHood = w0Matrix(nDomainsNonC),
               beta = c(0,1)
               #                xdt = spGenerator(nDomains, nTime)
  )
  
  if(spatialCont$nDomainsCont == 0 & temporalCont$nDomainsCont == 0) 
    # Setup without Contamination
    return(simRunSetup(setup)) # Function ends here
  
  outputNonC <- simRunSetup(setup)
  setupsCont <- list()
  
  if(spatioTemporalMessup) {
    setupsCont$Messup <- setup
    setupsCont$Messup@nDomains <- spatialCont$nDomainsCont + temporalCont$nDomainsCont
    setupsCont$Messup@sigma1 <- spatialCont$sigmaCont
    setupsCont$Messup@sigma2 <- temporalCont$sigmaCont
    setupsCont$Messup@seVar <- seSigmaContClosure(setupsCont$Messup@nDomains, nTime)
    setupsCont$Messup@neighbourHood <- w0Matrix(setupsCont$Messup@nDomains)
  } else{
    if(spatialCont$nDomainsCont > 0) {
      # Spatial Setting
      setupsCont$spatial <- setup
      setupsCont$spatial@nDomains <- spatialCont$nDomainsCont
      setupsCont$spatial@sigma1 <- spatialCont$sigmaCont
      setupsCont$spatial@seVar <- seSigmaContClosure(setupsCont$spatial@nDomains, nTime)
      setupsCont$spatial@neighbourHood <- w0Matrix(setupsCont$spatial@nDomains)
    }
    if(temporalCont$nDomainsCont > 0) {
      # Temporal Setting
      setupsCont$temporal <- setup
      setupsCont$temporal@nDomains <- temporalCont$nDomainsCont
      setupsCont$temporal@sigma2 <- temporalCont$sigmaCont
      setupsCont$temporal@seVar <- seSigmaContClosure(setupsCont$temporal@nDomains, nTime)
      setupsCont$temporal@neighbourHood <- w0Matrix(setupsCont$temporal@nDomains)
    }
  }
    
  output <- c(outputNonC, lapply(setupsCont, simRunSetup), recursive = TRUE)
  output <- Reduce("merge", output)
  output
}

#' merge.simSetup
#' 
#' @export
merge.simSetup <- function(setup1, setup2) {
  setup <- setup1
  setup@nDomains <- setup1@nDomains + setup2@nDomains
  setup@sigma <- rbind(setup1@sigma, setup2@sigma)
  setup@data <- mapply("combineData", setup1@data, setup2@data, SIMPLIFY = FALSE)
  neighbourHood <- matrix(0, ncol = setup@nDomains, nrow = setup@nDomains)
  neighbourHood[1:setup1@nDomains, 1:setup1@nDomains] <- setup1@neighbourHood
  neighbourHood[(setup1@nDomains+1):(setup1@nDomains+setup2@nDomains), 
                (setup1@nDomains+1):(setup1@nDomains+setup2@nDomains)] <- 
    setup2@neighbourHood
  setup@neighbourHood <- neighbourHood
  setup@sigmaSE <- c(setup1@sigmaSE, setup2@sigmaSE)
  setup
}

combineData <- function(dat1, dat2) {
  dat <- rbind(dat1, dat2)
  dat$Domain <- as.numeric(gl(n=max(dat1$Domain)+max(dat2$Domain), k=max(dat1$Time)))
  dat
}
