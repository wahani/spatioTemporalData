#' Function for the Sampling Error Part
#' @export
seSigmaClosure <- function(nDomains, nTime) {
  # Function for generating variance parameters for sampling error deterministically
  force(nDomains); force(nTime)
  alpha0 = 0.8
  alpha1 = 1.2
  sigmaFun <- function(d, t) (alpha1 - alpha0) * (nTime * (d-1) + t - 1) / (nDomains * nTime) + alpha0
  function() unlist(lapply(1:nDomains, sigmaFun, t = 1:nTime))
}


#' Function for generating Sampling errors with deterministic variance parameter
#' @export
seGenerator <- function(nDomains, nTime, n, sigmaFun) {
  # Function for generating Sampling errors with deterministic variance parameter
  #sigma <- seSigmaClosure(nDomains, nTime)
  #sigmaGenerated <- unlist(lapply(1:nDomains, sigmaFun, t = 1:nTime))
  sigmaGenerated <- sigmaFun()
  samplingError <- lapply(sigmaGenerated, function(sigma) rnorm(n, mean = 0, sd = sigma))
  do.call(rbind, samplingError)
}

#' Contaminated SamplingErrors
#' @export
seSigmaContClosure <- function(nDomains, nTime, nDomainsCont = 1, contFactor = 2, randomDomains = FALSE) {
  
  sigmaGenerated <- seSigmaClosure(nDomains, nTime)()
  sigmaContaminated <- contaminateSigma(sigmaGenerated, nDomains, nTime, nDomainsCont = nDomainsCont, contFactor = contFactor, randomDomains = randomDomains)
  
  function() return(sigmaContaminated)
}

contaminateSigma <- function(sigmaGenerated, nDomains, nTime, nDomainsCont = 2, contFactor = 2, randomDomains = FALSE) {
  if (nDomainsCont == 0) 
    return(sigmaGenerated) else {
      index <- gl(n=nDomains, k=nTime)
      selectedDomains <- if(!randomDomains) (nDomains:1)[1:nDomainsCont] else sample(nDomains:1, nDomainsCont)
      sigmaGenerated[index %in% selectedDomains] <- sigmaGenerated[index %in% selectedDomains] * contFactor
      return(sigmaGenerated)
    }
}
