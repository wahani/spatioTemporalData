#' Function for the Sampling Error Part
#' @export
seSigmaClosure <- function(nDomains, nTime) {
  # Function for generating variance parameters for sampling error deterministically
  force(nDomains); force(nTime)
  alpha0 = 0.8
  alpha1 = 1.2
  function(d, t) (alpha1 - alpha0) * (nTime * (d-1) + t - 1) / (nDomains * nTime) + alpha0
}

#' Function for generating Sampling errors with deterministic variance parameter
#' @export
seGenerator <- function(nDomains, nTime, n) {
  # Function for generating Sampling errors with deterministic variance parameter
  sigma <- seSigmaClosure(nDomains, nTime)
  sigmaGenerated <- unlist(lapply(1:nDomains, sigma, t = 1:nTime))
  samplingError <- lapply(sigmaGenerated, function(sigma) rnorm(n, mean = 0, sd = sigma))
  do.call(rbind, samplingError)
}

