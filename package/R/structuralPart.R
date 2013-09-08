# Functions for the Structural Part of a Model

spBdtClosure <- function(nDomains, nTime) {
  # Function for generating upper limit of deterministic regressor
  force(nDomains); force(nTime)
  function(d, t) {
    1 + (nTime*(d-1)+t) / nDomains
  }
}

spUdtClosure <- function(nTime) {
  # Function for generating Udt part of deterministc regressor
  force(nTime)
  function(t) t / (nTime + 1)
}

spXdtClosure <- function(nDomains, nTime) {
  # Function for generating deterministc regressor
  force(nDomains); force(nTime)
  adt <- 1 #lower limit of deterministic regressor
  bdt <- spBdtClosure(nDomains, nTime)
  udt <- spUdtClosure(nTime)
  function(d,t) (bdt(d, t) - adt) * udt(t) + adt
}

#' Generator for Regressor
#' 
#' @details Generator used in Marhuenda (2012)
#' 
#' @export
spGenerator <- function(nDomains, nTime) {
  # Function to construct bX Part of the model
  xdt <- spXdtClosure(nDomains, nTime)
  xdtGenerated <- unlist(lapply(1:nDomains, xdt, t = 1:nTime))
  xdtGenerated
}

#' Generator for Regressor
#' 
#' @details uniformly distributed random number between 9 and 12
#' 
#' @export
spGenerator1 <- function(nDomains, nTime) {
  # Function to construct bX Part of the model
  xdtGenerated <- runif(nDomains * nTime, 9, 12)
  xdtGenerated
}
