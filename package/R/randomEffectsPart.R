# Functions for Random Effects Part

#' reSar1Var
#' 
#' @description Constructs a covariance matrix for SAR1 process
#' @export
reSar1Var <- function(W0, sarCorr = 0.5) {
  #Compute variance/covariance matrice for SAR1 process
  I <- diag(1, nrow = nrow(W0), ncol = ncol(W0))
  W <- W0 / rowSums(W0)
  solve(t(I - sarCorr * W) %*% (I - sarCorr * W))
}

#' reAR1Var
#' 
#' @description Constructs a covariance matrix for AR1 process
#' @export
reAr1Var <- function(arCorr = 0.5, nTime = 10) {
  #Compute variance/covariance matrice for AR1 process
  if (nTime == 1) stop("nTime is equal to one. If you need only one time period this is not the covariance structure you want.")
  elements <- lapply(as.list((nTime-1):1), function(num, cor) cor^(1:num), arCorr)
  omega2 <- diag(1, nrow = nTime)
  omega2 <- as.vector(omega2)
  ind <- which(omega2 == 1)
  for(i in 1:(nTime-1)) {
    omega2[(ind[i]+1):(ind[i]+length(elements[[i]]))] <- elements[[i]]
  }
  omega2 <- matrix(omega2, nTime)
  omega2 <- omega2 + t(omega2) #symmetry
  diag(omega2) <- 1
  (1 - arCorr^2)^(-1) * omega2
}

#' Combine variance components (SAR and AR) - see Marhuenda (2012)
#' @export
reSarArVar <- function(sarVar, arVar, outputAsList = F) {
  #Combine variance components (SAR and AR) - see Marhuenda (2012)
  require(Matrix)
  omega <- list(sarVar)
  omega[2:(ncol(sarVar)+1)] <- list(arVar)
  if (outputAsList) {
    return(omega)
  } else {
    return(as.matrix(bdiag(omega)))
  }
}

reGenerator <- function(Sigma, mu = NULL) {
  #Closure for a Random Number generator for given mu and Sigma
  force(Sigma)
  if(is.null(mu)) mu <- rep(0, ncol(Sigma))
  function(n = 1) {
    require(MASS)
    mvrnorm(n, mu, Sigma)
  }
}

reGeneratorMC <- function(omega, mu = NULL, n = 1, mc = F) {
  # Generate random numbers for independent blocks in variance structure
  # Each Block will be generated on a differnet core
  generatorList <- lapply(omega, reGenerator, mu = mu)
  if (mc) {
    # No Performance gain at this point. Multicore functionality is switched off
    # by default
    require(parallel)
    numberOfCores <- getNumberOfCores()
    generatorList <- mclapply(X=generatorList, FUN=function(fun, n) t(fun(n)), n = n)
  } else {
    generatorList <- lapply(generatorList, function(fun, n) t(fun(n)), n = n)
  }
  do.call(rbind, generatorList)
}

#' reZ
#'
#' @description Function to generate 'Z' Matrix in mixed models. Function is designed for independent spatial and temporal random effect. Marhuenda et. al. (2013): page 310
#' @export
reZ <- function(nDomains = 10, nTime = 10) {
  #Construct Z-Matrix for mixed model
  I1 <- diag(1, nrow = nDomains)
  I2 <- rep(1, nTime)
  Z1 <- I1 %x% I2
  Z2 <- diag(1, nrow = nDomains * nTime)
  cbind(Z1, Z2)
}

#' reZ1
#' 
#' @description Marhuenda et. al. (2013): page 310
#' @export
#' 
reZ1 <- function (nDomains = 10, nTime = 10) {
  I1 <- diag(1, nrow = nDomains)
  I2 <- rep(1, nTime)
  I1 %x% I2
}