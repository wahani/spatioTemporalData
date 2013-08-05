# General Purpose Functions - Helper

getNumberOfCores <- function() {
  # How many cores are available. For Windows always 1.
  if (.Platform$OS.type == "windows") {
    return(1)
  } else {
    return(detectCores())
  }
}

#' w0Matrix
#' 
#' @description Creates a neighbourhood matrix as in Marhuenda 2013, ie. tridiagonal matrix. Inspired by package SoDA
#' @export
w0Matrix <- function(nDomains) {
  # Creates a neighbourhood matrix as in Marhuenda 2013, ie. tridiagonal matrix
  # Inspired by package SoDA
  w0Matrix <- diag(0, nrow = nDomains, ncol = nDomains)
  R <- row(w0Matrix)
  C <- col(w0Matrix)
  w0Matrix[C == R + 1] <- 1
  w0Matrix[C == R - 1] <- 1
  w0Matrix
}

#' row standardized neighbourhood matrix
#' 
#' @description Constructs a row standardized neighbourhood matrix as in Marhuenda (2013)
#' @export
wMatrix <- function(nDomains) {
  w0 <- w0Matrix(nDomains=nDomains)
  w0 / rowSums(w0)
}

#' Closure for a counter
#' @export
counterClosure <- function() {
  i <- 0
  function() {
    i <<- i + 1
    return(i)
  }
}