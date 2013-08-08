\name{simRunSetup}
\alias{simRunSetup}
\title{simRun}
\usage{
  simRunSetup(setup, mc = F)
}
\arguments{
  \item{setup}{is of class simSetup and best be supplied by
  simSetupMarhuenda.}

  \item{mc}{logical indicator if multicore functionality
  should be used. On windows, the statement is irrelevant.}
}
\value{
  Returns a list containing as many simSetup objects as
  possible scenarios are supplied by the input. The slot
  'data' will be filled with simulated data.
}
\description{
  Run simulation for given simSetup
}
\examples{
setup <- simSetupMarhuenda(nDomains=100, nTime=c(10, 20), sarCorr=0.5, arCorr=c(0, 0.5))
output <- simRun(setup)
length(slot(output[[1]], "data"))
dat <- slot(output[[1]], "data")[[1]]
summary(dat)
}

