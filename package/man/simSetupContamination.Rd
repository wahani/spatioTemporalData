\name{simSetupContamination}
\alias{simSetupContamination}
\title{simSetupContamination}
\usage{
  simSetupContamination(nDomains, nTime, sarCorr, arCorr,
    seVar = seSigmaContClosure(nDomains, nTime, nDomainsCont = 1, contFactor = 2, randomDomains = FALSE),
    n = 200)
}
\arguments{
  \item{nDomains}{numeric. Number of domains, can be a
  vector.}

  \item{nTime}{numeric. Number of time periods, can be a
  vector.}

  \item{sarCorr}{numeric. Autocorrelation for spatial
  process, can be a vector.}

  \item{arCorr}{numeric. Autocorrelation for AR(1) process,
  can be a vector.}

  \item{n}{numeric. Number of data to be simulated, is a
  scalar.}

  \item{seVar}{Function which returns the contaminated
  variances.}
}
\value{
  an object of class simSetup. Needs no further
  modification.
}
\description{
  Setup the properties of data to be simulated. Based on
  the simulation in Marhuenda (2013) with the possibility
  to add contaminated data.
}
\examples{
setup <- simSetupContamination(nDomains=100, nTime=20, sarCorr=0.5, arCorr=0.5, n = 50)
output <- simRun(setup)
}
