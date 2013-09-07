\name{simRunContamination}
\alias{simRunContamination}
\title{simRunContamination}
\usage{
  simRunContamination(nDomains, nTime, sarCorr, arCorr,
    seVar = seSigmaClosure1,
    spatialCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
    temporalCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
    beta = c(0, 1), spatioTemporalMessup = TRUE, n = 200,
    scenarioName = "")
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
  scalar. #}

  \item{seVar}{Function which returns the contaminated
  variances.}

  \item{spatialCont}{list. Settings for spatial
  contaminated Data - see defaults}

  \item{temporalCont}{list. Settings for temporal
  contaminated Data - see defaults}
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
output <- simRunContamination(100, 10, c(0, 0.5), c(0, 0.5), n = 200, spatioTemporalMessup=F,
                             spatialCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 0),
                             temporalCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2))
}

