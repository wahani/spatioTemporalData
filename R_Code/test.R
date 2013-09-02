library(spatioTemporalData)
# library(devtools)
# install_github("parallelTools", username = "wahani", subdir = "package")
# install_github("SAE", username = "wahani", subdir = "package")
library(SAE)

output <- simRunContamination(100, 10, c(0.5), c(0.5), n = 200, spatioTemporalMessup=F,
                              spatialCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2),
                              temporalCont = list(sigma = 1, sigmaCont = 10, nDomainsCont = 2))[[1]]


fitSTREBLUP(formula=y~x, dat=output@data[[1]], beta=c(0,1), sigma=c(1,1), rho=c(0.5,0.5))
fitSTEBLUP(formula=y~x, dat=output@data[[1]], beta=c(0,1), sigma=c(1,1), rho=c(0.5,0.5))[-1]

output <- simRunSetup(setup)[[1]]



test_package("spatioTemporalData")



