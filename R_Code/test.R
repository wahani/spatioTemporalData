library(spatioTemporalData)
# library(devtools)
# install_github("parallelTools", username = "wahani", subdir = "package")
# install_github("SAE", username = "wahani", subdir = "package")
library(SAE)

output <- simRunContamination(10, 3, c(0.5), c(0.5), n = 200, spatioTemporalMessup=T,
                              spatialCont = list(sigma = 1, sigmaCont = 9, nDomainsCont = 2),
                              temporalCont = list(sigma = 1, sigmaCont = 9, nDomainsCont = 3))

(output[[1]]@data[[1]])


fitSTREBLUP(formula=y~x, dat=output@data[[1]], beta=c(0,1), sigma=c(1,1), rho=c(0.5,0.5))
fitSTEBLUP(formula=y~x, dat=output@data[[1]], beta=c(0,1), sigma=c(1,1), rho=c(0.5,0.5))[-1]

output <- simRunSetup(setup)[[1]]


setup <- simSetupMarhuenda(nDomains=100, nTime=20, sarCorr=0.5, arCorr=0.5, n = 50)
output <- simRunSetup(setup)


library(testthat)
test_package("spatioTemporalData")



