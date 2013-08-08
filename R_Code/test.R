library(spatioTemporalData)
# library(devtools)
# install_github("SAE", username = "wahani", subdir = "package")
library(SAE)

nDomains <- 30
nTime <- 5

setup <- simSetupContamination(nDomains, nTime, 0.5, 0.5, n = 2, 
                               seVar=seSigmaContClosure(nDomains, nTime, nDomainsCont=2, contFactor=20))


output <- simRunSetup(setup)[[1]]

slot(output, "sigma")

test_package("spatioTemporalData")