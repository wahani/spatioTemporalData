library(spatioTemporalData)
library(devtools)
install_github("parallelTools", username = "wahani", subdir = "package")
install_github("SAE", username = "wahani", subdir = "package")
library(SAE)

setup <- simSetupContamination(40, 10, 0.5, 0.5, n = 2, spatioTemporalMessup=TRUE)

output <- simRunSetup(setup)[[1]]

fitSTREBLUP(formula=y~x, dat=output@data[[1]], beta=c(0,1), sigma=c(1,1), rho=c(0.5,0.5))

output <- simRunSetup(setup)[[1]]

seSigmaClosure

slot(output, "sigma")

test_package("spatioTemporalData")