test_that("contaminateSigma", {
  nDomains <- 10
  nTime <- 2
  contFactor <- 2
  sigmas <- seSigmaClosure(nDomains, nTime)()
  contSigmas <- sigmas
  contSigmas[(nTime*nDomains-nTime+1):(nTime*nDomains)] <- sigmas[(nTime*nDomains-nTime+1):(nTime*nDomains)] * contFactor
  expect_that(seSigmaContClosure(nDomains, nTime, nDomainsCont = 1, contFactor, randomDomains = FALSE)(),
              equals(contSigmas))
  expect_that(seSigmaContClosure(nDomains, nTime, nDomainsCont = 0, contFactor, randomDomains = FALSE)(),
              equals(sigmas))
})