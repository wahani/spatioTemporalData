test_that("Simulation Marhuenda still works", {
  set.seed(1)
  setup <- simSetupMarhuenda(30, 5, 0.5, 0.5, n = 2)
  output <- simRunSetup(setup=setup)
  expect_that(slot(output[[1]], "data"), equals(testSimMarhuendaOutput()))
})