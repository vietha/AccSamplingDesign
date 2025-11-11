## -----------------------------------------------------------------------------
## test-plots.R
##
## Author: Ha Truong
## Created: 12 Nov 2025
## Purpose: Test all plot functions
## -----------------------------------------------------------------------------



# Tests for plot.AttrPlan()
# -----------------------------------------------------------------------------

test_that("plot.AttrPlan runs without error", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  
  expect_silent({
    plot(plan)
  })
})

test_that("plot.AttrPlan runs with custom pd", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  pd <- seq(0, 0.1, length.out = 20)
  
  expect_silent({
    plot(plan, pd = pd)
  })
})


test_that("plot.AttrPlan produces a plot object", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  
  expect_silent({
    plot(plan)
  })
  
  p <- recordPlot()
  expect_true(inherits(p, "recordedplot"))
})


test_that("accProb agrees with binomial pbinom", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  p <- 0.03
  
  expect_equal(
    accProb(plan, p),
    pbinom(plan$c, plan$n, p),
    tolerance = 1e-6
  )
})


# Tests for plot.VarPlan()
# --------------------------------------------------------------------

test_that("plot.VarPlan (by='pd') runs without error", {
  plan <- optVarPlan(PRQ = 0.005, CRQ = 0.03,
                     alpha = 0.05, beta = 0.10,
                     distribution = "normal", sigma_type = "known")
  
  expect_silent({
    plot(plan, by = "pd")
  })
})

test_that("plot.VarPlan (by='pd') works with custom pd", {
  plan <- optVarPlan(PRQ = 0.005, CRQ = 0.03,
                     alpha = 0.05, beta = 0.10,
                     distribution = "normal", sigma_type = "known")
  
  pd <- seq(0, 0.1, length.out = 20)
  
  expect_silent({
    plot(plan, pd = pd, by = "pd")
  })
})

test_that("plot.VarPlan produces a recordedplot (by='pd')", {
  plan <- optVarPlan(PRQ = 0.005, CRQ = 0.03,
                     alpha = 0.05, beta = 0.10,
                     distribution = "normal", sigma_type = "known")
  
  plot(plan, by = "pd")
  p <- recordPlot()
  
  expect_true(inherits(p, "recordedplot"))
})

test_that("plot.VarPlan errors when sigma_type missing", {
  plan <- list(
    PRQ = 0.01, CRQ = 0.05,
    distribution = "normal",
    sigma = 1, USL = 10, LSL = 0
  )
  class(plan) <- "VarPlan"
  
  expect_error(plot(plan), "sigma_type")
})

test_that("plot.VarPlan errors when sigma missing", {
  plan <- list(PRQ = 0.01, CRQ = 0.05, distribution = "normal")
  class(plan) <- "VarPlan"
  
  expect_error(plot(plan), "sigma")
})


test_that("plot.VarPlan errors for invalid by argument", {
  plan <- optVarPlan(PRQ = 0.005, CRQ = 0.03,
                     alpha = 0.05, beta = 0.10,
                     distribution = "normal", sigma_type = "known",
                     USL = 0.05)
  
  expect_error(plot(plan, by="xyz"), "should be one of")
})
