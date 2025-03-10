## -----------------------------------------------------------------------------
## test-optVarPlan.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Test variable plan calculations
##
## Changelogs:
## -----------------------------------------------------------------------------

test_that("Normal plan creates valid parameters", {
  plan <- optVarPlan(PRQ = 0.01, CRQ = 0.1, spec_limit = 10,
                     distribution = "normal", sigma = 0.5)
  expect_gt(plan$k, 0)
  expect_gt(plan$n, 0)
})