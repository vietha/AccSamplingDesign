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

test_that("Normal plan and known sigma - creates valid parameters", {
  plan <- optVarPlan(PRQ = 0.005, CRQ = 0.03, alpha = 0.05, beta = 0.10, 
                     distribution = "normal", sigma_type = "known")
  expect_gt(plan$k, 0)
  expect_gt(plan$n, 0)
})

test_that("Normal plan and unknown sigma - creates valid parameters", {
  plan <- optVarPlan(PRQ = 0.005, CRQ = 0.03, alpha = 0.05, beta = 0.10, 
                     distribution = "normal", sigma_type = "unknown")
  expect_gt(plan$k, 0)
  expect_gte(plan$n, 2)
})