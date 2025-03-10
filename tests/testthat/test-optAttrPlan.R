## -----------------------------------------------------------------------------
## test-optAttrPlan.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Test attribute plan calculations
##
## Changelogs:
## -----------------------------------------------------------------------------

test_that("Attribute plan meets risk requirements", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  expect_lte(pbinom(plan$c, plan$n, 0.05), 0.10)  # Verify beta
  expect_gte(pbinom(plan$c, plan$n, 0.01), 0.95)  # Verify 1-alpha
})