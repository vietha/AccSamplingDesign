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

test_that("optAttrPlan returns valid binomial plan", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  
  expect_true(is.list(plan))
  expect_true(all(c("n", "c") %in% names(plan)))
  expect_gt(plan$n, 0)
  expect_gte(plan$c, 0)
})

test_that("optAttrPlan returns valid poisson plan", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "poisson")
  
  expect_true(is.list(plan))
  expect_true(all(c("n", "c") %in% names(plan)))
  expect_gt(plan$n, 0)
  expect_gte(plan$c, 0)
})

test_that("optAttrPlan rejects invalid alpha or beta", {
  expect_error(optAttrPlan(PRQ = 0.01, CRQ = 0.05, alpha = -0.1))
  expect_error(optAttrPlan(PRQ = 0.01, CRQ = 0.05, beta = 1.1))
})

# test_that("optAttrPlan dispatches binomial to find_plan_binomial", {
#   mock <- mockery::mock(list(n = 10, c = 1))
#   mockery::stub(optAttrPlan, "find_plan_binomial", mock)
#   
#   optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
#   mockery::expect_called(mock, 1)
# })

test_that("optAttrPlan plan meets risk constraints (binomial)", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  
  # Consumer's risk: P(accept at CRQ) ≤ beta
  pa_crq <- pbinom(plan$c, plan$n, 0.05)
  expect_lte(pa_crq, 0.10)
  
  # Producer's risk: P(accept at PRQ) ≥ 1 - alpha
  pa_prq <- pbinom(plan$c, plan$n, 0.01)
  expect_gte(pa_prq, 0.95)
})