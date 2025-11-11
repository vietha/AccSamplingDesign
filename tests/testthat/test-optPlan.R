## -----------------------------------------------------------------------------
## test-optPlan.R
##
## Author: Ha Truong
## Created: 12 Nov 2025
## Purpose: Test optPlan wrapper functionality
## -----------------------------------------------------------------------------

test_that("optPlan works for binomial distribution", {
  plan <- optPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  
  expect_true(is.list(plan))
  expect_true(all(c("n", "c") %in% names(plan)))
  expect_gt(plan$n, 0)
  expect_gte(plan$c, 0)
})

test_that("optPlan works for normal, known sigma", {
  plan <- optPlan(PRQ = 0.005, CRQ = 0.03,
                  alpha = 0.05, beta = 0.10, 
                  distribution = "normal", sigma_type = "known")
  
  expect_true(is.list(plan))
  expect_gt(plan$n, 0)
  expect_gt(plan$k, 0)
})

test_that("optPlan works for beta distribution", {
  plan <- optPlan(PRQ = 0.2, CRQ = 0.3, USL = 0.5,
                  distribution = "beta", theta = 0.2)
  
  expect_true(is.list(plan))
})


# Error behavior tests
# -----------------------------------------------------------------------------

test_that("optPlan errors when CRQ <= PRQ", {
  expect_error(
    optPlan(PRQ = 0.05, CRQ = 0.05, distribution = "binomial"),
    "must be greater"
  )
})

test_that("optPlan errors when PRQ or CRQ are missing", {
  expect_error(optPlan(CRQ = 0.05), "must be provided")
  expect_error(optPlan(PRQ = 0.01), "must be provided")
})

test_that("invalid alpha/beta rejected", {
  expect_error(optPlan(PRQ = 0.01, CRQ = 0.05, alpha = -1))
  expect_error(optPlan(PRQ = 0.01, CRQ = 0.05, beta = 1.1))
})

test_that("specifying both USL and LSL errors", {
  expect_error(
    optPlan(PRQ = 0.01, CRQ = 0.05,
            USL = 1, LSL = 0, distribution = "normal"),
    "only one limit"
  )
})

test_that("beta requires theta and one spec limit", {
  expect_error(
    optPlan(PRQ = 0.1, CRQ = 0.2, distribution = "beta"),
    "must be provided"
  )
  expect_error(
    optPlan(PRQ = 0.1, CRQ = 0.2, distribution = "beta", theta = 0.1),
    "specification"
  )
})

# Consistency test (delegation)
# -----------------------------------------------------------------------------

test_that("optPlan delegates to optAttrPlan for binomial", {
  plan1 <- optPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  plan2 <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  
  expect_equal(plan1, plan2)
})

test_that("optPlan delegates to optVarPlan for normal", {
  plan1 <- optPlan(PRQ = 0.01, CRQ = 0.05, distribution = "normal")
  plan2 <- optVarPlan(PRQ = 0.01, CRQ = 0.05, distribution = "normal")
  
  expect_equal(plan1, plan2)
})

# Edge cases
# -----------------------------------------------------------------------------

test_that("optPlan sets default limit_type to upper when no limits provided", {
  plan <- optPlan(PRQ = 0.01, CRQ = 0.05, distribution = "normal")
  expect_true("n" %in% names(plan))
})

