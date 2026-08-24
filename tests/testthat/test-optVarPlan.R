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

test_that("unknown-theta Beta plans support all methods", {
  plans <- lapply(
    c("delta_mle", "delta_mom", "gk_adjustment"),
    function(method) {
      optVarPlan(
        PRQ = 0.025, CRQ = 0.10, alpha = 0.05, beta = 0.10,
        distribution = "beta", theta_type = "unknown", theta = 6.6e8,
        LSL = 5.65e-6, method = method
      )
    }
  )

  expect_identical(vapply(plans, `[[`, character(1), "method"),
                   c("delta_mle", "delta_mom", "gk_adjustment"))
  for (plan in plans) {
    expect_true(is.finite(plan$n) && plan$n > 0)
    expect_true(is.finite(plan$k) && plan$k > 0)
    expect_true(is.finite(plan$PR) && is.finite(plan$CR))
    expect_lte(plan$PR, 0.055)
    expect_lte(plan$CR, 0.105)
  }
})

test_that("Delta-MLE is the default unknown-theta Beta method", {
  plan <- optVarPlan(
    PRQ = 0.025, CRQ = 0.10,
    distribution = "beta", theta_type = "unknown", theta = 6.6e8,
    LSL = 5.65e-6
  )
  expect_identical(plan$method, "delta_mle")
})

test_that("Delta plan optimization supports an upper specification limit", {
  plan <- optVarPlan(
    PRQ = 0.01, CRQ = 0.05, alpha = 0.05, beta = 0.10,
    distribution = "beta", theta_type = "unknown", theta = 300,
    USL = 0.05, method = "delta_mle"
  )

  expect_lte(plan$PR, 0.0505)
  expect_lte(plan$CR, 0.1005)
})

test_that("method is rejected outside unknown-theta Beta plans", {
  expect_error(
    optVarPlan(0.01, 0.05, distribution = "normal", method = "delta_mle"),
    "only applicable"
  )
  expect_error(
    optVarPlan(0.01, 0.05, distribution = "beta", theta = 100, USL = 0.1,
               method = "delta_mle"),
    "only applicable"
  )
})
