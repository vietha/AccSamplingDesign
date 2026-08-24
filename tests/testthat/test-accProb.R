## -----------------------------------------------------------------------------
## test-accProb.R
##
## Author: Ha Truong
## Created: 12 Nov 2025
## Purpose: Test accProb functions
## -----------------------------------------------------------------------------

# Attributes Sampling Plan
## -----------------------------------------------------------------------------
test_that("accProb.AttrPlan works for binomial", {
  plan <- list(
    distribution = "binomial",
    n = 20,
    c = 1
  )
  
  p <- 0.05
  expect_equal(
    accProb.AttrPlan(plan, p),
    pbinom(plan$c, plan$n, p)
  )
})

test_that("accProb.AttrPlan works for poisson", {
  plan <- list(
    distribution = "poisson",
    n = 20,
    c = 1
  )
  
  p <- 0.05
  expect_equal(
    accProb.AttrPlan(plan, p),
    ppois(plan$c, plan$n * p)
  )
})

test_that("accProb.AttrPlan errors on unknown distribution", {
  plan <- list(distribution = "xxx")
  expect_error(accProb.AttrPlan(plan, 0.1), "Unknown distribution")
})

# Attribute plans - extreme p
test_that("accProb.AttrPlan handles extreme p", {
  plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  
  expect_equal(accProb(plan, 0), 1)   # No defectives, always accept
  expect_equal(accProb(plan, 1), 0)   # All defective, never accept
  
  plan_pois <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "poisson")
  expect_equal(accProb(plan_pois, 0), 1)
})



# Variables Sampling Plan (Normal dist)
## -----------------------------------------------------------------------------

test_that("accProb.VarPlan works for normal (sigma known)", {
  plan <- list(
    distribution = "normal",
    n = 10,
    k = 1.5,
    sigma_type = "known"
  )
  
  p <- 0.05
  pa <- accProb.VarPlan(plan, p)
  
  expect_type(pa, "double")
  expect_true(pa >= 0 && pa <= 1)
})

test_that("accProb.VarPlan works for normal (sigma unknown)", {
  plan <- list(
    distribution = "normal",
    n = 10,
    k = 1.5,
    sigma_type = "unknown"
  )
  
  p <- 0.05
  pa <- accProb.VarPlan(plan, p)
  
  expect_type(pa, "double")
  expect_true(pa >= 0 && pa <= 1)
})


# Variables Sampling Plan (Beta dist)
## -----------------------------------------------------------------------------
test_that("accProb.VarPlan works for Beta upper limit", {
  plan <- list(
    distribution = "beta",
    m = 50,
    k = 1.5,
    theta = 100,
    theta_type = "unknown",
    USL = 0.10,
    n = 10
  )
  
  p <- 0.05
  pa <- accProb.VarPlan(plan, p)
  
  expect_type(pa, "double")
  expect_true(pa >= 0 && pa <= 1)
})

test_that("accProb.VarPlan works for Beta lower limit", {
  plan <- list(
    distribution = "beta",
    m = 50,
    k = 1.5,
    theta = 100,
    theta_type = "unknown",
    LSL = 0.10,
    n = 10
  )
  
  p <- 0.05
  pa <- accProb.VarPlan(plan, p)
  
  expect_type(pa, "double")
  expect_true(pa >= 0 && pa <= 1)
})

test_that("unknown-theta Beta acceptance probability dispatches by method", {
  base_plan <- list(
    distribution = "beta", n = 45, k = 2.2, theta = 300,
    theta_type = "unknown", USL = 0.05
  )

  probabilities <- vapply(
    c("delta_mle", "delta_mom", "gk_adjustment"),
    function(method) accProb.VarPlan(c(base_plan, list(method = method)), 0.01),
    numeric(1)
  )
  expect_true(all(is.finite(probabilities)))
  expect_true(all(probabilities >= 0 & probabilities <= 1))
  expect_equal(length(unique(round(probabilities, 8))), 3)
})

test_that("missing unknown-theta method uses Delta-MLE", {
  legacy_plan <- list(
    distribution = "beta", n = 45, k = 2.2, theta = 300,
    theta_type = "unknown", USL = 0.05
  )
  explicit_plan <- c(legacy_plan, list(method = "delta_mle"))

  expect_equal(
    accProb.VarPlan(legacy_plan, 0.01),
    accProb.VarPlan(explicit_plan, 0.01)
  )
})

test_that("Govindaraju-Kissling adjustment reproduces the 0.0.8 calculation", {
  plan <- list(
    distribution = "beta", n = 45, k = 2.2, theta = 300,
    theta_type = "unknown", method = "gk_adjustment", USL = 0.05
  )
  mu <- muEst(0.01, USL = plan$USL, theta = plan$theta, dist = "beta")
  effective_n <- plan$n / (1 + 0.85 * plan$k^2)
  shape1 <- effective_n * mu * plan$theta
  shape2 <- effective_n * (1 - mu) * plan$theta
  discriminant <- (2 * plan$theta * plan$USL + plan$k^2)^2 -
    4 * (plan$theta + plan$k^2) * plan$theta * plan$USL^2
  lower_root <- (2 * plan$theta * plan$USL + plan$k^2 -
    sqrt(discriminant)) / (2 * (plan$theta + plan$k^2))

  expect_equal(
    accProb.VarPlan(plan, 0.01),
    pbeta(lower_root, shape1, shape2),
    tolerance = 1e-12
  )
})

test_that("unknown-theta Beta acceptance rejects invalid method metadata", {
  plan <- list(
    distribution = "beta", n = 45, k = 2.2, theta = 300,
    theta_type = "unknown", method = "bad", USL = 0.05
  )
  expect_error(accProb.VarPlan(plan, 0.01), "arg.*one of")
})

test_that("accProb.VarPlan errors when no limit provided in Beta model", {
  plan <- list(
    distribution = "beta",
    m = 50,
    k = 1.5,
    theta = 100,
    n = 10
  )
  expect_error(accProb.VarPlan(plan, 0.05))
})

test_that("accProb.VarPlan errors on unknown distribution", {
  plan <- list(
    distribution = "xxx",
    n = 10
  )
  expect_error(accProb.VarPlan(plan, 0.1))
})

# Variable plans - edge limits
test_that("accProb.VarPlan works when pd at boundary", {
  plan_norm <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                          distribution = "normal", sigma_type = "known", sigma = 1)
  
  expect_true(accProb(plan_norm, 0) >= 0 & accProb(plan_norm, 0) <= 1)
  expect_true(accProb(plan_norm, 1) >= 0 & accProb(plan_norm, 1) <= 1)
})
