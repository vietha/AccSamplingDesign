test_that("manual Delta-MLE plans reject undefined or invalid inspection parameters", {
  make_plan <- function(n = 45, k = 2.2) manualPlan(
    distribution = "beta", n = n, k = k, theta = 300,
    theta_type = "unknown", USL = 0.05
  )
  for (n in list(1, 1.9, 0, -1, NA_real_, NaN, Inf, numeric(), c(2, 3), "2", 2+1i)) {
    expect_error(make_plan(n = n), "Delta-MLE.*n >= 2")
  }
  for (k in list(-1, NA_real_, NaN, Inf, numeric(), c(1, 2), "1", 1+1i)) {
    expect_error(make_plan(k = k), "Delta-MLE.*constant k")
  }

  expect_identical(make_plan(n = 2, k = 0)$method, "delta_mle")
  expect_true(is.finite(accProb(make_plan(n = 2, k = 0), 0.01)))
  # Step 1 retains the continuous-size reporting convention; rounding is
  # intentionally not changed in this safety patch.
  continuous <- make_plan(n = 2.5)
  expect_equal(continuous$n, 2.5)
  expect_equal(continuous$sample_size, 3)

  known <- manualPlan(distribution = "beta", n = 1, k = 0,
                      theta = 300, theta_type = "known", USL = 0.05)
  expect_true(is.finite(accProb(known, 0.01)))
})

test_that("Delta-MLE probability evaluation rejects damaged and legacy invalid plans", {
  valid <- manualPlan(distribution = "beta", n = 45, k = 2.2,
                      theta = 300, theta_type = "unknown", USL = 0.05)
  for (n in list(1, 1.9, NA_real_, Inf, "2", 2+1i, c(2, 3))) {
    damaged <- valid
    damaged$n <- n
    expect_error(accProb(damaged, 0.01), "n")
    damaged$method <- NULL # Missing method metadata still means Delta-MLE.
    expect_error(accProb(damaged, 0.01), "n")
  }
  for (k in list(-1, NA_real_, Inf)) {
    damaged <- valid
    damaged$k <- k
    expect_error(accProb(damaged, 0.01), "constant k")
  }

  legacy <- valid
  legacy$m <- 1 # The old m field overrides n in accProb().
  expect_error(accProb(legacy, 0.01), "n >= 2")
  legacy$m <- 45
  legacy$n <- 1
  expect_equal(accProb(legacy, 0.01), accProb(valid, 0.01))
})

test_that("the reported n=1 regression cannot return an invalid Delta-MLE plan", {
  limit <- qbeta(0.01, 0.3, 3)
  for (upper in c(FALSE, TRUE)) {
    args <- list(PRQ = 0.01, CRQ = 0.03, distribution = "beta",
                 theta = 3.3, theta_type = "unknown", method = "delta_mle")
    args[[if (upper) "USL" else "LSL"]] <- if (upper) 1-limit else limit
    plan <- tryCatch(do.call(optPlan, args), error = identity)
    if (inherits(plan, "error")) {
      expect_match(conditionMessage(plan), "Delta-MLE.*analytically feasible|refinement failed")
    } else {
      expect_true(is.finite(plan$n) && plan$n >= 2)
      expect_true(is.finite(plan$k) && plan$k >= 0)
      expect_lte(1-accProb(plan, args$PRQ), 0.05+1e-6)
      expect_lte(accProb(plan, args$CRQ), 0.10+1e-6)
    }
  }
})

test_that("invalid finite or nonfinite optimizer results cannot escape as MLE plans", {
  check_parameters <- function(par, pattern) {
    local_mocked_bindings(
      optim = function(...) list(par = par, convergence = 0L),
      .package = "AccSamplingDesign"
    )
    expect_error(
      optVarPlan(0.01, 0.03, distribution = "beta", theta = 500,
                 theta_type = "unknown", USL = 0.05, method = "delta_mle"),
      pattern
    )
  }
  check_parameters(c(1, 1), "n >= 2")
  check_parameters(c(Inf, 1), "finite sample size")
  check_parameters(c(30, NA_real_), "finite non-negative")
  check_parameters(c(30, -1), "finite non-negative")
})

test_that("converged but infeasible MLE refinement is rejected", {
  local_mocked_bindings(
    optim = function(par, fn, method, ...) {
      if (method == "L-BFGS-B") {
        list(par = c(30, 1), convergence = 0L)
      } else {
        # These probes must not evaluate the OC at n < 2 or pass Inf to it.
        for (eta in c(-1000, -100, 0, 1000)) {
          expect_true(is.finite(fn(c(eta, 0))))
        }
        # n=30, k=1 is finite but does not meet the consumer-risk target.
        list(par = c(log(28), 0), convergence = 0L)
      }
    },
    .package = "AccSamplingDesign"
  )
  expect_error(
    optVarPlan(0.01, 0.03, distribution = "beta", theta = 500,
               theta_type = "unknown", USL = 0.05, method = "delta_mle"),
    "analytically feasible.*No plan was returned"
  )
})

test_that("overflow when decoding MLE refinement cannot return a plan", {
  for (bad in list(c(1000, 0), c(0, 1000))) {
    check_decode <- function(bad) {
      local_mocked_bindings(
        optim = function(par, fn, method, ...) list(
          par = if (method == "L-BFGS-B") c(30, 1) else bad,
          convergence = 0L
        ),
        .package = "AccSamplingDesign"
      )
      expect_error(
        optVarPlan(0.01, 0.03, distribution = "beta", theta = 500,
                   theta_type = "unknown", USL = 0.05, method = "delta_mle"),
        "Delta-MLE requires one finite"
      )
    }
    check_decode(bad)
  }
})
