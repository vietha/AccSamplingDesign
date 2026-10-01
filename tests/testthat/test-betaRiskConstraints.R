## -----------------------------------------------------------------------------
## test-betaRiskConstraints.R ---
##
## Author: Ha Truong
##
## Created: 02 Oct 2026
##
## Purposes: Known-theta Beta plans must satisfy both risk constraints at
## the delivered (rounded-up) sample size. Captures the user-reported cases
## where Pa(p1)/Pa(p2) missed the risk limits after rounding the sample
## size, plus regression guards for the untouched unknown-theta methods.
##
## Changelogs:
## -----------------------------------------------------------------------------

# The six user-reported cases: three moisture plans (upper limit, theta =
# 500) and three protein plans (lower limit, theta = 2e5), all with
# PRQ = 0.005, alpha = 0.05, beta = 0.10. Before the fix, the two tightest
# moisture plans violated both constraints at the rounded sample size; the
# remaining four passed only because rounding adds a small favorable margin
# to a raw plan that already violated the producer constraint.
.user_cases <- data.frame(
  name = c("Moisture", "Moisture", "Moisture",
           "Protein", "Protein", "Protein"),
  USL = c(0.05, 0.05, 0.05, NA, NA, NA),
  LSL = c(NA, NA, NA, 0.24, 0.24, 0.24),
  theta = c(500, 500, 500, 2e5, 2e5, 2e5),
  CRQ = c(0.01, 0.015, 0.025, 0.01, 0.015, 0.025),
  # Minimal feasible integer sample sizes: rounding up from the refined
  # binding solution lands exactly on these.
  expected_n = c(117, 45, 20, 138, 53, 23)
)

# Pa of the plan actually applied (sample size rounded up) — the quantity a
# practitioner recomputes from the printed plan.
.delivered_pa <- function(plan, p) {
  delivered <- plan
  delivered$n <- plan$sample_size
  accProb(delivered, p)
}

test_that("Known-theta Beta plans meet both risks at the delivered sample size", {
  for (i in seq_len(nrow(.user_cases))) {
    case <- .user_cases[i, ]
    plan <- optVarPlan(
      PRQ = 0.005, CRQ = case$CRQ, alpha = 0.05, beta = 0.10,
      distribution = "beta", theta_type = "known", theta = case$theta,
      USL = if (is.na(case$USL)) NULL else case$USL,
      LSL = if (is.na(case$LSL)) NULL else case$LSL
    )
    label <- paste(case$name, "CRQ =", case$CRQ)

    # Risks reported by the plan are those of the delivered size
    expect_lte(plan$PR, 0.05, label = paste(label, "reported PR"))
    expect_lte(plan$CR, 0.10, label = paste(label, "reported CR"))

    # Independent recomputation at the rounded sample size
    expect_gte(.delivered_pa(plan, 0.005), 0.95,
               label = paste(label, "Pa(p1) at rounded n"))
    expect_lte(.delivered_pa(plan, case$CRQ), 0.10,
               label = paste(label, "Pa(p2) at rounded n"))

    # Known-theta plans are delivered at an integer size: the plan's n is
    # the applied sample size, and it is the minimal feasible one.
    expect_equal(plan$n, plan$sample_size,
                 label = paste(label, "n equals delivered size"))
    expect_equal(plan$sample_size, case$expected_n,
                 label = paste(label, "minimal feasible n"))
  }
})

test_that("Known-theta constraints hold for a stricter risk pair", {
  # alpha = 0.01, beta = 0.05. Both configurations returned infeasible
  # plans before the fix (PR around 0.023 and CR around 0.056 at the
  # rounded size, against limits of 0.01 and 0.05).
  configs <- list(
    list(USL = 0.05, LSL = NULL, theta = 500),
    list(USL = NULL, LSL = 0.24, theta = 2e5)
  )
  for (cfg in configs) {
    plan <- optVarPlan(
      PRQ = 0.005, CRQ = 0.01, alpha = 0.01, beta = 0.05,
      distribution = "beta", theta_type = "known",
      USL = cfg$USL, LSL = cfg$LSL, theta = cfg$theta
    )
    label <- paste(if (is.null(cfg$USL)) "LSL" else "USL", cfg$theta)
    expect_lte(plan$PR, 0.01, label = paste(label, "reported PR"))
    expect_lte(plan$CR, 0.05, label = paste(label, "reported CR"))
    expect_gte(.delivered_pa(plan, 0.005), 0.99,
               label = paste(label, "Pa(p1) at rounded n"))
    expect_lte(.delivered_pa(plan, 0.01), 0.05,
               label = paste(label, "Pa(p2) at rounded n"))
  }
})

test_that("Unknown-theta Beta plans keep their pre-fix behavior", {
  # The known-theta fix must not leak into the unknown-theta methods: they
  # still refine to a continuous binding solution and report risks at the
  # raw sample size. Improving their delivered-size accuracy is a separate
  # change (the Govindaraju-Kissling plan below is known to still violate
  # at its rounded size; pinned here so any unintended change to that
  # behavior fails loudly).
  plan <- optVarPlan(
    PRQ = 0.005, CRQ = 0.01, alpha = 0.05, beta = 0.10,
    distribution = "beta", theta_type = "unknown", theta = 500,
    USL = 0.05, method = "delta_mle"
  )
  expect_equal(plan$sample_size, 431)
  expect_equal(plan$n, 430.468315, tolerance = 1e-4)
  expect_equal(plan$k, 2.841516, tolerance = 1e-4)
  expect_true(plan$n != plan$sample_size)  # raw-n semantics preserved

  gk <- optVarPlan(
    PRQ = 0.005, CRQ = 0.01, alpha = 0.05, beta = 0.10,
    distribution = "beta", theta_type = "unknown", theta = 500,
    USL = 0.05, method = "gk_adjustment"
  )
  expect_equal(gk$sample_size, 880)
  expect_equal(gk$k, 2.839136, tolerance = 1e-4)
})

test_that("accProb handles a zero acceptability constant for Beta plans", {
  # k = 0 accepts whenever the sample mean is inside the limit; the
  # quadratic then degenerates (discriminant = 0) and the computed
  # discriminant used to land a few ulps below zero, producing NaN.
  plan <- manualPlan(distribution = "beta", n = 5, k = 0,
                     USL = 0.05, theta = 500, theta_type = "known")
  Pa <- accProb(plan, 0.005)
  mu <- muEst(0.005, USL = 0.05, theta = 500, dist = "beta")
  expect_equal(Pa, pbeta(0.05, 5 * mu * 500, 5 * (1 - mu) * 500))
})