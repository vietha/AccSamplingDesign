test_that("manualPlan stores and applies unknown-theta Beta methods", {
  plans <- lapply(
    c("delta_mle", "delta_mom", "gk_adjustment"),
    function(method) manualPlan(
      distribution = "beta", n = 45, k = 2.2,
      theta = 300, theta_type = "unknown", USL = 0.05,
      method = method
    )
  )

  expect_identical(vapply(plans, `[[`, character(1), "method"),
                   c("delta_mle", "delta_mom", "gk_adjustment"))
  expect_true(all(vapply(plans, function(plan) {
    probability <- accProb(plan, 0.01)
    is.finite(probability) && probability >= 0 && probability <= 1
  }, logical(1))))
})

test_that("manualPlan defaults unknown-theta Beta plans to Delta-MLE", {
  plan <- manualPlan(
    distribution = "beta", n = 45, k = 2.2,
    theta = 300, theta_type = "unknown", USL = 0.05
  )
  expect_identical(plan$method, "delta_mle")
})

test_that("manualPlan rejects method outside unknown-theta Beta plans", {
  expect_error(
    manualPlan(distribution = "normal", n = 10, k = 1,
               method = "delta_mle"),
    "only applicable"
  )
  expect_error(
    manualPlan(distribution = "beta", n = 10, k = 1, theta = 100,
               USL = 0.1, method = "delta_mle"),
    "only applicable"
  )
  expect_error(
    manualPlan(distribution = "binomial", n = 10, c = 1,
               method = "delta_mle"),
    "only applicable"
  )
})
