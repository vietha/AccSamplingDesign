test_that("VarPlan print and summary report the unknown-theta method", {
  plan <- manualPlan(
    distribution = "beta", n = 45, k = 2.2,
    theta = 300, theta_type = "unknown", USL = 0.05,
    method = "delta_mom"
  )

  expect_output(print(plan), "Unknown-theta method: Delta-MoM", fixed = TRUE)
  expect_output(summary(plan), "Unknown-theta method: Delta-MoM", fixed = TRUE)
})

test_that("VarPlan output gives missing method metadata the new default label", {
  plan <- structure(
    list(
      distribution = "beta", n = 45, sample_size = 45, k = 2.2,
      theta = 300, theta_type = "unknown", USL = 0.05,
      PR = 0.05, CR = 0.10, PRQ = 0.01, CRQ = 0.05
    ),
    class = "VarPlan"
  )

  expect_output(print(plan), "Delta-MLE", fixed = TRUE)
  expect_output(summary(plan), "Delta-MLE", fixed = TRUE)
})

test_that("known-theta plan output does not report an unknown-theta method", {
  plan <- manualPlan(
    distribution = "beta", n = 20, k = 1.5,
    theta = 100, theta_type = "known", USL = 0.1
  )

  print_output <- capture.output(print(plan))
  summary_output <- capture.output(summary(plan))
  expect_false(any(grepl("Unknown-theta method", print_output, fixed = TRUE)))
  expect_false(any(grepl("Unknown-theta method", summary_output, fixed = TRUE)))
})
