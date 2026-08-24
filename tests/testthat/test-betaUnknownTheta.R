test_that("unknown-theta method validation recognizes supported methods", {
  normalize <- AccSamplingDesign:::.normalize_beta_theta_method

  expect_identical(
    normalize(NULL, "beta", "unknown", method_missing = TRUE),
    "delta_mle"
  )
  expect_identical(
    normalize("delta_mom", "beta", "unknown"),
    "delta_mom"
  )
  expect_identical(
    normalize("gk_adjustment", "beta", "unknown"),
    "gk_adjustment"
  )
  expect_error(
    normalize("unsupported", "beta", "unknown"),
    "arg.*one of"
  )
})

test_that("unknown-theta method is rejected when it is not applicable", {
  normalize <- AccSamplingDesign:::.normalize_beta_theta_method

  expect_null(normalize(NULL, "normal", "unknown", method_missing = TRUE))
  expect_null(normalize(NULL, "beta", "known", method_missing = TRUE))
  expect_error(
    normalize("delta_mle", "normal", "unknown"),
    "only applicable"
  )
  expect_error(
    normalize("delta_mle", "beta", "known"),
    "only applicable"
  )
})

test_that("Beta raw moments match closed-form reference values", {
  moments <- AccSamplingDesign:::.beta_raw_moments(0.25, 20, 4)

  # For Beta(5, 15), E[Y^r] = (5)_r / (20)_r.
  expected <- c(
    5 / 20,
    (5 * 6) / (20 * 21),
    (5 * 6 * 7) / (20 * 21 * 22),
    (5 * 6 * 7 * 8) / (20 * 21 * 22 * 23)
  )
  expect_equal(moments, expected, tolerance = 1e-14)
})

test_that("analytical MoM covariance is finite and symmetric", {
  covariance <- AccSamplingDesign:::.beta_mom_covariance(0.25, 20)

  # Frozen from direct raw-moment propagation for Beta(5, 15).
  expected <- matrix(
    c(0.00892857142857143, -0.454545454545456,
      -0.454545454545456, 834.466403162061),
    nrow = 2,
    byrow = TRUE
  )
  expect_equal(covariance, expected, tolerance = 1e-10)
  expect_equal(covariance, t(covariance), tolerance = 1e-14)
  expect_true(all(eigen(covariance, symmetric = TRUE)$values >= -1e-10))
})

test_that("Beta moment helpers reject invalid parameters", {
  raw_moments <- AccSamplingDesign:::.beta_raw_moments
  mom_covariance <- AccSamplingDesign:::.beta_mom_covariance

  expect_error(raw_moments(0, 20), "mu")
  expect_error(raw_moments(0.5, -1), "theta")
  expect_error(raw_moments(0.5, 20, 0), "max_order")
  expect_error(mom_covariance(1, 20), "mu")
})
