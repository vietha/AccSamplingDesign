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

test_that("analytical MLE covariance inverts Beta Fisher information", {
  information <- AccSamplingDesign:::.beta_mle_information(0.25, 20)
  covariance <- AccSamplingDesign:::.beta_mle_covariance(0.25, 20)

  expect_equal(information, t(information), tolerance = 1e-14)
  expect_equal(covariance, t(covariance), tolerance = 1e-14)
  expect_equal(information %*% covariance, diag(2), tolerance = 1e-10)
  expect_true(all(eigen(covariance, symmetric = TRUE)$values > 0))
})

test_that("analytical MLE covariance matches a frozen reference", {
  covariance <- AccSamplingDesign:::.beta_mle_covariance(0.25, 20)

  # Inverse of the expected (mu, theta) Fisher information for Beta(5, 15).
  expected <- matrix(
    c(0.00891453953147548, -0.482730372568412,
      -0.482730372568412, 772.623438103779),
    nrow = 2,
    byrow = TRUE
  )
  expect_equal(covariance, expected, tolerance = 1e-6)
})

test_that("Beta MLE covariance rejects invalid parameters", {
  mle_information <- AccSamplingDesign:::.beta_mle_information
  mle_covariance <- AccSamplingDesign:::.beta_mle_covariance

  expect_error(mle_information(NA_real_, 20), "mu")
  expect_error(mle_information(0.5, 0), "theta")
  expect_error(mle_covariance(1, 20), "mu")
})

test_that("Delta decision gradients agree with finite differences", {
  statistic <- AccSamplingDesign:::.beta_delta_statistic
  gradient <- AccSamplingDesign:::.beta_delta_gradient
  mu <- 0.25
  theta <- 20
  k <- 1.4
  step <- 1e-6

  for (limit_type in c("upper", "lower")) {
    analytical <- gradient(mu, theta, k, limit_type)
    numerical_mu <- (statistic(mu + step, theta, k, limit_type) -
      statistic(mu - step, theta, k, limit_type)) / (2 * step)
    numerical_theta <- (statistic(mu, theta + step, k, limit_type) -
      statistic(mu, theta - step, k, limit_type)) / (2 * step)

    expect_equal(unname(analytical[["mu"]]), numerical_mu, tolerance = 1e-7)
    expect_equal(
      unname(analytical[["theta"]]), numerical_theta, tolerance = 1e-7
    )
  }
})

test_that("Delta acceptance probabilities match frozen references", {
  delta_pa <- AccSamplingDesign:::.beta_delta_acceptance_probability

  expect_equal(
    delta_pa(0.03, 300, 45, 2.2, 0.05, "upper", "delta_mle"),
    0.312756442044726,
    tolerance = 1e-6
  )
  expect_equal(
    delta_pa(0.08, 100, 30, 1.5, 0.05, "lower", "delta_mom"),
    0.0404806618576654,
    tolerance = 1e-6
  )
})

test_that("Delta acceptance probability validates numerical inputs", {
  delta_pa <- AccSamplingDesign:::.beta_delta_acceptance_probability

  expect_error(delta_pa(0.3, 20, 0, 1, 0.5, "upper", "delta_mle"), "n")
  expect_error(delta_pa(0.3, 20, 10, -1, 0.5, "upper", "delta_mle"), "k")
  expect_error(delta_pa(0.3, 20, 10, 1, 1, "upper", "delta_mle"), "limit")
  expect_error(delta_pa(0.3, 20, 10, 1, 0.5, "upper", "bad"), "arg")
})
