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
