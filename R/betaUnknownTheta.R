## Internal methods for Beta variables plans with an estimated precision.
##
## These helpers are package-native implementations of the analytical methods.
## They intentionally have no dependency on research scripts, reports, or data.

.beta_theta_methods <- c("delta_mle", "delta_mom", "gk_adjustment")

.normalize_beta_theta_method <- function(method, distribution, theta_type,
                                         method_missing = FALSE) {
  applicable <- identical(distribution, "beta") &&
    identical(theta_type, "unknown")

  if (!applicable) {
    if (!method_missing) {
      stop(
        "method is only applicable when distribution = \"beta\" and ",
        "theta_type = \"unknown\".",
        call. = FALSE
      )
    }
    return(NULL)
  }

  if (method_missing || is.null(method)) {
    return(.beta_theta_methods[[1L]])
  }

  match.arg(method, .beta_theta_methods)
}

.validate_beta_parameters <- function(mu, theta) {
  if (length(mu) != 1L || !is.finite(mu) || mu <= 0 || mu >= 1) {
    stop("mu must be one finite value strictly between 0 and 1.", call. = FALSE)
  }
  if (length(theta) != 1L || !is.finite(theta) || theta <= 0) {
    stop("theta must be one finite positive value.", call. = FALSE)
  }
}

.beta_raw_moments <- function(mu, theta, max_order = 4L) {
  .validate_beta_parameters(mu, theta)
  if (length(max_order) != 1L || max_order < 1L || max_order != as.integer(max_order)) {
    stop("max_order must be a positive integer.", call. = FALSE)
  }

  alpha <- mu * theta
  vapply(seq_len(max_order), function(order) {
    offsets <- seq.int(0, order - 1L)
    prod((alpha + offsets) / (theta + offsets))
  }, numeric(1L))
}

.beta_mom_covariance <- function(mu, theta) {
  # Return the per-observation asymptotic covariance Sigma for (mu_hat,
  # theta_hat). The covariance for a sample of size n is Sigma / n.
  moments <- .beta_raw_moments(mu, theta, max_order = 4L)
  m1 <- moments[[1L]]
  m2 <- moments[[2L]]
  variance <- m2 - m1^2

  moment_covariance <- matrix(
    c(
      variance,
      moments[[3L]] - m1 * m2,
      moments[[3L]] - m1 * m2,
      moments[[4L]] - m2^2
    ),
    nrow = 2L,
    byrow = TRUE
  )

  # Jacobian of (mu_hat, theta_hat) with respect to the first two raw
  # moments. Keeping the off-diagonal covariance is important near the
  # boundaries of the Beta distribution.
  dtheta_dm1 <- ((1 - 2 * m1) * variance +
    2 * m1^2 * (1 - m1)) / variance^2
  dtheta_dm2 <- -m1 * (1 - m1) / variance^2
  jacobian <- matrix(
    c(1, 0, dtheta_dm1, dtheta_dm2),
    nrow = 2L,
    byrow = TRUE
  )

  covariance <- jacobian %*% moment_covariance %*% t(jacobian)
  (covariance + t(covariance)) / 2
}

.beta_mle_information <- function(mu, theta) {
  .validate_beta_parameters(mu, theta)

  alpha_trigamma <- trigamma(mu * theta)
  beta_trigamma <- trigamma((1 - mu) * theta)
  theta_trigamma <- trigamma(theta)

  # Expected information for one observation, parameterized directly by
  # (mu, theta). This ordering must agree with the decision-rule gradient.
  information <- matrix(
    c(
      theta^2 * (alpha_trigamma + beta_trigamma),
      theta * (mu * alpha_trigamma - (1 - mu) * beta_trigamma),
      theta * (mu * alpha_trigamma - (1 - mu) * beta_trigamma),
      mu^2 * alpha_trigamma +
        (1 - mu)^2 * beta_trigamma - theta_trigamma
    ),
    nrow = 2L,
    byrow = TRUE
  )

  if (any(!is.finite(information))) {
    stop("Beta MLE Fisher information is not finite.", call. = FALSE)
  }
  information
}

.beta_mle_covariance <- function(mu, theta) {
  information <- .beta_mle_information(mu, theta)
  covariance <- tryCatch(
    solve(information),
    error = function(error) {
      stop(
        "Beta MLE Fisher information is singular: ", conditionMessage(error),
        call. = FALSE
      )
    }
  )

  if (any(!is.finite(covariance))) {
    stop("Beta MLE covariance is not finite.", call. = FALSE)
  }
  # Return per-observation covariance; callers divide by sample size.
  (covariance + t(covariance)) / 2
}
