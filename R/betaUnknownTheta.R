## Internal methods for Beta variables plans with an estimated precision.
##
## These helpers are package-native implementations of the analytical methods.
## They intentionally have no dependency on research scripts, reports, or data.

.beta_theta_methods <- c("delta_mle", "delta_mom", "gk_adjustment")

.beta_theta_method_label <- function(method) {
  labels <- c(
    delta_mle = "Delta-MLE",
    delta_mom = "Delta-MoM",
    gk_adjustment = "Govindaraju-Kissling adjustment"
  )
  method <- match.arg(method, names(labels))
  unname(labels[[method]])
}

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
  .validate_beta_parameters(mu, theta)
  alpha <- mu * theta
  beta_shape <- (1 - mu) * theta

  # Inverting in the conventional (alpha, beta) parameterization avoids the
  # severe scale imbalance of direct (mu, theta) information when theta is
  # large. The Jacobian then maps the covariance back to (mu, theta).
  shape_information <- matrix(
    c(
      trigamma(alpha) - trigamma(theta), -trigamma(theta),
      -trigamma(theta), trigamma(beta_shape) - trigamma(theta)
    ),
    nrow = 2L,
    byrow = TRUE
  )
  shape_covariance <- tryCatch(
    solve(shape_information),
    error = function(error) {
      stop(
        "Beta MLE Fisher information is singular: ", conditionMessage(error),
        call. = FALSE
      )
    }
  )
  transformation <- matrix(
    c((1 - mu) / theta, -mu / theta, 1, 1),
    nrow = 2L,
    byrow = TRUE
  )
  covariance <- transformation %*% shape_covariance %*% t(transformation)

  if (any(!is.finite(covariance))) {
    stop("Beta MLE covariance is not finite.", call. = FALSE)
  }
  # Return per-observation covariance; callers divide by sample size.
  (covariance + t(covariance)) / 2
}

.beta_delta_statistic <- function(mu, theta, k, limit_type) {
  .validate_beta_parameters(mu, theta)
  limit_type <- match.arg(limit_type, c("upper", "lower"))
  if (length(k) != 1L || !is.finite(k) || k < 0) {
    stop("k must be one finite non-negative value.", call. = FALSE)
  }

  direction <- if (limit_type == "upper") 1 else -1
  standard_deviation <- sqrt(mu * (1 - mu) / (theta + 1))
  mu + direction * k * standard_deviation
}

.beta_delta_gradient <- function(mu, theta, k, limit_type) {
  .validate_beta_parameters(mu, theta)
  limit_type <- match.arg(limit_type, c("upper", "lower"))
  if (length(k) != 1L || !is.finite(k) || k < 0) {
    stop("k must be one finite non-negative value.", call. = FALSE)
  }

  # The sign changes both k-dependent derivatives for a lower limit. The
  # parameter order is (mu, theta), matching both covariance implementations.
  direction <- if (limit_type == "upper") 1 else -1
  c(
    mu = 1 + direction * k * (1 - 2 * mu) /
      (2 * sqrt((theta + 1) * mu * (1 - mu))),
    theta = -direction * k * sqrt(mu * (1 - mu)) /
      (2 * (theta + 1)^(3 / 2))
  )
}

.beta_delta_acceptance_probability <- function(mu, theta, n, k, limit,
                                               limit_type, method) {
  .validate_beta_parameters(mu, theta)
  limit_type <- match.arg(limit_type, c("upper", "lower"))
  method <- match.arg(method, c("delta_mle", "delta_mom"))
  if (length(n) != 1L || !is.finite(n) || n <= 0) {
    stop("n must be one finite positive value.", call. = FALSE)
  }
  if (length(limit) != 1L || !is.finite(limit) || limit <= 0 || limit >= 1) {
    stop("The Beta specification limit must be strictly between 0 and 1.",
         call. = FALSE)
  }

  covariance <- if (method == "delta_mle") {
    .beta_mle_covariance(mu, theta)
  } else {
    .beta_mom_covariance(mu, theta)
  }
  gradient <- .beta_delta_gradient(mu, theta, k, limit_type)
  statistic <- .beta_delta_statistic(mu, theta, k, limit_type)

  # Covariance helpers return per-observation Sigma. Divide the quadratic form
  # by n exactly once to obtain the finite-sample Delta variance.
  delta_variance <- drop(t(gradient) %*% covariance %*% gradient) / n
  if (!is.finite(delta_variance) || delta_variance <= 0) {
    stop("Delta-method variance must be finite and positive.", call. = FALSE)
  }

  z_value <- if (limit_type == "upper") {
    (limit - statistic) / sqrt(delta_variance)
  } else {
    (statistic - limit) / sqrt(delta_variance)
  }
  probability <- pnorm(z_value)
  if (length(probability) != 1L || !is.finite(probability) ||
      probability < 0 || probability > 1) {
    stop("Delta acceptance probability is not finite or is outside [0, 1].",
         call. = FALSE)
  }
  probability
}
