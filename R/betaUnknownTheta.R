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
