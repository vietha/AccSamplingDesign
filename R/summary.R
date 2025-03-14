## -----------------------------------------------------------------------------
## summary.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Summary methods for acceptance sampling plans
##
## Changelogs:
## -----------------------------------------------------------------------------

#' @export
summary.AttrPlan <- function(object, ...) {
  cat("Attribute Acceptance Sampling Plan\n")
  cat("----------------------------------\n")
  cat("Sample Size (n):", object$n, "\n")
  cat("Acceptance Number (c):", object$c, "\n")
  cat("Producer's Risk (alpha =", object$alpha, ") at PRQ =", object$PRQ, "\n")
  cat("Consumer's Risk (beta =", object$beta, ") at CRQ =", object$CRQ, "\n")
  if(object$measurement_error > 0) {
    cat("Measurement Error:", object$measurement_error, "\n")
  }
  invisible(object)
}

#' @export
summary.VarPlan <- function(object, ...) {
  cat("Variable Acceptance Sampling Plan\n")
  cat("--------------------------------\n")
  cat("Distribution:", object$distribution, "\n")
  if (!is.null(object$limit_type) && !is.null(object$spec_limit)) {
    cat("Specification Limit:", object$limit_type, object$spec_limit, "\n")
  }
  
  if(object$distribution == "normal") {
    cat("Sample Size (n):", object$sample_size, "\n")
    cat("Acceptance Limit (k):", round(object$k, 3), "\n")
    cat("Population Standard Deviation:", object$sigma_type, "\n")
  } else {
    cat("Sample Size (m):", object$sample_size, "\n")
    cat("Acceptance Constant (k):", round(object$k, 3), "\n")
    cat("Population Precision Parameter (theta):", object$theta_type, "\n")
  }
  cat("Producer's Risk (alpha =", object$r_alpha, ") at PRQ =", object$PRQ, "\n")
  cat("Consumer's Risk (beta =", object$r_beta, ") at CRQ =", object$CRQ, "\n")
  if(!is.null(object$measurement_error)) {
    cat("Measurement Error:", object$measurement_error, "\n")
  }
  invisible(object)
}