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
  cat("Attributes Acceptance Sampling Plan\n")
  cat("-----------------------------------\n")
  cat("Distribution:", object$distribution, "\n")
  cat("Sample Size (n):", object$n, "\n")
  cat("Acceptance Number (c):", object$c, "\n")
  cat("Producer's Risk (PR =", object$PR, ") at PRQ =", object$PRQ, "\n")
  cat("Consumer's Risk (CR =", object$CR, ") at CRQ =", object$CRQ, "\n")
  if(object$measurement_error > 0) {
    cat("Measurement Error:", object$measurement_error, "\n")
  }
  cat("----------------------------------\n")
  invisible(object)
}

#' @export
summary.VarPlan <- function(object, ...) {
  cat("Variables Acceptance Sampling Plan\n")
  cat("----------------------------------\n")
  cat("Distribution:", object$distribution, "\n")

  cat("Sample Size (n):", object$sample_size, "\n")
  cat("Acceptance Limit (k):", round(object$k, 3), "\n")
  if(object$distribution == "normal") {
    cat("Population Standard Deviation:", object$sigma_type, "\n")
  } else {
    cat("Population Precision Parameter (theta):", object$theta_type, "\n")
  }
  cat("Producer's Risk (PR =", object$PR, ") at PRQ =", object$PRQ, "\n")
  cat("Consumer's Risk (CR =", object$CR, ") at CRQ =", object$CRQ, "\n")
  if (!is.null(object$LSL)) {
    cat("Lower Specification Limit:", object$LSL, "\n")
  }
  if (!is.null(object$USL)) {
    cat("Uper Specification Limit:", object$USL, "\n")
  }
  if(!is.null(object$measurement_error)) {
    cat("Measurement Error:", object$measurement_error, "\n")
  }
  cat("----------------------------------\n")
  invisible(object)
}