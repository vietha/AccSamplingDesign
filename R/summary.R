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


# S3 methods for AttrPlan
#' @export
print.AttrPlan <- function(x, ...) {
  cat("AttrPlan object:\n")
  cat(" Distribution:", x$distribution, "\n")
  cat(" Sample size (n):", x$n, "\n")
  if (length(x$c) > 0) cat(" Acceptance number (c):", x$c, "\n")
  cat("\nNOTE: summary(plan) for detail report,",
      "\n      plot(plan) for quick OC visualization,",
      "\n      OCdata(plan) to extract data for evaluation and custom plots.\n")
}

#' @export
summary.AttrPlan <- function(object, ...) {
  cat("Attributes Acceptance Sampling Plan: \n")
  cat(" Distribution:", object$distribution, "\n")
  cat(" Sample Size (n):", object$n, "\n")
  cat(" Acceptance Number (c):", object$c, "\n")
  cat(" Producer's Risk (PR =", object$PR, ") at PRQ =", object$PRQ, "\n")
  cat(" Consumer's Risk (CR =", object$CR, ") at CRQ =", object$CRQ, "\n")
  if(object$measurement_error > 0) {
    cat(" Measurement Error:", object$measurement_error, "\n")
  }
  invisible(object)
}


# S3 methods for VarPlan
#' @export
print.VarPlan <- function(x, ...) {
  cat("VarPlan object:\n")
  cat(" Distribution:", x$distribution, "\n")
  if(x$distribution == "normal") {
    cat(" Sigma type:", x$sigma_type, "\n")
  } else {
    cat(" Theta type:", x$theta_type, "\n")
  }
  cat(" Sample size (n):", x$sample_size, "\n")
  if (length(x$k) > 0) cat(" Acceptability constant (k):", x$k, "\n")
  cat("\nNOTE: summary(plan) for detail report,",
      "\n      plot(plan) for quick OC visualization,",
      "\n      OCdata(plan) to extract data for evaluation and custom plots.\n")
}

#' @export
summary.VarPlan <- function(object, ...) {
  cat("Variables Acceptance Sampling Plan\n")
  cat(" Distribution:", object$distribution, "\n")
  cat(" Sample Size (n):", object$sample_size, "\n")
  cat(" Acceptability Constant (k):", round(object$k, 3), "\n")
  if(object$distribution == "normal") {
    cat(" Population Standard Deviation:", object$sigma_type, "\n")
  } else {
    cat(" Population Precision Parameter (theta):", object$theta_type, "\n")
  }
  cat(" Producer's Risk (PR =", object$PR, ") at PRQ =", object$PRQ, "\n")
  cat(" Consumer's Risk (CR =", object$CR, ") at CRQ =", object$CRQ, "\n")
  if (!is.null(object$LSL)) {
    cat(" Lower Specification Limit (LSL):", object$LSL, "\n")
  }
  if (!is.null(object$USL)) {
    cat(" Uper Specification Limit (USL):", object$USL, "\n")
  }
  if(!is.null(object$measurement_error)) {
    cat(" Measurement Error:", object$measurement_error, "\n")
  }
  invisible(object)
}