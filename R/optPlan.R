## -----------------------------------------------------------------------------
## optPlan.R --- 
##
## Author: Ha Truong
##
## Created: 22 Apr 2025
##
## Purposes: Design variable acceptance sampling plans (normal/beta)
##
## Changelogs:
## -----------------------------------------------------------------------------

#' Optimal Acceptance Sampling Plan
#' @export
optPlan <- function(PRQ, CRQ, alpha = 0.05, beta = 0.10, USL = NULL, LSL = NULL,
                    distribution = c("binomial", "poisson", "normal", "beta"),
                    sigma_type = c("known", "unknown"),
                    theta_type = c("known", "unknown"),
                    sigma = NULL, theta = NULL) {
  
  # Match arguments to ensure valid input
  distribution <- match.arg(distribution)
  sigma_type <- match.arg(sigma_type)
  theta_type <- match.arg(theta_type)
  
  # Ensure PRQ, CRQ, alpha, and beta are within valid ranges based on distribution
  check_quality <- function(q, distribution) {
    if (distribution == "normal" && (q < 0 || q > 1)) {
      return(FALSE)
    }
    if (distribution == "beta" && (q <= 0 || q >= 1)) {
      return(FALSE)
    }
    if (distribution == "binomial" && (q <= 0 || q >= 1)) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Ensure PRQ and CRQ are provided
  if (missing(PRQ) || missing(CRQ)) {
    stop("Producer and Consumer Risk Points must be provided.")
  }
  
  # Ensure Consumer Risk Quality (CRQ) > Producer Risk Quality (PRQ)
  if (CRQ <= PRQ) {
    stop("Consumer Risk Quality (CRQ) must be greater than Producer Risk Quality (PRQ).")
  }
  
  if (!check_quality(PRQ, distribution) || !check_quality(CRQ, distribution)) {
    stop("PRQ and CRQ are out of bounds for the selected distribution.")
  }
  
  if (alpha <= 0 || alpha >= 1 || beta <= 0 || beta >= 1) {
    stop("alpha and beta must be between 0 and 1 (exclusive).")
  }
  
  # Set default if not provided
  if (is.null(sigma_type)) sigma_type <- "known"
  if (is.null(theta_type)) theta_type <- "known"
  
  if (!is.null(USL) && !is.null(LSL)) {
    stop("Double specification limits (both USL and LSL) are not supported. 
         Please specify only one limit: either USL or LSL.")
  }
  # This upper limit case
  if (!is.null(USL)) {
    limit_type <- "upper"
    spec_limit <- USL
  } else if (!is.null(LSL)) { # This lower limit case
    limit_type <- "lower"
    spec_limit <- LSL
  } else {
    limit_type <- "upper" # set this as default
  }
  
  # Additional checks for beta distribution
  if (distribution == "beta") {
    if(is.null(USL) && is.null(LSL)){
      stop("For the beta distribution, a specification limit must be provided.")
    }
    if (is.null(theta)) {
      stop("For the beta distribution, theta must be provided.")
    }
  }
  
  # Check measurement error is non-negative
  # if (measurement_error < 0) {
  #   stop("measurement_error must be non-negative.")
  # }
  
  # Init an optimal plan
  opt_plan = NULL
  
  # ------------Binomial ----------
  if (distribution == "binomial"|| distribution == "poisson") {
    opt_plan <- optAttrPlan(PRQ = PRQ, CRQ = CRQ, alpha = alpha, beta = beta, 
                            distribution = distribution)
  }
  # ------------Normal/Beta ------------
  if (distribution == "normal" || distribution == "beta") {
    opt_plan <- optVarPlan(PRQ = PRQ, CRQ = CRQ, alpha = alpha, beta = beta, 
                           USL = USL, LSL = LSL, distribution = distribution,
                           sigma_type = sigma_type, theta_type = theta_type,
                           sigma = sigma, theta = theta)
  }
  return(opt_plan)
}