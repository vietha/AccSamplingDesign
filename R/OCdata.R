## -----------------------------------------------------------------------------
## OCdata.R --- 
##
## Author: Ha Truong
##
## Created: 11 Mar 2025
##
## Purposes: Generate OC curve data
##
## Changelogs:
## -----------------------------------------------------------------------------
#' @importFrom methods new

#' @export
setClass("OCdata",
         slots = list(
           pd = "numeric",
           paccept = "numeric",
           process_means = "numeric",
           dist = "character",
           n = "numeric", # sample size
           c = "numeric", # acceptance number (sampling by attribute)
           k = "numeric"  # acceptance constant
         ))

#' @export
OCdata <- function(plan = NULL, pd = NULL,
                   distribution = c("binomial", "normal", "beta"),
                   PRQ = NULL, CRQ = NULL, alpha = NULL, beta = NULL,
                   USL = NULL, LSL = NULL, 
                   n = NULL, c = NULL, k = NULL,
                   sigma_type = c("known", "unknown"),
                   theta_type = c("known", "unknown"),
                   sigma = NULL, theta = NULL) {
  
  if (!is.null(plan)) {
    # Use plan directly
    if (is.null(pd)) {
      pd <- seq(1e-10, min(plan$CRQ * 2, 1), length.out = 100)
    }
    paccept <- sapply(pd, function(p) accProb(plan, p))
    
    mean_level <- NULL
    if (!is.null(plan$USL) || !is.null(plan$LSL)) {
      mean_level <- sapply(pd, function(p) muEst(
        p, USL = plan$USL, LSL = plan$LSL,
        sigma = plan$sigma,
        theta = plan$theta,
        dist = plan$distribution
      ))
    }
    
    return(new("OCdata",
               pd = pd,
               paccept = paccept,
               process_means = if (is.null(mean_level)) numeric(0) else mean_level,
               dist = if (!is.null(plan$distribution)) plan$distribution else NA_character_,
               n = plan$sample_size,
               c = if (!is.null(plan$c)) plan$c else numeric(0),
               k = if (!is.null(plan$k)) plan$k else numeric(0)))
  }
  
  # Argument matching
  distribution <- match.arg(distribution)
  #limit_type <- match.arg(limit_type)
  sigma_type <- match.arg(sigma_type)
  theta_type <- match.arg(theta_type)
  
  # Input validation
  if (is.null(PRQ) || is.null(CRQ)) {
    stop("PRQ and CRQ must be provided when plan is not specified.")
  }
  #if (is.null(alpha) || is.null(beta) || alpha <= 0 || alpha >= 1 || beta <= 0 || beta >= 1) {
  #  stop("alpha and beta must be between 0 and 1 (exclusive).")
  #}
  if (CRQ <= PRQ) {
    stop("CRQ must be greater than PRQ.")
  }
  
  if (distribution == "binomial") {
    if (is.null(n) || is.null(c)) stop("n and c must be provided for binomial distribution.")
    plan <- structure(list(n = n, c = c, PRQ = PRQ, CRQ = CRQ, 
                           PR = alpha, CR = beta,
                           USL = USL, LSL = LSL,
                           sample_size = n,
                           distribution = distribution),
                      class = "AttrPlan")
    
  } else if (distribution %in% c("normal", "beta")) {
    if (is.null(n) || is.null(k)) stop("n and k must be provided for variable plans.")
    if (distribution == "beta" && is.null(theta)) {
      stop("For beta distribution, theta parameter must be provided.")
    }
    if (distribution == "beta" && is.null(USL) && is.null(LSL)) {
      stop("For beta distribution, a specification limit (USL/LSL) must be provided.")
    }
    if (!is.null(USL) && !is.null(LSL)) {
      stop("Double specification limits (both USL and LSL) are not supported. 
         Please specify only one limit: either USL or LSL.")
    }
    plan <- structure(list(n = n, k = k, PRQ = PRQ, CRQ = CRQ,
                           USL = USL, LSL = LSL,
                           PR = alpha, CR = beta,
                           sample_size = n,
                           distribution = distribution,
                           sigma_type = sigma_type,
                           theta_type = theta_type,
                           sigma = sigma,
                           theta = theta),
                      class = "VarPlan")
  } else {
    stop("Unsupported distribution.")
  }
  
  # Recursively generate OCdata from created plan
  OCdata(plan, pd = pd)
}

# OCdata <- function(plan, pd = NULL) {
#   if (is.null(pd)) {
#     proportion_nonconforming <- seq(1e-10, min(plan$CRQ * 2, 1), length.out = 100)
#   } else {
#     proportion_nonconforming <- pd
#   }
#   probability_acceptance <- sapply(proportion_nonconforming, function(p) accProb(plan, p))
#   
#   mean_level <- NULL
#   if (is.null(plan$spec_limit) || is.null(plan$limtype)) {
#     obj <- data.frame(
#       x_p = proportion_nonconforming, 
#       y = probability_acceptance
#     )
#   } else {
#     mean_level <- sapply(proportion_nonconforming, function(p) muEst(
#       p, plan$spec_limit, 
#       sigma = plan$sigma,
#       theta = plan$theta,
#       dist = plan$distribution,
#       limtype = plan$limtype
#     ))
#   }
#   
#   # Create S4 object
#   new("OCdata",
#       pd = proportion_nonconforming,
#       paccept = probability_acceptance,
#       process_means = if (is.null(mean_level)) numeric(0) else mean_level,
#       dist = ifelse(!is.null(plan$distribution), plan$distribution, NA),
#       n = ifelse(!is.null(plan$sample_size), plan$sample_size, NA),
#       c = if (!is.null(plan$c)) plan$c else numeric(0),
#       k = if (!is.null(plan$k)) plan$k else numeric(0)
#   )
# }

#' @export
plot.OCdata <- function(x, by = c("pd", "mean"), ...) {
  if (!inherits(x, "OCdata")) {
    stop("Object is not of class 'OCdata'")
  }
  
  by <- match.arg(by)
  
  if (by == "pd") {
    plot(x@pd, x@paccept, type = "l", col = "red", lwd = 2,
         main = "OC Curve by Proportion Nonconforming", 
         xlab = "Proportion Nonconforming", ylab = "P(accept)", ...)
    grid()
    
  } else if (by == "mean") {
    if (length(x@process_means) > 0) {
      plot(x@process_means, x@paccept, type = "l", col = "blue", lwd = 2,
           main = "OC Curve by Mean Levels", xlab = "Mean Level", ylab = "P(accept)", ...)
      grid()
    } else {
      message("Mean-level plot not available for your plan!")
    }
  }
}



#' @export
setMethod("show", signature(object = "OCdata"), function(object) {
  cat("OC Data for Acceptance Sampling Plan\n")
  cat("--------------------------------------------------\n")
  cat(sprintf("  Distribution: %s\n", object@dist))
  cat(sprintf("  Sample size (n): %s\n", ifelse(length(object@n) > 0, object@n, "NA")))
  if (length(object@k) > 0) {
    cat(sprintf("  Acceptance constant (k): %s\n", object@k))
  } else if (length(object@c) > 0) {
    cat(sprintf("  Acceptance number (c): %s\n", object@c))
  }
  cat(sprintf("  # of pd values: %d\n", length(object@pd)))
  cat(sprintf("  # of P(accept) values: %d\n", length(object@paccept)))
  if (length(object@process_means) > 0) {
    cat(sprintf("  Process means available (length: %d)\n", length(object@process_means)))
  } 
  cat("--------------------------------------------------\n")
})
