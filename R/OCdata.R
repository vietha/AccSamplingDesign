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
OCdata <- function(plan, pd = NULL) {
  if (is.null(pd)) {
    proportion_nonconforming <- seq(1e-10, min(plan$CRQ * 2, 1), length.out = 100)
  } else {
    proportion_nonconforming <- pd
  }
  probability_acceptance <- sapply(proportion_nonconforming, function(p) accProb(plan, p))
  
  mean_level <- NULL
  if (is.null(plan$spec_limit) || is.null(plan$limtype)) {
    obj <- data.frame(
      x_p = proportion_nonconforming, 
      y = probability_acceptance
    )
  } else {
    mean_level <- sapply(proportion_nonconforming, function(p) muEst(
      p, plan$spec_limit, 
      sigma = plan$sigma,
      theta = plan$theta,
      dist = plan$distribution,
      limtype = plan$limtype
    ))
    # obj <- data.frame(
    #   x_p = proportion_nonconforming, 
    #   x_m = mean_level, 
    #   y = probability_acceptance
    # )
  }
  
  # S3 object
  #class(obj) <- "OCdata"  # Assign custom class
  #return(obj)
  
  # Create S4 object
  new("OCdata",
      pd = proportion_nonconforming,
      paccept = probability_acceptance,
      process_means = if (is.null(mean_level)) numeric(0) else mean_level,
      dist = ifelse(!is.null(plan$distribution), plan$distribution, NA),
      n = ifelse(!is.null(plan$sample_size), plan$sample_size, NA),
      c = if (!is.null(plan$c)) plan$c else numeric(0),
      k = if (!is.null(plan$k)) plan$k else numeric(0)
  )
}

#' @export
plot.OCdata <- function(x, ...) {
  if (!inherits(x, "OCdata")) {
    stop("Object is not of class 'OCdata'")
  }
  
  plot(x@pd, x@paccept, type = "l", col = "red", lwd = 2,
       main = "OC Curve by Proportion Nonconforming", 
       xlab = "Proportion Nonconforming", ylab = "P(accept)")
  grid()
  
  if (length(x@process_means) > 0) {
    plot(x@process_means, x@paccept, type = "l", col = "blue", lwd = 2,
         main = "OC Curve by Mean Levels", xlab = "Mean Level", ylab = "P(accept)")
    grid()
  }
  
  # # Plot default chart OC follow proportion nonconforming
  # plot(x$x_p, x$y, type = "l", col = "red", lwd = 2,
  #      main = "OC Curve by Proportion Nonconforming", xlab = "Proportion Nonconforming", ylab = "P(accept)")
  # grid()
  # 
  # # Check if 'x_m' exists; otherwise, use 'x_p'
  # if ("x_m" %in% names(x)) {
  #   plot(x$x_m, x$y, type = "l", col = "blue", lwd = 2,
  #        main = "OC Curve by Mean Levels", xlab = "Mean Levels", ylab = "P(accept)")
  # }
  # grid()
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
  # else {
  #   cat("  No process mean values available.\n")
  # }
  cat("--------------------------------------------------\n")
})
