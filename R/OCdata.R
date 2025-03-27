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
OCdata <- function(plan, pd = NULL) {
  if (is.null(pd)) {
    proportion_nonconforming <- seq(1e-10, min(plan$CRQ * 2, 1), length.out = 100)
  } else {
    proportion_nonconforming <- pd
  }
  probability_acceptance <- sapply(proportion_nonconforming, function(p) accProb(plan, p))
  
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
    obj <- data.frame(
      x_p = proportion_nonconforming, 
      x_m = mean_level, 
      y = probability_acceptance
    )
  }
  
  class(obj) <- "OCdata"  # Assign custom class
  return(obj)
}

#' @export
plot.OCdata <- function(x, ...) {
  if (!inherits(x, "OCdata")) {
    stop("Object is not of class 'OCdata'")
  }
  
  # Plot default chart OC follow proportion nonconforming
  plot(x$x_p, x$y, type = "l", col = "red", lwd = 2,
       main = "OC Curve by Proportion Nonconforming", xlab = "Proportion Nonconforming", ylab = "P(accept)")
  grid()
  
  # Check if 'x_m' exists; otherwise, use 'x_p'
  if ("x_m" %in% names(x)) {
    plot(x$x_m, x$y, type = "l", col = "blue", lwd = 2,
         main = "OC Curve by Mean Levels", xlab = "Mean Levels", ylab = "P(accept)")
  }
  grid()
}
