## -----------------------------------------------------------------------------
## optAttrPlan.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Design attribute acceptance sampling plans (binomial)
##
## Changelogs:
## -----------------------------------------------------------------------------

#' Attribute Acceptance Sampling Plan
#' @export
optAttrPlan <- function(PRQ, CRQ, alpha = 0.05, beta = 0.10, 
                        measurement_error = 0) {
  
  # Input validation
  if(PRQ >= CRQ) stop("PRQ must be < CRQ")
  if(alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if(beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
  
  ## Find ATTR plan use bruce force
  ## Plan calculation logic
  # n <- 1
  # while(TRUE) {
  #   for(c in 0:n) {
  #     pa_p <- pbinom(c, n, PRQ)
  #     pa_c <- pbinom(c, n, CRQ)
  #     if(pa_p >= (1 - alpha) && pa_c <= beta) {
  #       return(structure(
  #         list(
  #           n = n,
  #           c = c,
  #           PR = 1 - pa_p,
  #           CR = pa_c,
  #           PRQ = PRQ,
  #           CRQ = CRQ,
  #           measurement_error = measurement_error
  #         ),
  #         class = "AttrPlan"
  #       ))
  #     }
  #   }
  #   n <- n + 1
  # }
  # 
  max_n = 1e5
  for (n in 1:max_n) {
    # Smallest c meeting PA_P >= 1 - alpha
    c <- qbinom(1 - alpha, n, PRQ, lower.tail = TRUE)  
    pa_c <- pbinom(c, n, CRQ)
    pa_p <- pbinom(c, n, PRQ)
    if (pa_c <= beta) {
      return(structure(
        list(
          n = n,
          c = c,
          PR = 1 - pa_p,
          CR = pa_c,
          PRQ = PRQ,
          CRQ = CRQ,
          sample_size = ceiling(n),
          distribution = "binomial",
          measurement_error = measurement_error
        ),
        class = "AttrPlan"
      ))
    }
  }
  stop("No solution found within max_n = ", max_n)
}


#' @export
plot.AttrPlan <- function(x, pd = NULL, ...) {
  if (is.null(pd)) {
    pd <- seq(1e-10, min(x$CRQ * 2, 1), length.out = 100)
  }
  pa <- sapply(pd, function(p) accProb(x, p))
  
  plot(pd, pa, type = "l", col = "blue", lwd = 2,
       main = paste0("Attribute OC Curve - ", x$distribution, " distribution | n=", 
                     x$sample_size, ", c=", x$c),
       xlab = "Proportion Nonconforming", ylab = "P(accept)", ...)
  abline(v = c(x$PRQ, x$CRQ), lty = 2, col = "gray")
  abline(h = c(1 - x$PR, x$CR), lty = 2, col = "gray")
  grid()
}