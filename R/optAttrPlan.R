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

find_plan_binomial <- function(PRQ, CRQ, alpha, beta, measurement_error = 0) {
  # Input validation
  if(PRQ >= CRQ) stop("PRQ must be < CRQ")
  if(alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if(beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
  
  n_vec <- 1:1e5 #define Maximum vector n
  
  # Vectorized qbinom to find c for all n
  c_vec <- qbinom(1 - alpha, n_vec, PRQ, lower.tail = TRUE)
  
  for (i in seq_along(n_vec)) {
    n <- n_vec[i]
    c <- c_vec[i]
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
  stop("No solution found!")
}


find_plan_poisson <- function(PRQ, CRQ, alpha, beta, measurement_error = NULL) {
  # Input validation
  if (PRQ >= CRQ) stop("PRQ must be < CRQ")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if (beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
  
  max_n <- 1e5
  for (n in 1:max_n) {
    lambda_prq <- n * PRQ
    lambda_crq <- n * CRQ
    
    # Find smallest c satisfying producer’s risk
    c <- qpois(1 - alpha, lambda_prq, lower.tail = TRUE)
    
    pa_p <- ppois(c, lambda_prq)  # producer's risk at PRQ
    pa_c <- ppois(c, lambda_crq)  # consumer's risk at CRQ
    
    if (pa_p >= 1 - alpha && pa_c <= beta) {
      return(structure(
        list(
          n = n,
          c = c,
          PR = 1 - pa_p,
          CR = pa_c,
          PRQ = PRQ,
          CRQ = CRQ,
          sample_size = ceiling(n),
          distribution = "poisson",
          measurement_error = measurement_error
        ),
        class = "AttrPlan"
      ))
    }
  }
  
  stop("No solution found within max_n = ", max_n)
}

#' Attribute Acceptance Sampling Plan
#' @export
optAttrPlan <- function(PRQ, CRQ, alpha = 0.05, beta = 0.10, 
                        distribution = c("binomial", "poisson"),
                        measurement_error = 0) {
  distribution <- match.arg(distribution)
  # Input validation
  if(PRQ >= CRQ) stop("PRQ must be < CRQ")
  if(alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if(beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
  
  # Init an optimal plan
  attr_plan = NULL
  
  # ------------Binomial ----------
  if (distribution == "binomial") {
    attr_plan <- find_plan_binomial(PRQ = PRQ, CRQ = CRQ, 
                                    alpha = alpha, beta = beta, 
                                    measurement_error = measurement_error)
  }
  # ------------Poisson ------------
  if (distribution == "poisson") {
    attr_plan <- find_plan_poisson(PRQ = PRQ, CRQ = CRQ, 
                                   alpha = alpha, beta = beta, 
                                   measurement_error = measurement_error)
  }
  return(attr_plan)
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