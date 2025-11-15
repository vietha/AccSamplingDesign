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

# find_plan_binomial <- function(PRQ, CRQ, alpha, beta, measurement_error = 0) {
#   # Input validation
#   if(PRQ >= CRQ) stop("PRQ must be < CRQ")
#   if(alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
#   if(beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
#   
#   n_vec <- 1:1e5 #define Maximum vector n
#   
#   # Vectorized qbinom to find c for all n
#   c_vec <- qbinom(1 - alpha, n_vec, PRQ, lower.tail = TRUE)
#   
#   for (i in seq_along(n_vec)) {
#     n <- n_vec[i]
#     c <- c_vec[i]
#     pa_c <- pbinom(c, n, CRQ)
#     pa_p <- pbinom(c, n, PRQ)
#     if (pa_c <= beta) {
#       return(structure(
#         list(
#           n = n,
#           c = c,
#           PR = 1 - pa_p,
#           CR = pa_c,
#           PRQ = PRQ,
#           CRQ = CRQ,
#           sample_size = ceiling(n),
#           distribution = "binomial",
#           measurement_error = measurement_error
#         ),
#         class = "AttrPlan"
#       ))
#     }
#   }
#   stop("No solution found!")
# }

find_plan_binomial <- function(PRQ, CRQ, alpha, beta, measurement_error = 0) {
  
  if (PRQ >= CRQ) stop("PRQ must be < CRQ")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if (beta  <= 0 || beta  >= 1) stop("beta must be in (0,1)")
  if (!is.numeric(measurement_error) || length(measurement_error) != 1)
    stop("measurement_error must be a numeric scalar")
  
  n_vec <- 1:1e5
  
  c_vec <- qbinom(1 - alpha, n_vec, PRQ, lower.tail = TRUE)
  
  # Ensure valid c values
  c_vec <- pmax(c_vec, 0)
  
  pa_c_vec <- pbinom(c_vec, n_vec, CRQ)
  idx <- which(pa_c_vec <= beta)[1]
  
  if (is.na(idx)) stop("No solution found!")
  
  n <- n_vec[idx]
  c <- c_vec[idx]
  pa_p <- pbinom(c, n, PRQ)
  
  structure(
    list(
      n = n,
      c = c,
      PR = 1 - pa_p,
      CR = pa_c_vec[idx],
      PRQ = PRQ,
      CRQ = CRQ,
      sample_size = n,
      distribution = "binomial",
      measurement_error = measurement_error
    ),
    class = "AttrPlan"
  )
}


# find_plan_poisson <- function(PRQ, CRQ, alpha, beta, measurement_error = 0) {
#   # Input validation
#   if (PRQ >= CRQ) stop("PRQ must be < CRQ")
#   if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
#   if (beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
#   
#   max_n <- 1e5
#   for (n in 1:max_n) {
#     lambda_prq <- n * PRQ
#     lambda_crq <- n * CRQ
#     
#     # Find smallest c satisfying producer’s risk
#     c <- qpois(1 - alpha, lambda_prq, lower.tail = TRUE)
#     
#     pa_p <- ppois(c, lambda_prq)  # producer's risk at PRQ
#     pa_c <- ppois(c, lambda_crq)  # consumer's risk at CRQ
#     
#     if (pa_p >= 1 - alpha && pa_c <= beta) {
#       return(structure(
#         list(
#           n = n,
#           c = c,
#           PR = 1 - pa_p,
#           CR = pa_c,
#           PRQ = PRQ,
#           CRQ = CRQ,
#           sample_size = ceiling(n),
#           distribution = "poisson",
#           measurement_error = measurement_error
#         ),
#         class = "AttrPlan"
#       ))
#     }
#   }
#   
#   stop("No solution found within max_n = ", max_n)
# }

find_plan_poisson <- function(PRQ, CRQ, alpha, beta, measurement_error = 0) {
  
  # --- Input validation ---
  if (PRQ >= CRQ) stop("PRQ must be < CRQ")
  if (alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if (beta  <= 0 || beta  >= 1) stop("beta must be in (0,1)")
  if (!is.numeric(measurement_error) || length(measurement_error) != 1)
    stop("measurement_error must be a numeric scalar")
  
  # --- Vectorized n ---
  n_vec <- 1:1e5
  
  # λ for PRQ and CRQ
  lambda_prq_vec <- n_vec * PRQ
  lambda_crq_vec <- n_vec * CRQ
  
  # --- Compute c(n) under PRQ ---
  c_vec <- qpois(1 - alpha, lambda_prq_vec)
  
  # --- Compute Pa under PRQ and CRQ ---
  pa_p_vec <- ppois(c_vec, lambda_prq_vec)  # producer's acceptance probability
  pa_c_vec <- ppois(c_vec, lambda_crq_vec)  # consumer's acceptance probability
  
  # --- Condition: PR satisfied & CR satisfied ---
  # PR: pa_p >= 1 - alpha
  # CR: pa_c <= beta
  idx <- which(pa_p_vec >= 1 - alpha & pa_c_vec <= beta)[1]
  
  if (is.na(idx)) stop("No solution found!")
  
  # Extract optimal values
  n <- n_vec[idx]
  c <- c_vec[idx]
  
  structure(
    list(
      n = n,
      c = c,
      PR = 1 - pa_p_vec[idx],
      CR = pa_c_vec[idx],
      PRQ = PRQ,
      CRQ = CRQ,
      sample_size = n,
      distribution = "poisson",
      measurement_error = measurement_error
    ),
    class = "AttrPlan"
  )
}


#' Attribute Acceptance Sampling Plan
#' @export
optAttrPlan <- function(PRQ, CRQ, alpha = 0.05, beta = 0.10, 
                        distribution = c("binomial", "poisson")) {
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
                                    alpha = alpha, beta = beta)
  }
  # ------------Poisson ------------
  if (distribution == "poisson") {
    attr_plan <- find_plan_poisson(PRQ = PRQ, CRQ = CRQ, 
                                   alpha = alpha, beta = beta)
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
       main = paste0("Attributes Sampling OC Curve", " | n=", 
                     x$sample_size, ", c=", x$c, " | ", x$distribution),
       xlab = "Proportion Nonconforming", ylab = "P(accept)", ...)
  abline(v = c(x$PRQ, x$CRQ), lty = 2, col = "gray")
  abline(h = c(1 - x$PR, x$CR), lty = 2, col = "gray")
  grid()
}