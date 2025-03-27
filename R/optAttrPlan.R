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
optAttrPlan <- function(PRQ, CRQ, 
                        alpha = 0.05, beta = 0.10,
                        measurement_error = 0) {
  
  # Input validation
  if(PRQ >= CRQ) stop("PRQ must be < CRQ")
  if(alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1)")
  if(beta <= 0 || beta >= 1) stop("beta must be in (0,1)")
  
  # Plan calculation logic
  n <- 1
  while(TRUE) {
    for(c in 0:n) {
      pa_p <- pbinom(c, n, PRQ)
      pa_c <- pbinom(c, n, CRQ)
      if(pa_p >= (1 - alpha) && pa_c <= beta) {
        return(structure(
          list(
            n = n,
            c = c,
            PR = 1 - pa_p, 
            CR = pa_c,
            PRQ = PRQ,
            CRQ = CRQ,
            measurement_error = measurement_error
          ), 
          class = "AttrPlan"
        ))
      }
    }
    n <- n + 1
  }
}