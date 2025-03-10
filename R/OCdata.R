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
OCdata <- function(plan) {
  x_p <- seq(0, min(plan$CRQ * 2, 1), length.out = 100)
  # x_mu <- sapply(x_p, function(p) muEst(p, plan$limit, sigma = plan$sigma, 
  #                                       theta = plan$theta, 
  #                                       dist = plan$distribution, 
  #                                       limtype = plan$limtype))
  y <- sapply(x_p, function(p) accProb(plan, p))
  
  #return(data.frame(x_p = x_p, x_mu = x_mu, y = y))
  return(data.frame(x_p = x_p, y = y))
}
