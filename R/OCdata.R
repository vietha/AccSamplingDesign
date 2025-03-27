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
    return(data.frame(
      x_p = proportion_nonconforming, 
      y = probability_acceptance
    ))
  } else {
    mean_level <- sapply(proportion_nonconforming, function(p) muEst(
      p, plan$spec_limit, 
      sigma = plan$sigma,
      theta = plan$theta,
      dist = plan$distribution,
      limtype = plan$limtype
    ))
    return(data.frame(
      x_p = proportion_nonconforming, 
      x_m = mean_level, 
      y = probability_acceptance
    ))
  }
}
