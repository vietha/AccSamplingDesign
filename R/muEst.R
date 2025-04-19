## -----------------------------------------------------------------------------
## muEst.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: estimate mu corresponding to a given proportion nonconforming (p)
##
## Changelogs:
## -----------------------------------------------------------------------------

#' Estimate mu base on quality level pre-defined (like PRQ/CRQ)
#' @export
muEst <- function(p, USL = NULL, LSL = NULL, 
                  sigma = NULL, theta = NULL, 
                  dist = c("normal", "beta")) {
  dist <- match.arg(dist)
  ##limtype <- match.arg(limtype)
  if (is.null(USL) && is.null(LSL)){
    stop("A specification limit (USL or LSL) is required for mu estimation!")
  }
  # This upper limit case
  if (!is.null(USL)) {
    limtype <- "upper"
    limit <- USL
  } else if (!is.null(LSL)) { # This lower limit case
    limtype <- "lower"
    limit <- LSL
  } else {
    limtype <- "upper" # set this as default
  }
  
  if (dist == "normal") {
    if (is.null(sigma)) stop("MuEST failed: sigma must be provided for normal distribution")
    if (limtype == "lower") {
      mu <- limit - sigma * qnorm(p)
    } else {
      mu <- limit - sigma * qnorm(1 - p)
    }
    return(mu)
  }
  
  if (dist == "beta") {
    if (is.null(theta)) stop("MuEST failed: theta must be provided for beta distribution")
    
    # Handle edge case where limit = 0
    if (limit == 0) {
      if (limtype == "lower") {
        return(NA)  # No solution exists
      } else {
        return(1)  # If limit = 0 and "upper", mu should be 1
      }
    }
    
    # Define function to root-find based on limit type
    if (limtype == "lower") {
      f <- function(x) pbeta(limit, theta * x, theta * (1 - x)) - p
    } else {
      f <- function(x) 1 - pbeta(limit, theta * x, theta * (1 - x)) - p
    }
    
    # Find root numerically
    uniroot_result <- uniroot(f, interval = c(0, 1), tol = 1e-9)
    return(uniroot_result$root)
  }
}
