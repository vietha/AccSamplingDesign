## -----------------------------------------------------------------------------
## accProb.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Calculate acceptance probability
##
## Changelogs:
## -----------------------------------------------------------------------------

#' @export
accProb <- function(plan, p) {
  UseMethod("accProb")
}

#' @export
accProb.AttrPlan <- function(plan, p) {
  if (plan$distribution == "binomial") {
    pbinom(plan$c, plan$n, p)
  } else if (plan$distribution == "poisson") {
    ppois(plan$c, lambda = plan$n * p)
  } else {
    stop("Unknown distribution type: ", plan$distribution)
  }
}

#' @export
accProb.VarPlan <- function(plan, p) {
  if(plan$distribution == "normal") {
    n = plan$n
    k = plan$k
    
    if(plan$sigma_type == "unknown") {
      #Pa <- pnorm( sqrt(n/(1 + k^2/2)) * (qnorm(1-p) - k) )
      # This use t-distribution
      Pa <- 1- pt(k*sqrt(n), df=n-1, ncp=-qnorm(p)*sqrt(n))
    }
    else{
      Pa <- 1 - pnorm(sqrt(n) * (qnorm(p) + k))
    }
    return(round(Pa, 4))
  } else { # for Beta distribution
    #m = plan$m
    if (!is.null(plan$m)) { m = plan$m } else {m = plan$n}
    k = plan$k
    if(plan$theta_type == "unknown") {
      #m = m/(1 + 0.85*k^2) # follow R&K 2015 simulations
      m = m/(1 + 0.4*k^2) # revise
    }
    
    # This upper limit case
    if (!is.null(plan$USL)) {
      limtype <- "upper"
      limit <- plan$USL
    } else if (!is.null(plan$LSL)) { # This lower limit case
      limtype <- "lower"
      limit <- plan$LSL
    } else {
      limtype <- "upper" # set this as default
    }
    
    theta <- plan$theta
    mu <- muEst(p, USL = plan$USL, LSL = plan$LSL,
                theta = theta, dist = plan$distribution)
    # Generate beta-distributed measurements
    a <- m * mu * theta
    b <- m * (1 - mu) * theta

    NSIM = 0
    if(NSIM > 0) # -------- This use simulation
    {
      ym <- rbeta(NSIM, a, b)
      var <- ym * (1 - ym) / theta # calculate variance VAR(Y)
      
      # Calculate acceptance criterion
      if (limtype == "lower") {
        acc_cri <- ym - k * sqrt(var)
        pa <- mean(acc_cri > limit)
      } else {
        acc_cri <- ym + k * sqrt(var)
        pa <- mean(acc_cri < limit)
      }
      return(pa)
    }
    else { #use closed-form
      # Compute quadratic coefficients
      A <- theta + k^2
      B <- -(2 * theta * limit + k^2)
      C <- theta * limit^2
      
      # Calculate discriminant
      discriminant <- (2 * theta * limit + k^2)^2 - 4 * A * C
      sqrt_disc <- sqrt(discriminant)
      
      # Compute roots
      z1 <- ( (2 * theta * limit + k^2) - sqrt_disc ) / (2 * A)
      z2 <- ( (2 * theta * limit + k^2) + sqrt_disc ) / (2 * A)
      
      # Determine Pa based on limit type
      if (limtype == "lower") {
        pa <- 1 - pbeta(z2, a, b)
      } else {
        pa <- pbeta(z1, a, b)
      }
      
      if (length(pa) != 1 || !is.numeric(pa) || is.na(pa)) {
        stop("No solution - Paccept can not be calculated base on input plan.")
      }
      return(pa)
    }
  }
}
