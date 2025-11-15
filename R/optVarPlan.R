## -----------------------------------------------------------------------------
## optVarPlan.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Design variable acceptance sampling plans (normal/beta)
##
## Changelogs:
## 19 Apr, 2025: added S3 plot method
## 20 Apr, 2025: remove spec_limit and limit_type. Replace by USL/LSL
## -----------------------------------------------------------------------------


# Define optimization function to minimize difference between Pa and target 
# values for alpha and beta (t-distribution)
t_optimize_sampling_plan <- function(alpha, beta, p_alpha, p_beta, n_init, k_init) 
{
  # Define function to calculate Pa based on the non-central t-distribution
  Pa_tdist <- function(n, k, p) {
    # enforce n >= 2 to have valid df = n - 1
    n <- max(2, n)
    # Calculate the acceptance probability using non-central t-distribution
    non_central_t <- pt(k * sqrt(n), df = n - 1, ncp = -qnorm(p) * sqrt(n))
    return(1 - non_central_t)
  }
  
  # Objective function to minimize
  objective_function <- function(params) {
    n_s <- params[1]
    k_s <- params[2]
    
    # Calculate Pa for both p_alpha and p_beta
    Pa_alpha <- Pa_tdist(n_s, k_s, p_alpha)
    Pa_beta <- Pa_tdist(n_s, k_s, p_beta)
    
    # Minimize the differences from target acceptance probabilities
    return(abs(Pa_alpha - (1 - alpha)) + abs(Pa_beta - beta))
  }
  
  # Initial guess for n_s and k_s
  init_params <- c(n_init, k_init)  # Example initial guess
  
  # Solve the optimization problem
  result <- optim(init_params, objective_function)
  
  # Handle cases where the optimization fails to find an optimal solution
  if (result$convergence != 0) {
    return(list(
      success = FALSE,
      code    = result$convergence,
      message = result$message
    ))
  }
  
  # Optimal plan found!
  return(list(success = TRUE, n = result$par[1], k = result$par[2]))
  #return(result$par)  # Return optimized n_s and k_s
}

#' Variable Acceptance Sampling Plan
#' @export
optVarPlan <- function(PRQ, CRQ, alpha = 0.05, beta = 0.10,
                       USL = NULL, LSL = NULL,
                       distribution = c("normal", "beta"),
                       sigma_type = c("known", "unknown"),
                       theta_type = c("known", "unknown"),
                       sigma = NULL, theta = NULL) {
  
  # Match arguments to ensure valid input
  distribution <- match.arg(distribution)
  sigma_type <- match.arg(sigma_type)
  theta_type <- match.arg(theta_type)
  
  # Set default if not provided
  if (is.null(sigma_type)) sigma_type <- "known"
  if (is.null(theta_type)) theta_type <- "known"
  
  if (!is.null(USL) && !is.null(LSL)) {
    stop("Double specification limits (both USL and LSL) are not supported. 
         Please specify only one limit: either USL or LSL.")
  }
  # This upper limit case
  if (!is.null(USL)) {
    limit_type <- "upper"
    spec_limit <- USL
  } else if (!is.null(LSL)) { # This lower limit case
    limit_type <- "lower"
    spec_limit <- LSL
  } else {
    limit_type <- "upper" # set this as default
  }
  
  # Ensure PRQ, CRQ, alpha, and beta are within valid ranges based on distribution
  check_quality <- function(q, distribution) {
    if (distribution == "normal" && (q < 0 || q > 1)) {
      return(FALSE)
    }
    if (distribution == "beta" && (q <= 0 || q >= 1)) {
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (!check_quality(PRQ, distribution) || !check_quality(CRQ, distribution)) {
    stop("PRQ and CRQ are out of bounds for the selected distribution.")
  }
  
  if (alpha <= 0 || alpha >= 1 || beta <= 0 || beta >= 1) {
    stop("alpha and beta must be between 0 and 1 (exclusive).")
  }
  
  # Ensure PRQ and CRQ are provided
  if (missing(PRQ) || missing(CRQ)) {
    stop("Producer and Consumer Risk Points must be provided.")
  }
  
  # Additional checks for beta distribution
  if (distribution == "beta") {
    if(is.null(USL) && is.null(LSL)){
      stop("For the beta distribution, a specification limit must be provided.")
    }
    if (is.null(theta)) {
      stop("For the beta distribution, theta must be provided.")
    }
  }
  
  # Ensure Consumer Risk Quality (CRQ) > Producer Risk Quality (PRQ)
  if (CRQ <= PRQ) {
    stop("Consumer Risk Quality (CRQ) must be greater than Producer Risk Quality (PRQ).")
  }
  
  # Check measurement error is non-negative
  # if (measurement_error < 0) {
  #   stop("measurement_error must be non-negative.")
  # }
  
  if (distribution == "normal") {
    # Define quantile functions for standard normal
    u_alpha <- qnorm(1 - alpha)
    u_beta <- qnorm(1 - beta)
    u_p1 <- qnorm(1 - PRQ)
    u_p2 <- qnorm(1 - CRQ)
    
    # Compute sample size n
    n <- ((u_alpha + u_beta) / (u_p1 - u_p2))^2
    # Compute acceptability constant k
    k <- (u_p1 * u_beta + u_p2 * u_alpha) / (u_alpha + u_beta)
    
    if(sigma_type == "unknown") {
      # find n_s by approximate from n
      # n <- ceiling(n) * (1 + k^2/2)
      
      # init value of n, k for optimization process
      n_init <- max(2, n)
      k_init <- k
      
      # # Get optimal n_s and k_s set n,k as init value for optimize function
      # optimal_params <- t_optimize_sampling_plan(alpha, beta, PRQ, CRQ, 
      #                                            n_init, k_init) 
      # # final plan (n,k) after optimized
      # n = optimal_params[1]
      # k = optimal_params[2]
      
      # Get optimal n_s and k_s by optimizing
      opt <- t_optimize_sampling_plan(alpha, beta, PRQ, CRQ, n_init, k_init)
      
      # Check if optimization was successful
      if (!opt$success) {
        stop(
          paste("Optimization failed:",
                opt$message,
                "Please check inputs: alpha/beta, PRQ/CRQ or try different initial values.")
        )
      }
      
      # If success, set final plan
      n <- opt$n
      k <- opt$k
    }
    
    sample_size <- n
    
    objPlan <- structure(list(n = n, k = k, sigma_type = sigma_type,
                              distribution = distribution), 
                         class = "VarPlan")
    
    r_alpha <- 1 - accProb(objPlan, PRQ)
    r_beta <- accProb(objPlan, CRQ)
    #m <- NA  # Not applicable for normal
  } else {
    
    # Objective function: minimize n 
    # objective_function <- function(params) {
    #   return(params[1])  # Minimizing sample size n
    # }
    
    # Constraint function: Ensure PR <= alpha and CR <= beta
    constraint_function <- function(params) {
      n <- params[1]
      k <- params[2]
      
      planObj <- structure(list(n = n, k = k, USL = USL, LSL = LSL,
                                theta = theta, theta_type = "known",
                                distribution = distribution), 
                           class = "VarPlan")
      
      PR <- 1 - accProb(planObj, PRQ)
      CR <- accProb(planObj, CRQ)
      
      # Apply large penalty if constraints are violated
      penalty <- (pmax(PR - alpha, 0))^2 + (pmax(CR - beta, 0))^2 # smoother penalty 
      return(n + 1e4 * penalty)   # (objective + penalty combined)
    }
    
    # Get initial estimates using normal distribution
    normal_plan <- optVarPlan(PRQ = PRQ, CRQ = CRQ, distribution = "normal")
    
    # Define search ranges
    n_range <- c(floor(0.5 * normal_plan$n), ceiling(2 * normal_plan$n))
    k_range <- c(max(0.5 * normal_plan$k, 0.1), 2 * normal_plan$k)
    
    # Initial guess 
    initial_guess <- c(normal_plan$n, normal_plan$k)
    
    #Run optimization using optim (L-BFGS-B supports bounds)
    result <- optim(
      par = initial_guess,
      fn = constraint_function,
      method = "L-BFGS-B",
      lower = c(n_range[1], k_range[1]),
      upper = c(n_range[2], k_range[2])
    )
    
    if (result$convergence == 0) {
      # full success
      n_opt <- result$par[1]
      k_opt <- result$par[2]
      
    } else if (result$convergence == 51) {
      # warning but solution likely usable
      warning("Optimization returned a warning (code 51): ", result$message)
      n_opt <- result$par[1]
      k_opt <- result$par[2]
      
    } else if (result$convergence == 52) {
      # error — check if par is still valid
      if (all(is.finite(result$par))) {
        warning("Optimization returned a warning (code 52): ", result$message)
        n_opt <- result$par[1]
        k_opt <- result$par[2]
      } else {
        stop("Optimization failed with convergence code 52: ", result$message)
      }
      
    } else {
      stop("Optimization failed with convergence code:", result$convergence)
    }
    
    # Extract optimal values
    # n <- result$par[1]
    # k <- result$par[2]
    n <- n_opt
    k <- k_opt
    
    if(theta_type == "unknown") {
      ## This R ratio from paper of Govindaraju and Kissling (2015)
      R_ratio = (1 + 0.85*k^2)
      n <- ceiling(n)*R_ratio
    }
    
    objPlan <- structure(list(n = n, k = k, USL = USL, LSL = LSL,
                              theta = theta, theta_type = theta_type,
                              distribution = distribution), 
                         class = "VarPlan")
    
    r_alpha <- 1 - accProb(objPlan, PRQ)
    r_beta <- accProb(objPlan, CRQ)
    sample_size <- n
    #n <- m  # Not applicable for beta
  }
  
  return(structure(
    list(
      distribution = distribution,
      sigma = sigma, theta = theta,
      sigma_type = sigma_type, theta_type = theta_type,
      PRQ = PRQ, CRQ = CRQ, PR = r_alpha, CR = r_beta,
      USL = USL, LSL = LSL,
      #spec_limit = spec_limit, limtype = limit_type,
      sample_size = ceiling(sample_size), # round up the sample size for practical;
      n = n, # we keep the number before round up
      #m = m, # we keep the number before round up
      k = k
    ), 
    class = "VarPlan"
  ))
}

#' @export
plot.VarPlan <- function(x, pd = NULL, by = c("pd", "mean"), ...) {
  by <- match.arg(by)
  
  # Default pd if not supplied
  if (is.null(pd)) {
    pd <- seq(max(x$PRQ * 0.5, 1e-10), min(x$CRQ * 1.5, 1), length.out = 100)
  }
  
  if (is.null(x$sigma_type)) {
    stop("sigma_type must be provided to plot a VarPlan")
  }
  
  if (by == "pd") {
    pa <- sapply(pd, function(p) accProb(x, p))
    plot(pd, pa, type = "l", col = "red", lwd = 2,
         main = paste0("Variables Sampling OC Curve by Pd",
                       " | n=", x$sample_size, ", k=", round(x$k,3), " | ", x$distribution),
         xlab = "Proportion Nonconforming", ylab = "P(accept)", ...)
    abline(v = c(x$PRQ, x$CRQ), lty = 2, col = "gray")
    abline(h = c(1 - x$PR, x$CR), lty = 2, col = "gray")
    grid()
    
  } else if (by == "mean") {
    # Check that spec limits and limit_type exist
    if (is.null(x$USL) && is.null(x$LSL)) {
      stop("Mean-level plot requires 'USL' or 'LSL' in the plan.")
    }
    # estimate mu from PRQ/CRQ
    mu_PRQ <- muEst(x$PRQ, USL = x$USL, LSL = x$LSL, sigma = x$sigma, 
                    theta = x$theta, dist = x$distribution)
    mu_CRQ <- muEst(x$CRQ, USL = x$USL, LSL = x$LSL, sigma = x$sigma, 
                    theta = x$theta, dist = x$distribution)      
    # Estimate mean levels based on pd
    mu_vals <- sapply(pd, function(p) muEst(
      p, USL = x$USL, LSL = x$LSL,
      sigma = x$sigma, theta = x$theta, 
      dist = x$distribution
    ))
    
    if (any(is.na(mu_vals))) {
      stop("Some mean estimates could not be computed. Check the pd or spec limits.")
    }
    
    pa <- sapply(pd, function(p) accProb(x, p))
    plot(mu_vals, pa, type = "l", col = "blue", lwd = 2,
         main = paste0("Variables Sampling OC Curve by Mean",
                       " | n=", x$sample_size, ", k=", round(x$k,3), " | ", x$distribution),
         xlab = "Process Mean", ylab = "P(accept)", ...)
    abline(v = c(mu_PRQ, mu_CRQ), lty = 2, col = "gray")
    abline(h = c(1 - x$PR, x$CR), lty = 2, col = "gray")
    grid()
  }
}
