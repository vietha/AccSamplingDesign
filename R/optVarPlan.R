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

#' Variable Acceptance Sampling Plan
#' @export
optVarPlan <- function(PRQ, CRQ, alpha = 0.05, beta = 0.10,
                       distribution = c("normal", "beta"),
                       USL = NULL, LSL = NULL,
                       sigma_type = c("known", "unknown"),
                       theta_type = c("known", "unknown"),
                       sigma = NULL, theta = NULL,
                       measurement_error = 0) {
  
  # Match arguments to ensure valid input
  distribution <- match.arg(distribution)
  sigma_type <- match.arg(sigma_type)
  theta_type <- match.arg(theta_type)

  # Set default if not provided
  if (is.null(sigma_type)) sigma_type <- "known"
  if (is.null(theta_type)) theta_type <- "known"
  
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
  if (measurement_error < 0) {
    stop("measurement_error must be non-negative.")
  }
  
  if (distribution == "normal") {
    # Define quantile functions for standard normal
    u_alpha <- qnorm(1 - alpha)
    u_beta <- qnorm(1 - beta)
    u_p1 <- qnorm(1 - PRQ)
    u_p2 <- qnorm(1 - CRQ)
    
    # Compute sample size n
    n <- ((u_alpha + u_beta) / (u_p1 - u_p2))^2
    # Compute acceptance constant k
    k <- (u_p1 * u_beta + u_p2 * u_alpha) / (u_alpha + u_beta)

    if(sigma_type == "unknown") {
      # adjust k base on sample standard deviation
      n <- ceiling(n) * (1 + k^2/2)
    }
    
    sample_size <- n
    
    objPlan <- structure(list(n = n, k = k, sigma_type = sigma_type,
                              distribution = distribution), 
                              class = "VarPlan")
    
    r_alpha <- 1 - accProb(objPlan, PRQ)
    r_beta <- accProb(objPlan, CRQ)
    m <- NA  # Not applicable for normal
  } else {
    # nsim = 5000 
    # grid_step = c(0.5, 0.02)
    # p1 <- PRQ
    # p2 <- CRQ
    # # Get normal plan for initial estimates
    # normal_plan <- optVarPlan(PRQ = p1, CRQ = p2, 
    #                           spec_limit = spec_limit, 
    #                           distribution = "normal",
    #                           limit_type = limit_type)
    # # Set search ranges if not provided
    # m_range <- c(floor(0.5 * normal_plan$n), ceiling(2 * normal_plan$n))
    # k_range <- c(max(0.5 * normal_plan$k, 0.1), 2 * normal_plan$k)
    # 
    # # Create parameter grid
    # m_values <- seq(m_range[1], m_range[2], by = grid_step[1])
    # k_values <- seq(k_range[1], k_range[2], by = grid_step[2])
    # search_grid <- expand.grid(m = m_values, k = k_values)
    # 
    # # Risk calculation function
    # calculate_risks <- function(m, k) {
    #   planObj <- structure(list(m = m, k = k, spec_limit = spec_limit, theta = theta,
    #                             distribution = distribution, limtype = limit_type), 
    #                        class = "VarPlan")
    #   PR <- 1 - accProb(planObj, p1)
    #   CR <- accProb(planObj, p2)
    #   return(c(CR = CR, PR = PR))
    # }
    # 
    # # Evaluate all grid points
    # risks <- mapply(calculate_risks, search_grid$m, search_grid$k)
    # search_grid$CR <- risks[1, ]
    # search_grid$PR <- risks[2, ]
    # 
    # # Filter valid plans
    # valid_plans <- subset(search_grid, CR <= beta & PR <= alpha)
    # valid_plans <- valid_plans[order(valid_plans$m, valid_plans$k), ]
    # 
    # # Not found any optimal plan
    # if (nrow(valid_plans) == 0)   return(NULL) #stop("No valid plan found in search range")
    # 
    # # Return optimal plan
    # # Now we found the optimal beta plan here
    # opt_plan <- as.list(valid_plans[1, ])
    # k = opt_plan$k
    # m = opt_plan$m 
    # r_beta = opt_plan$CR 
    # r_alpha = opt_plan$PR 
    
    # Objective function: minimize m
    objective_function <- function(params) {
      return(params[1])  # Minimizing m
    }
    
    # Constraint function: Ensure PR <= alpha and CR <= beta
    constraint_function <- function(params) {
      m <- params[1]
      k <- params[2]
      
      planObj <- structure(list(m = m, k = k, USL = USL, LSL = LSL,
                                theta = theta, theta_type = "known",
                                distribution = distribution), 
                           class = "VarPlan")
      
      PR <- 1 - accProb(planObj, PRQ)
      CR <- accProb(planObj, CRQ)
      
      # Apply large penalty if constraints are violated
      penalty <- sum(pmax(PR - alpha, 0)) + sum(pmax(CR - beta, 0))
      return(params[1] + 1000 * penalty) 
    }
    
    # Get initial estimates using normal distribution
    normal_plan <- optVarPlan(PRQ = PRQ, CRQ = CRQ, distribution = "normal")
    
    # Define search ranges
    m_range <- c(floor(0.5 * normal_plan$n), ceiling(2 * normal_plan$n))
    k_range <- c(max(0.5 * normal_plan$k, 0.1), 2 * normal_plan$k)
    
    # Initial guess 
    initial_guess <- c(normal_plan$n, normal_plan$k)
    
    # Run optimization using optim (L-BFGS-B supports bounds)
    result <- optim(
      par = initial_guess,
      fn = constraint_function,
      method = "L-BFGS-B",
      lower = c(m_range[1], k_range[1]),
      upper = c(m_range[2], k_range[2])
    )
    # Extract optimal values
    m <- result$par[1]
    k <- result$par[2]
    
    if(theta_type == "unknown") {
      
      ## This R ratio from paper of Govindaraju and Kissling (2015)
      #R_ratio = (1 + 0.85*k^2)
      ## This R ration from my simulation
      R_ratio = (1 + 0.5*k^2)
      # By theoretically motivated adjustment 
      #R_ratio <- (1 + k^2)/2
      
      m <- ceiling(m)*R_ratio
    }
    
    objPlan <- structure(list(m = m, k = k, USL = USL, LSL = LSL,
                              theta = theta, theta_type = theta_type,
                              distribution = distribution), 
                         class = "VarPlan")
    
    r_alpha <- 1 - accProb(objPlan, PRQ)
    r_beta <- accProb(objPlan, CRQ)
    sample_size <- m
    n <- NA  # Not applicable for beta
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
      m = m, # we keep the number before round up
      k = round(k, 3)
    ), 
    class = "VarPlan"
  ))
}

#' @export
plot.VarPlan <- function(x, pd = NULL, by = c("pd", "mean"), ...) {
  by <- match.arg(by)
  
  # Default pd if not supplied
  if (is.null(pd)) {
    pd <- seq(1e-10, min(x$CRQ * 2, 1), length.out = 100)
  }
  
  if (by == "pd") {
    pa <- sapply(pd, function(p) accProb(x, p))
    plot(pd, pa, type = "l", col = "red", lwd = 2,
         main = paste0("Variable OC Curve - ", x$distribution,
                       " distribution | n=", x$sample_size, ", k=", x$k),
         xlab = "Proportion Nonconforming", ylab = "P(accept)", ...)
    abline(v = c(x$PRQ, x$CRQ), lty = 2, col = "gray")
    abline(h = c(1 - x$PR, x$CR), lty = 2, col = "gray")
    grid()
    
  } else if (by == "mean") {
    # Check that spec limits and limit_type exist
    if (is.null(x$USL) && is.null(x$LSL)) {
      stop("Mean-level plot requires 'USL' or 'LSL' in the plan.")
    }
    
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
         main = paste0("Variable OC Curve by Mean - ", x$distribution,
                       " | n=", x$sample_size, ", k=", x$k),
         xlab = "Process Mean", ylab = "P(accept)", ...)
    grid()
  }
}
