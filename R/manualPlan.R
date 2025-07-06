## -----------------------------------------------------------------------------
## manualPlan.R --- 
##
## Author: Ha Truong
##
## Created: 07 July 2025
##
## Purposes: Generate munual varPlan/attrPlan objs from parameters
##
## Changelogs:
## -----------------------------------------------------------------------------


#' Create an OCdata object
#' @export
manualPlan <- function(distribution = c("binomial", "poisson", "normal", "beta"),
                       n = NULL, c = NULL, k = NULL,
                       USL = NULL, LSL = NULL, sigma = NULL, theta = NULL, 
                       PRQ = NULL, CRQ = NULL, alpha = NULL, beta = NULL,
                       sigma_type = c("known", "unknown"),
                       theta_type = c("known", "unknown")) {
  sigma_type <- match.arg(sigma_type)
  theta_type <- match.arg(theta_type)
  distribution <- match.arg(distribution)
  
  
  if (!(distribution %in% c("binomial", "poisson", "normal", "beta"))) {
    stop("Unsupported distribution.")
  }
  # Assign NULL first
  plan <- NULL
  
  if (distribution %in% c("binomial", "poisson")) {
    if (is.null(n) || is.null(c)) stop("n and c must be provided.")
    plan <- structure(list(n = n, c = c, sample_size = n,
                           PRQ = PRQ, CRQ = CRQ, PR = alpha, CR = beta,
                           USL = USL, LSL = LSL,
                           distribution = distribution),
                      class = "AttrPlan")
  }
  
  if (distribution %in% c("normal", "beta")) {
    if (is.null(n) || is.null(k)) stop("n and k must be provided.")
    if (distribution == "beta" && is.null(theta)) stop("theta must be provided.")
    if (distribution == "beta" && is.null(USL) && is.null(LSL)) stop("USL or LSL must be provided.")
    if (!is.null(USL) && !is.null(LSL)) stop("Specify only one limit (USL or LSL), not both.")
    
    plan <- structure(list(n = n, k = k, m = n, sample_size = n,
                           PRQ = PRQ, CRQ = CRQ, PR = alpha, CR = beta,
                           USL = USL, LSL = LSL,
                           sigma_type = sigma_type,
                           theta_type = theta_type,
                           sigma = sigma, theta = theta,
                           distribution = distribution),
                      class = "VarPlan")
  }
  
  # final return plan
  return(plan)
}