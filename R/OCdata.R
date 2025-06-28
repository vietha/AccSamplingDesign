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
  UseMethod("OCdata")
}

#' @export
OCdata.AttrPlan <- function(plan, pd = NULL) {
  if (is.null(pd)) {
    pd <- seq(1e-10, min(plan$CRQ * 2, 1), length.out = 100)
  }
  paccept <- sapply(pd, function(p) accProb(plan, p))
  
  structure(list(
    pd = pd,
    paccept = paccept,
    process_means = numeric(0),
    dist = plan$distribution,
    n = plan$sample_size,
    c = plan$c,
    k = numeric(0)
  ), class = "OCdata")
}

#' @export
OCdata.VarPlan <- function(plan, pd = NULL) {
  if (is.null(pd)) {
    pd <- seq(1e-10, min(plan$CRQ * 2, 1), length.out = 100)
  }
  paccept <- sapply(pd, function(p) accProb(plan, p))
  
  mean_level <- NULL
  if (!is.null(plan$USL) || !is.null(plan$LSL)) {
    mean_level <- sapply(pd, function(p) muEst(
      p, USL = plan$USL, LSL = plan$LSL,
      sigma = plan$sigma,
      theta = plan$theta,
      dist = plan$distribution
    ))
  }
  
  structure(list(
    pd = pd,
    paccept = paccept,
    process_means = mean_level,
    dist = plan$distribution,
    n = plan$sample_size,
    c = numeric(0),
    k = plan$k
  ), class = "OCdata")
}


#' Create an OCdata object
#' @export
create_OCdata <- function(plan = NULL, pd = NULL,
                   distribution = c("binomial", "poisson", "normal", "beta"),
                   n = NULL, c = NULL, k = NULL,
                   USL = NULL, LSL = NULL, sigma = NULL, theta = NULL, 
                   PRQ = NULL, CRQ = NULL, alpha = NULL, beta = NULL,
                   sigma_type = c("known", "unknown"),
                   theta_type = c("known", "unknown")) {
  sigma_type <- match.arg(sigma_type)
  theta_type <- match.arg(theta_type)
  distribution <- match.arg(distribution)
  
  if (!is.null(plan)) {
    if (inherits(plan, "AttrPlan")) return(OCdata.AttrPlan(plan, pd))
    if (inherits(plan, "VarPlan")) return(OCdata.VarPlan(plan, pd))
    stop("Unsupported plan class.")
  }
  
  if (distribution %in% c("binomial", "poisson")) {
    if (is.null(n) || is.null(c)) stop("n and c must be provided.")
    plan <- structure(list(n = n, c = c, sample_size = n,
                           PRQ = PRQ, CRQ = CRQ, PR = alpha, CR = beta,
                           USL = USL, LSL = LSL,
                           distribution = distribution),
                      class = "AttrPlan")
    return(OCdata.AttrPlan(plan, pd))
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
    return(OCdata.VarPlan(plan, pd))
  }
  
  stop("Unsupported distribution.")
}

# S3 methods for OCdata
#' @export
print.OCdata <- function(x, ...) {
  cat("OCdata object:\n")
  cat(" Distribution:", x$dist, "\n")
  cat(" Sample size (n):", x$n, "\n")
  if (length(x$k) > 0) cat(" Acceptability constant (k):", x$k, "\n")
  if (length(x$c) > 0) cat(" Acceptance number (c):", x$c, "\n")
}

#' @export
summary.OCdata <- function(object, ...) {
  cat("Summary of OCdata:\n")
  print(object)
  cat(" # of pd values:", length(object$pd), "\n")
  cat(" # of P(accept) values:", length(object$paccept), "\n")
  if (length(object$process_means) > 0) {
    cat(" Process means available (length:", length(object$process_means), ")\n")
  }
}

#' @export
plot.OCdata <- function(x, by = c("pd", "mean"), ...) {
  by <- match.arg(by)
  
  if (by == "pd") {
    plot(x$pd, x$paccept, type = "l", col = "red", lwd = 2,
         main = "OC Curve by Proportion Nonconforming", 
         xlab = "Proportion Nonconforming", ylab = "P(accept)", ...)
    grid()
  } else {
    if (length(x$process_means) > 0) {
      plot(x$process_means, x$paccept, type = "l", col = "blue", lwd = 2,
           main = "OC Curve by Mean Levels", xlab = "Mean Level", ylab = "P(accept)", ...)
      grid()
    } else {
      message("Mean-level plot not available.")
    }
  }
}

#' @export
as.list.OCdata <- function(x, ...) {
  stopifnot(inherits(x, "OCdata"))
  unclass(x)
}

#' @export
as.data.frame.OCdata <- function(x, row.names = NULL, optional = FALSE, ...) {
  stopifnot(inherits(x, "OCdata"))
  
  df <- data.frame(
    pd = x$pd,
    paccept = x$paccept,
    stringsAsFactors = FALSE
  )
  
  if (!is.null(x$process_means) && length(x$process_means) == length(x$pd)) {
    df$process_means <- x$process_means
  }
  
  df
}

# Accessor functions for OCdata

#' @export
paccept <- function(x) {
  stopifnot(inherits(x, "OCdata"))
  x$paccept
}

#' @export
pd <- function(x) {
  stopifnot(inherits(x, "OCdata"))
  x$pd
}

#' @export
process_means <- function(x) {
  stopifnot(inherits(x, "OCdata"))
  x$process_means
}