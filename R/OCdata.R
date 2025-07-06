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
    c = plan$c#,
    #k = numeric(0)
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
    #c = numeric(0),
    k = plan$k
  ), class = "OCdata")
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

#' 
#' #' @export
#' OCdata.paccept <- function(x) {
#'   stopifnot(inherits(x, "OCdata"))
#'   x$paccept
#' }
#' 
#' #' @export
#' OCdata.process_means <- function(x) {
#'   stopifnot(inherits(x, "OCdata"))
#'   x$process_means
#' }