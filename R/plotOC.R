## -----------------------------------------------------------------------------
## plotOC.R --- 
##
## Author: Ha Truong
##
## Created: 09 Mar 2025
##
## Purposes: Generate and plot OC curves
##
## Changelogs:
## -----------------------------------------------------------------------------

#' @export
plotOC <- function(plan) {
  UseMethod("plotOC")
}

#' @export
plotOC.AttrPlan <- function(plan) {
  p <- seq(0, min(plan$CRQ*2, 1), length.out = 100)
  pa <- sapply(p, function(x) accProb(plan, x))
  
  plot(p, pa, type = "l", col = "blue", lwd = 2,
       main = "Attribute OC Curve",
       xlab = "Proportion Nonconforming", ylab = "P(accept)")
  abline(v = c(plan$PRQ, plan$CRQ), lty = 2, col = "gray")
  abline(h = c(1 - plan$alpha, plan$beta), lty = 2, col = "gray")
  grid()
}

#' @export
plotOC.VarPlan <- function(plan) {
  x <- seq(0, min(plan$CRQ*2, 1), length.out = 100)
  pa <- sapply(x, function(v) accProb(plan, v))
  xlab <- "Proportion Nonconforming"
  r_alpha <- 1 - accProb(plan, plan$PRQ)
  r_beta <- accProb(plan, plan$CRQ)
  
  plot(x, pa, type = "l", col = "red", lwd = 2,
       main = paste0("Variable OC Curve - ", plan$distribution, " distribution | n=", 
                    plan$sample_size, ", k=", plan$k),
       xlab = xlab, ylab = "P(accept)")
  abline(v = c(plan$PRQ, plan$CRQ), lty = 2, col = "gray")
  abline(h = c(1 - r_alpha, r_beta), lty = 2, col = "gray")
  grid()
}