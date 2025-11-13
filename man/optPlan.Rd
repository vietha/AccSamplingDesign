\name{optPlan}
\alias{optPlan}
\title{Optimal Acceptance Sampling Plan}
\description{
Design optimal variable acceptance sampling plans based on specified parameters. 
Supports different distributions (binomial, poisson, normal, beta) and accommodates known 
or unknown standard deviation and process parameters.
}
\usage{
optPlan(PRQ, CRQ, alpha = 0.05, beta = 0.10, USL = NULL, LSL = NULL,
        distribution = c("binomial", "poisson", "normal", "beta"),
        sigma_type = c("known", "unknown"),
        theta_type = c("known", "unknown"),
        sigma = NULL, theta = NULL)
}
\arguments{
  \item{PRQ}{Producer's risk quality level (e.g., acceptable quality level).}
  \item{CRQ}{Consumer's risk quality level (e.g., rejectable quality level).}
  \item{alpha}{Producer's risk (Type I error), default is 0.05.}
  \item{beta}{Consumer's risk (Type II error), default is 0.10.}
  \item{USL}{Upper Specification Limit. Required for variable sampling plans.}
  \item{LSL}{Lower Specification Limit. Required for variable sampling plans.}
  \item{distribution}{Distribution type used in the plan. Can be \code{"binomial"}, \code{"poisson"}, \code{"normal"}, or \code{"beta"}.}
  \item{sigma_type}{Indicates if the standard deviation (\code{sigma}) is known or unknown.}
  \item{theta_type}{Indicates if the process parameter (\code{theta}) is known or unknown.}
  \item{sigma}{Known standard deviation of the process, if applicable.}
  \item{theta}{Precision (dispersion) parameter for the beta distribution.
  If unknown, it can be estimated from historical data using functions such as \code{\link[VGAM]{betaff}}.}
}
\details{
This function designs optimal acceptance sampling plans by balancing producer's 
and consumer's risks under specified quality levels. It supports plans for 
attributes (binomial) and variables (normal or beta distributions), including 
cases with unknown standard deviation or distributional parameters.

  For the \code{"normal"} model, the plan can be computed assuming the standard deviation (\code{sigma}) is either known or estimated from data.  
  For the \code{"beta"} model, the dispersion (precision) parameter \code{theta} determines how concentrated the distribution is around the mean.  
  Users must either:
  \itemize{
    \item provide a known value for \code{theta}, typically obtained from prior process studies or historical data; or
    \item estimate it from sample data using a fitting function such as \code{\link[VGAM]{betaff}} in the \pkg{VGAM} package.
  }

  For example:
  \preformatted{
    library(VGAM)
    fit <- vglm(y ~ 1, betaff, data = mydata)
    coef(fit, matrix = TRUE)
  }
  The output gives the estimated mean (\eqn{\mu}) and precision (\eqn{\theta}) parameters for the fitted Beta distribution. The estimated \eqn{\theta} can then be supplied to \code{optPlan(theta = ...)}.

  The plan design accounts for known or unknown standard deviation in the normal case, and known or unknown precision parameter (\code{theta}) in the beta case.
}
\value{
Returns a list or data frame with optimal sample size(s) and critical value(s) 
based on the specified parameters and distribution.
  \item{distribution}{Selected distribution}
  \item{sample_size}{Final sample size after rounding up to the next integer, for practical application.}
  \item{n}{Sample size (unrounded for Variables Acceptance Sampling).}
  \item{k}{Acceptability constant - For Variables Acceptance Sampling.}
  \item{c}{Acceptance number - For Attributes Acceptance Sampling.}
}
\author{
Ha Truong
}
\examples{
# Example usage (normal distribution, known sigma):
optPlan(PRQ = 0.005, CRQ = 0.03, alpha = 0.05, beta = 0.10, 
        distribution = "normal", sigma_type = "known")

# Example usage (beta distribution, unknown theta):
optPlan(PRQ = 0.025, CRQ = 0.10, alpha = 0.05, beta = 0.10, 
        distribution = "beta", theta = 6.6e8, 
        theta_type = "unknown", LSL = 5.65e-6)
}
