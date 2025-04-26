\name{optPlan}
\alias{optPlan}
\title{Optimal Acceptance Sampling Plan}
\description{
Design optimal variable acceptance sampling plans based on specified parameters. 
Supports different distributions (binomial, normal, beta) and accommodates known 
or unknown standard deviation and process parameters.
}
\usage{
optPlan(PRQ, CRQ, alpha = 0.05, beta = 0.10, USL = NULL, LSL = NULL,
        distribution = c("binomial", "poisson", "normal", "beta"),
        sigma_type = c("known", "unknown"),
        theta_type = c("known", "unknown"),
        sigma = NULL, theta = NULL,
        measurement_error = 0)
}
\arguments{
  \item{PRQ}{Producer's risk quality level (e.g., acceptable quality level).}
  \item{CRQ}{Consumer's risk quality level (e.g., rejectable quality level).}
  \item{alpha}{Producer's risk (Type I error), default is 0.05.}
  \item{beta}{Consumer's risk (Type II error), default is 0.10.}
  \item{USL}{Upper Specification Limit. Required for variable sampling plans.}
  \item{LSL}{Lower Specification Limit. Required for variable sampling plans.}
  \item{distribution}{Distribution type used in the plan. Can be \code{"binomial"}, \code{"normal"}, or \code{"beta"}.}
  \item{sigma_type}{Indicates if the standard deviation (\code{sigma}) is known or unknown.}
  \item{theta_type}{Indicates if the process parameter (\code{theta}) is known or unknown.}
  \item{sigma}{Known standard deviation of the process, if applicable.}
  \item{theta}{Known process parameter (e.g., mean), if applicable.}
  \item{measurement_error}{Amount of measurement error to include in the design (default is 0).}
}
\details{
This function designs optimal acceptance sampling plans by balancing producer's 
and consumer's risks under specified quality levels. It supports plans for 
attributes (binomial) and variables (normal or beta distributions), including 
cases with unknown standard deviation or distributional parameters.
}
\value{
Returns a list or data frame with optimal sample size(s) and critical value(s) 
based on the specified parameters and distribution.
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
