\name{optVarPlan}
\alias{optVarPlan}
\title{Variable Acceptance Sampling Plan}
\description{
  Creates variable sampling plans for normal/beta distributed measurements.
}
\usage{
optVarPlan(PRQ, CRQ, spec_limit = NULL, distribution = c("normal", "beta"),
          limit_type = c("upper", "lower"), sigma_type = c("known", "unknown"),
          theta_type = c("known", "unknown"), sigma = NULL, theta = NULL,
          alpha = 0.05, beta = 0.10, measurement_error = 0)
}
\arguments{
  \item{PRQ}{Producer Risk Quality (must be within valid range for the chosen distribution).}
  \item{CRQ}{Consumer Risk Quality (must be greater than PRQ and within valid range).}
  \item{spec_limit}{Numerical specification limit (required for beta distribution).}
  \item{distribution}{Measurement distribution: \code{"normal"} or \code{"beta"}.}
  \item{limit_type}{Specification limit direction: \code{"upper"} or \code{"lower"}.}
  \item{sigma_type}{Indicates whether sigma (population standard deviation) is \code{"known"} or \code{"unknown"}.}
  \item{theta_type}{Indicates whether theta (population precision parameter for beta) is \code{"known"} or \code{"unknown"}.}
  \item{sigma}{Known standard deviation (normal distribution).}
  \item{theta}{Dispersion parameter (beta distribution, required when using beta).}
  \item{alpha}{Producer's risk (must be between 0 and 1).}
  \item{beta}{Consumer's risk (must be between 0 and 1).}
  \item{measurement_error}{Measurement error ratio (default is 0).}
}
\value{
  A \code{VarPlan} object containing:
  \item{distribution}{Used distribution.}
  \item{spec_limit}{Input limit value.}
  \item{k}{Acceptance limit.}
  \item{n}{Sample size for normal distribution.}
  \item{m}{Sample size for beta distribution.}
}

\references{
ISO 3951-1:2013 - Sampling procedures for inspection by variables  

Wilrich, PT. (2004). Single Sampling Plans for Inspection by Variables under a Variance Component Situation. In: Lenz, HJ., Wilrich, PT. (eds) Frontiers in Statistical Quality Control 7. Frontiers in Statistical Quality Control, vol 7. Physica, Heidelberg. https://doi.org/10.1007/978-3-7908-2674-6_4

K. Govindaraju and R. Kissling (2015). Sampling plans for Beta-distributed compositional fractions.
}
\author{
Ha Truong
}


\examples{
# Example for normal distribution plan
norm_plan <- optVarPlan(
  PRQ = 0.025,        # Acceptable quality level (% nonconforming)
  CRQ = 0.1,          # Rejectable quality level (% nonconforming)
  alpha = 0.05,       # Producer's risk
  beta = 0.1,         # Consumer's risk
  distribution = "normal"
)

# Example for beta distribution plan
beta_plan <- optVarPlan(
  PRQ = 0.025,        # Target quality level (% nonconforming)
  CRQ = 0.1,          # Minimum quality level (% nonconforming)
  alpha = 0.05,       # Producer's risk
  beta = 0.1,         # Consumer's risk
  distribution = "beta",
  theta = 44000000,   # Beta distribution parameter
  spec_limit = 0.00001, # Lower reliability limit
  limit_type = "lower"
)
}
