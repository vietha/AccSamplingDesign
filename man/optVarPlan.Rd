\name{optVarPlan}
\alias{optVarPlan}
\title{Variables Acceptance Sampling Plan}
\description{
  Creates Variable Acceptance Sampling plans for normal or beta distributed measurements.
}
\usage{
optVarPlan(PRQ, CRQ, alpha = 0.05, beta = 0.10, USL = NULL, LSL = NULL,
           distribution = c("normal", "beta"), sigma_type = c("known", "unknown"),
           theta_type = c("known", "unknown"), sigma = NULL, theta = NULL)
}
\arguments{
  \item{PRQ}{Producer Risk Quality (must be within valid range for the chosen distribution).}
  
  \item{CRQ}{Consumer Risk Quality (must be greater than PRQ and within valid range).}
  
  \item{alpha}{Producer's risk (numeric between 0 and 1).}
  
  \item{beta}{Consumer's risk (numeric between 0 and 1).}
  
  \item{USL}{Upper Specification Limit (numeric). Only one of \code{USL} or \code{LSL} should be provided.}
  
  \item{LSL}{Lower Specification Limit (numeric). Only one of \code{USL} or \code{LSL} should be provided.}
  
  \item{distribution}{Measurement distribution: \code{"normal"} or \code{"beta"}.}
  
  \item{sigma_type}{Indicates whether sigma (population standard deviation) is \code{"known"} or \code{"unknown"}.}
  
  \item{theta_type}{Indicates whether theta (population precision parameter for beta) is \code{"known"} or \code{"unknown"}.}
  
  \item{sigma}{Known standard deviation (used for normal distribution). Required if \code{sigma_type = "known"}.}
  
  \item{theta}{Precision (dispersion) parameter for the beta distribution.
  If unknown, it can be estimated from historical data using functions such as \code{\link[VGAM]{betaff}}.}
}
\details{
  The function generates variable acceptance sampling plans based on specified producer and consumer risks and either a normal or beta distribution model.

  The specification limit must be defined via either \code{USL} (upper specification limit) or \code{LSL} (lower specification limit), depending on whether the one-sided quality criterion concerns the upper or lower tail. Only one limit should be provided.

  For the \code{"normal"} model, the plan can be computed assuming the standard deviation 
  (\code{sigma}) is either known or estimated from data. Optimization for unknown \code{sigma} 
  is performed using the derivative-free Nelder-Mead method (Nelder and Mead, 1965) via 
  \code{\link[base]{optim}} in base R.
  
  For the \code{"beta"} model, the dispersion (precision) parameter \code{theta} determines 
  how concentrated the distribution is around the mean. Users must either:
  \itemize{
    \item provide a known value for \code{theta}, typically obtained from prior process studies or historical data; or
    \item estimate it from sample data using a fitting function such as \code{\link[VGAM]{betaff}} in the \pkg{VGAM} package.
  }
  Optimization for Beta plans is performed using \code{\link[base]{optim}} with the \code{"L-BFGS-B"} method 
  (Byrd et al., 1995) to handle bounds on sample size and acceptance numbers.
  
  This approach ensures stable and efficient plan calculation while relying solely on base R functionality.
}

\value{
  A \code{VarPlan} object containing:
  \item{distribution}{Distribution used ("normal" or "beta").}
  \item{sample_size}{Final sample size after rounding up to the next integer, for practical application.}
  \item{k}{Acceptability constant.}
  \item{n}{Unrounded sample size.}
}
\references{
  ISO 3951-1:2013 - Sampling procedures for inspection by variables.
  
  Wilrich, PT. (2004). Single Sampling Plans for Inspection by Variables under a Variance Component Situation. In: Lenz, HJ., Wilrich, PT. (eds) Frontiers in Statistical Quality Control 7. Physica, Heidelberg. \doi{10.1007/978-3-7908-2674-6_4}
  
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
  distribution = "normal",
  USL = 10
)
summary(norm_plan)

# Example for beta distribution plan
beta_plan <- optVarPlan(
  PRQ = 0.025,        # Target quality level (% nonconforming)
  CRQ = 0.1,          # Minimum quality level (% nonconforming)
  alpha = 0.05,       # Producer's risk
  beta = 0.1,         # Consumer's risk
  distribution = "beta",
  theta = 44000000,   # Beta distribution parameter
  LSL = 0.00001
)
summary(beta_plan)
}
