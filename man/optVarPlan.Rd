\name{optVarPlan}
\alias{optVarPlan}
\title{Variables Acceptance Sampling Plan}
\description{
  Creates Variable Acceptance Sampling plans for normal or beta distributed measurements.
}
\usage{
optVarPlan(PRQ, CRQ, alpha = 0.05, beta = 0.10, USL = NULL, LSL = NULL,
           distribution = c("normal", "beta"), sigma_type = c("known", "unknown"),
           theta_type = c("known", "unknown"), sigma = NULL, theta = NULL,
           method = c("delta_mle", "delta_mom", "gk_adjustment"))
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

  \item{method}{Unknown-theta Beta method: \code{"delta_mle"} (default),
  \code{"delta_mom"}, or \code{"gk_adjustment"}. It is only applicable when
  \code{distribution = "beta"} and \code{theta_type = "unknown"}.}
}
\details{
  The function generates variable acceptance sampling plans based on specified producer and consumer risks and either a normal or beta distribution model.

  The specification limit must be defined via either \code{USL} (upper specification limit) or \code{LSL} (lower specification limit), depending on whether the one-sided quality criterion concerns the upper or lower tail. Only one limit should be provided.

  For the \code{"normal"} model, the plan can be computed assuming the standard deviation 
  (\code{sigma}) is either known or estimated from data. Optimization for unknown \code{sigma} 
  is performed using the derivative-free Nelder-Mead method (Nelder and Mead, 1965) via 
  \code{\link[stats]{optim}} in base R.
  
  For the \code{"beta"} model, the dispersion (precision) parameter \code{theta} determines 
  how concentrated the distribution is around the mean. Users must either:
  \itemize{
    \item provide a known value for \code{theta}, typically obtained from prior process studies or historical data; or
    \item estimate it from sample data using a fitting function such as \code{\link[VGAM]{betaff}} in the \pkg{VGAM} package.
  }
  Optimization for Beta plans is performed using \code{\link[stats]{optim}} with the \code{"L-BFGS-B"} method
  (Byrd et al., 1995) to handle bounds on sample size and acceptance numbers, followed by a
  Nelder-Mead refinement that solves the two binding risk equations. For known-theta plans,
  the sample size is then rounded up from the binding solution and both risks are verified
  at the delivered (integer) size, so the plan meets \code{PR <= alpha} and \code{CR <= beta}
  as applied.

  For unknown precision, Delta--MLE uses inverse expected Fisher information
  and Delta--MoM uses analytical covariance propagated from the first four Beta
  raw moments. The Govindaraju--Kissling adjustment retains the package's
  earlier sample-size approximation.
  
  This approach ensures stable and efficient plan calculation while relying solely on base R functionality.
}

\value{
  A \code{VarPlan} object containing:
  \item{distribution}{Distribution used ("normal" or "beta").}
  \item{sample_size}{Final sample size after rounding up to the next integer, for practical application.}
  \item{k}{Acceptability constant.}
  \item{n}{The sample size the plan is evaluated at. For known-theta Beta
  plans this is the delivered integer size (equal to \code{sample_size});
  for other plans it is the unrounded value from the optimization.}
}
\references{
  * H. Truong, V. Miranda and R. Kissling (2026).
  AccSamplingDesign: An R Package for Optimizing Acceptance Sampling Plans.
  \emph{The R Journal}, 18(1), 368--381.
  \doi{10.32614/RJ-2026-007}.

  * ISO 3951-1:2013 - Sampling procedures for inspection by variables.
  
  * Wilrich, PT. (2004). Single Sampling Plans for Inspection by Variables under a Variance Component Situation. In: Lenz, HJ., Wilrich, PT. (eds) Frontiers in Statistical Quality Control 7. Physica, Heidelberg.
  
  * K. Govindaraju and R. Kissling (2015). Sampling plans for Beta-distributed compositional fractions.
  
  * J. A. Nelder and R. Mead. A simplex method for function minimization. The Computer Journal, 7(4): 308–313, 1965. DOI 10.1093/comjnl/7.4.308.
  
  * R. H. Byrd, P. Lu, J. Nocedal and C. Zhu. A limited memory algorithm for bound constrained optimization. SIAM Journal on Scientific Computing, 16(5): 1190–1208, 1995. DOI 10.1137/0916069.
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
