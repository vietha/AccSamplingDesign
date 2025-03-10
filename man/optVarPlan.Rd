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
  \item{PRQ}{Producer Risk Quality (must be within valid range for the chosen distribution)}
  \item{CRQ}{Consumer Risk Quality (must be greater than PRQ and within valid range)}
  \item{spec_limit}{Numerical specification limit (required for beta distribution)}
  \item{distribution}{Measurement distribution: "normal" or "beta"}
  \item{limit_type}{Specification limit direction: "upper" or "lower"}
  \item{sigma_type}{Indicates whether sigma (population standard deviation) is "known" or "unknown"}
  \item{theta_type}{Indicates whether theta (population precision parameter for beta) is "known" or "unknown"}
  \item{sigma}{Known standard deviation (normal distribution)}
  \item{theta}{Dispersion parameter (beta distribution, required when using beta)}
  \item{alpha}{Producer's risk (must be between 0 and 1)}
  \item{beta}{Consumer's risk (must be between 0 and 1)}
  \item{measurement_error}{Measurement error ratio (default is 0)}
}
\value{
VarPlan object containing:
\item{distribution}{Used distribution}
\item{spec_limit}{Input limit value}
\item{k}{Acceptance limit}
\item{n}{Sample size for normal distribution}
\item{m}{Sample size for beta distribution}
}
\examples{
plan <- optVarPlan(PRQ = 0.01, CRQ = 0.1, spec_limit = 10,
                  distribution = "normal", sigma_type = "known", sigma = 0.5)
}
