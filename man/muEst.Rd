\name{muEst}
\alias{muEst}
\title{Estimate Mean \eqn{\mu}{mu} Based on Specification Limits and Probability}
\description{
  Computes the estimated mean \eqn{\mu}{mu} for a given level of quality and specification limit under either a normal or beta distribution.
}
\usage{
muEst(p, USL = NULL, LSL = NULL, 
       sigma = NULL, theta = NULL, 
       dist = c("normal", "beta"))
}
\arguments{
  \item{p}{Level of quality (numeric, between 0 and 1).}

  \item{USL}{Upper specification limit (numeric). Only one of \code{USL} or \code{LSL} should be provided.}

  \item{LSL}{Lower specification limit (numeric). Only one of \code{USL} or \code{LSL} should be provided.}

  \item{sigma}{Standard deviation (numeric) for the normal distribution. Must be provided if \code{dist = "normal"}.}

  \item{theta}{Theta parameter (numeric) for the beta distribution. Must be provided if \code{dist = "beta"}.}

  \item{dist}{Distribution type. Either \code{"normal"} or \code{"beta"}.}
}
\details{
  The function estimates the mean \eqn{\mu}{mu} corresponding to a given tail probability \code{p}, assuming that the process output follows either a normal or beta distribution, and that the probability of being beyond the provided specification limit equals \code{1 - p}.

  \itemize{
    \item For the normal distribution, the mean is calculated using the inverse cumulative distribution function (quantile function) of the normal distribution.
    \item For the beta distribution, the mean is solved numerically such that the CDF at the specified limit equals \code{p}, given the shape determined by \code{theta}.
  }

  Exactly one of \code{USL} or \code{LSL} must be provided to define whether the probability refers to the upper or lower tail.
}
\value{
  Returns the estimated mean \eqn{\mu}{mu} as a numeric value.
}
\author{
Ha Truong
}
\examples{
  # Example for normal distribution with lower specification limit (LSL)
  muEst(p = 0.95, LSL = 10, sigma = 2, dist = "normal")

  # Example for beta distribution with upper specification limit (USL)
  muEst(p = 0.95, USL = 0.7, theta = 500, dist = "beta")
}
