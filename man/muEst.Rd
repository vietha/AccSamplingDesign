\name{muEst}
\alias{muEst}
\title{Estimate Mean \eqn{\mu}{mu} Based on Limit and Probability}
\description{
  Computes the estimated mean \eqn{\mu}{mu} for a given level of quality and limit under either a normal or beta distribution.
}
\usage{
muEst(p, limit, sigma = NULL, theta = NULL, 
      dist = c("normal", "beta"), 
      limtype = c("lower", "upper"))
}
\arguments{
  \item{p}{Level of quality (numeric, between 0 and 1).}
  \item{limit}{The specification limit (numeric).}
  \item{sigma}{Standard deviation (numeric) for the normal distribution. Must be provided if \code{dist = "normal"}.}
  \item{theta}{Theta parameter (numeric) for the beta distribution. Must be provided if \code{dist = "beta"}.}
  \item{dist}{Distribution type. Either \code{"normal"} or \code{"beta"}.}
  \item{limtype}{Type of limit. Either \code{"lower"} (lower specification limit) or \code{"upper"} (upper specification limit).}
}
\details{
  For the normal distribution, the mean \eqn{\mu}{mu} is estimated using the inverse of the normal CDF.

  For the beta distribution, the mean \eqn{\mu}{mu} is found numerically by solving for \(x\) such that the cumulative probability matches the given \code{p}.
}
\value{
  Returns the estimated mean \eqn{\mu}{mu} as a numeric value.
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
Ha Truong
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
  # Example for normal distribution
  muEst(p = 0.95, limit = 10, sigma = 2, dist = "normal", limtype = "lower")

  # Example for beta distribution
  muEst(p = 0.95, limit = 0.7, theta = 500, dist = "beta", limtype = "upper")
}
