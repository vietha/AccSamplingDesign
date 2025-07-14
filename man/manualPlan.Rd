\name{manualPlan}
\alias{manualPlan}
\title{Create manual acceptance sampling (AS) plan from User Inputs}
\description{
  Constructs an \code{AttrPlan} or \code{VarPlan} object from the given parameters.
}
\usage{
manualPlan(distribution = c("binomial", "poisson", "normal", "beta"),
           n = NULL, c = NULL, k = NULL,
           USL = NULL, LSL = NULL, sigma = NULL, theta = NULL,
           sigma_type = c("known", "unknown"),
           theta_type = c("known", "unknown"))
}
\arguments{
  \item{distribution}{One of \code{"binomial"}, \code{"poisson"}, \code{"normal"}, or \code{"beta"}.}

  \item{n}{Sample size.}

  \item{c}{Acceptance number (for attribute sampling).}

  \item{k}{Acceptability constant (for variable sampling).}

  \item{USL}{Upper specification limit.}

  \item{LSL}{Lower specification limit.}

  \item{sigma}{Standard deviation (for normal plans).}

  \item{theta}{Precision parameter (for beta plans).}

  \item{sigma_type}{Either \code{"known"} or \code{"unknown"} (for normal).}

  \item{theta_type}{Either \code{"known"} or \code{"unknown"} (for beta).}
}
\details{
  This function provides a user-friendly wrapper to construct AS plan directly
  from parameters.
  Internally, it constructs the appropriate \code{AttrPlan} or \code{VarPlan},
  from given paramenters.
}
\value{
  An object of class \code{"AttrPlan"} or \code{VarPlan}.
}
\seealso{
  \code{\link{optPlan}}, \code{\link{OCdata}}
}
\examples{
# Attribute sampling with user-defined parameters
plan1 <- manualPlan(n = 100, c = 2, distribution = "binomial")

# Variable sampling (normal)
plan2 <- manualPlan(n = 30, k = 1.5, distribution = "normal", USL = 10, sigma = 1)

}
\author{
  Ha Truong
}
