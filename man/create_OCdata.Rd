\name{create_OCdata}
\alias{create_OCdata}
\title{Construct OC Curve Data from User Inputs}
\description{
  Constructs an \code{AttrPlan} or \code{VarPlan} object from the given parameters
  and returns the associated OC curve data using \code{OCdata()}.
}
\usage{
create_OCdata(plan = NULL, pd = NULL,
              distribution = c("binomial", "poisson", "normal", "beta"),
              n = NULL, c = NULL, k = NULL,
              USL = NULL, LSL = NULL, sigma = NULL, theta = NULL,
              PRQ = NULL, CRQ = NULL, alpha = NULL, beta = NULL,
              sigma_type = c("known", "unknown"),
              theta_type = c("known", "unknown"))
}
\arguments{
  \item{plan}{Optional plan object of class \code{AttrPlan} or \code{VarPlan}.
  If provided, overrides all other inputs.}
  
  \item{pd}{Optional vector of quality levels (proportions of nonconforming items).}

  \item{distribution}{One of \code{"binomial"}, \code{"poisson"}, \code{"normal"}, or \code{"beta"}.}

  \item{n}{Sample size.}

  \item{c}{Acceptance number (for attribute sampling).}

  \item{k}{Acceptability constant (for variable sampling).}

  \item{USL}{Upper specification limit.}

  \item{LSL}{Lower specification limit.}

  \item{sigma}{Standard deviation (for normal plans).}

  \item{theta}{Precision parameter (for beta plans).}

  \item{PRQ}{Producer’s risk quality level.}

  \item{CRQ}{Consumer’s risk quality level.}

  \item{alpha}{Producer’s risk.}

  \item{beta}{Consumer’s risk.}

  \item{sigma_type}{Either \code{"known"} or \code{"unknown"} (for normal).}

  \item{theta_type}{Either \code{"known"} or \code{"unknown"} (for beta).}
}
\details{
  This function provides a user-friendly wrapper to construct OC data directly
  from parameters, rather than requiring the user to manually create a plan object.

  Internally, it constructs the appropriate \code{AttrPlan} or \code{VarPlan},
  and dispatches to \code{\link{OCdata}}.
}
\value{
  An object of class \code{"OCdata"}.
}
\seealso{
  \code{\link{OCdata}}, \code{\link{optAttrPlan}}, \code{\link{optVarPlan}}
}
\examples{
# Attribute sampling with user-defined parameters
oc1 <- create_OCdata(n = 100, c = 2, distribution = "binomial")

# Variable sampling (normal)
oc2 <- create_OCdata(n = 30, k = 1.5, distribution = "normal", USL = 10, sigma = 1)

# Use with plan
plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, alpha = 0.05, beta = 0.1, distribution = "poisson")
oc3 <- create_OCdata(plan)
}
\author{
  Ha Truong
}
