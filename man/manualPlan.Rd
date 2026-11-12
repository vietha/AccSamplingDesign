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
           theta_type = c("known", "unknown"),
           method = c("delta_mle", "delta_mom", "gk_adjustment",
                      "saddlepoint_mle", "saddlepoint_mom"))
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

  \item{method}{Method used by an unknown-theta Beta plan. Delta--MLE is the
  default; alternatives are \code{"delta_mom"}, \code{"gk_adjustment"},
  and experimental \code{"saddlepoint_mle"} or \code{"saddlepoint_mom"}.}
}
\details{
  The experimental \code{"saddlepoint_mom"} method uses the joint sample
  moments, the finite-integer-sample moment support, and the unchanged MOM
  inspection rule. Its numerical and finite-sample validation is incomplete:
  ordinary settings can be unsupported. Unsupported integrations or searches
  raise an error and do not return a substitute probability or plan. Population
  quadrature orders 128 and 256 and integration refinements must agree within
  0.00002. A conservative log-concavity check on the tilted logit-integration
  kernel also rejects cases with unresolved remote tilted mass; rejected nodes
  are not omitted and renormalized. This numerical discrepancy is not a statistical risk bound. Integer
  plan searches check working risks and adjacent sizes; global minimality is
  not established. The method is slower than the Delta methods and caches
  successful and failed population calculations. The default remains
  \code{"delta_mle"}.

  This function provides a user-friendly wrapper to construct AS plan directly
  from parameters.
  Internally, it constructs the appropriate \code{AttrPlan} or \code{VarPlan},
  from given paramenters.
  Delta--MLE requires finite \code{n >= 2} and finite nonnegative \code{k}.
  Saddlepoint MLE additionally requires an integer inspection size. Supply
  explicit quality points \code{pd} when using \code{OCdata()} for a manual
  saddlepoint plan.
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

# Unknown-theta Beta plan using analytical Delta--MoM
plan3 <- manualPlan(n = 45, k = 2.2, distribution = "beta",
                    theta = 300, theta_type = "unknown", method = "delta_mom",
                    USL = 0.05)

}
\author{
  Ha Truong
}
