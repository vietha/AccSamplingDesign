\name{OCdata}
\alias{OCdata}
\title{Generate OC Curve Data}
\description{
  Computes and returns an object of class \code{"OCdata"}, which contains the operating characteristic (OC) curve data. This includes the proportion of nonconforming items and the corresponding probability of acceptance, along with plan-related metadata. This function supports both pre-defined plan objects and ad-hoc parameter inputs.
}
\usage{
OCdata(plan = NULL, pd = NULL,
       distribution = c("binomial", "normal", "beta"),
       PRQ = NULL, CRQ = NULL, alpha = NULL, beta = NULL,
       USL = NULL, LSL = NULL,
       n = NULL, c = NULL, k = NULL,
       sigma_type = c("known", "unknown"),
       theta_type = c("known", "unknown"),
       sigma = NULL, theta = NULL)
}
\arguments{
  \item{plan}{An optional acceptance plan object of class \code{AttrPlan} or \code{VarPlan}. If supplied, this overrides other individual parameter inputs.}

  \item{pd}{An optional vector of proportions of nonconforming items. If \code{NULL}, the function generates a default sequence based on \code{CRQ}.}

  \item{distribution}{Distribution type used for the plan. Options are \code{"binomial"}, \code{"normal"}, or \code{"beta"}. Required if \code{plan} is not provided.}

  \item{USL}{Upper Specification Limit (used for variable sampling plans). Only one of \code{USL} or \code{LSL} should be provided.}

  \item{LSL}{Lower Specification Limit (used for variable sampling plans). Only one of \code{USL} or \code{LSL} should be provided.}

  \item{n}{Sample size.}

  \item{c}{Acceptance number (for attribute plans).}

  \item{k}{Acceptability constant (for variable plans).}

  \item{sigma_type}{Whether sigma is \code{"known"} or \code{"unknown"} (for normal distribution).}

  \item{theta_type}{Whether theta is \code{"known"} or \code{"unknown"} (for beta distribution).}

  \item{sigma}{Standard deviation (if \code{sigma_type = "known"}).}

  \item{theta}{Precision or shape parameter (if \code{theta_type = "known"}, for beta distribution).}
}
\details{
  The function evaluates the Operating Characteristic (OC) curve by computing the probability of acceptance across a range of proportions nonconforming (\code{pd}). This can be either directly specified or derived based on the plan inputs.

  If a \code{plan} object is supplied, it overrides the other input parameters and uses stored plan details. If no plan is provided, a new one will be constructed from the inputs.

  For:
  \itemize{
    \item \code{binomial} distribution: \code{n} and \code{c} must be provided.
    \item \code{normal} or \code{beta} distribution: \code{n} (or \code{m}) and \code{k} are required. Either \code{USL} or \code{LSL} must be specified to compute process mean values using \code{\link{muEst}}.
  }

  The resulting OC curve data includes acceptance probabilities at various quality levels and, for variable plans, optionally maps these probabilities to corresponding mean levels.
}
\value{
  An S4 object of class \code{"OCdata"} with the following slots:

  \tabular{ll}{
    \code{pd} \tab Vector of proportions nonconforming. \cr
    \code{paccept} \tab Probability of acceptance at each level of nonconformance. \cr
    \code{type} \tab Distribution type (\code{"binomial"}, \code{"normal"}, or \code{"beta"}). \cr
    \code{n} \tab Sample size used in the plan. \cr
    \code{k} \tab Acceptability constant (if applicable). \cr
    \code{c} \tab Acceptance number (if applicable). \cr
    \code{process_means} \tab Estimated mean values (only for variable plans with specification limits). \cr
  }
}
\author{
  Ha Truong
}
\examples{
# Example 1: Variable plan (normal distribution)
plan <- optVarPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.1,
  distribution = "normal"
)
# Generate OC data
oc_data <- OCdata(plan, pd = seq(0, 0.15, by = 0.001)
# Plot the OC curve
plot(oc_data)

# Example 2: Attribute plan using direct input
oc_data2 <- OCdata(n = 132, c = 5, distribution = "binomial", 
                   pd = seq(0, 0.15, by = 0.001))
oc_data2
plot(oc_data2)

}
