\name{OCdata}
\alias{OCdata}
\title{Generate OC Curve Data}
\description{
  Computes and returns an object of class \code{"OCdata"}, which contains the 
  Operating Characteristic (OC) curve data. This includes the proportion of 
  nonconforming items and the corresponding probability of acceptance, along 
  with plan-related metadata. This function supports both optimal plan objects 
  (e.g. found from \code{"optPlan()"} method) and custom plan parameter inputs.
  
  Accessor functions \code{paccept()}, \code{pd()}, and \code{process_means()} 
  provide safe access to components of the \code{OCdata} object.
}
\usage{
OCdata(plan = NULL, pd = NULL,
       distribution = c("binomial", "poisson", "normal", "beta"),
       n = NULL, c = NULL, k = NULL,
       USL = NULL, LSL = NULL, sigma = NULL, theta = NULL,
       PRQ = NULL, CRQ = NULL, alpha = NULL, beta = NULL,
       sigma_type = c("known", "unknown"),
       theta_type = c("known", "unknown"))
}
\arguments{
  \item{plan}{An optional acceptance plan object of class \code{AttrPlan} or 
  \code{VarPlan}. If supplied, this overrides other individual parameter inputs.}

  \item{pd}{An optional vector of proportions of nonconforming items. If 
  \code{NULL}, the function generates a default sequence based on \code{CRQ}.}

  \item{distribution}{Distribution type used for the plan. Options are 
  \code{"binomial"}, \code{"poisson"}, \code{"normal"}, or \code{"beta"}. 
  Required if \code{plan} is not provided.}
  
  \item{n}{Sample size.}

  \item{c}{Acceptance number (for Attributes Sampling).}

  \item{k}{Acceptability constant (for Variables Sampling).}
  
  \item{USL}{Upper Specification Limit (used for variable sampling plans). Only one of \code{USL} or \code{LSL} should be provided.}

  \item{LSL}{Lower Specification Limit (used for variable sampling plans). Only one of \code{USL} or \code{LSL} should be provided.}

  \item{sigma}{Standard Deviation for Normal distribution.}

  \item{theta}{Precision parameter for Beta distribution.}
  
  \item{PRQ}{Producer's Risk Quality level - optional.}

  \item{CRQ}{Consumer's Risk Quality level - optional.}

  \item{alpha}{Producer’s risk - optional.}

  \item{beta}{Consumer’s risk - optional.}
  
  \item{sigma_type}{Whether sigma is \code{"known"} or \code{"unknown"}.}

  \item{theta_type}{Whether theta is \code{"known"} or \code{"unknown"}.}
}
\details{
  The function evaluates the Operating Characteristic (OC) curve by computing the 
  probability of acceptance across a range of proportions of nonconforming items (\code{pd}). 
  This can be either directly specified or derived based on the plan inputs.

  If a \code{plan} object is supplied, it overrides the other input parameters 
  and uses stored plan details. If no plan is provided, a new one will be constructed 
  from the inputs.

  For:
  \itemize{
    \item \code{Binomial} or \code{Poisson} distribution: \code{n} and \code{c} 
    must be provided.
    \item \code{Normal} or \code{Beta} distribution: \code{n} (or \code{m}) and 
    \code{k} are required. Either \code{USL} or \code{LSL} must be specified to 
    compute process mean values using \code{\link{muEst}}.
  }

  The resulting OC curve data includes acceptance probabilities at various 
  quality levels and, for variable plans, optionally maps these probabilities 
  to corresponding mean levels.

  The accessor functions provide safe access to the internal components of the 
  \code{OCdata} list object without direct list indexing.
}
\value{
  For \code{OCdata}: an object of class \code{"OCdata"} (a list) with components:

  \tabular{ll}{
    \code{pd} \tab Numeric vector of proportions of nonconforming items (defective). \cr
    \code{paccept} \tab Numeric vector of probabilities of acceptance at each level of nonconformance. \cr
    \code{process_means} \tab Numeric vector of estimated mean values (only for variable plans with specification limits). \cr
    \code{dist} \tab Distribution type (\code{"binomial"}, \code{"poisson"}, \code{"normal"}, or \code{"beta"}). \cr
    \code{n} \tab Sample size used in the plan. \cr
    \code{k} \tab Acceptability constant (if applicable). \cr
    \code{c} \tab Acceptance number (if applicable). \cr
  }
}
\author{
  Ha Truong
}
\examples{
# Example 1: Variables Sampling (Normal distribution)
plan <- optVarPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.1,
  distribution = "normal"
)

# Generate OC data from optimal plan
oc_data <- OCdata(plan, pd = seq(0, 0.15, by = 0.001))

# Plot the OC curve
plot(oc_data)

# Example 2: Attributes Sampling
# Generate OC data from custom plan
oc_data2 <- OCdata(n = 132, c = 5, distribution = "binomial", 
                   pd = seq(0, 0.15, by = 0.001))

# Plot the OC curve
plot(oc_data2)
}