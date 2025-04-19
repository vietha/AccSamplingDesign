\name{OCdata}
\alias{OCdata}
\title{Generate OC Curve Data}
\description{
  Computes and returns an object of class \code{"OCdata"}, which contains the operating characteristic (OC) curve data. This includes the proportion of nonconforming items and the corresponding probability of acceptance, along with plan-related metadata. This function supports both pre-defined plan objects and ad-hoc parameter inputs.
}
\usage{
OCdata(plan = NULL, pd = NULL,
       distribution = c("binomial", "normal", "beta"),
       PRQ = NULL, CRQ = NULL,
       n = NULL, c = NULL, k = NULL,
       spec_limit = NULL,
       limit_type = c("upper", "lower"),
       sigma_type = c("known", "unknown"),
       theta_type = c("known", "unknown"),
       sigma = NULL, theta = NULL,
       alpha = NULL, beta = NULL)
}
\arguments{
  \item{plan}{An optional acceptance plan object of class \code{AttrPlan} or \code{VarPlan}. If supplied, this overrides other individual parameter inputs.}
  
  \item{pd}{An optional manual input vector of proportions of nonconforming items. If \code{NULL} (default), the function generates a sequence automatically based on the plan’s rejectable quality level (\code{CRQ}).}

  \item{distribution}{Distribution type used for the plan. Options are \code{"binomial"}, \code{"normal"}, or \code{"beta"}. Required if \code{plan} is not provided.}
  
  \item{PRQ}{Producer's Risk Quality level.}
  
  \item{CRQ}{Consumer's Risk Quality level.}
  
  \item{n}{Sample size.}
  
  \item{c}{Acceptance number (for attribute plans).}
  
  \item{k}{Acceptance constant (for variable sampling plans).}
  
  \item{spec_limit}{Specification limit for the variable plan (required for beta distribution).}
  
  \item{limit_type}{Type of specification limit: \code{"upper"} or \code{"lower"}.}
  
  \item{sigma_type}{Type of sigma: \code{"known"} or \code{"unknown"}.}
  
  \item{theta_type}{Type of theta: \code{"known"} or \code{"unknown"}.}
  
  \item{sigma}{Standard deviation (if known).}
  
  \item{theta}{Process parameter (e.g., shape or rate for beta distribution).}
  
  \item{alpha}{Producer’s risk (Type I error).}
  
  \item{beta}{Consumer’s risk (Type II error).}
}
\details{
  The function evaluates the operating characteristic (OC) curve by calculating the probability of acceptance across a range of proportions of nonconforming items (\code{pd}).
  
  If a \code{plan} object is supplied, it will be used directly. Otherwise, a plan will be constructed based on the provided parameters. For binomial distributions, \code{n} and \code{c} are required. For normal or beta variable plans, \code{n} and \code{k} are needed.
  
  When specification limits are included (for variable plans), the corresponding process mean values will also be estimated using \code{muEst()}.
  
  The returned object is of class \code{"OCdata"}, which includes OC curve values and metadata about the sampling plan.
}
\value{
  An S4 object of class \code{"OCdata"} with the following slots:

  \tabular{ll}{
    \code{pd} \tab Proportion of nonconforming items. \cr
    \code{paccept} \tab Probability of acceptance at each nonconforming level. \cr
    \code{type} \tab Type of the distribution (\code{"binomial"}, \code{"normal"}, or \code{"beta"}). \cr
    \code{n} \tab Sample size used in the plan. \cr
    \code{k} \tab Acceptance constant (for variable sampling plans), if available. \cr
    \code{c} \tab Acceptance number (for attribute plans), if available. \cr
    \code{process_means} \tab Estimated process means corresponding to each nonconforming proportion (only if specification limits are used). \cr
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
oc_data <- OCdata(plan)

# Plot the OC curve
plot(oc_data@pd, oc_data@paccept, type = "l", col = "blue", lwd = 2,
     main = "OC Curve (by Proportion Nonconforming)", xlab = "Proportion Nonconforming",
     ylab = "P(accept)")
grid()

# If applicable, plot mean-level based OC
if (length(oc_data@process_means) > 0) {
  plot(oc_data@process_means, oc_data@paccept, type = "l", col = "darkgreen", lwd = 2,
       main = "OC Curve (by Mean Levels)", xlab = "Mean Levels",
       ylab = "P(accept)")
  grid()
}

# Example 2: Attribute plan using only inputs (no plan object)
oc_data2 <- OCdata(n = 132, c = 5, PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
plot(oc_data2@pd, oc_data2@paccept, type = "l", col = "purple", lwd = 2)
}
