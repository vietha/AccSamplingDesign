\name{plot.VarPlan}
\alias{plot.VarPlan}
\title{Plot the OC Curve for Variable Sampling Plans}
\description{
  Plots the Operating Characteristic (OC) curve for an object of class \code{VarPlan}. Supports plotting against either the proportion of nonconforming items or the corresponding process mean levels, depending on availability.
}
\usage{
plot(x, pd = NULL, by = c("pd", "mean"), ...)
}
\arguments{
  \item{x}{An object of class \code{VarPlan} representing a variable acceptance sampling plan.}

  \item{pd}{Optional numeric vector of proportions of nonconforming items to evaluate. If \code{NULL} (default), a suitable range is generated automatically.}

  \item{by}{Character string indicating which x-axis to use for plotting. Either \code{"pd"} for proportion nonconforming (default) or \code{"mean"} for process mean levels. If \code{"mean"} is selected but the plan lacks specification limits, an error is raised.}

  \item{...}{Additional graphical parameters passed to \code{plot()}.}
}
\details{
  This plotting method visualizes the probability of acceptance (\code{P(accept)}) against the desired metric, based on the parameters of a variable sampling plan.

  If \code{by = "pd"}, the x-axis represents the proportion of nonconforming items. If \code{by = "mean"} and the plan defines \code{limit_type} and \code{spec_limit}, the function estimates corresponding process means using \code{\link{muEst}} and plots the OC curve by those mean values.

  Reference lines for the Producer's Risk Quality (PRQ) and Consumer's Risk Quality (CRQ), along with their respective acceptance probabilities, are shown when plotting by proportion.
}
\value{
  A plot showing the OC curve for the given variable sampling plan, either by nonconforming proportion or mean level.
}
\seealso{
  \code{\link{optVarPlan}}, \code{\link{accProb}}, \code{\link{muEst}}, \code{\link{OCdata}}, \code{\link{plot.OCdata}}
}
\examples{
# Variable sampling plan with specification limits
plan <- optVarPlan(
  PRQ = 0.025, CRQ = 0.1,
  alpha = 0.05, beta = 0.1,
  distribution = "normal",
  limit_type = "upper", spec_limit = 3, sigma = 0.1
)

# Plot by proportion nonconforming
plot(plan, by = "pd")

# Plot by estimated mean level (requires spec_limit and limit_type)
plot(plan, by = "mean")

# Custom pd vector
plot(plan, pd = seq(0.01, 0.15, by = 0.001))
}
\author{Ha Truong}
