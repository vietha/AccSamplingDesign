\name{plot.VarPlan}
\alias{plot.VarPlan}
\title{Plot the OC Curve for Variable Sampling Plans}
\description{
  Plots the Operating Characteristic (OC) curve for a variable sampling plan object of class \code{VarPlan}.
}
\usage{
plot(x, pd = NULL, ...)
}
\arguments{
  \item{x}{An object of class \code{VarPlan} representing a variable acceptance sampling plan.}

  \item{pd}{Optional vector of proportions of nonconforming items. If \code{NULL} (default), a range is automatically generated.}

  \item{...}{Additional graphical parameters passed to \code{plot()}.}
}
\details{
  This method computes and visualizes the probability of acceptance (\code{P(accept)}) versus the proportion of nonconforming items based on the parameters of a variable sampling plan.

  The plot displays reference lines for the producer and consumer quality levels (\code{PRQ}, \code{CRQ}) and associated acceptance probabilities.
}
\value{
  A plot showing the OC curve for the given variable sampling plan.
}
\seealso{
  \code{\link{optVarPlan}}, \code{\link{accProb}}, \code{\link{OCdata}}
}
\examples{
# Create variable sampling plan
plan <- optVarPlan(PRQ = 0.025, CRQ = 0.1, 
                   alpha = 0.05, beta = 0.1,
                   distribution = "normal")

# Plot OC curve
plot(plan)

# With custom pd
plot(plan, pd = seq(0.01, 0.15, by = 0.001))
}
\author{Ha Truong}
