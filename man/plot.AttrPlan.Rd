\name{plot.AttrPlan}
\alias{plot.AttrPlan}
\title{Plot the OC Curve for Attribute Sampling Plans}
\description{
  Plots the Operating Characteristic (OC) curve for an attribute sampling plan object of class \code{AttrPlan}.
}
\usage{
plot(x, pd = NULL, ...)
}
\arguments{
  \item{x}{An object of class \code{AttrPlan} representing an attribute acceptance sampling plan.}

  \item{pd}{Optional vector of proportions of nonconforming items. If \code{NULL} (default), a range is automatically generated.}

  \item{...}{Additional graphical parameters passed to \code{plot()}.}
}
\details{
  This method computes and visualizes the probability of acceptance (\code{P(accept)}) as a function of the proportion of nonconforming items in the population, based on the attribute sampling plan.

  The plot also includes reference lines at the plan's producer and consumer quality levels (\code{PRQ}, \code{CRQ}) and their corresponding acceptance probabilities.
}
\value{
  A plot showing the OC curve for the given attribute sampling plan.
}
\seealso{
  \code{\link{optAttrPlan}}, \code{\link{accProb}}, \code{\link{OCdata}}
}
\examples{
# Create attribute plan
plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)

# Plot OC curve
plot(plan)

# With custom pd
plot(plan, pd = seq(0, 0.15, by = 0.001))
}
\author{Ha Truong}
