\name{OCdata}
\alias{OCdata}
\title{Generate OC Curve Data}
\description{
  Computes and returns the operating characteristic (OC) curve data, including the proportion nonconforming and probability of acceptance.
}
\usage{
  OCdata(plan)
}
\arguments{
  \item{plan}{Acceptance plan object (AttrPlan/VarPlan)}
}
\details{
    This function calculates the probability of acceptance for different levels of proportion nonconforming.
}
\value{
  A data frame with the following columns:
  \itemize{
    \item{x_p}{ - Proportion of nonconforming.}
    \item{y}{ - Probability of acceptance.}
  }
}
\examples{
  # Example sampling plan
  plan <- optVarPlan(PRQ = 0.01, CRQ = 0.1, distribution = "normal")

  # Generate OC curve data
  oc_data <- OCdata(plan)

  # Plot OC curve for non-conforming proportion
  plot(oc_data$x_p, oc_data$y, type = "l", col = "blue", lwd = 2,
       main = "OC Curve", xlab = "Proportion Nonconforming", 
       ylab = "P(accept)")
  grid()
}
