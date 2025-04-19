\name{OCdata}
\alias{OCdata}
\title{Generate OC Curve Data}
\description{
  Computes and returns an object of class \code{"OCdata"}, which contains the operating characteristic (OC) curve data. This includes the proportion of nonconforming items and the corresponding probability of acceptance, along with plan-related metadata.
}
\usage{
OCdata(plan, pd = NULL)
}
\arguments{
  \item{plan}{An acceptance plan object of class \code{AttrPlan} or \code{VarPlan}. This object contains parameters related to the sampling plan, such as Producer's Risk Quality (PRQ), Consumer's Risk Quality (CRQ), producer’s risk (\code{alpha}), and consumer’s risk (\code{beta}).}
  
  \item{pd}{An optional manual input vector of proportions of nonconforming items. If provided, this sequence will be used directly. If \code{NULL} (default), the function generates a sequence automatically based on the plan’s rejectable quality level (\code{CRQ}).}
}
\details{
  The function evaluates the operating characteristic (OC) curve by calculating the probability of acceptance across a range of proportions of nonconforming items (\code{pd}). If specification limits are defined in the plan, it also estimates the corresponding process mean levels using the \code{muEst} function.
  
  The returned object is of class \code{"OCdata"}, which stores both the computed results and key information about the sampling plan used.

  Either the attribute sampling threshold (\code{c}) or the variable plan constant (\code{k}) will be included depending on the plan type. The other will be empty.
}
\value{
  An S4 object of class \code{"OCdata"} with the following slots:

  \tabular{ll}{
    \code{pd} \tab Proportion of nonconforming items. \cr
    \code{paccept} \tab Probability of acceptance at each nonconforming level. \cr
    \code{type} \tab Type of the distribution (e.g., \code{"normal"}, \code{"beta"}). \cr
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
# Example acceptance plan (variable plan, normal distribution)
plan <- optVarPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.1,
  distribution = "normal"
)

# Generate OC data automatically
oc_data <- OCdata(plan)

# Plot the OC curve
plot(oc_data@pd, oc_data@paccept, type = "l", col = "blue", lwd = 2,
     main = "OC Curve", xlab = "Proportion Nonconforming",
     ylab = "P(accept)")
grid()

# OC curve using mean levels (if applicable)
if (length(oc_data@process_means) > 0) {
  plot(oc_data@process_means, oc_data@paccept, type = "l", col = "darkgreen", lwd = 2,
       main = "OC Curve (by Mean Levels)", xlab = "Mean Levels",
       ylab = "P(accept)")
  grid()
}

# Example with custom pd
custom_pd <- seq(0.01, 0.5, by = 0.01)
oc_data2 <- OCdata(plan, pd = custom_pd)
}
