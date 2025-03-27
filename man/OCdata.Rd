\name{OCdata}
\alias{OCdata}
\title{Generate OC Curve Data}
\description{
  Computes and returns the operating characteristic (OC) curve data, which includes the proportion of nonconforming items (\code{x_p}) and the corresponding probability of acceptance (\code{y}).
}
\usage{
OCdata(plan, pd = NULL)
}
\arguments{
  \item{plan}{An acceptance plan object of class \code{AttrPlan} or \code{VarPlan}. This object contains parameters related to the sampling plan, such as acceptable quality level (AQL), rejectable quality level (RQL), producer’s risk (\code{alpha}), and consumer’s risk (\code{beta}).}
  
  \item{pd}{An optional manual input vector of proportions of nonconforming items. If provided, this sequence will be used as the \code{x_p} values for the OC curve. If \code{NULL} (default), the function generates a sequence of \code{x_p} values automatically, based on the plan's rejectable quality level (\code{CRQ}) and AQL (\code{PRQ}).}
}
\details{
  The function generates data for an OC curve by computing the probability of acceptance (\code{y}) for different levels of nonconforming proportions (\code{x_p}). If the manual sequence \code{pd} is provided, it will directly be used as the \code{x_p} values. Otherwise, a default sequence is generated based on the sampling plan's rejectable quality level (\code{CRQ}) and AQL (\code{PRQ}).

  The function also supports calculating the mean level (\code{x_m}) for different proportions of nonconforming items when specification limits are provided in the plan. This is done using the function \code{muEst}.
}
\author{
  Ha Truong
}
\value{
  A data frame with the following columns:
  \tabular{ll}{
    \code{x_p} \tab Proportion of nonconforming items  \cr
    \code{x_m} \tab Estimated mean level , if applicable \cr
    \code{y} \tab Probability of acceptance  \cr
  }
}
\examples{
  # Example acceptance plan
  plan <- optVarPlan(
    PRQ = 0.025,       # Acceptable quality level (% nonconforming)
    CRQ = 0.1,         # Rejectable quality level (% nonconforming)
    alpha = 0.05,      # Producer's risk
    beta = 0.1,        # Consumer's risk
    distribution = "normal")

  # Generate OC curve data with automatic proportion sequence
  oc_data <- OCdata(plan)

  # Plot OC curve for non-conforming proportion
  plot(oc_data$x_p, oc_data$y, type = "l", col = "blue", lwd = 2,
       main = "OC Curve", xlab = "Proportion Nonconforming",
       ylab = "P(accept)")
  grid()


  # Example of beta-based sampling plan
  beta_plan <- optVarPlan(
    PRQ = 0.05,       # Target quality level (% nonconforming)
    CRQ = 0.20,       # Minimum quality level (% nonconforming)
    alpha = 0.05,      # Producer's risk
    beta = 0.10,        # Consumer's risk
    distribution = "beta",
    theta = 4.4e+07,
    theta_type = "known",
    spec_limit = 10e-06,
    limit_type = "lower"
  )
  
  # Generate OC data, could use manual p_seq as need 
  p_seq <- seq(0.005, 0.5, by = 0.005)
  oc_data <- OCdata(beta_plan, pd = p_seq)
  
  # Plot the OC curve with Mean Level (x_m) and Probability of Acceptance (y)
  plot(oc_data$x_m, oc_data$y, type = "l", col = "blue", lwd = 2,
       main = "OC Curve", xlab = "Mean Levels",
       ylab = "P(accept)")
  grid()
}
