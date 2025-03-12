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
  \item{plan}{Acceptance plan object (AttrPlan/VarPlan).}
}
\details{
  This function calculates the probability of acceptance for different levels of proportion nonconforming.
}

\author{
Ha Truong
}



\value{
  A data frame with the following columns:
  \tabular{ll}{
    x_p  \tab Proportion of nonconforming \cr
    y  \tab Probability of acceptance
  }
}
\examples{
  # Example sampling plan
  plan <- optVarPlan(
    PRQ = 0.025,       # Acceptable quality level (% nonconforming)
    CRQ = 0.1,         # Rejectable quality level (% nonconforming)
    alpha = 0.05,      # Producer's risk
    beta = 0.1,        # Consumer's risk
    distribution = "normal")

  # Generate OC curve data
  oc_data <- OCdata(p