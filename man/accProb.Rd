\name{accProb}
\alias{accProb}
\title{Acceptance Probability}
\description{
  Calculate the probability of acceptance for a given quality level.
}
\usage{
accProb(plan, p)
}
\arguments{
  \item{plan}{Acceptance plan object (\code{AttrPlan}/\code{VarPlan}).}
  \item{p}{True quality level (proportion of nonconforming).}
}
\value{
  Numeric probability between 0 and 1.
}

\author{
Ha Truong
}

\examples{
# Example for attribute plan
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)
accProb(attr_plan, 0.05)

# Example for variable plan (normal distribution)
var_plan <- optVarPl