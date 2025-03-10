\name{accProb}
\alias{accProb}
\title{Acceptance Probability}
\description{
Calculate probability of acceptance for given quality level.
}
\usage{
accProb(plan, p)
}
\arguments{
  \item{plan}{Acceptance plan object (AttrPlan/VarPlan)}
  \item{p}{True quality level (proportion of nonconforming)}
}
\value{
Numeric probability between 0 and 1
}
\examples{
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)
accProb(attr_plan, 0.05)

var_plan <- optVarPlan(PRQ = 0.01, CRQ = 0.1, spec_limit = 10, 
                      distribution = "normal", sigma = 0.5)
accProb(var_plan, 0.05)
}