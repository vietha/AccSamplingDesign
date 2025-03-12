\name{plotOC}
\alias{plotOC}
\title{Operating Characteristic Curve}
\description{
Generate OC curve for acceptance sampling plan.
}
\usage{
plotOC(plan)
}
\arguments{
  \item{plan}{Acceptance plan object (AttrPlan/VarPlan)}
}
\examples{
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)
plotOC(attr_plan)

var_plan <- optVarPlan(
  PRQ = 0.025,       # Acceptable quality level (% nonconforming)
  CRQ = 0.1,         # Rejectable quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "normal"
)
plotOC(var_plan)
}