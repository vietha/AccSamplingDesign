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

var_plan <- optVarPlan(PRQ = 0.01, CRQ = 0.1, spec_limit = 0.90,
                      distribution = "normal")
plotOC(var_plan)
}