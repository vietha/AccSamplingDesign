\name{plotOC}
\alias{plotOC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot The Operating Characteristic Curve
}
\description{
Generate OC curve for acceptance sampling plan.
}
\usage{
plotOC(plan)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plan}{Acceptance plan object (AttrPlan/VarPlan)}
}

\author{
Ha Truong
}
\examples{
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0