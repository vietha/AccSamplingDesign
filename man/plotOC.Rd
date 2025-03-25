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
plotOC(plan, pd = NULL)
}
\arguments{
  \item{plan}{Acceptance plan object (AttrPlan/VarPlan)}
  \item{pd}{Manual input for sequence of proportion nonconforming values}
}
\author{
Ha Truong
}
\examples{
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)
plotOC(attr_plan)

var_plan <- optVarPlan(
  PRQ = 0.025,       # Acceptable quality level (\% nonconforming)
  CRQ = 0.1,         # Rejectable quality level (\% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "normal"
)
plotOC(var_plan)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
