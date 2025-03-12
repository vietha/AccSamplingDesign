\name{summary.VarPlan}
\alias{summary.VarPlan}
\title{Summarize Variable Acceptance Plan}
\description{
Detailed summaries for variable acceptance plans.
}
\usage{
\method{summary}{VarPlan}(object, ...)
}
\arguments{
  \item{object}{Plan object to summarize}
  \item{...}{Additional parameters (ignored)}
}
\examples{
var_plan <- optVarPlan(
  PRQ = 0.025,       # Acceptable quality level (% nonconforming)
  CRQ = 0.1,         # Rejectable quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "normal"
)
summary(var_plan)
}
