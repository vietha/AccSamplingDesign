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
var_plan <- optVarPlan(PRQ = 0.01, CRQ = 0.1, spec_limit = 10, 
                      distribution = "normal", sigma = 0.5)
summary(var_plan)
}
