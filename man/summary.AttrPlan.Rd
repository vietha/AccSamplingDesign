\name{summary.AttrPlan}
\alias{summary.AttrPlan}
\title{Summarize Attribute Acceptance Plan}
\description{
Detailed summaries for attribute acceptance plans.
}
\usage{
\method{summary}{AttrPlan}(object, ...)
}
\arguments{
  \item{object}{Plan object to summarize}
  \item{...}{Additional parameters (ignored)}
}
\examples{
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)
summary(attr_plan)
}
