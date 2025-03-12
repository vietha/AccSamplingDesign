\name{optAttrPlan}
\alias{optAttrPlan}
\title{Attribute Acceptance Sampling Plan}
\description{
Designs binomial-based acceptance sampling plans using producer/consumer risk criteria.
}
\usage{
optAttrPlan(PRQ, CRQ, alpha = 0.05, beta = 0.10, measurement_error = 0)
}
\arguments{
  \item{PRQ}{Producer Risk Quality (0 < PRQ < 1)}
  \item{CRQ}{Consumer Risk Quality (PRQ < CRQ < 1)}
  \item{alpha}{Producer's risk (0.05 default)}
  \item{beta}{Consumer's risk (0.10 default)}
  \item{measurement_error}{Inspection error ratio (0-1)}
}
\value{
AttrPlan object containing:
\item{n}{Sample size}
\item{c}{Acceptance number}
\item{PRQ}{Input PRQ value}
\item{CRQ}{Input CRQ value}
}
\examples{
plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1, alpha = 0.05, beta = 0.1)
}