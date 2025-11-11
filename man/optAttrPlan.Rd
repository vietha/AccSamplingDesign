\name{optAttrPlan}
\alias{optAttrPlan}
\title{Attributes Acceptance Sampling Plan}
\description{
Designs Attributes Acceptance Sampling plans using producer/consumer risk criteria.
}
\usage{
optAttrPlan(PRQ, CRQ, alpha = 0.05, beta = 0.10, 
            distribution = c("binomial", "poisson"))
}
\arguments{
  \item{PRQ}{Producer Risk Quality (0 < PRQ < 1)}
  \item{CRQ}{Consumer Risk Quality (PRQ < CRQ < 1)}
  \item{alpha}{Producer's risk (0.05 default)}
  \item{beta}{Consumer's risk (0.10 default)}
  \item{distribution}{Support binomial and poisson distribution}
}
\value{
AttrPlan object containing:
\item{n}{Sample size}
\item{c}{Acceptance number}
\item{PRQ}{Input PRQ value}
\item{CRQ}{Input CRQ value}
\item{distribution}{Selected distribution}
}

\references{
ISO 2859-1:1999 - Sampling procedures for inspection by attributes 

Schilling, E.G., & Neubauer, D.V. (2017). Acceptance Sampling in Quality Control (3rd ed.). Chapman and Hall/CRC. https://doi.org/10.4324/9781315120744
}
\author{
Ha Truong
}


\examples{
plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1, alpha = 0.05, beta = 0.1, 
                    distribution = "binomial")
}