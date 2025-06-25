\name{OCdata-accessors}
\alias{paccept}
\alias{pd}
\alias{process_means}
\title{Accessor Functions for OCdata Objects}
\description{
  Accessor functions to retrieve components from \code{OCdata} objects, such as the
  acceptance probabilities, proportions nonconforming, and process mean estimates.
}
\usage{
paccept(x)
pd(x)
process_means(x)
}
\arguments{
  \item{x}{An object of class \code{OCdata} as returned by \code{\link{OCdata}}.}
}
\details{
These functions provide a clean interface to access key components of \code{OCdata}
objects. They are intended to replace direct use of the \code{@} operator and support
users transitioning from the S4-style to the current S3-style implementation.

\itemize{
  \item \code{paccept(x)} returns the vector of probabilities of acceptance.
  \item \code{pd(x)} returns the vector of proportions nonconforming (defective).
  \item \code{process_means(x)} returns the estimated process means (only 
  applicable for variable sampling plans).
}
}
\value{
  A numeric vector corresponding to the requested OCdata component.
}
\examples{
plan <- OCdata(n = 125, c = 4, distribution = "binomial", 
               pd = seq(0, 0.15, by = 0.005))
paccept(plan)
pd(plan)

# Variable plan with process means
vplan <- OCdata(n = 50, k = 1.5, distribution = "normal", USL = 10,
                sigma = 2, pd = seq(0, 0.1, by = 0.01))
process_means(vplan)
}
\author{Ha Truong}
