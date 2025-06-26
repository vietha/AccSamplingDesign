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
objects. 

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
pdata <- OCdata(n = 125, c = 4, distribution = "binomial", 
               pd = seq(0, 0.15, by = 0.005))
paccept(pdata)
pd(pdata)

# Variable plan with process means
vpdata <- OCdata(n = 50, k = 1.5, distribution = "normal", USL = 10,
                sigma = 2, pd = seq(0.01, 0.1, by = 0.01))
process_means(vpdata)
}
\author{Ha Truong}
