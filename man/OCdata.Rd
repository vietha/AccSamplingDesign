\name{OCdata}
\alias{OCdata}
\alias{OCdata.AttrPlan}
\alias{OCdata.VarPlan}
\title{Generic function for OC Curve Generation}
\description{
  Generic function to compute Operating Characteristic (OC) curve data
  from an acceptance sampling plan.
}
\usage{
OCdata(plan, pd = NULL)
}
\arguments{
  \item{plan}{An object of class \code{AttrPlan} or \code{VarPlan}.}
  \item{pd}{Vector of quality levels (proportions of nonconforming items).}
}
\details{
  This is a generic function. Methods are defined for objects of class
  \code{AttrPlan} and \code{VarPlan}, which compute the probability of
  acceptance across a range of quality levels (proportions of nonconforming).

  See \code{\link{OCdata.AttrPlan}} and \code{\link{OCdata.VarPlan}} for details.
}
\value{
  An object of class \code{"OCdata"}, a list containing:
  \itemize{
    \item \code{pd} — quality levels (e.g. proportion defective)
    \item \code{paccept} — probability of acceptance at each level
    \item \code{process_means} — process means, if applicable
    \item \code{dist}, \code{n}, \code{k}, \code{c} — plan parameters
  }
}
\seealso{
  \code{\link{OCdata.AttrPlan}}, \code{\link{OCdata.VarPlan}}, \code{\link{create_OCdata}}
}
\author{
  Ha Truong
}
