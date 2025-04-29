\name{plot.OCdata}
\alias{plot.OCdata}
\title{Plot Method for OCdata Objects}
\description{
  Plots the Operating Characteristic (OC) curve from an object of class \code{"OCdata"}, either by proportion nonconforming or process mean levels.
}
\usage{
\method{plot}{OCdata}(x, by = c("pd", "mean"), ...)
}
\arguments{
  \item{x}{An object of class \code{"OCdata"}, typically generated using \code{OCdata()}.}

  \item{by}{A character string indicating the type of OC curve to plot. Options are:
    \describe{
      \item{\code{"pd"}}{(Default) Plot the OC curve by proportion nonconforming.}
      \item{\code{"mean"}}{Plot the OC curve by estimated process mean levels (only available for variable sampling plans).}
    }
  }

  \item{...}{Additional graphical parameters passed to the \code{plot()} function.}
}
\details{
  This method visualizes the OC curve based on the content of the \code{"OCdata"} object.

  By default, the curve is plotted against the proportion of nonconforming items (\code{@pd}). If \code{by = "mean"} is specified and the plan includes valid mean-level estimates (\code{@process_means}), the curve is plotted against mean levels.

  If \code{by = "mean"} is requested but no mean estimates are available (e.g., for attribute plans), a message will be shown and no plot will be drawn.
}

\value{
  A plot showing the OC curve for the given attribute/variable sampling plan.
}

\examples{
# Attribute plan
plan_attr <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
oc_attr <- OCdata(plan_attr)
plot(oc_attr)               # OC curve by pd (default)
plot(oc_attr, by = "mean")  # Will show message if not available

# Variable plan
plan_var <- optVarPlan(PRQ = 0.025, CRQ = 0.1, USL = 0.1,
                       distribution = "normal", sigma=0.01)
oc_var <- OCdata(plan_var)
plot(oc_var)                # OC curve by pd
plot(oc_var, by = "mean")   # OC curve by mean levels
}
\seealso{
  \code{\link{OCdata}}, \code{\link{optAttrPlan}}, \code{\link{optVarPlan}}
}
\author{
  Ha Truong
}
