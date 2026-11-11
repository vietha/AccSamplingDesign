\name{accProb}
\alias{accProb}
\title{Acceptance Probability}
\description{
  Calculate the probability of acceptance for a given quality level.
}
\usage{
accProb(plan, p)
}
\arguments{
  \item{plan}{Acceptance plan object (\code{AttrPlan}/\code{VarPlan}).}
  \item{p}{True quality level (proportion of nonconforming).}
}
\value{
  Numeric probability between 0 and 1.
}
\details{
  The experimental \code{"saddlepoint_mom"} method uses the joint sample
  moments, the finite-integer-sample moment support, and the unchanged MOM
  inspection rule. Its numerical and finite-sample validation is incomplete:
  ordinary settings can be unsupported. Unsupported integrations or searches
  raise an error and do not return a substitute probability or plan. Population
  quadrature orders 128 and 256 and integration refinements must agree within
  0.00002. A conservative log-concavity check on the tilted logit-integration
  kernel also rejects cases with unresolved remote tilted mass; rejected nodes
  are not omitted and renormalized. This numerical discrepancy is not a statistical risk bound. Integer
  plan searches check working risks and adjacent sizes; global minimality is
  not established. The method is slower than the Delta methods and caches
  successful and failed population calculations. The default remains
  \code{"delta_mle"}.

For Beta plans with unknown theta, the calculation follows the \code{method}
stored in the plan: analytical Delta--MLE, analytical Delta--MoM, the
Govindaraju--Kissling sample-size adjustment, or experimental Saddlepoint MLE OC.
The latter numerically integrates a normalized joint MLE density and returns
an approximate probability. It is more expensive than Delta evaluation, caches
the distribution for repeated constants, and raises an error if integration
does not converge. Integration error and finite-sample approximation error are
distinct. The endpoints \code{p=0} and \code{p=1} use their limiting probabilities.
}

\author{
Ha Truong
}

\examples{
# Example for attribute plan
attr_plan <- optAttrPlan(PRQ = 0.01, CRQ = 0.1)
accProb(attr_plan, 0.05)

# Example for variable plan (normal distribution)
var_plan <- optVarPlan(
  PRQ = 0.025,        # Acceptable quality level (% nonconforming)
  CRQ = 0.1,         # Rejectable quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "normal"
)
accProb(var_plan, 0.05)
}
