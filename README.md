# AccSamplingDesign <img src="https://cran.r-project.org/Rlogo.svg" align="right" height="100"/>

An R package for designing and analyzing **acceptance sampling plans**  
📦 Now available on
[CRAN](https://cran.r-project.org/package=AccSamplingDesign)! 🎉 —

See [NEWS.md](NEWS.md) for release notes and user-visible changes.

The package is described in Truong, Miranda, and Kissling (2026),
[*AccSamplingDesign: An R Package for Optimizing Acceptance Sampling
Plans*](https://doi.org/10.32614/RJ-2026-007), published in *The R Journal*.

## Overview

The **AccSamplingDesign** package provides flexible tools to create and
evaluate **acceptance sampling plans** in quality control, for both
attributes (pass/fail) and variables (measurable) data. It supports
optimization using nonlinear programming (NLP) to meet specified risks
while minimizing the required sample size.

### Key Features

- 🔍 **Attribute Sampling** (Binomial, Poisson): Decisions based on
  defect counts  
- 📈 **Variable Sampling** (Normal, Beta): Including **compositional
  data**  
- ⚙️ **Risk-Based Optimization**: Minimize sample size under `alpha` and
  `beta` constraints  
- 📊 **OC Curve Visualization**: Plot Operating Characteristic curves  
- 🔄 **Custom Plan Comparison**: Evaluate user-defined vs. optimized
  plans

------------------------------------------------------------------------

## Installation

``` r
# Install from CRAN
install.packages("AccSamplingDesign")

# Or install development version from GitHub
devtools::install_github("vietha/AccSamplingDesign")

# Load the package
library(AccSamplingDesign)
```

------------------------------------------------------------------------

## Examples

### 📌 Attribute Sampling (Binomial)

``` r
plan_attr <- optPlan(
  PRQ = 0.01,   # Acceptable quality
  CRQ = 0.05,   # Rejectable quality
  alpha = 0.02, # Producer's risk
  beta = 0.15,  # Consumer's risk
  distribution = "binomial"
)

summary(plan_attr)
accProb(plan_attr, 0.03)  # P(accept) if 3% defective
plot(plan_attr)           # OC curve
```

------------------------------------------------------------------------

### 📌 Variable Sampling (Normal, Known Sigma)

``` r
plan_var <- optPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.10,
  distribution = "normal",
  sigma_type = "known"
)

summary(plan_var)
plot(plan_var)
```

------------------------------------------------------------------------

### 📌 Variable Sampling (Normal, Unknown Sigma)

``` r
plan_var2 <- optPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.10,
  distribution = "normal",
  sigma_type = "unknown"
)

summary(plan_var2)
```

------------------------------------------------------------------------

### 📌 Variable Sampling (Beta, Known Theta)

``` r
plan_beta <- optPlan(
  PRQ = 0.05,
  CRQ = 0.2,
  alpha = 0.05,
  beta = 0.10,
  distribution = "beta",
  theta = 44000000,
  theta_type = "known",
  LSL = 0.00001         # Lower Specification Limit
)

summary(plan_beta)
plot(plan_beta)              # By defect level
plot(plan_beta, by = "mean") # By mean value
```

------------------------------------------------------------------------

### 📌 Variable Sampling (Beta, Unknown Theta)

Unknown-theta plans support analytical Delta--MLE (`"delta_mle"`), analytical
Delta--MoM (`"delta_mom"`), and the earlier Govindaraju--Kissling sample-size
adjustment (`"gk_adjustment"`). Version 0.1.1 also provides the experimental
Saddlepoint MLE OC (`"saddlepoint_mle"`). Delta--MLE is used when `method` is omitted.

`saddlepoint_mle` retains the MLE inspection rule and numerically integrates a
normalized joint saddlepoint density. It uses no simulation during optimization.
It has a substantially larger initial computation cost than the Delta methods;
repeated calculations over `k` reuse the joint distribution. Its probabilities
are bounded and monotone in `k`, but its finite-sample risk accuracy must still
be assessed. Nonconvergent integrations raise an error, without a Delta fallback.
New saddlepoint plans store their delivered integer size in both `n` and
`sample_size`, and expose numerical diagnostics in `integration`. Existing Delta
reporting conventions are preserved.

```r
plan_sp <- optPlan(
  PRQ = 0.01, CRQ = 0.03, alpha = 0.05, beta = 0.10,
  distribution = "beta", theta_type = "unknown", theta = 500,
  USL = 0.05, method = "saddlepoint_mle"
)
summary(plan_sp)
accProb(plan_sp, 0.03)
```

The construction uses the saddlepoint framework in
[McCullagh, *Tensor Methods in Statistics*, Chapter 6](https://www.stat.uchicago.edu/~pmcc/tensorbook/edition2.pdf).
It does not establish universal finite-sample risk control or a global
sample-size optimum. Numerical convergence diagnostics measure integration
error, not statistical approximation error.

``` r
plan_beta <- optPlan(
  PRQ = 0.05,
  CRQ = 0.2,
  alpha = 0.05,
  beta = 0.10,
  distribution = "beta",
  theta = 44000000,
  theta_type = "unknown",
  method = "delta_mle", # Default; may be omitted
  LSL = 0.00001
)

summary(plan_beta)
plot(plan_beta)              # By defect level
plot(plan_beta, by = "mean") # By mean value
```

------------------------------------------------------------------------

### 📌 Compare Custom vs. Optimal Plans

``` r
pd <- seq(0, 0.15, by = 0.001)

oc_opt <- OCdata(plan = plan_attr, pd = pd)

mplan1 <- manualPlan(n = plan_attr$n, c = plan_attr$c - 1, distribution = "binomial")
oc_alt1 <- OCdata(plan = mplan1, pd = pd)

plot(pd, oc_opt$paccept, type = "l", col = "blue", lwd = 2,
     xlab = "Proportion Defective", ylab = "Probability of Acceptance",
     main = "OC Curves Comparison for Attributes Sampling Plan")
lines(pd, oc_alt1$paccept, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Optimal Plan", "Manual Plan (c - 1)"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
```

------------------------------------------------------------------------

# Additional Notes

This README provides a quick start for using the **AccSamplingDesign**
package. For a full discussion of the statistical foundations, models,
and optimization methods used, please refer to the foundation sources
such as:

- Schilling, E.G., & Neubauer, D.V. (2017). *Acceptance Sampling in
  Quality Control* (3rd ed.). CRC Press.  
- Wilrich, P.T. (2004). In *Frontiers in Statistical Quality Control
  7*.  
- Govindaraju, K., & Kissling, R. (2015). *Quality Engineering*, 27(1),
  1–13.

------------------------------------------------------------------------

## Contributing

Contributions, suggestions, and bug reports are welcome!  
Please use [GitHub
Issues](https://github.com/vietha/AccSamplingDesign/issues) or submit a
pull request.


### Experimental saddlepoint MOM (0.1.2 development build)

`method = "saddlepoint_mom"` evaluates the unchanged MOM inspection rule through
its joint raw-moment distribution. It enforces the finite-sample moment support
and uses deterministic, positive quadrature. The numerical and finite-sample
validation is incomplete: some ordinary calculations fail convergence, and an
unsupported plan or probability raises an error. This method is an experimental
interface, not a validated replacement for either Delta MOM or saddlepoint MLE.
`delta_mle` remains the default. There is no simulation-fitted correction.

```r
plan <- manualPlan(distribution = "beta", theta_type = "unknown",
                   theta = 500, USL = 0.05, n = 121, k = 2.333335566,
                   method = "saddlepoint_mom")
# This development case is unsupported; retain the error as a diagnostic.
tryCatch(accProb(plan, 0.03), error = function(e) conditionMessage(e))
```

An integer-only experimental plan search is available through `optPlan()`.
Numerical support is required at every search point; a failed evaluation stops
that search. Numerical convergence alone does not guarantee finite-sample risks.

The integrated MOM interface also rejects tilts outside a sufficient
log-concavity regime for its numerical integration kernel. This conservative
support restriction can reject a mathematically valid Beta probability; it
does not discard nodes and renormalize the remaining mass.
