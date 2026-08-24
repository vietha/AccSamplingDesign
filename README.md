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
adjustment (`"gk_adjustment"`). Delta--MLE is used when `method` is omitted.

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
