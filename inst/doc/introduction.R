## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)
library(AccSamplingDesign)
library(VGAM)

## -----------------------------------------------------------------------------
# Create an attribute plan with binomial assumption
plan_attr <- optPlan(
  PRQ = 0.01,   # Acceptable quality level (1%)
  CRQ = 0.05,   # Rejectable quality level (5%)
  alpha = 0.02, # Producer's risk
  beta = 0.15,  # Consumer's risk
  distribution = "binomial"
)

# Summary of the plan
summary(plan_attr)

# Probability of accepting 3% defective
accProb(plan_attr, 0.03)

# Plot the OC curve
plot(plan_attr)

## -----------------------------------------------------------------------------
# Create a variable plan assuming known sigma
plan_var <- optPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.10,
  distribution = "normal",
  sigma_type = "known"
)

# Summary
summary(plan_var)

# Plot OC curve
plot(plan_var)

## -----------------------------------------------------------------------------
# Create a variable plan assuming known sigma
plan_var2 <- optPlan(
  PRQ = 0.025,
  CRQ = 0.1,
  alpha = 0.05,
  beta = 0.10,
  distribution = "normal",
  sigma_type = "unknown"
)

# Summary
summary(plan_var2)

## -----------------------------------------------------------------------------
# Create a variable plan using Beta distribution
plan_beta <- optPlan(
  PRQ = 0.05,
  CRQ = 0.2,
  alpha = 0.05,
  beta = 0.10,
  distribution = "beta",
  theta = 44000000,
  theta_type = "known",
  LSL = 0.00001          # Lower Specification Limit
)

# Summary
summary(plan_beta)

# Plot OC curve
plot(plan_beta)
# Plot OC curve be the process mean
plot(plan_beta, by = "mean")

## -----------------------------------------------------------------------------
# Create a variable plan using Beta distribution
plan_beta2 <- optPlan(
  PRQ = 0.05,
  CRQ = 0.2,
  alpha = 0.05,
  beta = 0.10,
  distribution = "beta",
  theta = 44000000,
  theta_type = "unknown",
  method = "delta_mle",
  LSL = 0.00001
)

# Summary
summary(plan_beta2)

## -----------------------------------------------------------------------------
plan_beta2$sample_size  # integer sample size used in plan
plan_beta2$n            # raw computed (non-rounded) value

## -----------------------------------------------------------------------------
# Example data
y <- c(0.75, 0.68, 0.72, 0.70, 0.76)

# Fit Beta model
fit <- vglm(y ~ 1, betaff, data = data.frame(y = y))

# Extract estimated parameters
coef(fit, matrix = TRUE)

## -----------------------------------------------------------------------------
theta_est <- exp(coef(fit, matrix = TRUE)[1, "loglink(phi)"])
theta_est
optPlan(PRQ = 0.05, CRQ = 0.20, alpha = 0.05, beta = 0.10,
           USL = 0.8, distribution = "beta", theta_type = "unknown",
           theta = theta_est)

## -----------------------------------------------------------------------------
# Define range of defect rates
pd <- seq(0, 0.15, by = 0.001)

# Generate OC data from optimal plan
oc_opt <- OCdata(plan = plan_attr, pd = pd)

# Compare with manual plans
mplan1 <- manualPlan(n = plan_attr$n, c = plan_attr$c - 1, distribution = "binomial")
oc_alt1 <- OCdata(plan = mplan1, pd = pd)

# Plot comparison
plot(pd, oc_opt$paccept, type = "l", col = "blue", lwd = 2,
     xlab = "Proportion Defective", ylab = "Probability of Acceptance",
     main = "OC Curves Comparison for Attributes Sampling Plan")
lines(pd, oc_alt1$paccept, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Optimal Plan", "Manual Plan c - 1"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

