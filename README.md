# AccSamplingDesign: Acceptance Sampling Plan Design
An R package for designing and analyzing acceptance sampling plans. 
This package is now available on [CRAN](https://cran.r-project.org/package=AccSamplingDesign)! 🎉

# 1. Introduction

The AccSamplingDesign package provides tools for designing acceptance sampling plans for both attribute and variable data. Key features include:

- **Attribute sampling plans** (pass/fail inspection based on nonconforming proportion)
- **Variable sampling plans** for normal and beta distributions, focusing on the proportion of nonconforming units
- **OC curve visualization** to evaluate sampling plan performance
- **Risk-based optimization** to minimize producer's and consumer's risks while meeting specified quality levels of Producer’s Risk Quality (PRQ) and Consumer’s Risk Quality (CRQ)

# 2. Installation

```{r eval=FALSE}
# Install from CRAN
R> install.packages("AccSamplingDesign")

# Install from GitHub
R> devtools::install_github("vietha/AccSamplingDesign")

# Load package
R> library(AccSamplingDesign)
```

# 3. Attribute Sampling Plans

## 3.1 Create Attribute Plan
```{r}
plan_attr <- optAttrPlan(
  PRQ = 0.01,   # Acceptable Quality Level (1% defects)
  CRQ = 0.05,   # Rejectable Quality Level (5% defects)
  alpha = 0.05, # Producer's risk
  beta = 0.10   # Consumer's risk
)
```

## 3.2 Plan Summary
```{r}
summary(plan_attr)
```

## 3.3 Acceptance Probability
```{r}
# Probability of accepting 3% defective lots
accProb(plan_attr, 0.03)
```

## 3.4 OC Curve
```{r}
plot(plan_attr)
```

# 4. Variable Sampling Plans

## 4.1 Normal Distribution 
### 4.1.1 Find an optimal plan and plot OC chart
```{r}
norm_plan <- optVarPlan(
  PRQ = 0.025,       # Acceptable quality level (% nonconforming)
  CRQ = 0.1,         # Rejectable quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "normal",
  sigma_type = "known"
)

summary(norm_plan)

# Generate OC curve data
oc_data_normal <- OCdata(norm_plan)
# show data of Proportion Nonconforming (x_p) vs Probability Acceptance (y)
#head(oc_data_normal, 15) 
plot(norm_plan)
```

### 4.1.2 Compare known vs unknown sigma plans
```{r}
p1 = 0.005
p2 = 0.03
alpha = 0.05
beta = 0.1

# known sigma plan
plan1 <- optVarPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "know")
summary(plan1)
plot(plan1)

# unknown sigma plan
plan2 <- optVarPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "unknow")
summary(plan2)
plot(plan2)

# Generate OC curve data
oc_data1 <- OCdata(plan1)
oc_data2 <- OCdata(plan2)

# Plot the first OC curve (solid red line)
plot(oc_data1@pd, oc_data1@paccept, type = "l", col = "red", lwd = 2,
     main = "Operating Characteristic (OC) Curve", 
     xlab = "Proportion Nonconforming", 
     ylab = "P(accept)")

# Add the second OC curve (dashed blue line)
lines(oc_data2@pd, oc_data2@paccept, col = "blue", lwd = 2, lty = 2)

abline(v = c(p1, p2), lty = 1, col = "gray")
abline(h = c(1 - alpha, beta), lty = 1, col = "gray")

legend1 = paste0("Known Sigma (n=", plan1$sample_size, ", k=", plan1$k, ")")
legend2 = paste0("Unknown Sigma (n=", plan2$sample_size, ", k=", plan2$k, ")")
# Add a legend to distinguish the two curves
legend("topright", legend = c(legend1, legend2), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# Add a grid
grid()
```

## 4.2 Beta Distribution

### 4.2.1 Find an optimal plan and plot OC chart
```{r}
beta_plan <- optVarPlan(
  PRQ = 0.05,        # Target quality level (% nonconforming)
  CRQ = 0.2,         # Minimum quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "beta",
  theta = 44000000,
  theta_type = "known",
  LSL = 0.00001
)
summary(beta_plan)

# Plot OC use plot function
plot(beta_plan)

# Generate OC data
p_seq <- seq(0.005, 0.5, by = 0.005)
oc_data <- OCdata(beta_plan, pd = p_seq)
#head(oc_data)

# plot use S3 method
plot(oc_data)

# Plot the OC curve with Mean Level (x_m) and Probability of Acceptance (y)
plot(oc_data@pd, oc_data@paccept, type = "l", col = "blue", lwd = 2,
     main = "OC Curve by the mean levels (plot by data)", xlab = "Mean Levels",
     ylab = "P(accept)")
grid()
```

### 4.2.2 Compare known vs unknown theta plans
```{r}
p1 = 0.005
p2 = 0.03
alpha = 0.05
beta = 0.1
spec_limit = 0.05 # use for Beta distribution
theta = 500

# My package for beta plan
beta_plan1 <- optVarPlan(
  PRQ = p1,       # Target quality level (% nonconforming)
  CRQ = p2,       # Minimum quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "beta",
  theta = theta,
  theta_type = "known",
  USL = spec_limit
)
summary(beta_plan1)

beta_plan2 <- optVarPlan(
  PRQ = p1,       # Target quality level (% nonconforming)
  CRQ = p2,       # Minimum quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "beta",
  theta = theta,
  theta_type = "unknown",
  USL = spec_limit
)
summary(beta_plan2)

# Generate OC curve data
oc_beta_data1 <- OCdata(beta_plan1)
oc_beta_data2 <- OCdata(beta_plan2)

# Plot the first OC curve (solid red line)
plot(oc_beta_data1@pd, oc_beta_data1@paccept, type = "l", col = "red", lwd = 2,
     main = "Operating Characteristic (OC) Curve", 
     xlab = "Proportion Nonconforming", 
     ylab = "P(accept)")

# Add the second OC curve (dashed blue line)
lines(oc_beta_data2@pd, oc_beta_data2@paccept, col = "blue", lwd = 2, lty = 2)

abline(v = c(p1, p2), lty = 1, col = "gray")
abline(h = c(1 - alpha, beta), lty = 1, col = "gray")

legend1 = paste0("Known Theta (n=", beta_plan1$sample_size, ", k=", beta_plan1$k, ")")
legend2 = paste0("Unknown Theta (n=", beta_plan2$sample_size, ", k=", beta_plan2$k, ")")
# Add a legend to distinguish the two curves
legend("topright", legend = c(legend1, legend2), 
       col = c("red", "blue"), lwd = 2, lty = c(1, 2))

# Add a grid
grid()
```

## 4.3 Variable Plan Acceptance Probability
```{r}
# Probability of accepting 10% defective
accProb(norm_plan, 0.1)

# Probability of accepting 5% defective
accProb(beta_plan, 0.05)
```

# 6. Technical Specifications

## 6.1 Attribute Plan
The Probability of Acceptance (Pa) is:

$$
Pa(p) = \sum_{i=0}^c \binom{n}{i}p^i(1-p)^{n-i}
$$

where:
- $n$ is sample size
- $c$ is acceptance number
- $p$ is the quality level (non-conforming proportion)

## 6.2 Normal Variable Plan (Case of Known $\sigma$)

The Probability of Acceptance (Pa) is:

$$
Pa(p) = \Phi\left( -\sqrt{n_{\sigma}} \cdot (\Phi^{-1}(p) + k_{\sigma}) \right)
$$

or:

$$
Pa(p) = 1 - \Phi\left( \sqrt{n_{\sigma}} \cdot (\Phi^{-1}(p) + k_{\sigma}) \right)
$$

where:
- $\Phi(\cdot)$ is the CDF of the standard normal distribution.
- $\Phi^{-1}(p)$ is the standard normal quantile corresponding to $p$.
- $n_{\sigma}$ is the sample size.
- $k_{\sigma}$ is the acceptability constant.

Sample size and acceptability constant:

$$
n_{\sigma} = \left( \frac{\Phi^{-1}(1 - \alpha) + \Phi^{-1}(1 - \beta)}{\Phi^{-1}(1 - PRQ) - \Phi^{-1}(1 - CRQ)} \right)^2
$$

$$
k_{\sigma} = \frac{\Phi^{-1}(1 - PRQ) \cdot \Phi^{-1}(1 - \beta) + \Phi^{-1}(1 - CRQ) \cdot \Phi^{-1}(1 - \alpha)}{\Phi^{-1}(1 - \alpha) + \Phi^{-1}(1 - \beta)}
$$

where:
- $\alpha$ and $\beta$ are the producer's and consumer's risks, respectively.
- $PRQ$ and $CRQ$ are the Producer's Risk Quality and Consumer's Risk Quality.

## 6.3 Normal Variable Plan (Case of Unknown $\sigma$)

The formula for the probability of acceptance (Pa) is:

$$
Pa(p) = \Phi \left( \sqrt{\frac{n_s}{1 + \frac{k_s^2}{2}}} \left( \Phi^{-1}(1 - p) - k_s \right) \right)
$$

where:
- $k_s = k_{\sigma}$ is the acceptability constant.
- $n_s$ is the adjusted sample size:

$$
n_s = n_{\sigma} \times \left( 1 + \frac{k_s^2}{2} \right)
$$

(Reference: Wilrich, P.T. (2004))

## 6.4 Beta Variable Plan (Case of Known $\theta$)

For Beta distributed data:

$$
f(x; a, b) = \frac{x^{a-1} (1 - x)^{b-1}}{B(a, b)}
$$

where $B(a, b)$ is the Beta function.

Reparameterized as:

$$
\mu = \frac{a}{a + b}, \quad \theta = a + b, \quad \sigma^2 \approx \frac{\mu(1 - \mu)}{\theta} \quad (\text{for large } \theta)
$$

Probability of acceptance:

$$
Pa = P(\mu - k \sigma \geq L \mid \mu, \theta, m, k)
$$

where:
- $L$ = lower specification limit
- $m$ = sample size
- $k$ = acceptability constant

Parameters $m$ and $k$ are found to satisfy:

$$
Pa(\mu_{PRQ}) = 1 - \alpha, \quad Pa(\mu_{CRQ}) = \beta
$$

**Implementation Note:**  
For a nonconforming proportion $p$ (e.g., PRQ or CRQ), the mean $\mu$ is derived by solving:

$$
P(X \leq L \mid \mu, \theta) = p
$$

where $X \sim \text{Beta}(\theta \mu, \theta (1-\mu))$.

> Problem is solved using Non-linear programming and Monte Carlo simulation.

## 6.5 Beta Variable Plan (Case of Unknown $\theta$)

For unknown $\theta$, sample size is adjusted:

$$
m_s = \left(1 + 0.5k^2\right)m_\theta
$$

where:
- $k$ remains the same.

This adjustment considers the variance ratio:

$$
R = \frac{\text{Var}(S)}{\text{Var}(\hat{\mu})}
$$

Unlike the normal distribution where $\text{Var}(S) \approx \frac{\sigma^2}{2n}$, in the Beta case, $R$ depends on $\mu$, $\theta$, and sample size $m$.

---

# 7. References
1. Schilling, E.G., & Neubauer, D.V. (2017). *Acceptance Sampling in Quality Control* (3rd ed.). [Link](https://doi.org/10.4324/9781315120744)
2. Wilrich, P.T. (2004). *Frontiers in Statistical Quality Control 7*. [Link](https://doi.org/10.1007/978-3-7908-2674-6_4)
3. Govindaraju, K., & Kissling, R. (2015). *Sampling plans for Beta-distributed compositional fractions*. [Link](https://doi.org/10.1016/j.chemolab.2015.12.009)
4. ISO 2859-1:1999 - Sampling procedures for inspection by attributes
5. ISO 3951-1:2013 - Sampling procedures for inspection by variables
