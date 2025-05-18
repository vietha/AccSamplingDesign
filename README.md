# AccSamplingDesign: Acceptance Sampling Plan Design
An R package for designing and analyzing acceptance sampling plans. 
This package is now available on [CRAN](https://cran.r-project.org/package=AccSamplingDesign)! 🎉

# 1. Introduction

The AccSamplingDesign package provides tools for designing Acceptance Sampling plans for both attributes and variables data. Key features include:

- **Attributes Sampling Plans** — pass/fail decisions based on the proportion of nonconforming units.
- **Variables Sampling Plans** — support for normal and beta distributions, including compositional data.
- **Operating Characteristic (OC) Curve Visualization** — assess and compare plan performance.
- **Risk-Based Optimization** — minimize sample size while meeting Producer’s Risk (PR) and Consumer’s Risk (CR) conditions.
- **Custom Plan Comparison** — compare user-defined plans against optimized designs.

# 2. Installation

```{r}
# Install from CRAN
R> install.packages("AccSamplingDesign")
```

```{r}
# Install from GitHub
R> devtools::install_github("vietha/AccSamplingDesign")
```

```{r}
# Load package
R> library(AccSamplingDesign)
```


# 3. Attributes Sampling Plans
> Note that we could use method optPlan() or optAttrPlan(), both work the same.

## 3.1 Create Attribute Plan
```{r}
plan_attr <- optPlan(
  PRQ = 0.01,   # Acceptable Quality Level (1% defects)
  CRQ = 0.05,   # Rejectable Quality Level (5% defects)
  alpha = 0.05, # Producer's risk
  beta = 0.10,   # Consumer's risk
  distribution = "binomial"
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

## 3.5 Compare Attributes Optimal Plan vs Custom Plan
```{r}
# Step1: Find an optimal Attributes Sampling plan
optimal_plan <- optPlan(PRQ = 0.01, CRQ = 0.05, alpha = 0.02, beta = 0.15,
                        distribution = "binomial") # could try "poisson" too
# Summarize the plan
summary(optimal_plan)

# Step2: Compare the optimal plan with two alternative plans 
pd <- seq(0, 0.15, by = 0.001)
oc_opt <- OCdata(plan = optimal_plan, pd = pd)
oc_alt1 <- OCdata(n = optimal_plan$n, c = optimal_plan$c - 1,
                  distribution = "binomial", pd = pd)
oc_alt2 <- OCdata(n = optimal_plan$n, c = optimal_plan$c + 1,
                  distribution = "binomial", pd = pd)

# Step3: Visualize results
plot(pd, oc_opt@paccept, type = "l", col = "blue", lwd = 2,
     xlab = "Proportion Defective", ylab = "Probability of Acceptance",
     main = "Attributes Sampling - OC Curves Comparison",
     xlim = c(0, 0.15), ylim = c(0, 1))
lines(pd, oc_alt1@paccept, col = "red", lwd = 2, lty = 2)
lines(pd, oc_alt2@paccept, col = "green", lwd = 2, lty = 3)
abline(v = c(0.01, 0.05), col = "gray50", lty = 2)
abline(h = c(1 - 0.02, 0.15), col = "gray50", lty = 2)
legend("topright", legend = c(sprintf("Optimal Plan (n = %d, c = %d)", 
       optimal_plan$n, optimal_plan$c),
       sprintf("Alt 1 (c = %d)", optimal_plan$c - 1),
       sprintf("Alt 2 (c = %d)", optimal_plan$c + 1)),
       col = c("blue", "red", "green"),
       lty = c(1, 2, 3), lwd = 2)
```

# 4. Variables Sampling Plans
> Note that we could use method optPlan() or optVarPlan(), both work the same.

## 4.1 Normal Distribution 
### 4.1.1 Find an optimal plan and plot OC chart
```{r}
# Predefine parameters
PRQ <- 0.025
CRQ <- 0.1        
alpha <- 0.05 
beta <- 0.1

norm_plan <- optPlan(
  PRQ = PRQ,       # Acceptable quality level (% nonconforming)
  CRQ = CRQ,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "known"
)

# Summary plan
summary(norm_plan)

# Probability of accepting 10% defective
accProb(norm_plan, 0.1)

# plot OC 
plot(norm_plan)
```

### 4.1.2 Optimal Plan vs Custom Plan
```{r}
# Setup a pd range to make sure all plans have use same pd range
pd <- seq(0, 0.2, by = 0.001)

# Generate OC curve data for designed plan
opt_pdata <- OCdata(norm_plan, pd = pd)

# Evaluated Plan 1: n + 6
eval1_pdata <- OCdata(n = norm_plan$n + 6, k = norm_plan$k,
                      distribution = "normal", pd = pd)

# Evaluated Plan 2: k + 0.1
eval2_pdata <- OCdata(n = norm_plan$n, k = norm_plan$k + 0.1,
                      distribution = "normal", pd = pd)

# Plot base
plot(100 *  opt_pdata@pd, 100 * opt_pdata@paccept,
     type = "l", lwd = 2, col = "blue",
     xlab = "Percentage Nonconforming (%)",
     ylab = "Probability of Acceptance (%)",
     main = "Normal Variables Sampling - Designed Plan with Evaluated Plans")

# Add evaluated plan 1: n + 6
lines(100 * eval1_pdata@pd, 100 * eval1_pdata@paccept,
      col = "red", lty = "longdash", lwd = 2)

# Add evaluated plan 2: k + 0.1
lines(100 * eval2_pdata@pd, 100 * eval2_pdata@paccept,
      col = "forestgreen", lty = "dashed", lwd = 2)

# Add vertical dashed lines at PRQ and CRQ
abline(v = 100 * PRQ, col = "gray60", lty = "dashed")
abline(v = 100 * CRQ, col = "gray60", lty = "dashed")

# Add horizontal dashed lines at 1 - alpha and beta
abline(h = 100 * (1 - alpha), col = "gray60", lty = "dashed")
abline(h = 100 * beta, col = "gray60", lty = "dashed")

# Add legend
legend("topright",
       legend = c(paste0("Designed Plan: n = ", round(norm_plan$n, 2), ", k = ", round(norm_plan$k, 2)), 
                  "Evaluated Plan: n + 6", 
                  "Evaluated Plan: k + 0.1"),
       col = c("blue", "red", "forestgreen"),
       lty = c("solid", "longdash", "dashed"),
       lwd = 2,
       bty = "n")
```


### 4.1.3 Compare known vs unknown sigma plans
```{r}
p1 = 0.005
p2 = 0.03
alpha = 0.05
beta = 0.1

# known sigma plan
plan1 <- optPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "know")
summary(plan1)
plot(plan1)

# unknown sigma plan
plan2 <- optPlan(
  PRQ = p1,        # Acceptable quality level (% nonconforming)
  CRQ = p2,         # Rejectable quality level (% nonconforming)
  alpha = alpha,      # Producer's risk
  beta = beta,        # Consumer's risk
  distribution = "normal",
  sigma_type = "unknow")
summary(plan2)
plot(plan2)
```

## 4.2 Beta Distribution

### 4.2.1 Find an Optimal Plan and Plot OC Chart
```{r}
beta_plan <- optPlan(
  PRQ = 0.05,        # Target quality level (% nonconforming)
  CRQ = 0.2,         # Minimum quality level (% nonconforming)
  alpha = 0.05,      # Producer's risk
  beta = 0.1,        # Consumer's risk
  distribution = "beta",
  theta = 44000000,
  theta_type = "known",
  LSL = 0.00001
)
# Summary Beta plan
summary(beta_plan)
# Probability of accepting 5% defective
accProb(beta_plan, 0.05)

# Plot OC use plot function
plot(beta_plan)
```

### 4.2.2 Plot OC by Defective Rate and by The Mean
```{r}
# Generate OC data
p_seq <- seq(0.005, 0.5, by = 0.005)
oc_data <- OCdata(beta_plan, pd = p_seq)

# plot use S3 method by default (defective rate)
plot(oc_data)
# plot use S3 method by default by mean levels
plot(oc_data, by = "mean")
```

# 5. Technical Specifications

## 5.1 Attributes Plan
The Probability of Acceptance (Pa) is:

$$
Pa(p) = \sum_{i=0}^c \binom{n}{i}p^i(1-p)^{n-i}
$$

where:
- $n$ is sample size
- $c$ is acceptance number
- $p$ is the quality level (non-conforming proportion)

## 5.2 Normal Variable Plan (Case of Known $\sigma$)

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

## 5.3 Normal Variable Plan (Case of Unknown $\sigma$)

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

## 5.4 Beta Variable Plan (Case of Known $\theta$)

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

> Problem is solved using Non-linear programming.

## 5.5 Beta Variable Plan (Case of Unknown $\theta$)

For unknown $\theta$, sample size is adjusted:

$$
m_s = \left(1 + 0.85k^2\right)m_\theta
$$

where:
- $k$ remains the same.

This adjustment considers the variance ratio:

$$
R = \frac{\text{Var}(S)}{\text{Var}(\hat{\mu})}
$$

Unlike the normal distribution where $\text{Var}(S) \approx \frac{\sigma^2}{2n}$, 
in the Beta case, $R$ depends on $\mu$, $\theta$, and sample size $m$. 

---

# 6. References
1. Schilling, E.G., & Neubauer, D.V. (2017). *Acceptance Sampling in Quality Control* (3rd ed.). [Link](https://doi.org/10.4324/9781315120744)
2. Wilrich, P.T. (2004). *Frontiers in Statistical Quality Control 7*. [Link](https://doi.org/10.1007/978-3-7908-2674-6_4)
3. Govindaraju, K., & Kissling, R. (2015). *Sampling plans for Beta-distributed compositional fractions*. [Link](https://doi.org/10.1016/j.chemolab.2015.12.009)
4. ISO 2859-1:1999 - Sampling procedures for inspection by attributes
5. ISO 3951-1:2013 - Sampling procedures for inspection by variables
