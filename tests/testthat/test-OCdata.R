## -----------------------------------------------------------------------------
## test-OCdata.R
##
## Author: Ha Truong
## Created: 12 Nov 2025
## Purpose: Test OCdata functions
## -----------------------------------------------------------------------------



# Attributes Plans
## -----------------------------------------------------------------------------
test_that("OCdata.AttrPlan works for binomial", {
  plan_attr <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  oc <- OCdata.AttrPlan(plan_attr)
  
  expect_s3_class(oc, "OCdata")
  expect_equal(oc$dist, "binomial")
  expect_length(oc$pd, 100)
  expect_length(oc$paccept, 100)
  
  # Custom pd
  custom_pd <- seq(0, 0.1, length.out = 5)
  oc2 <- OCdata.AttrPlan(plan_attr, pd = custom_pd)
  expect_equal(oc2$pd, custom_pd)
  expect_length(oc2$paccept, length(custom_pd))
})

test_that("OCdata.AttrPlan works for poisson", {
  plan_attr <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "poisson")
  oc <- OCdata.AttrPlan(plan_attr)
  
  expect_s3_class(oc, "OCdata")
  expect_equal(oc$dist, "poisson")
})

test_that("OCdata.AttrPlan creates valid object", {
  plan_attr <- optAttrPlan(PRQ = 0.01, CRQ = 0.05, distribution = "binomial")
  oc <- OCdata.AttrPlan(plan_attr)
  expect_s3_class(oc, "OCdata")
  expect_length(oc$pd, 100)
  expect_length(oc$paccept, 100)
})


# Variables Plans
## -----------------------------------------------------------------------------

test_that("OCdata.VarPlan creates valid object", {
  plan_var <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                         distribution = "normal", sigma_type = "known", sigma = 1)
  oc <- OCdata.VarPlan(plan_var)
  expect_s3_class(oc, "OCdata")
  expect_length(oc$paccept, 100)
})

test_that("OCdata.VarPlan works for normal, known sigma", {
  plan_var <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                         distribution = "normal", sigma_type = "known", sigma = 1)
  oc <- OCdata.VarPlan(plan_var)
  
  expect_s3_class(oc, "OCdata")
  expect_equal(oc$dist, "normal")
  expect_length(oc$paccept, length(oc$pd))
})

test_that("OCdata.VarPlan works for normal, unknown sigma", {
  plan_var <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                         distribution = "normal", sigma_type = "unknown")
  oc <- OCdata.VarPlan(plan_var)
  
  expect_s3_class(oc, "OCdata")
})

test_that("OCdata.VarPlan works for beta, upper and lower limits", {
  plan_beta_upper <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                                distribution = "beta", theta_type = "known",
                                theta = 100, USL = 0.1)
  oc_upper <- OCdata.VarPlan(plan_beta_upper)
  expect_s3_class(oc_upper, "OCdata")
  expect_length(oc_upper$paccept, 100)
  
  plan_beta_lower <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                                distribution = "beta", theta_type = "known",
                                theta = 100, LSL = 0.1)
  oc_lower <- OCdata.VarPlan(plan_beta_lower)
  expect_s3_class(oc_lower, "OCdata")
})

# S3 methods
## -----------------------------------------------------------------------------

test_that("print.OCdata and summary.OCdata work", {
  plan_attr <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  oc <- OCdata.AttrPlan(plan_attr)
  
  expect_output(print(oc), "OCdata object")
  expect_output(summary(oc), "Summary of OCdata")
})

test_that("plot.OCdata works", {
  plan_var <- optVarPlan(PRQ = 0.01, CRQ = 0.05,
                         distribution = "normal", sigma_type = "known", sigma = 1)
  oc <- OCdata.VarPlan(plan_var)
  
  expect_silent(plot(oc, by = "pd"))
  expect_message(plot(oc, by = "mean"))
  
  plan_attr <- optAttrPlan(PRQ = 0.01, CRQ = 0.05)
  oc_attr <- OCdata.AttrPlan(plan_attr)
  expect_message(plot(oc_attr, by = "mean"), "not available")
})

