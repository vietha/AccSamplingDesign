test_that("saddlepoint MLE is opt-in and is limited to unknown-theta Beta", {
  normalize<-AccSamplingDesign:::.normalize_beta_theta_method
  expect_identical(normalize(NULL,"beta","unknown",TRUE),"delta_mle")
  expect_identical(normalize("saddlepoint_mle","beta","unknown"),"saddlepoint_mle")
  expect_error(manualPlan(distribution="normal",n=20,k=1,method="saddlepoint_mle"),"only applicable")
  expect_error(manualPlan(distribution="beta",n=20,k=1,theta=100,USL=.05,method="saddlepoint_mle"),"only applicable")
  for(n in c(1,2.5,NA_real_,Inf))expect_error(manualPlan(distribution="beta",theta_type="unknown",
    theta=500,USL=.05,n=n,k=2,method="saddlepoint_mle"),"integer sample size")
})

test_that("saddlepoint MLE construction and probability dispatch agree", {
  plan<-optPlan(.01,.03,distribution="beta",theta_type="unknown",theta=500,USL=.05,
                method="saddlepoint_mle")
  expect_identical(plan$method,"saddlepoint_mle")
  expect_equal(plan$n,plan$sample_size)
  expect_equal(plan$n,floor(plan$n))
  expect_lte(plan$PR,.05+1e-7);expect_lte(plan$CR,.10+1e-7)
  expect_equal(1-accProb(plan,.01),plan$PR,tolerance=1e-10)
  expect_equal(accProb(plan,.03),plan$CR,tolerance=1e-10)
  expect_true(plan$integration$experimental)
  expect_true(all(plan$integration$numerical_error_estimate<2e-5))
  expect_output(print(plan),"Saddlepoint MLE OC (experimental)",fixed=TRUE)
  expect_output(summary(plan),"Saddlepoint MLE OC (experimental)",fixed=TRUE)
  manual<-manualPlan(distribution="beta",theta_type="unknown",theta=500,USL=.05,
                     n=plan$n,k=plan$k,method="saddlepoint_mle")
  pd<-c(.01,.03)
  oc<-OCdata(manual,pd=pd)
  expect_equal(oc$paccept,vapply(pd,function(p)accProb(plan,p),0),tolerance=1e-10)
  expect_equal(accProb(manual,0),1);expect_equal(accProb(manual,1),0)
  expect_error(accProb(manual,-.01),"proportion")
  expect_error(OCdata(manual),"Supply pd")
  expect_silent(plot(plan,pd=pd))
  expect_silent(plot(plan,pd=pd,by="mean"))
})
