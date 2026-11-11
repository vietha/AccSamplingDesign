test_that("MOM saddlepoint dispatch validates inputs and preserves the default", {
  expect_equal(AccSamplingDesign:::.normalize_beta_theta_method(NULL,"beta","unknown",TRUE),"delta_mle")
  expect_error(manualPlan(distribution="normal",n=23,k=1,method="saddlepoint_mom"),"only applicable")
  for(n in c(1,2.5,Inf,NA_real_))expect_error(manualPlan(distribution="beta",
    theta_type="unknown",theta=500,USL=.05,n=n,k=2,method="saddlepoint_mom"),"integer")
})

test_that("MOM saddlepoint probabilities, serialization and OC data agree", {
  theta<-1e10;mu<-.5;n<-23;k<-1.19397
  U<-mu+qnorm(.8)*sqrt(mu*(1-mu)/(theta+1))
  p<-pbeta(U,mu*theta,(1-mu)*theta,lower.tail=FALSE)
  z<-manualPlan(distribution="beta",theta_type="unknown",theta=theta,USL=U,
    n=n,k=k,method="saddlepoint_mom")
  got<-accProb(z,p);expected<-pt(k*sqrt(n-1),n-1,ncp=sqrt(n)*qnorm(.8),lower.tail=FALSE)
  expect_equal(got,expected,tolerance=2e-5)
  expect_equal(OCdata(z,pd=p)$paccept,got,tolerance=1e-12)
  expect_output(summary(z),"Saddlepoint MOM OC",fixed=TRUE)
  expect_output(print(z),"validation incomplete",fixed=TRUE)
  file<-tempfile();saveRDS(z,file);expect_equal(accProb(readRDS(file),p),got,tolerance=1e-12)
  expect_equal(accProb(z,0),1);expect_equal(accProb(z,1),0)
  expect_silent(plot(z,pd=p));expect_silent(plot(z,pd=p,by="mean"))
  expect_error(OCdata(z),"Supply pd")
})

test_that("unsupported MOM calculations fail explicitly and are cached", {
  z<-manualPlan(distribution="beta",theta_type="unknown",theta=500,USL=.05,
    n=121,k=2.33333556600565,method="saddlepoint_mom")
  expect_error(accProb(z,.03),"numerically unsupported")
  expect_error(accProb(z,.03),"No Delta/MLE fallback")
  expect_error(optPlan(.01,.03,distribution="beta",theta_type="unknown",theta=500,
    USL=.05,method="saddlepoint_mom"),"numerically unsupported")
})
