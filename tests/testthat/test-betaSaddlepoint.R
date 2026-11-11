test_that("saddlepoint KL and transformed density match direct shape formulas", {
  kl <- AccSamplingDesign:::.beta_sp_kl
  density <- AccSamplingDesign:::.beta_sp_logdensity
  for (p in list(c(.2,10,.25,15),c(.001,1000,.0015,1200),c(.7,30,.6,25))) {
    mu<-p[1];theta<-p[2];m<-p[3];th<-p[4];a<-m*th;b<-(1-m)*th
    a0<-mu*theta;b0<-(1-mu)*theta
    expected<-lbeta(a0,b0)-lbeta(a,b)+(a-a0)*digamma(a)+(b-b0)*digamma(b)-(th-theta)*digamma(th)
    expect_equal(kl(qlogis(m),th,mu,theta),expected,tolerance=1e-9)
    I<-matrix(c(trigamma(a)-trigamma(th),-trigamma(th),-trigamma(th),trigamma(b)-trigamma(th)),2)
    expected_log<-log(25/(2*pi))+.5*log(det(I))-25*expected+log(m*(1-m)*th^2)
    expect_equal(density(qlogis(m),th,mu,theta,25),expected_log,tolerance=1e-8)
    expect_equal(kl(qlogis(mu),theta,mu,theta),0,tolerance=1e-10)
  }
})

test_that("saddlepoint acceptance cutoffs implement the inspection rule", {
  cutoff<-AccSamplingDesign:::.beta_sp_cut
  for(limit in c(1e-5,.05,.8))for(theta in c(10,1000,1e8))for(k in c(0,1,3))
    for(type in c("upper","lower")) {
      m<-cutoff(limit,k,theta,type)
      value<-m+if(type=="upper")k*sqrt(m*(1-m)/(theta+1)) else -k*sqrt(m*(1-m)/(theta+1))
      expect_equal(value,limit,tolerance=2e-10)
    }
})

test_that("joint saddlepoint probabilities are bounded, monotone in k, and reflected", {
  f<-AccSamplingDesign:::.beta_saddlepoint_probability
  p<-vapply(seq(0,4,length.out=21),function(k)f(.03,300,45,k,.05,"upper"),0)
  expect_true(all(p>=0&p<=1))
  expect_true(all(diff(p)<=1e-12))
  expect_equal(f(.03,300,45,2.2,.05,"upper"),
               f(.97,300,45,2.2,.95,"lower"),tolerance=2e-5)
  expect_error(f(.03,300,1,2,.05,"upper"),"n >= 2")
  expect_error(f(.03,300,45,-1,.05,"upper"),"k >= 0")
  expect_error(f(.03,300,45,2,0,"upper"),"specification limit")
})

test_that("normalized saddlepoint OC approaches the exact Normal reference", {
  f<-AccSamplingDesign:::.beta_saddlepoint_probability
  core<-AccSamplingDesign:::.beta_sp_core
  mu<-.2;theta<-1e10;n<-23;k<-1.19397;sd<-sqrt(mu*(1-mu)/(theta+1))
  for(q in qnorm(c(.8,.95))) {
    limit<-mu+q*sd
    expected<-pt(k*sqrt(n-1),n-1,ncp=sqrt(n)*q,lower.tail=FALSE)
    expect_equal(f(mu,theta,n,k,limit,"upper"),expected,tolerance=2e-5)
    result<-core(mu,theta,n,limit,"upper")
    logZ<-.5*log(n/pi)-log(2)-(n-1)/2*log(n/2)+lgamma((n-1)/2)+n/2
    expect_equal(result$log_normalizer,logZ,tolerance=2e-5)
    expect_lt(result$numerical_error,2e-5)
  }
})
