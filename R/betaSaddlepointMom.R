# Experimental joint-moment saddlepoint engine.
# Nonconvergent calculations are errors; no fallback or tolerance relaxation.
.beta_spm_rules <- new.env(parent=emptyenv())
.validate_beta_spm_plan <- function(n,k) {
  if(!is.numeric(n)||is.complex(n)||length(n)!=1L||!is.finite(n)||n<2||n!=floor(n))
    stop("Saddlepoint MOM requires one integer inspection size n >= 2.",call.=FALSE)
  if(!is.numeric(k)||is.complex(k)||length(k)!=1L||!is.finite(k)||k<0)
    stop("Saddlepoint MOM requires one finite non-negative constant k.",call.=FALSE)
}
.beta_spm_rule <- function(mu,theta,order) {
  key<-paste(format(c(mu,theta),digits=17),order,collapse="|")
  if(exists(key,.beta_spm_rules,inherits=FALSE))return(get(key,.beta_spm_rules))
  a<-mu*theta;b<-(1-mu)*theta;sd<-sqrt(mu*(1-mu)/(theta+1))
  j<-seq_len(order-1L)
  diagonal<-c(mu,(j+a)/(2*j+theta)*(j+theta-1)/(2*j+theta-1)+
    j/(2*j+theta-1)*(j+b-1)/(2*j+theta-2))
  off<-c(sd,exp(.5*(log(j[-1])+log(j[-1]+a-1)+log(j[-1]+b-1)+
    log(j[-1]+theta-2)-2*log(2*j[-1]+theta-2)-log(2*j[-1]+theta-1)-log(2*j[-1]+theta-3))))
  J<-diag((diagonal-mu)/sd)
  J[cbind(j,j+1L)]<-off/sd;J[cbind(j+1L,j)]<-off/sd
  eig<-eigen(J,symmetric=TRUE);o<-order(eig$values)
  z<-eig$values[o]
  # Christoffel weights avoid tiny eigenvector components corrupted by
  # floating-point eigenvector noise under positive quadratic tilts.
  dz<-diag(J);ez<-off/sd
  previous<-rep(0,order);current<-rep(1,order);sumsq<-rep(1,order);logscale<-rep(0,order)
  for(k in seq_len(order-1L)) {
    nextp<-((z-dz[k])*current-if(k>1L)ez[k-1L]*previous else 0)/ez[k]
    large<-pmax(abs(nextp),abs(current))>1e80
    if(any(large)) {
      nextp[large]<-nextp[large]*1e-80;current[large]<-current[large]*1e-80
      previous[large]<-previous[large]*1e-80;sumsq[large]<-sumsq[large]*1e-160
      logscale[large]<-logscale[large]+80*log(10)
    }
    sumsq<-sumsq+nextp^2;previous<-current;current<-nextp
  }
  logw<--log(sumsq)-2*logscale
  peak<-max(logw);logw<-logw-peak-log(sum(exp(logw-peak)));weights<-exp(logw)
  skew<-2*(b-a)*sqrt(theta+1)/((theta+2)*sqrt(a*b))
  kurt<-3+6*((a-b)^2*(theta+1)-a*b*(theta+2))/(a*b*(theta+2)*(theta+3))
  c<-sqrt(kurt-1-skew^2)
  features<-cbind(z,(z^2-1-skew*z)/c)
  result<-list(features=features,logw=logw,weights=weights,sd=sd,
    skew=skew,kurt=kurt,c=c,order=order,mu=mu,theta=theta)
  keys<-.beta_spm_rules$keys
  if(length(keys)>=32L){rm(list=keys[1L],envir=.beta_spm_rules);keys<-keys[-1L]}
  assign(key,result,.beta_spm_rules);.beta_spm_rules$keys<-c(keys,key);result
}

.beta_spm_cgf <- function(lambda,rule) {
  lambda<-matrix(lambda,ncol=2)
  q<-lambda%*%t(rule$features)
  q<-sweep(q,2,rule$logw,"+")
  peak<-apply(q,1,max);w<-exp(q-peak);total<-rowSums(w);w<-w/total
  mean<-w%*%rule$features
  u<-rule$features[,1];v<-rule$features[,2]
  second<-w%*%cbind(u*u,u*v,v*v)
  h11<-second[,1]-mean[,1]^2;h12<-second[,2]-mean[,1]*mean[,2]
  h22<-second[,3]-mean[,2]^2
  list(K=peak+log(total),mean=mean,h11=h11,h12=h12,h22=h22,det=h11*h22-h12^2)
}

.beta_spm_tilt <- function(target,rule,tolerance=1e-9,maxit=100L) {
  target<-matrix(target,ncol=2);M<-nrow(target)
  # Start at the population tilt; dual line search guards every Newton step.
  lambda<-matrix(0,M,2)
  done<-rep(FALSE,M);failure<-rep(FALSE,M)
  for(iteration in seq_len(maxit)) {
    ids<-which(!done&!failure);if(!length(ids))break
    z<-.beta_spm_cgf(lambda[ids,,drop=FALSE],rule)
    g<-z$mean-target[ids,,drop=FALSE]
    scaled<-apply(abs(g)/(1+abs(target[ids,,drop=FALSE])),1,max)
    newly<-scaled<tolerance;done[ids[newly]]<-TRUE
    bad<-!is.finite(z$det)|z$det<=1e-18
    failure[ids[bad&!newly]]<-TRUE
    use<-which(!newly&!bad);if(!length(use))next
    indices<-ids[use]
    delta<-cbind((z$h22[use]*g[use,1]-z$h12[use]*g[use,2])/z$det[use],
                 (z$h11[use]*g[use,2]-z$h12[use]*g[use,1])/z$det[use])
    decrement<-rowSums(g[use,,drop=FALSE]*delta)
    old<-z$K[use]-rowSums(lambda[indices,,drop=FALSE]*target[indices,,drop=FALSE])
    step<-rep(1,length(indices));accepted<-rep(FALSE,length(indices))
    for(backtrack in 1:35) {
      remain<-which(!accepted);if(!length(remain))break
      cand<-lambda[indices[remain],,drop=FALSE]-step[remain]*delta[remain,,drop=FALSE]
      cz<-.beta_spm_cgf(cand,rule)
      obj<-cz$K-rowSums(cand*target[indices[remain],,drop=FALSE])
      ok<-is.finite(obj)&obj<=old[remain]-1e-4*step[remain]*decrement[remain]+1e-14
      if(any(ok)) {
        lambda[indices[remain[ok]],]<-cand[ok,,drop=FALSE]
        accepted[remain[ok]]<-TRUE
      }
      step[remain[!ok]]<-step[remain[!ok]]/2
    }
    failure[indices[!accepted]]<-TRUE
  }
  z<-.beta_spm_cgf(lambda,rule)
  residual<-apply(abs(z$mean-target)/(1+abs(target)),1,max)
  valid<-is.finite(z$det)&z$det>0&residual<tolerance*5
  list(lambda=lambda,rate=rowSums(lambda*target)-z$K,logdet=log(z$det),
       valid=valid,residual=residual)
}

.beta_spm_support <- function(m,theta,n) {
  j<-floor(n*m);fraction<-n*m-j
  vmax<-(j+fraction^2)/n-m^2
  variance<-m*(1-m)/(theta+1)
  m>0&m<1&variance>0&variance<vmax
}

.beta_spm_logdensity <- function(eta,fitted_theta,mu,theta,n,order=128L) {
  m<-plogis(eta);rule<-.beta_spm_rule(mu,theta,order)
  variance<-m*(1-m)/(fitted_theta+1)
  keep<-.beta_spm_support(m,fitted_theta,n)
  ans<-rep(-Inf,length(m));if(!any(keep))return(ans)
  target_u<-(m[keep]-mu)/rule$sd
  target_w<-(variance[keep]/rule$sd^2+target_u^2-1-rule$skew*target_u)/rule$c
  target<-cbind(target_u,target_w)
  tilt<-.beta_spm_tilt(target,rule)
  if(any(!tilt$valid))stop("MOM tilt did not converge: ",sum(!tilt$valid),"/",sum(keep),
    "; max residual ",max(tilt$residual),"; order ",order,call.=FALSE)
  # Finite quadrature can falsely agree while missing a separated remote mode.
  # Restrict the integrated experimental interface to a sufficient log-concavity
  # regime for the physical-x tilted logit integration kernel
  # a*log(x)+b*log(1-x)+A*x^2+B*x. This is a numerical support restriction,
  # not a change to the MOM estimator or its acceptance event.
  a<-mu*theta;b<-(1-mu)*theta
  A<-tilt$lambda[,2]/(rule$c*rule$sd^2)
  concavity_bound<-(a^(1/3)+b^(1/3))^3/2
  if(any(!is.finite(A)|A>=concavity_bound))
    stop("Tilt outside the supported log-concave quadrature regime; remote modes cannot be certified.",call.=FALSE)
  if(any(tilt$rate< -1e-8))stop("Negative MOM saddlepoint rate.",call.=FALSE)
  logv<- -abs(eta[keep])-2*log1p(exp(-abs(eta[keep])))
  jac<-2*logv+log(fitted_theta)-2*log1p(fitted_theta)-3*log(rule$sd)-log(rule$c)
  ans[keep]<-log(n/(2*pi))-.5*tilt$logdet-n*tilt$rate+jac
  ans
}

.beta_spm_covariance <- function(mu,theta) {
  r<-.beta_spm_rule(mu,theta,32L)
  B<-rbind(c(r$sd,0),c((1-2*mu)/r$sd-(theta+1)*r$skew,-(theta+1)*r$c))
  B%*%t(B)
}

.beta_spm_conditional <- function(logtheta_z,setup,tolerance=2e-6) {
  th<-setup$theta*exp(setup$sv*logtheta_z)
  logf<-function(z).beta_spm_logdensity(setup$eta+setup$sm*z,th,setup$mu,setup$theta,setup$n,setup$order)
  radius<-max(10,abs(setup$rho*logtheta_z)+6)
  for(attempt in 1:4) {
    z<-seq(-radius,radius,length.out=33);f<-logf(z);j<-which.max(f)
    if(j>1L&&j<length(z)&&is.finite(f[j]))break
    radius<-radius*1.5
  }
  if(j==1L||j==length(z)||!is.finite(f[j]))stop("MOM conditional mode unsupported.",call.=FALSE)
  mode<-optimize(function(z)-logf(z),c(z[j-1],z[j+1]),tol=1e-7)$minimum
  peak<-logf(mode);h<-.02
  curvature<--(logf(mode+h)-2*peak+logf(mode-h))/h^2
  if(!is.finite(curvature)||curvature<=0)stop("MOM conditional curvature unsupported.",call.=FALSE)
  scale<-1/sqrt(curvature);density<-function(u)exp(logf(mode+scale*u)-peak)
  span<-8
  while(max(density(c(-span,span)))>1e-12&&span<160)span<-span*1.3
  if(max(density(c(-span,span)))>1e-10)stop("MOM conditional tails did not converge; fitted theta=",
    th,"; mu=",setup$mu,"; n=",setup$n,"; mode=",mode,"; scale=",scale,
    "; peak=",peak,"; span=",span,"; tails=",paste(density(c(-span,span)),collapse=","),call.=FALSE)
  converged<-FALSE
  for(cells in c(64L,128L,256L,512L,1024L,2048L,4096L,8192L)) {
    u<-seq(-span,span,length.out=2*cells+1L);f<-density(u);width<-2*span/cells
    left<-seq.int(1L,2*cells-1L,by=2L);mass<-width/6*(f[left]+4*f[left+1L]+f[left+2L])
    total<-sum(mass);cdf<-c(0,cumsum(mass))/total;knots<-u[seq.int(1,length(u),by=2L)]
    cl<-seq.int(1L,2*cells-3L,by=4L);cmass<-2*width/6*(f[cl]+4*f[cl+2L]+f[cl+4L])
    ccdf<-c(0,cumsum(cmass))/sum(cmass)
    cf<-stats::splinefun(knots[seq.int(1L,length(knots),by=2L)],ccdf,method="hyman")
    error<-max(abs(cf(knots)-cdf),abs(total/sum(cmass)-1))
    if(is.finite(error)&&error<tolerance){converged<-TRUE;break}
  }
  if(!converged)stop("MOM conditional CDF did not converge: ",error,
    "; theta_hat=",th,"; mu=",setup$mu,"; n=",setup$n,"; span=",span,
    "; scale=",scale,call.=FALSE)
  fun<-stats::splinefun(knots,cdf,method="hyman");eta_mode<-setup$eta+setup$sm*mode;eta_scale<-setup$sm*scale
  list(theta=th,logmass=peak+log(eta_scale)+log(total),error=error,cdf=function(eta) {
    u<-(eta-eta_mode)/eta_scale
    if(u<=-span)return(0);if(u>=span)return(1);fun(u)
  })
}

.beta_spm_core <- function(mu,theta,n,limit,limit_type,order=128L,tolerance=2e-5,levels=5L) {
  if(mu>.5)return(.beta_spm_core(1-mu,theta,n,1-limit,if(limit_type=="upper")"lower" else "upper",order,tolerance,levels))
  if(n<2||n!=floor(n))stop("MOM requires integer n >= 2.",call.=FALSE)
  cov<-.beta_spm_covariance(mu,theta)
  setup<-list(mu=mu,theta=theta,n=n,eta=qlogis(mu),sm=sqrt(cov[1,1]/n)/(mu*(1-mu)),
    sv=sqrt(cov[2,2]/n)/theta,rho=cov[1,2]/sqrt(cov[1,1]*cov[2,2]),order=order)
  rule_fun<-.beta_sp_legendre
  probability<-.beta_sp_probability_core
  q<-abs(limit-mu)/sqrt(mu*(1-mu)/(theta+1));probes<-unique(pmax(0,c(0,q+c(-.5,-.2,0,.2,.5))))
  previous<-NULL
  for(level in seq_len(levels)) {
    N<-c(24L,48L,96L,192L,384L)[level];radius<-c(6,8,10,12,14)[level]
    rule<-rule_fun(N);conditional<-lapply(rule$x*radius,.beta_spm_conditional,setup=setup)
    logs<-vapply(conditional,`[[`,0,"logmass")+log(rule$w*radius*setup$sv)
    peak<-max(logs);weights<-exp(logs-peak);lognorm<-peak+log(sum(weights));weights<-weights/sum(weights)
    core<-list(conditional=conditional,weights=weights,theta_hat=vapply(conditional,`[[`,0,"theta"),
      limit=limit,limit_type=limit_type,log_normalizer=lognorm,order=N,radius=radius)
    probs<-vapply(probes,function(k)probability(core,k),0)
    if(!is.null(previous)) {
      error<-max(abs(probs-previous$p),abs(expm1(lognorm-previous$lognorm)))+
        max(vapply(conditional,`[[`,0,"error"))

      if(error<tolerance){core$numerical_error<-error;return(core)}
    }
    previous<-list(p=probs,lognorm=lognorm)
  }
  stop("MOM outer quadrature did not converge.",call.=FALSE)
}

.beta_spm_cache <- new.env(parent=emptyenv())
.beta_spm_population_core <- function(mu,theta,n,limit,limit_type) {
  .validate_beta_parameters(mu,theta)
  .validate_beta_spm_plan(n,0)
  if(!is.numeric(limit)||length(limit)!=1L||!is.finite(limit)||limit<=0||limit>=1)
    stop("The Beta specification limit must be strictly between 0 and 1.",call.=FALSE)
  limit_type<-match.arg(limit_type,c("upper","lower"))
  if(mu>.5){mu<-1-mu;limit<-1-limit;limit_type<-if(limit_type=="upper")"lower" else "upper"}
  key<-paste(format(c(mu,theta,n,limit),digits=17),limit_type,collapse="|")
  if(exists(key,.beta_spm_cache,inherits=FALSE)) {
    result<-get(key,.beta_spm_cache)
    if(inherits(result,"error"))stop(result)
    return(result)
  }
  result<-tryCatch({
    previous<-.beta_spm_core(mu,theta,n,limit,limit_type,order=128L)
    current<-.beta_spm_core(mu,theta,n,limit,limit_type,order=256L)
    q<-abs(limit-mu)/sqrt(mu*(1-mu)/(theta+1))
    probes<-unique(pmax(0,c(0,q+c(-.5,-.2,0,.2,.5))))
    delta<-max(vapply(probes,function(k)abs(.beta_sp_probability_core(current,k)-
      .beta_sp_probability_core(previous,k)),0),abs(expm1(current$log_normalizer-previous$log_normalizer)))
    if(!is.finite(delta)||delta>=2e-5)stop("Population CGF quadrature did not converge.",call.=FALSE)
    current$population_quadrature_order<-256L
    current$numerical_error<-max(current$numerical_error,previous$numerical_error,delta)
    current
  },error=function(e)simpleError(paste0("Saddlepoint MOM OC is numerically unsupported: ",
    conditionMessage(e)," No Delta/MLE fallback was used; experimental validation remains incomplete.")))
  keys<-.beta_spm_cache$keys
  if(length(keys)>=8L){rm(list=keys[1L],envir=.beta_spm_cache);keys<-keys[-1L]}
  assign(key,result,.beta_spm_cache);.beta_spm_cache$keys<-c(keys,key)
  if(inherits(result,"error"))stop(result)
  result
}

.beta_spm_probability <- function(mu,theta,n,k,limit,limit_type) {
  .validate_beta_spm_plan(n,k)
  core<-.beta_spm_population_core(mu,theta,n,limit,limit_type)
  p<-.beta_sp_probability_core(core,k)
  if(!is.finite(p)||p<0||p>1)stop("Invalid saddlepoint MOM probability.",call.=FALSE)
  p
}

.beta_spm_plan <- function(PRQ,CRQ,alpha,beta,USL,LSL,theta,sigma_type,sigma) {
  type<-if(!is.null(USL))"upper" else "lower";limit<-if(type=="upper")USL else LSL
  .validate_beta_parameters(.5,theta)
  if(!is.numeric(limit)||length(limit)!=1L||!is.finite(limit)||limit<=0||limit>=1)
    stop("The Beta specification limit must be strictly between 0 and 1.",call.=FALSE)
  mus<-vapply(c(PRQ,CRQ),.beta_saddlepoint_mean,0,theta=theta,limit=limit,limit_type=type)
  # A Normal-size warm start avoids inheriting raw-moment cancellation from
  # the legacy Delta MOM optimizer at large, nearly symmetric Beta shapes.
  z1<-qnorm(1-PRQ);z2<-qnorm(1-CRQ);za<-qnorm(1-alpha);zb<-qnorm(1-beta)
  k0<-(z1*zb+z2*za)/(za+zb)
  initial_size<-max(2,ceiling(((za+zb)/(z1-z2))^2*(1+k0^2/2)))
  cache<-new.env(parent=emptyenv())
  at_size<-function(n) {
    key<-as.character(n)
    if(exists(key,cache,inherits=FALSE))return(get(key,cache))
    # Consumer endpoint first: unsupported numerical work must abort the design.
    cr<-.beta_spm_population_core(mus[2],theta,n,limit,type)
    pr<-.beta_spm_population_core(mus[1],theta,n,limit,type)
    pa<-function(k,j).beta_sp_probability_core(if(j==1)pr else cr,k)
    if(pa(0,1)<1-alpha)result<-list(feasible=FALSE) else {
      hi<-2
      while(pa(hi,1)>1-alpha&&hi<1e6)hi<-hi*2
      if(hi>=1e6)stop("Saddlepoint MOM constant could not be bracketed.",call.=FALSE)
      k<-uniroot(function(k)pa(k,1)-(1-alpha),c(0,hi),tol=2e-8)$root
      result<-list(feasible=pa(k,2)<=beta,k_high=k,cores=list(pr,cr))
    }
    assign(key,result,cache);result
  }
  n<-initial_size;test<-at_size(n)
  if(test$feasible) {
    hi<-n;lo<-max(2,floor(n/2))
    while(lo>2&&at_size(lo)$feasible){hi<-lo;lo<-max(2,floor(lo/2))}
    if(at_size(lo)$feasible)hi<-lo
  } else {
    lo<-n;hi<-2*n
    while(hi<=100000&&!at_size(hi)$feasible){lo<-hi;hi<-2*hi}
    if(hi>100000)stop("No supported feasible saddlepoint MOM size below 100000.",call.=FALSE)
  }
  while(hi-lo>1) {
    mid<-floor((lo+hi)/2)
    if(at_size(mid)$feasible)hi<-mid else lo<-mid
  }
  n<-hi;selected<-at_size(n)
  while(n>2&&at_size(n-1)$feasible){n<-n-1;selected<-at_size(n)}
  if(!selected$feasible)stop("No analytically feasible saddlepoint MOM plan.",call.=FALSE)
  cores<-selected$cores;pa<-function(k,j).beta_sp_probability_core(cores[[j]],k)
  k_low<-if(pa(0,2)<=beta)0 else uniroot(function(k)pa(k,2)-beta,
    c(0,max(2,selected$k_high*2)),tol=2e-8)$root
  if(k_low>selected$k_high+1e-7)stop("No feasible saddlepoint MOM constant interval.",call.=FALSE)
  k<-(k_low+selected$k_high)/2;risks<-c(1-pa(k,1),pa(k,2))
  if(any(!is.finite(risks))||risks[1]>alpha+1e-7||risks[2]>beta+1e-7)
    stop("Saddlepoint MOM integer plan failed working-risk verification.",call.=FALSE)
  structure(list(distribution="beta",sigma=sigma,theta=theta,sigma_type=sigma_type,
    theta_type="unknown",method="saddlepoint_mom",PRQ=PRQ,CRQ=CRQ,PR=risks[1],CR=risks[2],
    USL=USL,LSL=LSL,n=n,sample_size=n,k=k,
    integration=list(experimental=TRUE,validation="incomplete",
      numerical_error_estimate=vapply(cores,`[[`,0,"numerical_error"),
      population_quadrature_order=256L,quadrature_order=vapply(cores,`[[`,0L,"order"))),class="VarPlan")
}
