# Experimental normalized joint saddlepoint distribution of the Beta MLE.
# For fitted shapes h and population shapes a, the density with respect to h
# is proportional to sqrt(det(I(h))) * exp(-n * KL(Beta(h) || Beta(a))).
# All quadrature weights are positive and are fixed independently of k.

.validate_beta_saddlepoint_plan <- function(n,k) {
  if(!is.numeric(n)||is.complex(n)||length(n)!=1L||!is.finite(n)||n<2||n!=floor(n))
    stop("Saddlepoint MLE inspection requires one integer sample size n >= 2.",call.=FALSE)
  if(!is.numeric(k)||is.complex(k)||length(k)!=1L||!is.finite(k)||k<0)
    stop("Saddlepoint MLE requires one finite non-negative constant k.",call.=FALSE)
}

.beta_sp_log1pmx <- function(x) {
  ans <- log1p(x) - x
  small <- abs(x) < 0.01
  z <- x[small]
  ans[small] <- z^2*(-1/2+z*(1/3+z*(-1/4+z*(1/5+z*(-1/6+
    z*(1/7+z*(-1/8+z*(1/9-z/10))))))))
  ans
}

.beta_sp_stirling <- function(x) {
  ans <- numeric(length(x)); large <- x > 20
  z <- 1/x[large]
  ans[large] <- z/12-z^3/360+z^5/1260-z^7/1680+z^9/1188
  z <- x[!large]
  ans[!large] <- lgamma(z)-(z-.5)*log(z)+z-.5*log(2*pi)
  ans
}

.beta_sp_residual <- function(x, kind) {
  ans <- numeric(length(x)); large <- x > 100
  z <- 1/x[large]; y <- x[!large]
  if (kind == "digamma") {
    ans[large] <- -.5-z/12+z^3/120-z^5/252+z^7/240
    ans[!large] <- y*(digamma(y)-log(y))
  } else if (kind == "trigamma1") {
    ans[large] <- z/2+z^2/6-z^4/30+z^6/42-z^8/30
    ans[!large] <- y*trigamma(y)-1
  } else {
    ans[large] <- .5+z/6-z^3/30+z^5/42-z^7/30
    ans[!large] <- y^2*trigamma(y)-y
  }
  ans
}

.beta_sp_kl <- function(eta, fitted_theta, mu, theta) {
  m <- plogis(eta); om <- plogis(-eta)
  a <- m*fitted_theta; b <- om*fitted_theta
  d <- m-mu
  # Stable Bernoulli KL with cancellation of its two linear terms.
  bernoulli <- -mu*.beta_sp_log1pmx(d/mu)-
    (1-mu)*.beta_sp_log1pmx(-d/(1-mu))
  far <- abs(d) > .01*min(mu,1-mu)
  if (any(far)) {
    logm <- -pmax(-eta[far],0)-log1p(exp(-abs(eta[far])))
    logom <- -pmax(eta[far],0)-log1p(exp(-abs(eta[far])))
    bernoulli[far] <- mu*(log(mu)-logm)+(1-mu)*(log1p(-mu)-logom)
  }
  a0 <- mu*theta; b0 <- (1-mu)*theta
  logv <- -abs(eta)-2*log1p(exp(-abs(eta)))
  .5*(logv-log(mu)-log1p(-mu)+log(fitted_theta/theta))+
    theta*bernoulli+
    .beta_sp_stirling(a0)+.beta_sp_stirling(b0)-.beta_sp_stirling(theta)-
    .beta_sp_stirling(a)-.beta_sp_stirling(b)+.beta_sp_stirling(fitted_theta)+
    (1-a0/a)*.beta_sp_residual(a,"digamma")+
    (1-b0/b)*.beta_sp_residual(b,"digamma")-
    (1-theta/fitted_theta)*.beta_sp_residual(fitted_theta,"digamma")
}

.beta_sp_logdensity <- function(eta, fitted_theta, mu, theta, n) {
  kl <- .beta_sp_kl(eta,fitted_theta,mu,theta)
  ans <- rep(-Inf,length(eta))
  active <- is.finite(kl) & n*kl < 10000
  if (!any(active)) return(ans)
  e <- eta[active]; m <- plogis(e); om <- plogis(-e)
  a <- m*fitted_theta; b <- om*fitted_theta
  sd <- sqrt(m*om/(fitted_theta+1))
  i11 <- (fitted_theta*sd)^2*(trigamma(a)+trigamma(b))
  i12 <- fitted_theta*sd*(.beta_sp_residual(a,"trigamma1")-
                         .beta_sp_residual(b,"trigamma1"))
  i22 <- .beta_sp_residual(a,"trigamma2")+
    .beta_sp_residual(b,"trigamma2")-.beta_sp_residual(fitted_theta,"trigamma2")
  det <- i11*i22-i12^2
  if (any(!is.finite(det)|det<=0) || any(kl[active] < -1e-8)) {
    stop("Saddlepoint MLE density is numerically unsupported for these parameters.",call.=FALSE)
  }
  logv <- -abs(e)-2*log1p(exp(-abs(e)))
  # Shape -> (logit mean, log precision) Jacobian is included here.
  ans[active] <- log(n/(2*pi))+.5*log(det)+.5*logv+
    .5*log1p(fitted_theta)-n*kl[active]
  ans
}

.beta_sp_legendre <- local({
  cache <- new.env(parent=emptyenv())
  function(order) {
    key <- as.character(order)
    if (exists(key,cache,inherits=FALSE)) return(get(key,cache))
    i <- seq_len(order-1L); b <- i/sqrt(4*i*i-1)
    J <- matrix(0,order,order)
    J[cbind(i,i+1L)] <- b;J[cbind(i+1L,i)] <- b
    e <- eigen(J,symmetric=TRUE);o <- order(e$values)
    result <- list(x=e$values[o],w=2*e$vectors[1,o]^2)
    assign(key,result,cache);result
  }
})

.beta_sp_conditional <- function(logtheta_z, setup, tolerance=2e-6) {
  th <- setup$theta*exp(setup$sv*logtheta_z)
  if (!is.finite(th)||th<=0) stop("Unsupported saddlepoint precision range.",call.=FALSE)
  logf <- function(z) .beta_sp_logdensity(setup$eta+setup$sm*z,th,
                                         setup$mu,setup$theta,setup$n)
  radius <- max(12,abs(setup$rho*logtheta_z)+6)
  underflow <- FALSE
  for (attempt in 1:4) {
    z <- seq(-radius,radius,length.out=33)
    f <- logf(z)
    underflow <- all(!is.finite(f))
    if(underflow)f<--.beta_sp_kl(setup$eta+setup$sm*z,th,setup$mu,setup$theta)
    j <- which.max(f)
    if(j>1L && j<length(z)) break
    radius <- radius*2
  }
  if(j==1L||j==length(z))stop("Saddlepoint mean mode could not be bracketed.",call.=FALSE)
  objective <- if(underflow)function(z).beta_sp_kl(setup$eta+setup$sm*z,th,setup$mu,setup$theta) else
    function(z)-logf(z)
  mode <- optimize(objective,c(z[j-1L],z[j+1L]),tol=1e-7)$minimum
  peak <- logf(mode)
  # These remote precision nodes have exp(-n*KL) below floating-point range.
  if(!is.finite(peak)||peak< -700)return(list(theta=th,logmass=-Inf,error=0,cdf=function(eta)0))
  h <- .02;curvature <- -(logf(mode+h)-2*peak+logf(mode-h))/h^2
  if(!is.finite(curvature)||curvature<=0)stop("Unsupported saddlepoint mean curvature.",call.=FALSE)
  scale <- 1/sqrt(curvature)
  density <- function(u)exp(logf(mode+scale*u)-peak)
  span <- 10
  while(max(density(c(-span,span)))>1e-12 && span<80)span<-span*1.5
  if(max(density(c(-span,span)))>1e-10)stop("Saddlepoint mean tails did not converge.",call.=FALSE)
  converged <- FALSE
  for(cells in c(64L,128L,256L,512L,1024L)) {
    u <- seq(-span,span,length.out=2*cells+1L);f<-density(u);width<-2*span/cells
    left<-seq.int(1L,2*cells-1L,by=2L)
    mass<-width/6*(f[left]+4*f[left+1L]+f[left+2L])
    total<-sum(mass);cdf<-c(0,cumsum(mass))/total;knots<-u[seq.int(1L,length(u),by=2L)]
    coarse_left<-seq.int(1L,2*cells-3L,by=4L)
    cmass<-2*width/6*(f[coarse_left]+4*f[coarse_left+2L]+f[coarse_left+4L])
    ccdf<-c(0,cumsum(cmass))/sum(cmass)
    cf<-stats::splinefun(knots[seq.int(1L,length(knots),by=2L)],ccdf,method="hyman")
    error<-max(abs(cf(knots)-cdf),abs(total/sum(cmass)-1))
    if(is.finite(error)&&error<tolerance){converged<-TRUE;break}
  }
  if(!converged)stop("Saddlepoint conditional CDF did not converge (error = ",
    format(error,digits=4),", mu = ",setup$mu,", theta = ",setup$theta,
    ", n = ",setup$n,", fitted theta = ",th,
    ").",call.=FALSE)
  fun<-stats::splinefun(knots,cdf,method="hyman")
  eta_mode<-setup$eta+setup$sm*mode;eta_scale<-setup$sm*scale
  list(theta=th,logmass=peak+log(eta_scale)+log(total),error=error,
       cdf=function(eta) {
         u<-(eta-eta_mode)/eta_scale
         if(u<=-span)return(0)
         if(u>=span)return(1)
         fun(u)
       })
}

.beta_sp_cut <- function(limit,k,theta,limit_type) {
  c<-k/sqrt(theta+1)
  upper<-function(s) {
    result<-numeric(length(c));big<-c>1
    result[!big]<-2*s*s/(2*s+c[!big]^2+c[!big]*sqrt(c[!big]^2+4*s*(1-s)))
    r<-1/c[big]
    result[big]<-2*(s*r)^2/(1+2*s*r*r+sqrt(1+4*s*(1-s)*r*r))
    result
  }
  if(limit_type=="upper")upper(limit) else 1-upper(1-limit)
}

.beta_sp_probability_core <- function(core,k) {
  cut<-.beta_sp_cut(core$limit,k,core$theta_hat,core$limit_type)
  p<-vapply(seq_along(core$conditional),function(i)core$conditional[[i]]$cdf(qlogis(cut[i])),0)
  if(core$limit_type=="lower")p<-1-p
  sum(core$weights*p)/sum(core$weights)
}

.beta_sp_cache <- new.env(parent=emptyenv())
.beta_sp_core <- function(mu,theta,n,limit,limit_type,tolerance=2e-5) {
  key<-paste(format(c(mu,theta,n,limit,tolerance),digits=17),limit_type,collapse="|")
  if(exists(key,.beta_sp_cache,inherits=FALSE))return(get(key,.beta_sp_cache))
  cov<-.beta_mle_covariance(mu,theta)
  sm<-sqrt(cov[1,1]/n)/(mu*(1-mu));sv<-sqrt(cov[2,2]/n)/theta
  rho<-cov[1,2]/sqrt(cov[1,1]*cov[2,2])
  setup<-list(mu=mu,theta=theta,n=n,eta=qlogis(mu),sm=sm,sv=sv,rho=rho)
  direction<-if(limit_type=="upper")1 else -1
  sigma<-sqrt(mu*(1-mu)/(theta+1));q<-direction*(limit-mu)/sigma
  grad<-.beta_delta_gradient(mu,theta,max(0,q),limit_type)
  sek<-sqrt(drop(t(grad)%*%cov%*%grad)/n)/sigma
  probes<-unique(c(0,pmax(0,q+c(-2,-1.64,-1.28,0,1.28,1.64,2)*sek)))
  previous<-NULL
  for(level in 1:4) {
    order<-c(32L,64L,128L,256L)[level];radius<-c(12,16,20,24)[level]
    rule<-.beta_sp_legendre(order);nodes<-rule$x*radius
    conditional<-lapply(nodes,.beta_sp_conditional,setup=setup,tolerance=min(2e-6,tolerance/8))
    logs<-vapply(conditional,`[[`,0,"logmass")+log(rule$w*radius*sv)
    peak<-max(logs);weights<-exp(logs-peak);lognorm<-peak+log(sum(weights));weights<-weights/sum(weights)
    core<-list(conditional=conditional,weights=weights,
      theta_hat=vapply(conditional,`[[`,0,"theta"),limit=limit,limit_type=limit_type,
      log_normalizer=lognorm,order=order,radius=radius)
    probabilities<-vapply(probes,function(k).beta_sp_probability_core(core,k),0)
    if(!is.null(previous)) {
      error<-max(abs(probabilities-previous$p),abs(expm1(lognorm-previous$lognorm)))+
        max(vapply(conditional,`[[`,0,"error"))
      if(error<tolerance) {
        core$numerical_error<-error
        keys<-.beta_sp_cache$keys
        if(length(keys)>=48L) {
          rm(list=keys[1L],envir=.beta_sp_cache);keys<-keys[-1L]
        }
        assign(key,core,.beta_sp_cache);.beta_sp_cache$keys<-c(keys,key)
        return(core)
      }
    }
    previous<-list(p=probabilities,lognorm=lognorm)
  }
  stop("Saddlepoint MLE quadrature did not converge; no Delta fallback was used.",call.=FALSE)
}

.beta_sp_population_core <- function(mu,theta,n,limit,limit_type) {
  if(mu>.5) {
    mu<-1-mu;limit<-1-limit
    limit_type<-if(limit_type=="upper")"lower" else "upper"
  }
  .beta_sp_core(mu,theta,n,limit,limit_type)
}

.beta_saddlepoint_probability <- function(mu,theta,n,k,limit,limit_type) {
  .validate_beta_parameters(mu,theta)
  if(!is.numeric(n)||length(n)!=1L||!is.finite(n)||n<2||
     !is.numeric(k)||length(k)!=1L||!is.finite(k)||k<0)
    stop("Saddlepoint MLE requires finite n >= 2 and k >= 0.",call.=FALSE)
  if(length(limit)!=1L||!is.finite(limit)||limit<=0||limit>=1)
    stop("The Beta specification limit must be strictly between 0 and 1.",call.=FALSE)
  limit_type<-match.arg(limit_type,c("upper","lower"))
  core<-.beta_sp_population_core(mu,theta,n,limit,limit_type)
  p<-.beta_sp_probability_core(core,k)
  if(!is.finite(p)||p<0||p>1)stop("Invalid saddlepoint acceptance probability.",call.=FALSE)
  p
}

.beta_saddlepoint_mean <- function(p,theta,limit,limit_type) {
  if(!is.numeric(p)||length(p)!=1L||!is.finite(p)||p<0||p>1)
    stop("p must be one finite proportion between 0 and 1.",call.=FALSE)
  if(p==0)return(if(limit_type=="upper")0 else 1)
  if(p==1)return(if(limit_type=="upper")1 else 0)
  objective<-function(m)pbeta(limit,m*theta,(1-m)*theta,lower.tail=limit_type=="lower")-p
  root<-uniroot(objective,c(.Machine$double.eps,1-.Machine$double.eps),tol=.Machine$double.eps)$root
  if(abs(objective(root))>1e-7)stop("Saddlepoint MLE quality inversion did not converge.",call.=FALSE)
  root
}

.beta_saddlepoint_plan <- function(PRQ,CRQ,alpha,beta,USL,LSL,theta,sigma_type,sigma) {
  limit_type<-if(!is.null(USL))"upper" else "lower"
  limit<-if(limit_type=="upper")USL else LSL
  .validate_beta_parameters(.5,theta)
  mus<-vapply(c(PRQ,CRQ),.beta_saddlepoint_mean,0,theta=theta,limit=limit,limit_type=limit_type)
  cache<-new.env(parent=emptyenv())
  at_size<-function(n) {
    key<-format(n,digits=17)
    if(exists(key,cache,inherits=FALSE))return(get(key,cache))
    cores<-lapply(mus,.beta_sp_population_core,theta=theta,n=n,limit=limit,limit_type=limit_type)
    pa<-function(k,which).beta_sp_probability_core(cores[[which]],k)
    if(pa(0,1)<1-alpha) {
      result<-list(feasible=FALSE,excess=1,n=n)
    } else {
      hi<-2
      while(pa(hi,1)>1-alpha&&hi<1e6)hi<-hi*2
      if(hi>=1e6)stop("Saddlepoint MLE acceptance constant could not be bracketed.",call.=FALSE)
      k<-uniroot(function(k)pa(k,1)-(1-alpha),c(0,hi),tol=2e-8)$root
      cr<-pa(k,2)
      result<-list(feasible=cr<=beta,excess=cr-beta,n=n,k_high=k,cores=cores)
    }
    assign(key,result,cache);result
  }
  # The Normal approximation supplies a starting size, never the returned OC.
  z1<-qnorm(1-PRQ);z2<-qnorm(1-CRQ);za<-qnorm(1-alpha);zb<-qnorm(1-beta)
  k0<-(z1*zb+z2*za)/(za+zb)
  initial<-max(10,((za+zb)/(z1-z2))^2*(1+k0^2/2))
  hi<-initial
  while(!at_size(hi)$feasible&&hi<1e5)hi<-hi*2
  if(hi>=1e5)stop("Saddlepoint MLE could not find a supported feasible size below 100000.",call.=FALSE)
  lo<-max(2,hi/2)
  while(at_size(lo)$feasible&&lo>2) {hi<-lo;lo<-max(2,lo/2)}
  continuous<-if(at_size(lo)$feasible)lo else uniroot(function(n)at_size(n)$excess,
    c(lo,hi),tol=1e-3)$root
  n<-max(2,ceiling(continuous));selected<-at_size(n)
  for(attempt in 1:100) {
    if(selected$feasible)break
    n<-n+1;selected<-at_size(n)
  }
  if(!selected$feasible)stop("Saddlepoint integer plan is analytically infeasible.",call.=FALSE)
  # Verify adjacent integer feasibility; no claim of global minimality over
  # all possible sizes is made for this experimental numerical method.
  while(n>2) {
    previous<-at_size(n-1)
    if(!previous$feasible)break
    n<-n-1;selected<-previous
  }
  cores<-selected$cores;pa<-function(k,which).beta_sp_probability_core(cores[[which]],k)
  if(pa(0,2)<=beta)k_low<-0 else {
    upper<-max(2,selected$k_high)
    while(pa(upper,2)>beta&&upper<1e6)upper<-upper*2
    k_low<-uniroot(function(k)pa(k,2)-beta,c(0,upper),tol=2e-8)$root
  }
  if(k_low>selected$k_high+1e-7)stop("No feasible saddlepoint acceptance-constant interval.",call.=FALSE)
  k<-(k_low+selected$k_high)/2
  risks<-c(1-pa(k,1),pa(k,2))
  if(any(!is.finite(risks))||risks[1]>alpha+1e-7||risks[2]>beta+1e-7)
    stop("Saddlepoint MLE plan failed analytical risk verification.",call.=FALSE)
  structure(list(distribution="beta",sigma=sigma,theta=theta,sigma_type=sigma_type,
    theta_type="unknown",method="saddlepoint_mle",PRQ=PRQ,CRQ=CRQ,
    PR=risks[1],CR=risks[2],USL=USL,LSL=LSL,sample_size=n,n=n,k=k,
    n_continuous=continuous,
    integration=list(numerical_error_estimate=vapply(cores,`[[`,0,"numerical_error"),
      quadrature_order=vapply(cores,`[[`,0L,"order"),experimental=TRUE)),
    class="VarPlan")
}
