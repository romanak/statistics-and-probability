wald.test <- function(x,n, p=0.5, pdiff=0, 
  conf.level=0.95, alternative="two.sided") {
  ### Wald Test function
  ### 
  ### This is the basic proportions test using the 
  ### Hawkes formula. Use it only for Hawkes. Other 
  ### formulas give a much better result... MUCH better
  ###
  ### What do we mean by "better"?
  ###  
  
  #
  if( (length(x)==2) && (length(n)==1) )  n=c(n,n)
  dn = paste(deparse(substitute(x)),"out of", deparse(substitute(n)))


  #
  if(length(x)==1) {   ## One-Sample Wald Test
  	x=x[1];n=n[1]
    if( x<5 || n-x<5 ) warning("The sample size is not large enough (see Section 8.4)")
    test = "One-Sample Wald Test (Hawkes only!!)"
    nv   = "proportions"
    phat=x/n
    se = sqrt( phat*(1-phat)/n)
    
    sew = sqrt( p*(1-p)/n)
    ts = (phat-p)/sew
    c = 1-conf.level
    if(alternative=="two.sided") { 
      pval = 2*pnorm(-abs(ts))
      alternativeHypothesis = paste("true proportion is not equal to",p)
      c=(1-conf.level)/2
    } else if(alternative=="less") {
      pval = pnorm(ts)
      alternativeHypothesis = paste("true proportion is less than",p)
    } else if (alternative=="greater") {
      pval = 1-pnorm(ts)
      alternativeHypothesis = paste("true proportion is greater than",p)
    } else {
      stop("Alternative hypothesis is not one of the three required:\n\t\"less\", \"two.sided\", or \"greater\"")
    }
    
    E = -qnorm(c)*se
    lcl = phat - E
    ucl = phat + E
    
    est=phat
    pest=phat
    names(est)[1] <- paste( "proportion of",deparse(substitute(x1)) )
    expectedValue=phat
  }
  
  
  
  if(length(x)==2) { ## Two-sample Wald tets
  	x1=x[1];x2=x[2];n1=n[1];n2=n[2]
  	
    if( x1<5 || n1-x1<5 || x2<5 || n2-x2<5 ) warning("At least one sample size is not large enough (see Section 9.4)")
    test  = "Two-Sample Wald Test (Hawkes only!!)"
    nv    = "difference in proportions"
    phat1 = x1/n1; phat2=x2/n2
    se = sqrt(phat1*(1-phat1)/n1 + phat2*(1-phat2)/n2)
    
    p = (x1+x2)/(n1+n2)
    ts = ((phat1-phat2)-pdiff) / sqrt(p*(1-p)*(1/n1+1/n2))
    
    c=1-conf.level
    if(alternative=="two.sided") { 
      pval = 2*pnorm(-abs(ts))
      alternativeHypothesis = paste("true difference in proportions is not equal to",pdiff)
      c = c/2
    } else if(alternative=="less") {
      pval = pnorm(ts)
      alternativeHypothesis = paste("true difference in proportions less than",pdiff)
    } else if (alternative=="greater") {
      pval = 1-pnorm(ts)
      alternativeHypothesis = paste("true difference in proportions is greater than",pdiff)
    } else { 
      stop("Alternative hypothesis is not one of the three required:\n\t\"less\", \"two.sided\", or \"greater\"")
    }
    
    E = -qnorm(c) * se
    lcl = phat1-phat2 - E
    ucl = phat1-phat2 + E
    est=c(phat1,phat2)
    pest=phat1-phat2
    names(est)[1] <- "prop 1"
    names(est)[2] <- "prop 2"
    expectedValue = pdiff
  }
#



### Set up the return list
#
# Populate the results object
results <- list(
  statistic=ts, 
  parameter= Inf,
  p.value=pval,
  conf.int=c(lcl, ucl),
  estimate=est,
  null.value=expectedValue,
  alternative=alternativeHypothesis,
  method=test,
  data.name=dn
  )
#
attr(results$conf.int,"conf.level") <- conf.level
names(results$statistic) = "z"
names(results$parameter) = "df"
names(results$null.value) = nv
#
class(results) <- "htest"
return(results)
}