onevar.test <- function(x, s2=1, conf.level=0.95, alternative="two.sided") {

# Notes: This will perform the one-population variance 
# test. 
#
# The default confidence level is 0.95. This
# can be changed by altering parameter: conf.level
#
# The default null hypothesis is sigma^2 = 1. This
# can be changed by altering parameter: s2
#


###
# Test for good input

error <- ""

if( s2 <= 0 ) { 
  error="Error: Hypothesized variance out of bounds, must be positive" 
}
if(0>conf.level || 1<conf.level) { 
  error="Error: Confidence interval out of bounds"
}
if(alternative!="two.sided" && alternative!="less" && alternative!="greater") {
  error = "Error: Alternative specification not recognized"
}

if(error!="") {
  return(error)
}


###
# Create the test

n = length(x)


# The test statistic
  ts = (n-1)*var(x)/s2
  
  
  pvu = pchisq(ts, df=n-1, lower.tail=FALSE)
  pvl = pchisq(ts, df=n-1, lower.tail=TRUE)



# Particular info
alpha  <- 1-conf.level
if(alternative=="two.sided") {
  pval <- 2*min(pvu,pvl)
  lcl = (n-1)*var(x)/qchisq(1-alpha/2, df=n-1)
  ucl = (n-1)*var(x)/qchisq(alpha/2, df=n-1)
}
if(alternative=="less") {
  pval <- pvl
  lcl <- -Inf
  ucl <- (n-1)*var(x)/qchisq(alpha, df=n-1)
}
if(alternative=="greater") {
  pval <- pvu
  lcl <- (n-1)*var(x)/qchisq(1-alpha, df=n-1)
  ucl <- Inf
}


# Create results list
  res <- list()
  class(res) <- "htest"
  res$conf.level=conf.level

  res$statistic=ts
  names(res$statistic)="X2"

  res$parameter = n-1
  names(res$parameter) = "df"

  res$p.value = pval

  res$alternative=alternative
  res$data.name=deparse(substitute(x))
  res$conf.int=c( lcl,ucl )
  res$method="One-Sample Variance Test"
  
  res$estimate = var(x)
  names(res$estimate) = paste(" variance of",res$data.name)

  res$null.value = s2
  names(res$null.value) = "variance"

  attr(res$conf.int,"conf.level") <- conf.level

return(res)
}




 