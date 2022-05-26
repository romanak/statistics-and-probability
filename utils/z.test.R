
z.test <- function(x, y=NULL, 
    sigmax, sigmay=sigmax, mu = 0,
    alternative="two.sided", 
    conf.level=0.95
    ) {

# Orig: 3/11/2016
# Curr: 4/25/2019

### This is the z-test for one and two samples.
### The input is standard. The test produces an
### object of class htest, as expected.
#
### The input:
###            x (and y, for two samples), the samples
###            sigmax (and sigmay, for two samples), the pop stdev
###            mu, the hypothesized population mean (for one sample)
###                 or difference (for two-samples)
###            alternative, one of "two.sided" (default), "greater", or "less"
###            conf.level (0.95 is default, corresponding to alpha = 0.05)
#



###
# Test for good input

error <- ""

if( sigmax <= 0 || sigmay<=0) { 
  error="Error: Variance out of bounds" 
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
# Perform the test

if(is.null(y)) {
  method <- "One-sample z-test"
  sx     <- sqrt( sigmax^2/length(x) )
  se     <- sx
  xbar   <- mean(x)
  z      <- (xbar-mu)/se
  est    <- xbar
  pest   <- xbar
  dn     <- deparse(substitute(x))
  names(est) <- paste("mean of",dn)
  nv     <- "mean"
} else {  
  method <- "Two-sample z-test"
  sx     <- sqrt( sigmax^2/length(x) )
  sy     <- sqrt( sigmay^2/length(y) )
  se     <- sqrt( sx^2 + sy^2 )
  xbar   <- mean(x)
  ybar   <- mean(y)
  z      <- ((xbar-ybar)-(mu))/se
  est    <- c(xbar,ybar)
  pest   <- xbar-ybar
  names(est)[1] <- paste( "mean of",deparse(substitute(x)) )
  names(est)[2] <- paste( "mean of",deparse(substitute(y)) )
  dn     <- paste(deparse(substitute(x)),"(s=",sigmax,") and ",deparse(substitute(y)),"(s=",sigmay,")",sep="")
  nv     <- "difference in means"
  if(sigmax!=sigmay) method <- paste(method,", unequal variances",sep="")
}


# Particular info
alpha  <- 1-conf.level
if(alternative=="two.sided") {
  p   <- 2*pnorm(-abs(z))
  lcp <- pest - qnorm(1-alpha/2) * se
  ucp <- pest + qnorm(1-alpha/2) * se
}
if(alternative=="less") {
  p   <- pnorm(z)
  lcp <- -Inf
  ucp <- pest + qnorm(1-alpha) * se
}
if(alternative=="greater") {
  p   <- 1-pnorm(z)
  lcp <- pest - qnorm(1-alpha) * se
  ucp <- Inf
}




###
# Create the results
parmtr        <- Inf
names(parmtr) <- "df"
stat          <- z
names(stat)   <- "z"
names(mu)     <- nv


# Populate the results object
results <- list(
  statistic=stat, 
  parameter= parmtr,
  p.value=p,
  conf.int=c(lcp, ucp),
  estimate=est,
  null.value=mu,
  alternative=alternative,
  method=method,
  data.name=dn,
  sigmax=sigmax,
  sigmay=sigmay
  )
attr(results$conf.int,"conf.level") <- conf.level
class(results) <- "htest"

return(results)
}



