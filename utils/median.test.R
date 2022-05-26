median.test <- function(x, mu=0, conf.level=0.95){
# This function performs a simple median test.
# 
# x is the data
# mu is the hypothesized median
# conf.level is the confidence level
#
# This will print as a typical htest in R.
#

dr = which( is.na(x) )
y = x[-dr]
  above = sum(y>mu)
  below = sum(y<mu)
  equal = sum(y==mu)
  TS = min(above,below)

  p1 = pbinom(TS, size=length(y)-equal, prob=0.5)
  pv = 1-2*abs(0.5-p1)

  est = median(y)
  method <- "Median Test"
 
  names(est)[1] <- paste( "median of",deparse(substitute(x)) )

###
# Create the results
#parmtr        <- NA
#names(parmtr) <- "df"
stat          <- TS
names(stat)   <- "M"
p             <- pv
names(mu)     <- "population median"
dn     <- deparse(substitute(x))
 

# Populate the results object
results <- list(
  statistic=stat, 
#  parameter= parmtr,
  p.value=p,
#  conf.int=c(NA,NA),
#  estimate=est,
  null.value=mu,
  alternative="two.sided",
  method=method,
  data.name=dn
  )
#attr(results$conf.int,"conf.level") <- conf.level
class(results) <- "htest"

return(results)

}


