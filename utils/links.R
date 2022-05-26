logit       <- function(x) log( x/(1-x) )
logit.inv   <- function(x) exp(x)/(1+exp(x))
logistic    <- logit.inv

probit      <- function(x) qnorm(x)
probit.inv  <- function(x) pnorm(x)

cauchit     <- function(x) tan(pi * (x-0.5))
cauchit.inv <- function(x) 0.5 + atan(x)/pi

cloglog     <- function(x) log( -log(1-x) )
cloglog.inv <- function(x) 1-exp( -exp(x) )

loglog      <- function(x) -log( -log( x) )
loglog.inv  <- function(x) exp( -exp(-x) )
