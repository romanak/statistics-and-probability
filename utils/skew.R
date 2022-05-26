skew <- function(x) {
# This function calculates the sample skewness 
# of a variable. Skew is the third central
# moment of a distribution. It is a measure
# of asymmetry.

  n = length(x)
  xbar = mean(x)
  m2 = sum((x-xbar)^2)/n
  m3 = sum((x-xbar)^3)/n
  g1 = m3/m2^(3/2)

g1*sqrt(n*(n-1))/(n-2)
}

