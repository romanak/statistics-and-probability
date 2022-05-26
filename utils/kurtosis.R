kurtosis <- function(x) {
# This function calculates the unbiased estimator
# of the sample excess kurtosis of a variable. 
# Kurtosis is the fouth central moment of a 
# distribution. It is a measure of concentration 
# about the mean.

  n = length(x)
  xbar = mean(x)
  m2 = sum((x-xbar)^2)/n
  m4 = sum((x-xbar)^4)/n
  g2 = m4/m2^2 - 3

(n-1)/(n^2-5*n+6)*((n+1)*g2+6)
}

