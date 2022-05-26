quartile <- function(x, q=0:4) {
# Helper function to calculate the quartiles of a variable. 
#
#  x is the data
#  q is the quartile desired
#
  qs = length(q)
  if( qs<1 ) qs=0:4

  if(min(q)<0) stop("Specified quartile less than 0. \n\tPlease use quartiles between 0 and 4, inclusive")
  if(max(q)>4) stop("Specified quartile greater than 4.\n\tPlease use quartiles between 0 and 4, inclusive")

  ptile = q/4
  res = quantile(x,ptile)
  names(res) = q
res
}