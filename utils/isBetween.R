isBetween <- function(x, min=0, max=1) {
#
# This function returns TRUE if x is between
# min and max, and FALSE otherwise.
#

	res = ( x>= min ) & (x <= max)
return(res)
}

isBetween <- Vectorize(isBetween)

