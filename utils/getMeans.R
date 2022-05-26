getMeans <- function(x, n=1) {
#
# This is a helper function for calculating sample
# means from observed data. Note that this can
# be used to illustrate the Central Limit Theorem
#
	N = length(x)
	tt = x
	r = N%%n
	if(r==1) tt = x[-N]
	if(r>1) tt = x[-(1:r)]

	mm = matrix(tt, nrow=n)
	vals = apply(mm,2,mean)

return(vals)
}
