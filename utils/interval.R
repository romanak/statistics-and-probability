
interval <- function(x, type="observation", level=0.95) {
#
# This function calculates three types of intervals, given
# the values: observation, prediction, and confidence. All
# three are based on the assumption mu and sigma are unknown.
#

type = tolower(type)
n = length(x)
a2 = (1-level)/2

if(type=="o" || type=="observation") {
	ub = quantile(x,1-a2)
	lb = quantile(x,a2)
}

if(type=="c" || type=="confidence") {
	ub = mean(x)-qt(a2, df=n-1)*sd(x)/sqrt(n)
	lb = mean(x)+qt(a2, df=n-1)*sd(x)/sqrt(n)
}

if(type=="p" || type=="prediction") {
	ub = mean(x)-qt(a2, df=n-1)*sd(x)*sqrt(1+1/n)
	lb = mean(x)+qt(a2, df=n-1)*sd(x)*sqrt(1+1/n)
}

res = list(upper=as.numeric(ub), lower=as.numeric(lb), level=level)
return(res)
}


