
############################
## Measures of dispersion ##
############################

# calculate the sample variance
weight = c(3, 2, 5, 6, 4)
var(weight)

# calculate the population variance
weight = c(3, 2, 5, 6, 4)
# the multiplier is (n-1)/n
var(weight)*(4/5)

# create your own function to calculate
# the population variance in R
varPop = function(x) {
	mean((x-mean(x))^2)
}

# use the function
varPop(weight)

# calculate the sample standard deviation
x = c(11, 18, 25, 51, 44, 29, 30, 17, 29, 47, 52, 60)
sd(x)

# calculate the population standard deviation
sqrt(varPop(x))

