cv <- function(x, na.rm=TRUE) {
# This function calculates the coefficient
# of variation of a variable. The CV is
# the ratio of the standard deviation to 
# the mean of a variable. It is a measure 
# of spread that can be compared across different
# variables.

ss = sd(x, na.rm=na.rm)
mm = mean(x, na.rm=na.rm)

ss/mm
}

