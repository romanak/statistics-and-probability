###########################
## Mean, median and mode ##
###########################

# calculate the mean of the distribution
sleep = c(5, 6, 8, 10, 4, 6, 9)
mean(sleep)

# weighted mean
weights = c(40, 20, 10, 30)
grades = c(95, 45, 66, 90)
sum(grades*weights)/sum(weights)

grades = c(95, 45, 66, 100)
sum(grades*weights)/sum(weights)

grades = c(95, 45, 66, 0)
sum(grades*weights)/sum(weights)

grades = c(95, 45, 66, 54.7)
sum(grades*weights)/sum(weights)

# calculate the median
median(c(3,4,6,7,2,8,9))
median(c(5,7,8,1,4,9,8,9))

# calculate the mode
source("https://rfs.kvasaheim.com/stat200.R")
modal(c(6,4,6,1,7,8,7,2,5,7))

# calculate mean, median, and mode
age = c(84, 80, 82, 77, 78, 80, 79, 42)
mean(age)
median(age)
modal(age)

# determine the skewness
hildebrand.rule(age)