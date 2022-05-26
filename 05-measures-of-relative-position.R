
###################################
## Measures of relative position ##
###################################

# obtain the quartiles
a = c(60, 62, 63, 65, 67, 70, 71, 71, 75, 78, 79, 80, 81)
summary(a)

b = c(59, 66, 67, 67, 72, 74, 75, 75, 75, 76, 78, 79, 80, 81, 85)
summary(b)

# create a basic box plot
boxplot(a)

# the horizontal boxplot
boxplot(a, horizontal=TRUE, col="orange")

# calculate standard scores (z-scores)
source("https://rfs.kvasaheim.com/stat200.R")
a = c(60, 62, 63, 65, 65, 67, 70, 71, 71, 75, 78, 79, 80, 81)
# negative value indicates how many sd the data point is below average
zscore(a)[1]

