
#################################
## Graphical displayes of data ##
#################################

# tabulate the data to get the frequency distribution
table(census4)

# plot the pie chart
pie(table(census4))

# bar graph
barplot(table(census4))

# pareto chart
barplot(sort(table(census4), decreasing=TRUE))

# horizontal pareto chart
barplot(sort(table(census4)), horiz=TRUE)

# stacked bar graph
table(census4, domPolCulture)
barplot(table(census4, domPolCulture), legend=TRUE)
# switch the order of variables for stacked bar graph
barplot(table(domPolCulture, census4), legend=TRUE)
# add colors
barplot(table(census4, domPolCulture), legend=TRUE, col=1:4)
barplot(table(domPolCulture, census4), legend=TRUE, col=4:6)

# side-by-side bar graph
barplot(table(domPolCulture, census4), beside=TRUE, legend=TRUE, col=4:6)
barplot(table(census4, domPolCulture), beside=TRUE, legend=TRUE, col=4:7)

# histogram
hist(vcrime90)
# you need to do multiple histograms to get a better feel about the data
hist(vcrime90, breaks=11)
hist(vcrime90, breaks=21)
hist(vcrime90, breaks=51)
hist(vcrime90, breaks=1001)
hist(vcrime90, breaks=5)
hist(vcrime90, breaks=2)
hist(vcrime90, breaks=seq(0,2500,100))
hist(vcrime90, breaks=seq(0,2600,200))
hist(vcrime90, breaks=seq(0,2600,250))
hist(vcrime90, breaks=seq(0,2600,10))
hist(vcrime90, breaks=seq(0,2600,150))

# stem-and-leaf plot
stem(vcrime90)

# line graph
year = seq(1920, 2010, 10)
cpi = c(20.0, 16.7, 14.0, 24.1, 29.6, 38.8, 82.4, 130.7, 172.2, 218.1)
plot(year, cpi, type="b") # lines and dots
plot(year, cpi, type="l") # lines
points(year, cpi) # add dots

