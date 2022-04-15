
##############################
## Basic operations on data ##
##############################

# import data
dt = read.csv("http://rfs.kvasaheim.com/data/crime.csv")

# try get a variable and get an error
repub

# instead first refer to the data
dt$repub

# in case we refer to variable often
attach(dt)

# now we can refer to variable directly
repub

# get the names of the variables
names(dt)

# get the summary of the variables
summary(dt)
















