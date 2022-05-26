
####################################
# Population parameters estimation #
####################################

# mean
x = 0:3
p = c(0.125, 0.375, 0.375, 0.125)
sum(x*p)

# variance
x = 0:3
p = c(0.125, 0.375, 0.375, 0.125)
sum((x-1.5)^2*p)
sqrt(sum((x-1.5)^2*p))

# median
x = 0:3
p = c(0.125, 0.375, 0.375, 0.125)
# we need arg of cumsum >= 0.5 from left and right
cumsum(p) # cumsum from the left
rev(cumsum(rev(p))) # cumsum from the right

# mean
x = 0:5
p = c(0.1, 0.1, 0.2, 0.4, 0.1, 0.1)
sum(x*p)

# variance
x = 0:5
p = c(0.1, 0.1, 0.2, 0.4, 0.1, 0.1)
sum((x-2.6)^2*p)
sqrt(sum((x-2.6)^2*p))

# median
x = 0:5
p = c(0.1, 0.1, 0.2, 0.4, 0.1, 0.1)
# we need arg of cumsum >= 0.5 from left and right
cumsum(p) # cumsum from the left
rev(cumsum(rev(p))) # cumsum from the right













