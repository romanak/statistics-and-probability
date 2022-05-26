laakso <- function(seats) {
# This function calculates the "effective number 
# of seats" according to Laakso 1979. Note that 
# this is very similar to the "tapd" function
# that measures the 'variance' of a categorical
# variable.
#
# Here, the only variable needed is a vector of the
# number of seats held by the political parties.
#

  n = sum(seats)
  p = seats/n
1/sum(p^2)
}

