##### New runs.test function
runs.test <- function(e, orderBy=x, alternative="two.sided") {
# Add this to the next version
# alternative="positive.correlated") {
# This is a wrapper function for the runs.test
# function at lawstat.

  require(lawstat)
  dd=lawstat:::runs.test(e[order(orderBy)],alternative=alternative)
  eName = deparse(substitute(e))
  xName = deparse(substitute(orderBy))

  dd$data.name = paste(eName,", as ordered by ",xName, sep="")
dd
}

