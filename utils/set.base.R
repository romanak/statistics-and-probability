set.base <-
function(var, base, data=NULL) {

  # Check that base is a current level
  lev <- with(data, levels(var) )
  t <- length(which(lev==base))
  if (t<1) { 
    # Not a current level, this means an error
    stop("Error:\tThe base selected is not a current factor level. \n\t\tPlease select a current factor level as a base level.")
  }
  lev <- unique( c(base, with(data,levels(var)) ) )
  ret <- with(data, factor(var, levels=lev, ordered=is.ordered(var) ))
  return(ret)
}

