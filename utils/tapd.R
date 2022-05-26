tapd <- function(x, na.rm=TRUE) {
### Function to calculate the total absolute proportion 
### deviation (tapd) of a single categorical variable.
### This number ranges between 1 (uniform) and 0 (single
### outcome).


  if(na.rm) {
    drop = which( is.na(x) )
    if(length(drop)>0) x = x[-drop]
  }

  tt = table(x)  
  g = length(tt)
  n = length(x)
  nbar = n/g

1-sum( abs(as.numeric(tt)-nbar) )/n
}




