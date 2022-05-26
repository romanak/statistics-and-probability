modal <- function(x) {
### R function to calculate
### the mode of a (categorical) 
### variable

 tt <- table(x)
 mc <- which(tt==max(tt))
 if(length(mc) == length(x)) {
  modal = "There is no mode."
 } else {
  modal = names(tt[mc])
 }
return(modal)
}

