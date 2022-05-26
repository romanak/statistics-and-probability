ogive <- function(var) {
  xCut = table(var) 
  relFreq=c(cumsum(xCut))/length(var)
data.frame(x=sort(var),relFreq)

}
