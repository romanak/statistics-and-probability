tukey.var.test <- function(x,y) {
  X = abs(x-median(x))
  Y = abs(y-median(y))
  wilcox.test(X,Y)
}


