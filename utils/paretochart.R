paretochart = function(x, ...) {
# 
 if(class(x)=="table") {
  barplot( sort( x, decr=TRUE ), ... )
 } else {
  barplot( sort( table(x), decr=TRUE ), ... )
 }
}

