shapiroTest <- function(y, ...) {

## This test allows you to easily perform
## the Shapiro-Wilk test of Normality across
## several levels of a grouping variable.
##
## y is the measurement (response variable)
## g is the grouping variable (indep var)
##
## This can be entered as either y,g or as y~g
## v0.98

UseMethod("shapiroTest")
}





shapiroTest.default = function( y, g=NA, ... ) {

res = list(Title="Shapiro-Wilk Test")

if(is.na(g[1])) {

shapiro.test(y)

} else {
  lev = unique(g) 
  ss  = aggregate(y, list(g), length) 
  drp = which( (ss[ ,2])<3 | ss[ ,2]>5000 )
  st  = function(m) { 
         ww=shapiro.test(m)$p.value*adj
         ifelse(ww>1,1,ww)
        }

  adj = 1; adjust="Bonferroni"  #####
  if(adjust=="Bonferroni") { adj = length(lev); res$adjustment=paste("Bonferroni (",adj,")",sep="") }

  if(length(drp)>0) {
    warning(paste("\tThe following levels have fewer than 3 or greater than 5000 values:",
      "\n\t",(ss[drp,1]),
      "\n\tThey have been removed from consideration.",sep="") 
    )

    reeN = numeric()
    reeV = numeric()
    for( j in 1:length( lev ) ) {
      ss=length(which(g==lev[j]))
      if( ss>=3 & ss<=5000 ) {
        reeN[j] = as.character(lev[j])  
        reeV[j] = st( y[grp==lev[j]])
      }
    }
    ree = data.frame(reeN,reeV)
  } else {
    ree = aggregate(y, list(g), st)
  }
  class(ree[,2]) = "numeric"
  res$results = ree
  names(res$results) = c("Level","p.value")
res

}

}







shapiroTest.formula = function(formula, na.rm=TRUE, ...) {
  if (missing(formula) || (length(formula) != 3L)) 
      stop("'formula' missing or incorrect.\nOnly one grouping variable allowed")
  m <- match.call(expand.dots = FALSE)

  if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
  m[[1L]] <- quote(model.frame)

  mf <- eval(m, parent.frame())
  if (length(mf) != 2L) 
      stop("'formula' should be of the form response ~ group")

  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  xx <- do.call("shapiroTest.default", as.list(mf))

xx
}






