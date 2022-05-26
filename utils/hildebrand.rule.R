
### Work in progress

hildebrand.rule <- function(x, ...) {
UseMethod("hildebrand.rule")
}



hildebrand.rule.default = function(x, y=NA, rec=0, na.rm=TRUE) {
	if( is.na(y[1]) ) {
		r=(mean(x, na.rm=na.rm)-median(x, na.rm=na.rm) )/sd(x, na.rm=na.rm)
	} else {
		y = as.factor(y)
		r = data.frame(NA,NA,NA)
		colnames(r) = c("Group","Ratio","Skew")
		t = length(unique(y))
		for(k in 1:t) {
			r[k,1] = as.character(unique(y)[k])
			r[k,2] = hildebrand.rule.default( x[y==levels(y)[k]],rec=1 )
				r[k,3]="No Skew"
			if(r[k,2] > +0.20) r[k,3]="Positive Skew"
			if(r[k,2] < -0.20) r[k,3]="Negative Skew"
		}
}

	if( rec<1 & is.null(dim(r)) ) {
		r2 = data.frame("xx ",r,NA)
			r2[1,3]="No Skew"
		if(r > +0.20) r2[1,3]="Positive Skew"
		if(r < -0.20) r2[1,3]="Negative Skew"
		colnames(r2) = c("Group","Ratio","Skew")
		r=r2
	}
r
}





hildebrand.rule.formula = function(formula, na.rm=TRUE) {
  if (missing(formula) || (length(formula) != 3L)) 
      stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)

  if (is.matrix(eval(m$data, parent.frame()))) 
      m$data <- as.data.frame(data)
  m[[1L]] <- quote(model.frame)

  mf <- eval(m, parent.frame())
  if (length(mf) != 2L) 
      stop("'formula' should be of the form response ~ group")

  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  y <- do.call("hildebrand.rule.default", as.list(mf))

y
}



