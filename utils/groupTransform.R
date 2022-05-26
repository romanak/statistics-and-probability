
groupTransform <- function(groups, names) {

	g = length(groups)
	ng = numeric(g)
	gv = rep(names[1],length(groups[[1]]))
	xv = groups[[1]]
	
	for(i in 2:g) {
		gv = c(gv,rep(names[i],length(groups[[i]])))
		xv = c(xv,groups[[i]])
	}

rm(list=c("i","g","ng"))
gv = as.factor(gv)
data.frame(gv,xv)

}

