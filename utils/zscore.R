zscore <- function(x, na.rm=TRUE, names=NA) {
# This function calculates the z-score
# for all values in the x variable

	m = mean(x, na.rm=na.rm)
	s = sd(x, na.rm=na.rm)
	zscore = (x-m)/s
	if( !is.na(names[1]) ) names(zscore)=names
return(zscore)
}

