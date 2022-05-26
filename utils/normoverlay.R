normoverlay <- function(data, na.rm=TRUE, breaks="Sturges", 
	from=NA, to=NA, length=1e4,
	col="dodgerblue", fill="aliceblue", 
	rug=FALSE, onTop=FALSE, xlab=NA, yaxs="i", 
	main=NA, ... ) {
### Creates a histogram and overlays it with a Normal density. This 
### function requires the data. Additional parameters are what to 
### do with NA values (na.rm=TRUE) and the breaks for the histogram.
### One may also specify the start and end of the curve (to, from), 
### the color of the curve (col), the fill color (fill), and
### whether the curve is on top of the histogram or behind it.
###
### v1.1.1
###

plotHist <- function(hh, col="black", fill="white") {
  hhmin = min(hh$breaks); hhmax = max(hh$breaks); 
  mult=hh$density[1]/hh$count[1]; lineMargin=0.15*(hh$breaks[2]-hh$breaks[1])
  for( i in 1:length(hh$density) ) {
    rect( hh$breaks[i],0,hh$breaks[i+1],hh$density[i],border=col,col="grey90")
    if(max(hh$counts)>50) next
    if(hh$counts[i]<2) next
    for(k in 2:hh$counts[i]-1) {
      segments( hh$breaks[i]+lineMargin,k*mult, hh$breaks[i+1]-lineMargin,k*mult,
		col="white", lwd=2)
    }
  }
}


numna = sum(is.na(data))
if( na.rm && numna==1 ) warning(paste("There is one (1) missing value in the data. It was \n\tremoved for the histogram."))
if( na.rm && numna>1 )  warning(paste("There are",numna,"missing values in the data. They \n\twere removed for the histogram."))
if(!na.rm && numna>0 )  stop   (paste("There are",numna,"missing values in the data. A \n\thistogram cannot be created."))



x = data
if( na.rm==TRUE ) { x = data[ !is.na(data) ] }

tt = hist(x, breaks=breaks, plot=FALSE)

if( is.na(from) ) from=min(tt$breaks)
if( is.na(to) ) to=max(tt$breaks)

xx = seq(from,to,length=length)
yy = dnorm(xx, m=mean(x),s=sd(x) )

dataName = deparse(substitute(data))
if(is.na(main)) {
  maintitle=paste("Histogram for",dataName)
} else {
  maintitle=main
}
if(is.na(xlab)) xlab=dataName


## Begin plotting

plot.new()
plot.window( xlim=c(from,to), ylim=c(0,max(tt$density,yy)*1.05),yaxs=yaxs )
abline(h=0)
polygon( x=c(xx,rev(xx)), y=c(yy,rep(0,length(xx))), col=fill, border=col)

if( onTop ) {

  # polygon over the histogram
  plotHist(tt)
  rug(jitter(x))
  if( fill!="" || is.na(fill) ) {
    polygon( x=c(xx,rev(xx)), y=c(yy,rep(0,length(xx))), col=fill, border=col)
  } else {
    lines(xx,yy, col=col)
  }

} else {
  # histogram over the polygon
  plotHist(tt)
  lines(xx,yy, col=col)
}

axis(1); axis(2)
title(xlab=xlab )
title(ylab="Relative Frequency")
title(main=maintitle)

}

