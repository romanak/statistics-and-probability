##
## function: histogram
##
## input:    x, breaks
##
## output:   graphic


histogram = function(x,
	breaks=NULL, xlab=NULL,
	xlim=NULL, ylim=NULL, 
      xaxt="n", yaxt="n",
	col=NULL, border=NULL,
	xaxs="i",yaxs="i",
	ww=NULL ) {

if( is.null(breaks)  ) breaks = 21
if( length(breaks)<2 ) breaks = seq(min(x),max(x),length=breaks)
if( length(breaks)>1 ) breaks = breaks

if(is.null(xlab))   xlab=deparse(substitute(x))
if(is.null(border)) border="white"
if(is.null(col))    col="cyan"


hh = hist(x, breaks, plot=FALSE)
if(is.null(xlim)) {
  x.min = min(hh$breaks)
  x.max = max(hh$breaks)
} else {
  x.min=xlim[1]
  x.max=xlim[2]
}

if(is.null(ylim)) {
  y.min = 0
  y.max = max(hh$density)*1.02
} else {
  y.min = ylim[1]
  y.max = ylim[2]
}


plot( c(x.min,x.max),c(0,y.max),
	type="n", xlab="", ylab="", bty="n",
      xaxt=xaxt, yaxt=yaxt, xaxs=xaxs, yaxs=yaxs )


for(i in 1:length(hh$density) ) {
  rect( hh$breaks[i],0,hh$breaks[i+1],hh$density[i],col=col,border=border )
}
segments(x.min,y.min,x.max,y.min)

}

