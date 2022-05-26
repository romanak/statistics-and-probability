binom.plot <- function( x, n=rep(sum(x),length(x)), 
        conf.level=0.95, 
        names=NULL, 
        xlim=NULL, ylim=NULL, xlab=NA, ylab="Proportion",
        boxcol=NULL, border=NA, midlinecol="black",midlinety=1,
        ... ) {

# Here is a little plotting function that allows you to plot
# quasi-barcharts for proportion data. To use it, run it, then
# call the function like the examples in this script.
# You will need to run it each time you start R if you
# want to use it.
#
# If you forget, you will get an error message saying 
#       Error: could not find function "binom.plot"
#

  g = length(x)
  if( length(n)!=g ) stop("The lengths of x and n must be the same");
  if(length(conf.level)>1) stop("The confidence level needs to be a single value.")
  if( conf.level<=0.50 || conf.level>=1.00) stop("The confidence level must be between 0.50 and 1.00, exclusive.")

  if( is.null(boxcol) ) {
        boxcol="honeydew2"
  }
  boxcol = rep(boxcol,g)

  ucl = numeric()
  lcl = numeric()

  for(i in 1:g) {
    tt=binom.test(x[i],n[i], conf.level=conf.level)
    lcl[i]=tt$conf.int[1]
    ucl[i]=tt$conf.int[2]
  }

  if(is.null(xlim)) xlim=c(0.5,g+0.5)
  if(is.null(ylim)) ylim=c(0,1) 

  plot.new()
  plot.window(xlim=xlim,ylim=ylim)

  if(!is.null(names)) mtext(side=1, at=1:g, text=names)
  if(is.null(names))  mtext(side=1, at=1:g, text=1:g)
  axis(2)
  title(xlab=xlab)
  title(ylab=ylab)

  for(i in 1:g) {
    thisColor = boxcol[i]
    rect(i-0.15,lcl[i],i+0.15,ucl[i], col=thisColor, border=border)
    segments(i-0.25,x[i]/n[i],i+0.25,x[i]/n[i], lwd=2, col=midlinecol, lty=midlinety)
  }



}

