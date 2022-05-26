overlay<-function(x, dist="norm", na.rm=TRUE, breaks="Sturges", 
	from=NA, to=NA, length=1e4, xlim=NA, ylim=NA,
	col="dodgerblue", fill="aliceblue", 
	rug=FALSE, onTop=FALSE, yaxs="i", las=1,
	xlab="The Variable", ylab="", main="", 
	maxDF=1000,yaxt="n", ... ) {
### This function decides whether the distribution is one of the 
#   listed distributions. If it is, then plot the histogram ++ PLUS ++
#   the density function in the background.
#
#   If not, only plot the histogram with a warning about not having the
#   distribution.
#
#   There are several defaults hard-coded. I think these need to be 
#   removed, eventually.
#
###



### Preamble: New function to make things easier
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





### Preamble: The density function (pdf of given density)
ddensity <- function(dist, from, to, maxDF=1001, length=length) {


xx = seq(from, to, length=length)
res="ERROR"






### Exponential distribution
  if (dist =="exp") {
    if (min(x)<0) stop("Cannot have negative values in the data, because \n\tthe Exponential distribution has a non-negative sample \n\tspace.")
    res=dexp(xx, rate=1/mean(x))
    param = list(lambda=1/mean(x))
  }
#


### Normal distribution
  if(dist == "norm"){
    res=dnorm(xx, m=mean(x), s=sd(x))
    param = list(mu=mean(x), sd=sd(x))
  } 
#


### Student's t distribution
  if (dist == "t"){
    L = numeric() # Log-likelihood
    for (nu in 1:maxDF){
      L[nu] = sum(log(dt(x, df=nu)))
    }
    df = which.max(L)
    res=dt(xx, df)
    if(df == maxDF) notation="Estimated df at edge of space. Perhaps increase maxDF."
    param = list(df=df)
  }
#


### Chi-square distribution
  if (dist == "chisq")	{
    L = numeric() # Log-likelihood
    if (min(x)<0){
      stop("Error: Cannot have negative values in the data, because the Chi-square distribution has a non-negative sample space.")
	} else {
      for (nu in 1:maxDF){
        L[nu] = sum(log(dchisq(x, df=nu)))
      }
      df = which.max(L)
      res=dchisq(xx, df)
      if(df == maxDF) notation="Estimated df at edge of space. Perhaps increase maxDF."
      param=list(df=df)
    }
  }
#


### Gamma distribution
  if (dist == "gamma"){
    if (min(x)<0){
      stop("Error: Cannot have negative values in the data, because the Gamma distribution has a non-negative sample space.")
    } else {
      shape = (mean(x))^2/var(x)
      rate  = mean(x)/var(x)
      res=dgamma(xx, shape=shape, rate=rate)
      param=list(shape=shape, rate=rate)
    }
  }
#


### Cauchy distribution
  if (dist == "cauchy"){
    theta = median(x)
    gamma = IQR(x)/2
    res=dcauchy(xx, location=theta, scale=gamma)
    param=list(location=theta, scale=gamma)
  }
#


### Uniform distribution
  if (dist == "unif"){
    a = min(x)
    b = max(x)
    res=dunif(xx, min=a, max=b)
    param = list(min=a, max=b)
  }
#
list(density=res, parameters=param)
}


### End of the Preamble












### convert input of dist into lowercase. This helps reduce checking.
dist = tolower(dist)





### Check the input should be the next section of a function. This 
#   provides appropriate warnings for various situations.
numna = sum(is.na(x))
if( na.rm && numna==1 ) warning(paste("There is one (1) missing value in the data. It was \n\tremoved for the histogram."))
if( na.rm && numna>1 )  warning(paste("There are",numna,"missing values in the data. They \n\twere removed for the histogram."))
if(!na.rm && numna>0 )  stop(paste("There are",numna,"missing values in the data. A \n\thistogram cannot be created."))







# If we're excluding the missing data, then x2 is a vector 
# containing *only* available data
x2 = x
if( na.rm==TRUE ) { x2 = x[ !is.na(x) ] }






###  Provides the information in the data's histogram
x  = x2
tt = hist(x, breaks=breaks, plot=FALSE)


# Start determining the graph of the density
ww = max(tt$breaks)-min(tt$breaks)
if( is.na(from) ) from = min(tt$breaks)-0.5*ww
if( is.na(to)   ) to   = max(tt$breaks)+0.5*ww
xx = seq(from,to,length=length)



#y is the density of the distribution
output = ddensity(dist, from=from, to=to, length=length)
yy = output$density





if(is.na(xlim[1])) 	xlim=c(from,to)
if(is.na(ylim[1])) 	ylim=c(0,max(tt$density,yy))

## Begin plotting
par(las=las)
par(yaxt="n")
par(yaxs=yaxs)
par(mar=c(3,0,0,0)+1)
par(font.lab=2, cex.lab=1.2, cex.axis=0.9)

plot.new()
plot.window( xlim=xlim, ylim=ylim, yaxs=yaxs )


abline(h=0)
polygon( x=c(xx,rev(xx)), y=c(yy,rep(0,length(yy))), col=fill, border=col)

if( onTop ) {

  # polygon over the histogram
  plotHist(tt)
  rug(jitter(x))
  if( fill!="" || is.na(fill) ) {
    polygon( x=c(xx,rev(xx)), y=c(yy,rep(0,length(yy))), col=fill, border=col)
  } else {
    lines(xx,yy, col=col)
  }

} else {
  # histogram over the polygon
  plotHist(tt)
  lines(xx, yy, col=col)
}

axis(1)


title(xlab=xlab )
title(ylab="")
title(main=main)



return( output[[-1]] )


}  ## Function End



