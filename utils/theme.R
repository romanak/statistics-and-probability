theme <- function(th="STAT200", ...) {
	# This function allows on to quickly set the theme for all graphics.

  if(th=="STAT200") {
    par(xaxs="i", yaxs="i")
    par(family="serif", las=1)
    par(mar=c(3,3,0,1)+1)
    par(cex=1, cex.lab=1.2, cex.axis=0.9, cex.sub=0.9, cex.main=1.2 )
    par(font=1, font.lab=2, font.axis=1, font.sub=3, font.main=2)
    par(bg="transparent", xpd=NA, pch=20, col=1)
  }


  if(th=="book") {
    par(xaxs="i", yaxs="i")
    par(family="sans", las=1)
    par(mar=c(3,0,0,0)+1)
    par(cex=1, cex.lab=1.2, cex.axis=0.9, cex.sub=0.9, cex.main=1.2 )
    par(font=1, font.lab=2, font.axis=1, font.sub=3, font.main=2)
    par(bg="transparent", pch=20, col=1)
  }


  if(th=="slides") {
    par(cex=1.35)
    par(cex.axis=1.1, mar=c(4,3,1,1) )
  }

}
