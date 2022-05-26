means <- 
function(x, type="all", trim=0, na.rm=FALSE) {
if(na.rm) { 
rm=which(is.na(x))
if(length(rm)==0) break
x=x[-rm]
}
if(min(x)<=0 && type!="arithmetic") { 
type="arithmetic"
warning("Variable contains non-positive value. Only arithmetic mean will be returned.", call.=FALSE)
}
n=length(x)
if(trim<0.0) trim=0.0
if(trim>0.5&&trim<1.0) trim=0.5
if(trim>=1&&trim<=n/2) trim=trim/n
if(trim>n/2) trim=0.5
cutno=n*trim
x=sort(x)[(cutno+1):(n-cutno)]
if(type=="arithmetic" || type=="all") m=ma=mean(x, trim=trim)
if(type=="geometric"  || type=="all") m=mg=exp( mean(log(x)) )
if(type=="harmonic"   || type=="all") m=mh=(sum(1/x))^(-1)*length(x)
if(type=="all") {
return( list(Arithmetic=ma, Geometric=mg, Harmonic=mh, trim=trim) )
} else {
 return ( list(Mean=m, trim=trim) )
}
}