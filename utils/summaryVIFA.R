# 
# v0.2
#
# Function to print out the regression table
# when adjusting for the VIF
#
summaryVIFA = function(mod,showVIF=FALSE) {

vv=vif(mod)
ss=summary(mod)
nvars=length(vv)

newTable = list( Name="(Intercept)",Estimate=ss$coefficients[1,1],StdErr=ss$coefficients[1,2],tValue=ss$coefficients[1,3],pValue=ss$coefficients[1,4])

for(i in 1:nvars) {
  v = i + 1
  sif = sqrt(vv[i])                      # Std Err inflation factor
  ase = ss$coefficients[v,2]/sif         # Adjusted Std Err
  ats = ss$coefficients[v,1]/ase         # Adjusted test statistic
  apv = 2*(1-pt(abs(ats), df=ss$df[2]))  # Adjusted p-value

newTable[["Name"]][v]=attr(ss$terms,"term.labels")[i]
newTable[["Estimate"]][v]=ss$coefficient[v,1]
newTable[["StdErr"]][v]=ase
newTable[["tValue"]][v]=ats
newTable[["pValue"]][v]=apv

}

if(showVIF) {
  results=cbind( mod$coefficients, newTable$StdErr, newTable$tValue, newTable$pValue, c(NA,vv) )
  colnames(results) <- c("Estimate","Std. Error","t value","Pr(>|t|)","VIF")
} else {
  results=cbind( mod$coefficients, newTable$StdErr, newTable$tValue, newTable$pValue )
  colnames(results) <- c("Estimate","Std. Error","t value","Pr(>|t|)")
}

return(results)

} # END FUNCTION

