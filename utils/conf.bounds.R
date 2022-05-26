### Given model

conf.bounds = function(mod, conf.level=0.95, from=NA, to=NA, smoothness=1e4)  {
### This function calculates the Working-Hotelling confidence
### bounds for an OLS regression model. Simple linear regression
### only works for this function.
###
### The required variable is the lm model.
###
### Optional parameters are the confidence level (95%) and
### the level of curve smoothness (10,000) -- the more the smoother.
###


indepVars = names(mod$model)[-1]
nVars = length(indepVars)
if(nVars>1) {
  stop("This procedure only works in the case of 1 independent variable.")
}

xx=mod$model
x=xx[[2]]
y=xx[[1]]

minX = from; maxX = to
if(is.na(from)) minX = min(x)
if(is.na(to))   maxX = max(x)




## distributional mutiplier
n = length(x)
FCV = sqrt( 2*qf(conf.level, df1=2,df2=n-2) )

## fit and standard error
newX = seq(minX,maxX,length=smoothness)
modTemp = lm(y~x)
pr = predict(modTemp, newdata=data.frame(x=newX), se.fit=TRUE)

ucb = pr$fit + FCV * pr$se.fit
lcb = pr$fit - FCV * pr$se.fit


results = list(x=newX, y=pr$fit, ucb=ucb, lcb=lcb, conf.level=conf.level)

return(results)
}

