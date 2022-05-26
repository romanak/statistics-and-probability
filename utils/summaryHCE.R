summaryHCE <- function(model) {

### This function returns the Huber-White Heteroskedasticity
### Consistent Error adjustment to the usual regressions
###
### Required: model
###
### Returned: Regression table with HCE adjustment
###
### Tested to work with lm
###
### v1.0

  s  = summary(model)
  X  = model.matrix(model)
  u2 = residuals(model)^2

  Du = diag(u2)
  XpDX = t(X)%*%Du%*%X
  XpXi = solve(t(X)%*%X)
  varcovar = XpXi %*% XpDX %*% XpXi

  dfadj = sqrt(nrow(X)) / sqrt(nrow(X)-ncol(X))
  stderr = dfadj * sqrt( diag(varcovar) )

  t = model$coefficients/stderr
  p = 2*pnorm(-abs(t))
  p = round(p,6)

results = cbind(model$coefficients,stderr,t,p)
dimnames(results) <- dimnames(s$coefficients)
return(results)
}


