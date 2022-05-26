# Model Comparison Measures

AIC2 <- function(model) {
k   <- attr( logLik(model), "df" )
lnl <- as.numeric(logLik(model))
ret <- -2*lnl + 2*k
return(ret)
}

SBC <- function(model) {
k   <- attr( logLik(model), "df" )
lnl <- as.numeric(logLik(model))
n   <- nobs(model)
ret <- -2*lnl + log(n)*k
return(ret)
}


AICc <- function(model) {
k   <- attr( logLik(model), "df" )
n   <- nobs(model)
ret <- AIC(model) + 2*k*(k+1)/(n-k-1)
return(ret)
}

SBCc <- function(model) {
k   <- attr( logLik(model), "df" )
n   <- nobs(model)
ret <- SBC(model) + 2*k*(k+1)/(n-k-1)
return(ret)
}


c.hat <- function(model) {
model$deviance/model$df.residual
}

QAIC <- function(model) {
k   <- attr( logLik(model), "df" )
lnl <- as.numeric(logLik(model))
ret <- -2*lnl/c.hat(model) + 2*k
return(ret)
}

QAICc <- function(model) {
k   <- attr( logLik(model), "df" )
n   <- nobs(model)
lnl <- as.numeric(logLik(model))
ret <- QAIC(model) + 2*k*(k+1)/(n-k-1)
return(ret)
}


Cp <- function(model) {
k   <- attr( logLik(model), "df" )
n   <- nobs(model)
SSE <- sum(model$residuals^2)
s   <- sd(model$y)
ret <- SSE/s^2 + 2*k-n
return(ret)
}


model.fit <- function(model) {
cat("\nModel Fit Statistics\n\n")

cat("\t-2log(Lik)\t",-2*logLik(model),"\n\n")

cat("\tAIC\t\t",AIC(model),"\n")
cat("\tAICc\t\t",AICc(model),"\n\n")

cat("\tSBC\t\t", SBC(model),"\n")
cat("\tSBCc\t\t",SBCc(model),"\n\n")

cat("\tQAIC\t\t", QAIC(model),"\n")
cat("\tQAICc\t\t",QAICc(model),"\n")

cat("\n\n")
}






