### binom.pred
binom.pred <- function(x,n) {
# This function predicts the future proportion
# This is *not* the confidence interval. It is
# the prediction interval.
#
# x is th enumber of successes
# n is the number of trials
#
 phat = x/n
 lpl = phat+qnorm(0.025)*sqrt(phat*(1-phat)*2*1/n)
 upl = phat+qnorm(0.975)*sqrt(phat*(1-phat)*2*1/n)
list(name="Binomial Prediction Intervals",lower=lpl,upper=upl)
}
