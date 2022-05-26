bayes.law <- function(
	conmat=NULL, 
	pwild=NULL, 
	sensitivity=NULL, 
	specificity=NULL
	) {

## This function has four parameters:
##   conmat is the confusion matrix in canonical form
##   pwild is the disease prevalence       P[D]
##   sensitivity is the test sensitivity   P[+ | D]
##   specificity is the test specificity   P[- |!D]
##
## Canonical form is the first column is Diseased
## counts and the first row is Positive test.
##
## v1.1
##

## Prepare

pdis = acc = fpr = fnr = NULL


## Have a table?

if( !is.null(conmat) ) {

  # Parse the table
    S = conmat[1,2]
    T = conmat[2,2]
    U = conmat[2,1]
    V = conmat[1,1]

    sensitivity = V/(U+V)
    specificity = T/(S+T)

    acc  = (V+T)/sum(conmat)

} 


## No table
if( !is.null(sensitivity) & !is.null(specificity) ) {
    acc = NULL
}



# Calculate some accuracy statistics
  fpr  = 1 - specificity
  fnr  = 1 - sensitivity



# Prepare to use Bayes Law

if( !is.null(pwild) ) {

  # Calculate P[D|+]
    num = sensitivity*pwild
    den = num+( (fpr)*(1-pwild) )
    pdis = num/den
}




# Prepare output as a list
  res = list(title="Bayes Law Calculator")
  res$Sensitivity = sensitivity
  res$Specificity = specificity
  res$FPR = fpr
  res$FNR = fnr
  res$DiseasePrevalence = pwild
  res$Accuracy = acc
  res$ConfusionMatrix=conmat
  res$ProbDiseased = pdis


# Return output
return(res)
}