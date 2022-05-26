
cat("\n\nLoading additional functionality to base R.\n
   \tThis will make your STAT200 experience much easier \n\tand more fulfilling.\n\n")

path <- dirname(sys.frame(1)$ofile)

cm = "\U2713"

fctns=c(
	"bayes.law",
	"binom.plot",
	"binom.pred",
	"conf.bounds",
	"cv",
	"forsberg.test",
	"getMeans",
	"groupTransform",
	"hildebrand.rule",
	"histogram",
	"interval",
	"isBetween",
	"kruskalMCT",
	"kurtosis",
	"laakso",
	"links",
	"make.link",
	"means",
	"median.test",
	"model.fit",
	"modal",
	"normoverlay",
	"ogive",
	"onevar.test",
	"overlay",
	"paretochart",
	"predictionEllipse",
	"quartile",
	"runs.test",
	"set.base",
	"shapiroTest",
	"skew",
	"summaryHCE",
	"summaryVIFA",
	"tapd",
	"theme",
	"tukey.var.test",
	"wald.test",
	"z.test",
	"zscore"
	)


  cat("\nFunctions loaded:")

  for( i in 1:length(fctns) ) {
    fn = paste(path, "/", fctns[i],".R", sep="")
    source(fn)
  cat("\n\t",cm,fctns[i])
  flush.console()
  }

cat("\n\n")







#cat("\n\nLibraries loaded:")
#
#library("agricolae")
#cat("\n\t",cm,"agricolae")

cat("\n\n\n\nHappy analyzing!!!\n\n\n")
