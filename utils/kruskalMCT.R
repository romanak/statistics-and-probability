kruskalMCT <- function(x,g, conf.level=0.95, p.adjust.method = "BH") {
# The Kruskal-Wallis multiple comparisons test
# x is the measurement variable
# g is the grouping variable
#
#
 message("\n\nNote:\tThese results are only meaningful if the\n\tKruskal-Wallis test was run and the p-value \n\twas small enough.\n")
#
 pairwise.wilcox.test(x,g, conf.level=conf.level, p.adjust.method=p.adjust.method)
}
