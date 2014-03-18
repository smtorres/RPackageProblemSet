setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/ProblemSet6/")
current.code <- as.package("RPackageProblemSet")
load_all(current.code) # Load all of the functions so you can use them
document(current.code) # Make the help files
check(current.code)
install(pkg=current.code, local=TRUE) # Install the package
?RPackageProblemSet
?reg.vars
read.table(exampleDataset.rda)
mat<-matrix(c(3,6,7,2,3,6,1,6,1), ncol=3)
dy<-mat[,2]*2.6
reg.vars(dy,mat)
sig.store(dy,mat)
average.rsqr(dy,mat)
