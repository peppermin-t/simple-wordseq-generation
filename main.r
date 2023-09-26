setwd("txtfiles") ## comment out of submitted
a <- scan("4300-0.txt", what="character", skip=73, nlines=32858-73)
a <- gsub("_(","", a, fixed=TRUE) ## remove "_("
print(a)

