library(ribiosArg)

myval <- parseNumVec(c("2", "3", "5.5"), expLen=3, failVal=c(3,4,5))
stopifnot(identical(myval, c(2, 3,5.5)))

myval2 <- parseNumVec(c("2,3,5.5"), expLen=3, failVal=c(3,4,5))
stopifnot(identical(myval, c(2, 3,5.5)))

myval3 <- parseNumVec(c("2,3,", "4"), expLen=3, failVal=c(3,4,5))
stopifnot(identical(myval3, c(2, 3,4)))

myval4 <- parseNumVec(c("2,3,", "5.5"), expLen=4, failVal=c(3,4,5))
stopifnot(identical(myval4, c(3,4,5)))

myval5 <- parseNumVec(c("2", "3", "5.5", "7"), expLen=NULL, failVal=c(3,4,5))
stopifnot(identical(myval5, c(2,3,5.5,7)))

myval6 <- parseNumVec(c("2", "3", "5.5", "HSV"), expLen=4, failVal=c(3,4,5,5))
stopifnot(identical(myval6, c(3,4,5,5)))
