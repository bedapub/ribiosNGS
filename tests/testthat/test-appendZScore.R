library(ribiosNGS)
library(ribiosUtils)

myDgeTable <- data.frame(FeatureID=sprintf("Gene%d", 1:100),
                         logFC=rnorm(100),
                         PValue=runif(100))
appendedDgeTbl <- appendZScore(myDgeTable)

test_that("appendZScore works as expected", {
  testthat::expect_setequal(colnames(appendedDgeTbl),
                            c(colnames(myDgeTable), "zScore"))
  testthat::expect_equal(appendedDgeTbl$zScore,
                         ribiosUtils::pScore(myDgeTable$PValue,
                                             sign=myDgeTable$logFC))
})
