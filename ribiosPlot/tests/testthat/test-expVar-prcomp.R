library(ribiosPlot)
library(testthat)
library(ribiosUtils)
library(stats)

myMatrix <- matrix(rnorm(9), nrow=3)
myPrcomp <- stats::prcomp(myMatrix, scale=TRUE)
myExpPrcompEv <- myPrcomp$sdev^2/sum(myPrcomp$sdev^2)


test_that("expVar works for prcomp objects", {
  expect_identical(expVar(myPrcomp),
                   myExpPrcompEv)
})

test_that("expVarLabels works for prcomp objects", {
  expect_identical(expVarLabel(myPrcomp, choices=NULL, compact=FALSE),
                   sprintf(
                     "Principal component %d (%s variance explained)",
                     seq(along = myExpPrcompEv),
                     ribiosUtils::percentage(myExpPrcompEv)
                   ))
  expect_identical(expVarLabel(myPrcomp, choices=c(1,3), compact=FALSE),
                   sprintf(
                     "Principal component %d (%s variance explained)",
                     c(1,3),
                     ribiosUtils::percentage(myExpPrcompEv[c(1,3)])
                   ))
  expect_identical(expVarLabel(myPrcomp, choices=NULL, compact=TRUE),
                   sprintf(
                     "PC%d (%s)",
                     seq(along = myExpPrcompEv),
                     ribiosUtils::percentage(myExpPrcompEv)
                   ))
  expect_identical(expVarLabel(myPrcomp, choices=c(1,3), compact=TRUE),
                   sprintf(
                     "PC%d (%s)",
                     c(1,3),
                     ribiosUtils::percentage(myExpPrcompEv[c(1,3)])
                   ))
})