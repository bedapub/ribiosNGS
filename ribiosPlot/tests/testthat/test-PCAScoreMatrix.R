library(ribiosPlot)
library(testthat)
library(ribiosUtils)

myMatrix <- matrix(rnorm(9), nrow=3)
myExpVar <- c(0.5, 0.3, 0.2)
myPCAscores <- PCAScoreMatrix(myMatrix, myExpVar)

test_that("PCAScoreMatrix methods work as expected",  {
  expect_identical(as.matrix(myPCAscores), myMatrix)
  expect_identical(expVar(myPCAscores), myExpVar)
})

test_that("expVarLabel works well for PCAScoreMatrix", {
  expect_identical(
    expVarLabel(myPCAscores, choices = NULL, compact = FALSE),
    sprintf(
      "Principal component %d (%s variance explained)",
      seq(along = myExpVar),
      ribiosUtils::percentage(myExpVar)
    )
  )
  
  expect_identical(
    expVarLabel(myPCAscores, choices = NULL, compact = TRUE),
    sprintf(
      "PC%d (%s)",
      seq(along = myExpVar),
      ribiosUtils::percentage(myExpVar)
    )
  )
  
  expect_identical(
    expVarLabel(
      myPCAscores,
      choices = seq(along = myExpVar),
      compact = TRUE
    ),
    sprintf(
      "PC%d (%s)",
      seq(along = myExpVar),
      ribiosUtils::percentage(myExpVar)
    )
  )
  
  expect_identical(
    expVarLabel(myPCAscores, choices = c(1, 3), compact = TRUE),
    sprintf("PC%d (%s)",
            c(1, 3),
            ribiosUtils::percentage(myExpVar[c(1, 3)]))
  )
})