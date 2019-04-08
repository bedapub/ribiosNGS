context("pScore")

testPvals <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1)
testPvalSign <- rep(c(-1,1), 3)
testPvalLogFC <- c(-2, 3, -4, 5, -1.1, 1.2)
testLogical <- rep(c(TRUE, FALSE),3)

test_that("pScore correctly wrapps pAbsLog10Score and pQnormScore", {
  expect_equal(pScore(testPvals, sign=testPvalSign, method="absLog10"),
               pAbsLog10Score(testPvals, sign=testPvalSign))
  expect_equal(pScore(testPvals, sign=testPvalSign, method="qnorm"),
               pQnormScore(testPvals, sign=testPvalSign))
})

test_that("pAbsLog10Score works as expected", {
  expect_equal(pScore(testPvals, method="absLog10"),
               abs(log10(testPvals)))
  expect_equal(pScore(testPvals, sign=testPvalSign, method="absLog10"),
               abs(log10(testPvals))*sign(testPvalSign))
  expect_equal(pScore(testPvals, sign=testPvalLogFC, method="absLog10"),
               abs(log10(testPvals))*sign(testPvalLogFC))
  expect_equal(pScore(testPvals, sign=testLogical, method="absLog10"),
               abs(log10(testPvals))*ifelse(testLogical, 1, -1))
})

invGaussianCdf <- function(p) qnorm(1-p/2, lower.tail=TRUE)

test_that("pQnormScore works as expected", {
  expect_equal(pScore(testPvals, method="qnorm"),
               invGaussianCdf(testPvals))
  expect_equal(pScore(testPvals, sign=testPvalSign, method="qnorm"),
               invGaussianCdf(testPvals)*sign(testPvalSign))
  expect_equal(pScore(testPvals, sign=testPvalLogFC, method="qnorm"),
               invGaussianCdf(testPvals)*sign(testPvalLogFC))
  expect_equal(pScore(testPvals, sign=testLogical, method="qnorm"),
               invGaussianCdf(testPvals)*ifelse(testLogical, 1, -1))
})
