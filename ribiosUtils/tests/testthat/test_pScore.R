context("pScore")

testPvals <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1)
testPvalSign <- rep(c(-1,1), 3)
testPvalLogFC <- c(-2, 3, -4, 5, -1.1, 1.2)
testLogical <- rep(c(TRUE, FALSE),3)

test_that("pScore works as expected", {
  expect_equal(pScore(testPvals),
               abs(log10(testPvals)))
  expect_equal(pScore(testPvals, sign=testPvalSign),
               abs(log10(testPvals))*sign(testPvalSign))
  expect_equal(pScore(testPvals, sign=testPvalLogFC),
               abs(log10(testPvals))*sign(testPvalLogFC))
  expect_equal(pScore(testPvals, sign=testLogical),
               abs(log10(testPvals))*ifelse(testLogical, 1, -1))
})