library(testthat)
library(ribiosNGS)
library(tibble)

testFct <- data.frame(Char=LETTERS[1:6],
                   Integer=1:6,
                   Number=pi*1:6,
                   Factor=gl(2,3, labels = c("level 1", "level 2")), stringsAsFactors=FALSE)
testChar <- data.frame(Char=paste(LETTERS[1:6],letters[1:6]),
                   Integer=1:6,
                   Number=pi*1:6,
                   Factor=gl(2,3, labels = c("level 1", "level 2")), stringsAsFactors=FALSE)


testFix <- data.frame(Char=paste0(LETTERS[1:6],letters[1:6]),
                      Integer=1:6,
                      Number=pi*1:6,
                      Factor=gl(2,3, labels = c("level1", "level2")), stringsAsFactors=FALSE)

testthat::context("Test checkBiokitSampleAnnotation")

testthat::test_that("checkBiokitSampleAnnotation works for data.frame", {
  testthat::expect_error(checkBiokitSampleAnnotation(testFct),
                         regexp = "level 1.*level 2")
  testthat::expect_error(checkBiokitSampleAnnotation(testChar),
                         regexp = "A a,B b,C c,D d,E e,F f")
  testthat::expect_silent(checkBiokitSampleAnnotation(testFix))
})

testthat::test_that("checkBiokitSampleAnnotation works for tbl_df/tibble", {
  testthat::expect_error(checkBiokitSampleAnnotation(tibble::as_tibble(testFct)),
                         regexp = "level 1.*level 2")
  testthat::expect_error(checkBiokitSampleAnnotation(tibble::as_tibble(testChar)),
                         regexp = "A a,B b,C c,D d,E e,F f")
  testthat::expect_silent(checkBiokitSampleAnnotation(tibble::as_tibble(testFix)))
})
