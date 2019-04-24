library(testthat)
library(ribiosUtils)

context("Testing functions relevant for Roche compounds. Only artificial IDs are used!")

test_that("isRocheCompoundID works properly for full IDs", {
  expect_true(isRocheCompoundID("RO1234567"))
  expect_true(isRocheCompoundID("RO-1234567"))
  expect_true(isRocheCompoundID("RO1234567-001"))
  expect_true(isRocheCompoundID("RO1234567-001-005"))
})

test_that("isRocheCompoundID works properly for short IDs", {
  expect_true(isRocheCompoundID("RO4567"))
  expect_true(isRocheCompoundID("RO-4567"))
  expect_true(isRocheCompoundID("RO67"))
  expect_true(isRocheCompoundID("RO-67"))
})

test_that("isRocheCompoundID negates properly too short IDs", {
  expect_false(isRocheCompoundID("RO7"))
  expect_false(isRocheCompoundID("RO-7"))
})

test_that("rocheCore works properly for full Roche IDs", {
  expect_identical(rocheCore("RO1234567", short=FALSE), "RO1234567")
  expect_identical(rocheCore("RO1234567-005", short=FALSE), "RO1234567")
  expect_identical(rocheCore("RO1234567-005-001", short=FALSE), "RO1234567")
})

test_that("rocheCore works properly in case short=TRUE", {
  expect_identical(rocheCore("RO1234567", short=TRUE), "RO4567")
  expect_identical(rocheCore("RO1234567-005", short=TRUE), "RO4567")
  expect_identical(rocheCore("RO1234567-005-001", short=TRUE), "RO4567")
})

test_that("rocheCore works properly for a vector", {
  expect_identical(rocheCore(c("RO1234567", "RO1234567-000", "ROtest", "not-relevant"),
                             short=TRUE),
                   c("RO4567", "RO4567", "ROtest", "not-relevant"))
})
