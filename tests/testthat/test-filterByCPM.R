library(testthat)
library(ribiosNGS)

test_that("filterByCPM.matrix returns logical vector", {
  set.seed(1887)
  mat <- rbind(matrix(rbinom(125, 5, 0.25), nrow = 25), rep(0, 5))
  res <- filterByCPM(mat)
  expect_is(res, "logical")
  expect_length(res, 26)
  expect_false(res[26])
})

test_that("filterByCPM.DGEList removes zero-count rows", {
  set.seed(1887)
  mat <- rbind(matrix(rbinom(150, 5, 0.25), nrow = 25), rep(0, 6))
  d <- DGEList(mat, group = rep(1:3, each = 2),
               genes = data.frame(Gene = sprintf("Gene%d", 1:nrow(mat))))
  df <- filterByCPM(d)
  expect_equal(nrow(df$counts.unfiltered), 26)
  expect_true(nrow(df$counts) < 26)
})

test_that("filterByCPM.EdgeObject removes lowly expressed genes", {
  set.seed(1234)
  myFac <- gl(3, 2)
  myMat <- matrix(rpois(1200, 100), nrow = 200, ncol = 6)
  myMat[1:3, ] <- 0
  myEdgeObj <- EdgeObject(myMat,
    DesignContrast(
      designMatrix = model.matrix(~myFac),
      contrastMatrix = matrix(c(0, 1, 0), ncol = 1),
      groups = myFac
    ),
    fData = data.frame(GeneSymbol = sprintf("Gene%d", 1:200))
  )
  filtered <- filterByCPM(myEdgeObj)
  expect_true(nrow(counts(filtered)) < 200)
  expect_equal(nrow(counts(filtered, filter = FALSE)), 200)
})
