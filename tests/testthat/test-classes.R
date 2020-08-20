library(ribiosNGS)
library(testthat)

testEdgeObj <- exampleEdgeObject(nfeat=25, nsample=15, ngroup=3)

test_that("exampleEdgeObject works", {
  testDgeList <- dgeList(testEdgeObj)
  expect_equal(nrow(testDgeList), 25)
  expect_equal(ncol(testDgeList), 15)

  descon <- designContrast(testEdgeObj)
  expDesignMat <- matrix(c(rep(1L, 5), rep(0L, 10),
                           rep(0L, 5), rep(1L, 5), rep(0L, 5),
                           rep(0L, 10), rep(1L, 5)),
                         byrow=FALSE, ncol=3L,
                         dimnames=list(1:15, sprintf("Group%d", 1:3)))
  gotDesignMat <- designMatrix(descon)
  expect_equivalent(expDesignMat, gotDesignMat)
  
  expContrastMat <- matrix(c(-1,1,0,-1, 0,1),
                           byrow=FALSE, ncol=2,
                           dimnames=list(sprintf("Group%s", 1:3),
                                         c("Group2.vs.Group1", 
                                           "Group3.vs.Group1")))
  gotContrastMat <- contrastMatrix(descon)
  expect_equivalent(expContrastMat, gotContrastMat)
})

testEdgeResult <- dgeWithEdgeR(testEdgeObj)
show(testEdgeResult)

testEdgeResult2 <- updateSigFilter(testEdgeResult, logCPM=2)
