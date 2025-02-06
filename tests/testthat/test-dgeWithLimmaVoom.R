library(ribiosNGS)
library(testthat)

testthat::context("Testing dgeWithLimmaVoom")

set.seed(1887)
exObj <- exampleEdgeObject()
exLimmaVoomRes <- dgeWithLimmaVoom(exObj)
dgeTable(exLimmaVoomRes)

## compare with edgeR
dgeTable(dgeWithEdgeR(exObj))

## LimmaVoomResult can be also used with exportEdgeResult
exportEdgeResult(exLimmaVoomRes, tempdir(), "overwrite")
