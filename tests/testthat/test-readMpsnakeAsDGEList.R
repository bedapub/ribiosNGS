library(ribiosNGS)
library(testthat)

test_that("readMpsnakeAsDGEList works", {
  mpsnakeDir <- system.file("extdata/mpsnake-minimal-outdir", package="ribiosNGS")
  mpsDgeList <- readMpsnakeAsDGEList(mpsnakeDir)
  
  #' ## equivalent
  mpsnakeResDir <- system.file("extdata/mpsnake-minimal-outdir/results", package="ribiosNGS")
  mpsDgeList2 <- readMpsnakeAsDGEList(mpsnakeResDir)
  
  sampleAnno <- ribiosIO::readTable(system.file("extdata/mpsnake-minimal-outdir",
                                                "results/annot/phenoData.meta", 
                                                package="ribiosNGS"),
                                    row.names = TRUE)
  featAnno <- ribiosIO::readTable(system.file("extdata/mpsnake-minimal-outdir",
                                                "results/annot/feature.annot", 
                                                package="ribiosNGS"),
                                    row.names = FALSE)
  counts <- ribiosIO::read_gct_matrix(system.file("extdata/mpsnake-minimal-outdir",
                                                  "results/gct/unnamed-molphen-project.gct", 
                                                  package="ribiosNGS"))
  expect_identical(mpsDgeList, mpsDgeList2)
  expect_identical(as.character(mpsDgeList$samples$group),
                   as.character(sampleAnno$GROUP))
  expect_identical(rownames(sampleAnno),
                   colnames(mpsDgeList$counts))
  expect_equivalent(featAnno, mpsDgeList$genes)
  expect_equivalent(as.matrix(counts), mpsDgeList$counts)
})

