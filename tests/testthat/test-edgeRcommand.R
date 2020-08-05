library(ribiosNGS)

mat <- matrix(rnbinom(100, mu=5, size=2), ncol=10)
rownames(mat) <- sprintf("gene%d", 1:nrow(mat))
myFac <- gl(2,5, labels=c("Control", "Treatment"))
y <- edgeR::DGEList(counts=mat, group=myFac)
myDesign <- model.matrix(~myFac); colnames(myDesign) <- levels(myFac)
myContrast <- limma::makeContrasts(Treatment, levels=myDesign)
myDir <- tempdir()
comm <- edgeRcommand(y, designMatrix=myDesign, contrastMatrix=myContrast,
                     outfilePrefix="test", outdir=myDir)
inputDir <- file.path(myDir, "input_data")
expComm <- paste("/pstore/apps/bioinfo/geneexpression//bin/ngsDge_edgeR.Rscript",
                 sprintf("-infile %s", file.path(inputDir, "test-counts.gct")),
                 sprintf("-designFile %s", file.path(inputDir, "test-designMatrix.txt")),
                 sprintf("-contrastFile %s", file.path(inputDir, "test-contrastMatrix.txt")),
                 sprintf("-sampleGroups %s", file.path(inputDir, "test-sampleGroups.txt")),
                 sprintf("-groupLevels %s", file.path(inputDir, "test-sampleGroupLevels.txt")),
                 sprintf("-featureAnnotationFile %s", file.path(inputDir, "test-featureAnno.txt")),
                 sprintf("-phenoData %s", file.path(inputDir, "test-sampleAnno.txt")),
                 sprintf("-outdir %s", myDir),
                 sprintf("-log %s.log", myDir),
                 "-writedb")

test_that("edgeRcommand works with default options", {
  expect_equal(expComm, comm)
})

## testing the mps option

mpsComm <- edgeRcommand(y, designMatrix=myDesign, contrastMatrix=myContrast,
                        outfilePrefix="test", outdir=myDir, mps=TRUE)
expMpsComm <- paste(expComm, "-mps")
test_that("edgeRcommand works with the mps option", {
  expect_equal(mpsComm, expMpsComm)
})

## testing the appendGmt option

appendGmt <- tempfile()
writeLines("GeneSetA\tDescription\tAKT1\tAKT2\tAKT3", appendGmt)
gmtComm <- edgeRcommand(y, designMatrix=myDesign, contrastMatrix=myContrast,
                        outfilePrefix="test", outdir=myDir, appendGmt=appendGmt)
expGmtComm <- paste(expComm, sprintf("-appendGmt %s", appendGmt))

## testing the appendGmt option

test_that("edgeRcommand works with the appendGmt option, when appendGmt is a character string, path to the GMT file", {
  ## appendGmt
  expect_equal(gmtComm, expGmtComm)
})

## testing the debug option

debugComm <- edgeRcommand(y, designMatrix=myDesign, contrastMatrix=myContrast,
                          outfilePrefix="test", outdir=myDir, debug=TRUE)
expDebugComm  <- gsub("\\/bin\\/", "\\/rsrc\\/", expComm)

test_that("edgeRcommand works with the debug option", {
  expect_equal(debugComm, expDebugComm)
})
