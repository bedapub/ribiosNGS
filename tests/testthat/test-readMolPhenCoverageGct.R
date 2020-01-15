library(ribiosNGS)
library(testthat)

testDir <- ifelse(interactive(),
                  "~/ribios/ribiosNGS/inst",
                  system.file(package="ribiosNGS"))
testFile <- file.path(testDir,
                      "extdata",
                      "AmpliSeq_files/MolPhen-coverage-example-20200115.gct")
stopifnot(file.exists(testFile))

coverageList <- readMolPhenCoverageGct(testFile)

targetRowNames <- c("A2M;EntrezGeneID=2",
                    "NAT2;EntrezGeneID=10",
                    "PSEN1;EntrezGeneID=5663",
                    "ADA;EntrezGeneID=100",
                    "ADRB2;EntrezGeneID=154",
                    "AGT;EntrezGeneID=183")
targetMatrix <- matrix(c(0, 258, 33209, 1122, 173, 62,
                         0, 161, 20346, 667, 145, 10,
                         1, 77, 19398, 595, 268, 2),
                       dimnames = list(targetRowNames,
                                       c("Sample1", "Sample2", "Sample3")),
                       nrow=6, ncol=3, byrow = FALSE)
targetTranscripts <- c("NM_000014",
                       "NM_000015",
                       "NM_000021",
                       "NM_000022",
                       "NM_000024",
                       "NM_000029")
targetCoverage <- ribiosIO::GctMatrix(targetMatrix,
                            desc=targetTranscripts)
targetGenes <- data.frame(GeneID=as.integer(c(2,10, 5663, 100, 154, 183)),
                          GeneSymbol=I(c("A2M", "NAT2", "PSEN1",
                                       "ADA", "ADRB2", "AGT")),
                          Transcript=I(targetTranscripts),
                          row.names=targetRowNames)

testthat::context("Testing readMolPhenCoverageGct")

testthat::test_that("readMolPhenCoverageGct works as expected",
                    {
                      testthat::expect_identical(coverageList$coverage,
                                                 targetCoverage)
                      testthat::expect_identical(coverageList$genes,
                                                 targetGenes)
                    })
