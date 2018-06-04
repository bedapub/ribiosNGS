library (biomaRt)
library (RMySQL)

context("Bulk Queries")

if(!exists("testDB")) {
  skip("No test database specified (add it to 'helper.testDB.R' or create a new helper.*.R file)")
}

test_that("Extract full gene annotation", {
  geneLocal <- getLocalMartGeneAnnotationSample()
  geneRemote <- getRemoteMartGeneAnnotationSample()

  expect_equal(geneRemote, geneLocal)
})

test_that("Extract full transcript annotation", {
  transcriptLocal <- getLocalMartTranscriptAnnotationSample()
  transcriptRemote <- getRemoteMartTranscriptAnnotationSample()

  expect_equal(transcriptRemote, transcriptLocal)
})
