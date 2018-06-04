library (biomaRt)
library (RMySQL)

context("Bulk Queries")

if(!exists("testDB")) {
  skip("No test database specified (add it to 'helper.testDB.R' or create a new helper.*.R file)")
}

test_that("Extract full gene annotation", {
  dataset <- "rnorvegicus_gene_ensembl"
  attributes <- c("ensembl_gene_id_version", "ensembl_gene_id","entrezgene",
                  "gene_biotype", "external_gene_name")

  localData <- queryLocal(dataset, attributes)
  remoteData <- queryRemote(dataset, attributes)
  expect_equal(localData, remoteData)
})

test_that("Extract full transcript annotation", {
  dataset <- "rnorvegicus_gene_ensembl"
  attributes <- c("ensembl_transcript_id_version",
                  "ensembl_transcript_id", "ensembl_gene_id_version",
                  "transcript_biotype", "external_transcript_name")

  localData <- queryLocal(dataset, attributes)
  remoteData <- queryRemote(dataset, attributes)
  expect_equal(localData, remoteData)
})
