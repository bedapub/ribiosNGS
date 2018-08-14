library (biomaRt)
library (data.table)
library (RMySQL)

context("Bulk Queries")

if(!exists("testDB")) {
  skip("No test database specified (add it to 'helper.testDB.R' or create a new helper.*.R file)")
}

test_that("Extract full gene annotation with chromosome name", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <-  c("ensembl_gene_id", "gene_biotype", "external_gene_name",
                   "description", "chromosome_name")

  localData <- queryLocal(dataset, attributes)
  remoteData <- queryRemote(dataset, attributes)
  expect_equal(localData, remoteData)
})

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

test_that("Extract all homologies between human and rat", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <- c("ensembl_gene_id", "rnorvegicus_homolog_ensembl_gene")

  localData <- queryLocal(dataset,
                          attributes,
                          filter="with_rnorvegicus_homolog",
                          values=TRUE)
  remoteData <- queryRemote(dataset,
                            attributes,
                            filter="with_rnorvegicus_homolog",
                            values=TRUE)
  expect_equal(localData, remoteData)
})

test_that("Extract all homologies between human and mouse", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <- c("ensembl_gene_id", "mmusculus_homolog_ensembl_gene")

  localData <- queryLocal(dataset,
                          attributes,
                          filter="with_mmusculus_homolog",
                          values=TRUE)
  remoteData <- queryRemote(dataset,
                            attributes,
                            filter="with_mmusculus_homolog",
                            values=TRUE)
  expect_equal(localData, remoteData)
})

test_that("Query exactly one gene", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <- c('ensembl_gene_id_version', 'description')
  filterList <- 'ensembl_gene_id_version'
  values <- c('ENSG00000099194.5')

  localData <- queryLocal(dataset,
                          attributes,
                          filter=filterList,
                          values=values)
  remoteData <- queryRemote(dataset,
                            attributes,
                            filter=filterList,
                            values=values)
  expect_equal(localData, remoteData)
})

context("Sample Queries from biomaRt tutorial")

test_that("Annotate a set of Affymetrix identifiers with HUGO symbol and chromosomal locations of corresponding genes", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <- c('affy_hg_u133_plus_2', 'hgnc_symbol', 'chromosome_name',
                  'start_position', 'end_position', 'band')
  filterList <- 'affy_hg_u133_plus_2'
  values <- c("202763_at","209310_s_at","207500_at")

  localData <- queryLocal(dataset,
                          attributes,
                          filter=filterList,
                          values=values)
  remoteData <- queryRemote(dataset,
                            attributes,
                            filter=filterList,
                            values=values)
  expect_equal(localData, remoteData)
})

test_that("Annotate a set of EntrezGene identifiers with GO annotation", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <- c('entrezgene', 'go_id')
  filterList <- 'entrezgene'
  values <- c("673","837")

  localData <- queryLocal(dataset,
                          attributes,
                          filter=filterList,
                          values=values)
  remoteData <- queryRemote(dataset,
                            attributes,
                            filter=filterList,
                            values=values)

  expect_equal(localData, remoteData)
})

test_that("Annotate set of idenfiers with INTERPRO protein domain identifiers", {
  dataset <- "hsapiens_gene_ensembl"
  attributes <- c("refseq_mrna","interpro","interpro_description")
  filterList <- "refseq_mrna"
  values <- c("NM_005359","NM_000546")

  localData <- queryLocal(dataset,
                          attributes,
                          filter=filterList,
                          values=values)
  remoteData <- queryRemote(dataset,
                            attributes,
                            filter=filterList,
                            values=values)
  expect_equal(localData, remoteData)
})

##TODO this kind of query is not supported
# test_that("Select all Affymetrix identifiers on the hgu133plus2 chip and Ensembl gene identifiers for genes located on chromosome 16 between basepair 1100000 and 1250000
# ", {
#   dataset <- "hsapiens_gene_ensembl"
#   attributes <- c('chromosome_name','start','end')
#   filterList <- "refseq_mrna"
#   values <- list(16,1100000,1250000)
#
#   localData <- queryLocal(dataset,
#                           attributes,
#                           filter=filterList,
#                           values=values)
#   remoteData <- queryRemote(dataset,
#                             attributes,
#                             filter=filterList,
#                             values=values)
#   expect_equal(localData, remoteData)
# })
