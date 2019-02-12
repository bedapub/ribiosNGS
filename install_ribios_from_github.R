#!/usr/bin/env Rscript

## install ribios packages from github

if(require("devtools") && require("BiocManager")) {
  install_github("Accio/ribios/ribiosBase")
  install_github("Accio/ribios/ribiosDemo")
  install_github("Accio/ribios/ribiosUtils")
  install_github("Accio/ribios/ribiosIO")
  install_github("Accio/ribios/ribiosArg")
  install_github("Accio/ribios/ribiosAuth")
  install_github("Accio/ribios/ribiosMath")
  install_github("Accio/ribios/ribiosPlot")
  install_github("Accio/ribios/ribiosAnnotation")

  install.packages("RCurl") ## to be replaced by httr
  install_github("Accio/ribios/ribiosUDIS")
  install_github("Accio/ribios/ribiosCGI")

  BiocManager::install(c("edgeR", "limma", "Biobase", "latticeExtra", "globaltest", "data.table"))
  install_github("Accio/ribios/ribiosExpression")
  install_github("Accio/ribios/ribiosGSEA")

  BiocManager::install(c("igraph", "sva", "gage"))
  install_github("Accio/ribios/ribiosNGS")
  ## install_github("Accio/ribios/ribiosBatchJobs") ## to be merged
  ## install_github("Accio/ribios/ribiosBic") ## to be merged
  ## install_github("Accio/ribios/ribiosDiwa") ## to be merged
  ## install_github("Accio/ribios/ribiosNetwork") ## to be merged
  ## install_github("Accio/ribios/ribiosPCA")
  ## install_github("Accio/ribios/ribiosQC")
  ## install_github("Accio/ribios/ribiosReposition")
  ## install_github("Accio/ribios/ribiosRiboSeq")
  install_github("Accio/ribios/ribiosROGER")
  ## install_github("Accio/ribios/ribiosSeq")
} else {
  stop("Please install 'devtools' and 'BiocManager' first!")
}
