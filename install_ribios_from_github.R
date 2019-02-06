#!/usr/bin/env Rscript

## install ribios packages from github

if(require("devtools")) {
  install_github("Accio/ribios/ribiosBase")
  install_github("Accio/ribios/ribiosUtils")
  install_github("Accio/ribios/ribiosIO")
  install_github("Accio/ribios/ribiosExpression")
  install_github("Accio/ribios/ribiosAuth")
  install_github("Accio/ribios/ribiosMath")
  install_github("Accio/ribios/ribiosAnnotation")
  install_github("Accio/ribios/ribiosCGI")
  install_github("Accio/ribios/ribiosUDIS")
  install_github("Accio/ribios/ribiosGSEA")
  install_github("Accio/ribios/ribiosPlot")
  install_github("Accio/ribios/ribiosArg")
  install_github("Accio/ribios/ribiosNGS")
  install_github("Accio/ribios/ribiosBatchJobs")
  #install_github("Accio/ribios/ribiosBic")
  install_github("Accio/ribios/ribiosDemo")
  install_github("Accio/ribios/ribiosDiwa")
  install_github("Accio/ribios/ribiosNetwork")
  install_github("Accio/ribios/ribiosPCA")
  install_github("Accio/ribios/ribiosQC")
  install_github("Accio/ribios/ribiosReposition")
  #install_github("Accio/ribios/ribiosRiboSeq")
  install_github("Accio/ribios/ribiosROGER")
  install_github("Accio/ribios/ribiosSeq")
} else {
  stop("Please install 'devtools' first!")
}
