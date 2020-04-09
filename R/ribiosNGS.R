#' ribiosNGS package provides functionalities used for next-generation sequencing data
#'
#' @name ribiosNGS-package
#' @docType package
#' @description Provides data structures and functions for next-generation sequencing gene expression analysis
#' @keywords package
NULL

#' @importFrom methods setClass setGeneric setMethod setRefClass setReplaceMethod
#' @importFrom ribiosExpression designMatrix contrastMatrix
#' @importFrom Biobase `annotation<-` assayData `fData<-` fData `pData<-`
#'             featureNames pData exprs `exprs<-` annotation
#'             featureData phenoData sampleNames
#'             `featureNames<-` storageMode
#' @importMethodsFrom ribiosExpression designMatrix contrastMatrix
#' @importMethodsFrom BiocGenerics counts annotation nrow ncol normalize
#' @export `annotation<-` assayData `fData<-` fData `pData<-`
#' @export featureNames pData exprs `exprs<-` annotation
#' @export featureData phenoData sampleNames
#' @export `featureNames<-`
NULL
