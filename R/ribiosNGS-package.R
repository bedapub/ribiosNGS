#' _PACKAGE
#' @name ribiosNGS
#' @description ribiosNGS provides data structures and functions for
#'  next-generation sequencing gene expression analysis
#' @keywords package
NULL

#' @importFrom edgeR DGEList cpm
#' @importFrom ribiosExpression DesignContrast
#' @importFrom methods setClass setGeneric setMethod setRefClass setReplaceMethod
#' @importFrom ribiosExpression designMatrix contrastMatrix
#' @importFrom Biobase `annotation<-` assayData `fData<-` fData `pData<-`
#'             featureNames pData exprs `exprs<-` annotation
#'             featureData phenoData sampleNames
#'             `featureNames<-` storageMode
#' @importFrom graphics abline grid par strwidth
#' @importFrom methods new show slot slotNames validObject
#' @importFrom stats cor median model.matrix quantile 
#' @importFrom utils head write.table
#' @importFrom magrittr %>%
#' @importFrom ribiosUtils assertDir assertFile munion matrix2longdf
#' @importFrom ribiosPlot pdf2png openFileDevice closeFileDevice 
#' @importFrom lattice panel.points panel.abline panel.smoothScatter
#' @importFrom edgeR estimateGLMCommonDisp estimateGLMTagwiseDisp estimateGLMTrendedDisp
#' @importMethodsFrom ribiosExpression designMatrix contrastMatrix
#' @importMethodsFrom BiocGenerics counts annotation nrow ncol normalize
#' @export `annotation<-` assayData `fData<-` fData `pData<-`
#' @export featureNames pData exprs `exprs<-` annotation
#' @export featureData phenoData sampleNames
#' @export `featureNames<-`
NULL

#' @export
edgeR::DGEList

#' @export
ribiosExpression::DesignContrast

#' @export
edgeR::cpm

