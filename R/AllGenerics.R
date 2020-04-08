#' @include ribiosNGS.R AllClasses.R

#' Extract dgeList from the object
#' @export
setGeneric("dgeList", function(object) standardGeneric("dgeList"))

#' Extract normalisation factors from the object
#' @export
setGeneric("normFactors", function(object) standardGeneric("normFactors"))

setGeneric("cpmFilter", function(object) standardGeneric("cpmFilter"))
setGeneric("convertPDF2PNG", function(object, ...) standardGeneric("convertPDF2PNG"))

setGeneric("setRnks", function(object, names) standardGeneric("setRnks"))

setGeneric("commonBCV", function(x) standardGeneric("commonBCV"))
setGeneric("tagwiseBCV", function(x) standardGeneric("tagwiseBCV"))
setGeneric("trendedBCV", function(x) standardGeneric("trendedBCV"))

setGeneric("BCV", function(x) standardGeneric("BCV"))
setGeneric("plotBCV", function(x, ...) standardGeneric("plotBCV"))



setGeneric("volcanoPlot", function(object,...) standardGeneric("volcanoPlot"))
setGeneric("smearPlot", function(object,...) standardGeneric("smearPlot"))

setGeneric("modLogCPM", function(object, ...) standardGeneric("modLogCPM"))

setGeneric("voom", function(object, ...) standardGeneric("voom"))

setGeneric("commonDisp", function(object) standardGeneric("commonDisp"))
setGeneric("commonDisp<-", function(object,value) standardGeneric("commonDisp<-"))
setGeneric("hasCommonDisp", function(object) standardGeneric("hasCommonDisp"))
setGeneric("setCommonDispIfMissing", function(object, common.disp) standardGeneric("setCommonDispIfMissing"))

setGeneric("sniffFeatures", function(object) standardGeneric("sniffFeatures"))

setGeneric("isAnnotated", function(object) standardGeneric("isAnnotated"))


setGeneric("designMatrix<-", function(object, value) standardGeneric("designMatrix<-"))
setGeneric("contrastMatrix<-", function(object, value) standardGeneric("contrastMatrix<-"))

setGeneric("updateDesignMatrixBySVA", 
           function(object, design,  ...) standardGeneric("updateDesignMatrixBySVA"))

setGeneric("inferSV", function(object, design, ...) standardGeneric("inferSV"))
setGeneric("voomSVA", function(object, design, ...) standardGeneric("voomSVA"))

#' Get human gene symbols for gene-set enrichment analysis
#' @export
setGeneric("humanGeneSymbols", function(object) standardGeneric("humanGeneSymbols"))

#' Fit generalized linear model
#' @export
setGeneric("fitGLM", function(object,...) standardGeneric("fitGLM"))

#' Estimate generalized linear model dispersion
#' @export
setGeneric("estimateGLMDisp", function(object) standardGeneric("estimateGLMDisp"))

#' Test GLM
#' @export
setGeneric("testGLM", function(object, fit) standardGeneric("testGLM"))
##----------------------------------##
## Riboseq
##----------------------------------##
#' RiboSeq generic function
#' @export
setGeneric("RiboSeq", function(RNA, RPF, groups, ...) standardGeneric("RiboSeq"))

#' Get RNA counts
#' @export
setGeneric("countRNA", function(object) standardGeneric("countRNA"))

#' Get RPF counts
#' @export
setGeneric("countRPF", function(object) standardGeneric("countRPF"))

#' Get RNA counts per million (cpm)
#' @export
setGeneric("cpmRNA", function(object,...) standardGeneric("cpmRNA"))

#' Get RPF counts per million (cpm)
#' @export
setGeneric("cpmRPF", function(object,...) standardGeneric("cpmRPF"))

#' Get group sum of RNA cpm
#' @export
setGeneric("cpmRNAGroupSum", function(object) standardGeneric("cpmRNAGroupSum"))

#' Get group sum of RPF cpm
#' @export
setGeneric("cpmRPFGroupSum", function(object) standardGeneric("cpmRPFGroupSum"))
