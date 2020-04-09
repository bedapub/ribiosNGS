#' @include ribiosNGS.R AllClasses.R

#' Extract dgeList from the object
#' @export
setGeneric("dgeList", function(object) standardGeneric("dgeList"))

#' Extract normalisation factors from the object
#' @export
setGeneric("normFactors", function(object) standardGeneric("normFactors"))

#' Filter by counts per million (cpm)
#' @export
setGeneric("cpmFilter", function(object) standardGeneric("cpmFilter"))

#' Set ranks
#' @export
setGeneric("setRnks", function(object, names) standardGeneric("setRnks"))

#' Common, tagwise and trended biological coefficients of variance (BCV)
#' @param x An object
#' @aliases tagwiseBCV trendedBCV BCV
#' @export
setGeneric("commonBCV", function(x) standardGeneric("commonBCV"))

#' @describeIn commonBCV common BCV
#' @export
setGeneric("tagwiseBCV", function(x) standardGeneric("tagwiseBCV"))
#' @describeIn trendedBCV trended BCV
#' @export
setGeneric("trendedBCV", function(x) standardGeneric("trendedBCV"))

#' @describeIn commonBCV A \code{data.frame} of BCV values
#' @export
setGeneric("BCV", function(x) standardGeneric("BCV"))

#' Plot BCV
#' @param x An object
#' @param ... Other paramters
#' @export
setGeneric("plotBCV", function(x, ...) standardGeneric("plotBCV"))

#' Common dispersion
#' @param object An object
#' @param value Numeric, value to be specified as common dispersion
#' @aliases commonDisp `commonDisp<-` hasCommonDisp setCOmmonDispIfMissing
#' @export
setGeneric("commonDisp", function(object) standardGeneric("commonDisp"))
setGeneric("commonDisp<-", function(object,value) standardGeneric("commonDisp<-"))
setGeneric("hasCommonDisp", function(object) standardGeneric("hasCommonDisp"))
setGeneric("setCommonDispIfMissing", function(object, value) standardGeneric("setCommonDispIfMissing"))

#' Modulated logCPM
#' @param object An object
#' @export
setGeneric("modLogCPM", function(object, ...) standardGeneric("modLogCPM"))

#' Volcano plot
#' @param object An object
#' @param ... Other parameters
#' @export
setGeneric("volcanoPlot", function(object,...) standardGeneric("volcanoPlot"))

#' Smear plot
#' @param object An object
#' @param ... Other parameters
#' @export
setGeneric("smearPlot", function(object,...) standardGeneric("smearPlot"))


#' Perform VOOM analysis
#' @export
setGeneric("voom", function(object, ...) standardGeneric("voom"))

#' Sniff features
#' @export
setGeneric("sniffFeatures", function(object) standardGeneric("sniffFeatures"))

#' Is the object annotated
#' @export
setGeneric("isAnnotated", function(object) standardGeneric("isAnnotated"))

#' Assign design matrix
#' @export
setGeneric("designMatrix<-", function(object, value) standardGeneric("designMatrix<-"))

#' Assign contrast matrix
#' @export
setGeneric("contrastMatrix<-", function(object, value) standardGeneric("contrastMatrix<-"))

#' Update design matrix by SVA
#' @export
setGeneric("updateDesignMatrixBySVA", 
           function(object, design,  ...) standardGeneric("updateDesignMatrixBySVA"))

#' Infer surrogate variables
#' @export
setGeneric("inferSV", function(object, design, ...) standardGeneric("inferSV"))

#' Run SVA on a count matrix transformed by voom
#' 
#' @param object A count matrix
#' @param design Design matrix
#' 
#' @return SV matrix
#' @examples
#' set.seed(1887)
#' exCounts <- matrix(rpois(12000, 10), nrow=2000, ncol=6)
#' exCounts[1:100, 2:3] <- exCounts[1:100,2:3]+20
#' exDesign <- model.matrix(~gl(2,3))
#' voomSVA(exCounts, design=exDesign)
#' @export
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
