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

#' Common biological coefficients of variance (BCV)
#' @exportMethod commonBCV
setGeneric("commonBCV", function(x) standardGeneric("commonBCV"))

#' Tagwise biological coefficients of variance
#' @exportMethod tagwiseBCV
setGeneric("tagwiseBCV", function(x) standardGeneric("tagwiseBCV"))

#' Trended biological coefficients of variance
#' @exportMethod trendedBCV
setGeneric("trendedBCV", function(x) standardGeneric("trendedBCV"))

#' Return a data.frame of BCV values
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
#' @export
setGeneric("commonDisp", function(object) standardGeneric("commonDisp"))

#' Set common dispersion
#' @name commonDisp-set
#' @export
setGeneric("commonDisp<-", function(object,value) standardGeneric("commonDisp<-"))

#' Tells whether common dispersion has been set
#' @export
setGeneric("hasCommonDisp", function(object) standardGeneric("hasCommonDisp"))

#' Set common dispersion if missing
#' @export
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

#' Assign the design matrix
#' @name designMatrix-set
#' @export
setGeneric("designMatrix<-", function(object, value) standardGeneric("designMatrix<-"))

#' Assign contrast matrix
#' @name contrastMatrix-set
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
