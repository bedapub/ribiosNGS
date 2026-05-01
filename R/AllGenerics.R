#' @include ribiosNGS-package.R AllClasses.R 

#' Extract dgeList from the object
#' @param object An object from which a \code{DGEList} object can be extracted
#' @return A \code{DGEList} object.
#' @exportMethod dgeList
setGeneric("dgeList", function(object) standardGeneric("dgeList"))

#' Extract normalisation factors from the object
#' @param object An object
#' @return A numeric vector of normalisation factors.
#' @exportMethod normFactors
setGeneric("normFactors", function(object) standardGeneric("normFactors"))

#' Filter by counts per million (cpm)
#' @param object An object
#' @return The filtered object.
#' @export
setGeneric("cpmFilter", function(object) standardGeneric("cpmFilter"))

#' Common biological coefficients of variance (BCV)
#' @param x An object
#' @return A numeric value of the common BCV.
#' @exportMethod commonBCV
setGeneric("commonBCV", function(x) standardGeneric("commonBCV"))

#' Tagwise biological coefficients of variance
#' @param x An object
#' @return A numeric vector of tagwise BCV values.
#' @exportMethod tagwiseBCV
setGeneric("tagwiseBCV", function(x) standardGeneric("tagwiseBCV"))

#' Trended biological coefficients of variance
#' @param x An object
#' @return A numeric vector of trended BCV values.
#' @exportMethod trendedBCV
setGeneric("trendedBCV", function(x) standardGeneric("trendedBCV"))

#' Return a data.frame of BCV values
#' @param x An object
#' @return A \code{data.frame} of BCV values.
#' @export
setGeneric("BCV", function(x) standardGeneric("BCV"))

#' Plot BCV
#' @param x An object
#' @param ... Other paramters
#' @return Called for its side effect of plotting; returns invisibly NULL.
#' @export
setGeneric("plotBCV", function(x, ...) standardGeneric("plotBCV"))

#' Common dispersion
#' @param object An object
#' @return A numeric value of the common dispersion.
#' @export
setGeneric("commonDisp", function(object) standardGeneric("commonDisp"))

#' Set common dispersion
#' @name commonDisp<-
#' @title commonDisp-set
#' @param object An object
#' @param value Numeric value
#' @return The updated object with the common dispersion set.
#' @export
setGeneric("commonDisp<-", function(object,value) standardGeneric("commonDisp<-"))

#' Tells whether common dispersion has been set
#' @param object An object
#' @return Logical, whether the common dispersion has been set.
#' @export
setGeneric("hasCommonDisp", function(object) standardGeneric("hasCommonDisp"))

#' Set common dispersion if missing
#' @param object An object
#' @param value Numeric
#' @return The object, with common dispersion set if it was previously missing.
#' @export
setGeneric("setCommonDispIfMissing", function(object, value) standardGeneric("setCommonDispIfMissing"))

#' Modulated logCPM
#' @param object An object
#' @param ... Other parameters
#' @return A numeric matrix of modulated log-CPM values.
#' @export
setGeneric("modLogCPM", function(object, ...) standardGeneric("modLogCPM"))

#' Volcano plot
#' @param object An object
#' @param ... Other parameters
#' @return Called for its side effect of plotting; returns invisibly NULL.
#' @export
setGeneric("volcanoPlot", function(object,...) standardGeneric("volcanoPlot"))

#' Smear plot
#' @param object An object
#' @param ... Other parameters
#' @return Called for its side effect of plotting; returns invisibly NULL.
#' @export
setGeneric("smearPlot", function(object,...) standardGeneric("smearPlot"))


#' Perform VOOM analysis
#' @param object An object
#' @param ... Other parameters
#' @return An \code{EList} object containing voom-transformed values.
#' @export
setGeneric("voom", function(object, ...) standardGeneric("voom"))

#' Is the object annotated
#' @param object An object
#' @return Logical, whether the object is annotated.
#' @export
setGeneric("isAnnotated", function(object) standardGeneric("isAnnotated"))

#' Assign the design matrix
#' @param object An object
#' @param value Matrix
#' @return The updated object with the new design matrix.
#' @name designMatrix-set
#' @aliases designMatrix<-
#' @export
setGeneric("designMatrix<-", function(object, value) standardGeneric("designMatrix<-"))

#' Assign contrast matrix
#' @param object An object
#' @param value Matrix
#' @return The updated object with the new contrast matrix.
#' @aliases contrastMatrix<-
#' @name contrastMatrix-set
#' @export
setGeneric("contrastMatrix<-", function(object, value) standardGeneric("contrastMatrix<-"))

#' Update design matrix by SVA
#' @param object An object
#' @param design Design matrix or formula
#' @param ... Other parameters
#' @return A design matrix updated with surrogate variables.
#' @export
setGeneric("updateDesignMatrixBySVA", 
           function(object, design,  ...) standardGeneric("updateDesignMatrixBySVA"))

#' Infer surrogate variables
#' @export
#' @param object An object
#' @param design Design matrix or formula
#' @param ... Other parameters
#' @return A matrix of surrogate variables.
setGeneric("inferSV", function(object, design, ...) standardGeneric("inferSV"))

#' Run SVA on a count matrix transformed by voom
#' 
#' @param object A count matrix
#' @param design Design matrix or formula
#' @param ... Other parameters
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
#' @param object An object
#' @return A character vector of human gene symbols.
#' @export
setGeneric("humanGeneSymbols", function(object) standardGeneric("humanGeneSymbols"))

#' Fit generalized linear model
#' @param object An object
#' @param ... Other parameters
#' @return A \code{DGEGLM} object containing the GLM fit.
#' @export
setGeneric("fitGLM", function(object,...) standardGeneric("fitGLM"))

#' Test GLM
#' @param object An object
#' @param fit A fit object
#' @return An \code{EdgeResult} object containing the test results.
#' @export
setGeneric("testGLM", function(object, fit) standardGeneric("testGLM"))
