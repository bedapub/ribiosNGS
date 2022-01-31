#' @include AllClasses.R AllGenerics.R ribiosNGS-package.R utils.R
NULL

##-----------------------------------##
## Dimensionalities
##-----------------------------------##

#' Return number of features 
#' @importMethodsFrom BiocGenerics nrow
#' @param x An EdgeResults object
#' @return Integer
#' @export
setMethod("nrow", "EdgeResult", function(x) nrow(x@dgeList))

#' Return number of samples 
#' @importMethodsFrom BiocGenerics ncol
#' @param x An EdgeResults object
#' @return Integer
#' @export
setMethod("ncol", "EdgeResult", function(x) ncol(x@dgeList))

#' Dimensions of an EdgeResults
#' @param x An EdgeResult object
#' @export
dim.EdgeResult <- function(x) c(nrow(x), ncol(x))

##-----------------------------------##
## counts
##-----------------------------------##

#' Return counts in a DGEList object
#' @importMethodsFrom BiocGenerics counts
#' @param object A \code{DGEList} object.
#' @export
setMethod("counts", "DGEList", function(object) object$counts)

#' Return counts in EdgeObject
#' 
#' @param object An EdgeObject
#' @param filter Logical, whether filtered matrix (by default) or unfiltered matrix should be returned
#' 
#' @seealso \code{\link{filterByCPM}}
#' @export
setMethod("counts", "EdgeObject", function(object, filter=TRUE) {
  if(filter) {
    return(object@dgeList$counts)
  } else {
    return(object@dgeList$counts.unfiltered)
  }
})

##---------------------------------------------------##
## Getting/setting feature and sample annotations
##---------------------------------------------------##

#' Get annotation information from an EdgeObject
#' @param object An \code{EdgeObject}.
#' @return A character string, indicating the annotation type
#' @importMethodsFrom BiocGenerics annotation
#' @export
setMethod("annotation", "EdgeObject", function(object) {
  return(object@dgeList$annotation)
})

#' @describeIn isAnnotated Method for EdgeObject
#' @export
setMethod("isAnnotated", "EdgeObject", function(object) {
  anno <- annotation(object)
  res <- !is.null(anno) && !is.na(anno) && anno!=""
  return(res)
})

#' Get fData
#' @param object A DGEList
#' @importMethodsFrom Biobase fData fData<-
#' @export
setMethod("fData", "DGEList", function(object) object$genes)

#' Set fData
#' @param object A DGEList
#' @param value A \code{data.frame}
#' @export
setMethod("fData<-", c("DGEList", "data.frame"), function(object, value) {
  object@genes <- value
  return(object)
})

#' Get fData
#' @param object An EdgeObject
#' @export
setMethod("fData", "EdgeObject", function(object) {
  return(object@dgeList$genes)
})

#' Set fData
#' @param object An EdgeObject
#' @param value A \code{data.frame}
#' @export
setMethod("fData<-", c("EdgeObject", "data.frame"), function(object, value) {
  object@dgeList$genes <- value
  return(object)
})

#' Get pData (sample annotation)
#' @param object A DGEList
#' @importMethodsFrom Biobase pData pData<-
#' @export
setMethod("pData", "DGEList", function(object) object$samples)

#' Set pData (sample annotation)
#' @param object A DGEList
#' @param value A \code{data.frame}
#' @export
setMethod("pData<-", c("DGEList", "data.frame"), function(object, value) {
  object@samples <- value
  return(object)
})

#' Get pData
#' @param object An EdgeObject
#' @export
setMethod("pData", "EdgeObject", function(object) {
  return(object@dgeList$samples)
})

#' Set pData (sample annotation)
#' @param object A DGEList
#' @param value A \code{data.frame}
#' @export
setMethod("pData<-", c("EdgeObject", "data.frame"), function(object, value) {
  object@dgeList$samples <- value
  return(object)
})

##-------------------------------------##
## Human gene symbols
##-------------------------------------##
fDataHumanGeneSymbol <- function(object) {
  fd <- fData(object)
  fdcols <- colnames(fd)
  isHumanGeneSymbol <- grepl("humangenesymbol", fdcols, ignore.case=TRUE)
  if(any(isHumanGeneSymbol)) {
      if(sum(isHumanGeneSymbol)>1) {
	warning("Following columns found for HumanGeneSymbol, only the first is used",
		paste(fdcols[isHumanGeneSymbol], collapse=","))
      }
      gs <- fd[, which(isHumanGeneSymbol)[1]]
  } else {
      gs <- fd$GeneSymbol
  }
  if(!is.character(gs) && !is.null(gs)) {
    gs <- as.character(gs)
  }
  if(is.null(gs))
     warning("No HumanGeneSymbol or GeneSymbol column was found!")
  return(gs)
}

#' @describeIn humanGeneSymbols Method for DGEList
setMethod("humanGeneSymbols", "DGEList", function(object) fDataHumanGeneSymbol(object))
#' @describeIn humanGeneSymbols Method for EdgeObject
setMethod("humanGeneSymbols", "EdgeObject", function(object) fDataHumanGeneSymbol(object))

##-------------------------------------##
## Retrieve normalization factors
##-------------------------------------##
#' @describeIn normFactors Method for DGEList
setMethod("normFactors", "DGEList", function(object) {
  return(object$samples$norm.factors)
})
#' @describeIn normFactors Method for EdgeObject
setMethod("normFactors", "EdgeObject", function(object) {
  return(normFactors(object@dgeList))
})

##-------------------------------------##
## GLM dispersion estimation
##-------------------------------------##
#' @describeIn estimateGLMDisp Method for EdgeObject
#' 
#' @param object An \code{EdgeObject} object
#' @importFrom edgeR estimateDisp
#' @return An updated \code{EdgeObject} object
#' @export
setMethod("estimateGLMDisp", "EdgeObject", function(object) {
  dge <- object@dgeList
  design <- designMatrix(object@designContrast)
  dge <- estimateDisp(dge, design)
  object@dgeList <- dge
  return(object)
})

##-------------------------------------##
## Fit GLM
##-------------------------------------##
#' @describeIn fitGLM Method for EdgeObject
#' 
#' @param object An \code{EdgeObject} object
#' @param ... Passed to \code{\link[edgeR]{glmFit}}
#' 
#' @return The fit object
#' 
#' @importFrom edgeR glmFit
#' @export
setMethod("fitGLM", "EdgeObject", function(object, ...) {
  fit <- edgeR::glmFit(object@dgeList,
                       design=designMatrix(object@designContrast),
                      ...)
  return(fit)
})

##-----------------------------------## 
## Test GLM
##-----------------------------------## 

utils::globalVariables("logCPM")

#' @describeIn testGLM Method for EdgeObject and DGEGLM.
#' @importFrom edgeR glmLRT topTags
#' @export
setMethod("testGLM", c("EdgeObject", "DGEGLM"),
          function(object, fit) {
            contrasts <- contrastMatrix(object@designContrast)
            toptables <- apply(contrasts, 2, function(x) {
              lrt <- edgeR::glmLRT(fit, contrast=x)
              x <- edgeR::topTags(lrt, n=nrow(lrt$table))$table
              assertEdgeToptable(x)
              return(x)
            })
            
            return(EdgeResult(edgeObj=object,
                              dgeGLM=fit,
                              dgeTables=toptables))
          })

#' Extract DGEList from an EdgeObject 
#' @param object An EdgeObject
#' @describeIn dgeList Extract DGEList from EdgeObject
#' @export
setMethod("dgeList", "EdgeObject", function(object) return(object@dgeList))

#' Extract DGEList from an EdgeResult object
#' @param object An EdgeResult object
#' @describeIn dgeList Extract DGEList from EdgeResult
#' @export
setMethod("dgeList", "EdgeResult", function(object) return(object@dgeList))

#' Extract design matrix from an EdgeObject object
#' @param object An EdgeObject object
#' @importMethodsFrom ribiosExpression designMatrix
#' @importFrom ribiosExpression designMatrix
#' @export
setMethod("designMatrix", "EdgeObject", function(object) ribiosExpression::designMatrix(object@designContrast))

#' Extract design matrix from an EdgeResult object
#' @param object An EdgeResult object
#' @export
setMethod("designMatrix", "EdgeResult", function(object) ribiosExpression::designMatrix(object@designContrast))

#' Extract contrast matrix from an EdgeObject object
#' @param object An EdgeObject object
#' @importMethodsFrom ribiosExpression contrastMatrix
#' @importFrom ribiosExpression contrastMatrix
#' @export
setMethod("contrastMatrix", "EdgeObject", function(object) ribiosExpression::contrastMatrix(object@designContrast))

#' Extract contrast matrix from an EdgeResult object
#' @param object An EdgeResult object
#' @export
setMethod("contrastMatrix", "EdgeResult", function(object) ribiosExpression::contrastMatrix(object@designContrast))

#' Extract contrast names from an EdgeObject object
#' @param object An EdgeObject object
#' @importMethodsFrom ribiosExpression contrastNames
#' @export
setMethod("contrastNames", "EdgeObject", function(object) colnames(contrastMatrix(object)))

#' Extract contrast names from an EdgeResult object
#' @param object An EdgeResult object
#' @export
setMethod("contrastNames", "EdgeResult", function(object) colnames(contrastMatrix(object)))

#' @describeIn designMatrix-set Method for EdgeObject
#' @export
setMethod("designMatrix<-", c("EdgeObject", "matrix"), function(object, value) {
  object@designContrast@design <- value
  return(object)
})

#' @describeIn contrastMatrix-set Method for EdgeObject
#' @export
setMethod("contrastMatrix<-", c("EdgeObject", "matrix"), function(object, value) {
  object@designContrast@contrasts <- value
  return(object)
})

#' Return the number of contrasts
#' @importMethodsFrom ribiosExpression nContrast
#' @param object An \code{EdgeResult} object.
#' @export
setMethod("nContrast", "EdgeResult", function(object) {nContrast(object@designContrast)})

#' Extract contrast sample indices
#' @importMethodsFrom ribiosExpression contrastSampleIndices
#' @param object An EdgeResult object.
#' @param contrast Character, indicating the contrast of interest.
#' @export
setMethod("contrastSampleIndices", c("EdgeResult", "character"), function(object, contrast) {
              contrastSampleIndices(object@designContrast, contrast)
          })

#' Extract contrast sample indices
#' @param object An EdgeResult object.
#' @param contrast Character, indicating the contrast of interest.
#' @export
setMethod("contrastSampleIndices", c("EdgeResult", "integer"), function(object, contrast) {
              contrastSampleIndices(object@designContrast, contrast)
          })

#' Return either NA (if input is NULL) or sqrt
#' @param x Numeric value
#' @return \code{NA} or sqrt value
naOrSqrt <- function(x) {
  if(is.null(x)) { return (NA)}
  return(sqrt(x))
}

#' Show DGEList
#' @param object A DGEList object
#' @export
setMethod("show", "DGEList", function(object) {
  cat(sprintf("A DGEList object with %s features and %d samples\n",
              dim(object)[1], dim(object)[2]))
  cat(sprintf("  - Following items are available: %s\n",
              paste(names(object), collapse=",")))
})

#' Show DGEListList
#' @param object A DGEListList object
#' @export
setMethod("show", "DGEListList", function(object) {
  cat(sprintf("A list of %d DGEList objects:\n", length(object)))
  for(i in seq(along=object@.Data)) {
    cat(sprintf("[[%d]] ", i))
    show(object@.Data[[i]])
  }
})

#' Return sample names from a DGEList object
#' @param object A DGEList object
#' @importMethodsFrom Biobase sampleNames
#' @export
setMethod("sampleNames", "DGEList", function(object) colnames(object$counts))

##----------------------------------------##
## BCV methods
##----------------------------------------## 

#' @describeIn commonBCV method for DGEList
#' @export
setMethod("commonBCV", "DGEList", function(x) {
  naOrSqrt(x$common.dispersion)
})

#' @describeIn tagwiseBCV method for DGEList
#' @export
setMethod("tagwiseBCV", "DGEList", function(x) {
  naOrSqrt(x$tagwise.dispersion)
})

#' @describeIn trendedBCV method for DGEList
#' @export
setMethod("trendedBCV", "DGEList", function(x) {
  naOrSqrt(x$trended.dispersion)
})

#' @describeIn commonBCV method for EdgeResult
#' @export
setMethod("commonBCV", "EdgeResult", function(x)  {
  commonBCV(dgeList(x))
})

#' @describeIn tagwiseBCV method for EdgeResult
#' @export
setMethod("tagwiseBCV", "EdgeResult", function(x)  {
  tagwiseBCV(dgeList(x))
})

#' @describeIn trendedBCV method for EdgeResult
#' @export
setMethod("trendedBCV", "EdgeResult", function(x)  {
  trendedBCV(dgeList(x))
})

#' @describeIn BCV Method for DGEList
#' @importFrom edgeR getDispersion
#' @export
setMethod("BCV", "DGEList", function(x) {
  A <- x$AveLogCPM
  if(is.null(getDispersion(x))) stop("No dispersion available")
  res <- data.frame(aveLogCPM=A,
                    commonBCV = commonBCV(x),
                    tagwiseBCV=tagwiseBCV(x),
                    trendedBCV=trendedBCV(x))
  rownames(res) <- rownames(x$counts)
  return(res)
})

#' @describeIn BCV Method for EdgeResult
#' @export
setMethod("BCV", "EdgeResult", function(x) {
  BCV(dgeList(x))
})

## edgeR::aveLogCPM is a S3 method
#' @importFrom edgeR aveLogCPM
aveLogCPM.EdgeResult <- function(y,...) {
    return(aveLogCPM(dgeList(y)))
}

#' Get sample groups from an EdgeObject object
#' @importMethodsFrom ribiosExpression groups
#' @param object An \code{EdgeObject} object
#' @export
setMethod("groups", "EdgeObject", function(object) {
  return(groups(object@designContrast))
})

#' Get display labels of sample groups
#' @importMethodsFrom ribiosExpression dispGroups
#' @param object An \code{EdgeObject} object
#' @export
setMethod("dispGroups", "EdgeObject", function(object) {
  return(dispGroups(object@designContrast))
})

#' @describeIn modLogCPM Method for DGEList
#' @importFrom edgeR cpm
#' @param object A DGEList object
#' @param prior.count Integer, prior count.
#' @export
setMethod("modLogCPM", "DGEList", function(object, prior.count=2) {
  return(edgeR::cpm(object, prior.count=prior.count, log=TRUE))
})

#' @describeIn modLogCPM Method for EdgeObject
#' @export
setMethod("modLogCPM", "EdgeObject", function(object) {
  return(modLogCPM(dgeList(object)))
})

##----------------------------------------##
## voom
##----------------------------------------##

#' @describeIn voom Method for DGEList
#' @export
setMethod("voom", "DGEList", function(object,...) {
  limma::voom(object, ...)
})

#' @describeIn voom Method for matrix
#' @export
setMethod("voom", "matrix", function(object,...) {
  limma::voom(object, ...)
})


#' @describeIn voom Method for matrix
#' @export
setMethod("voom", "ExpressionSet", function(object,...) {
  limma::voom(object, ...)
})

#' @describeIn voom Method for EdgeObject, norm.factors are calculated first if not done yet
#' @export
setMethod("voom", "EdgeObject", function(object,...) {
  dgelist <- calcNormFactorsIfNot(dgeList(object))
  limma::voom(dgelist,
              design=designMatrix(object),
              ...)
})

##----------------------------------------## 
## disp
##----------------------------------------## 

#' @describeIn commonDisp Method for DGEList
#' @export
setMethod("commonDisp", "DGEList", function(object) {
  return(object$common.dispersion)
})

#' @describeIn commonDisp Method for EdgeObject
#' @export
setMethod("commonDisp", "EdgeObject", function(object) {
  return(commonDisp(dgeList(object)))
})

#' @describeIn hasCommonDisp Method for DGEList
#' @export
setMethod("hasCommonDisp", "DGEList", function(object) {
  disp <- commonDisp(object)
  return(!is.na(disp) & !is.null(disp))
})

#' @describeIn hasCommonDisp Method for EdgeObject
#' @export
setMethod("hasCommonDisp", "EdgeObject", function(object) {
  return(hasCommonDisp(dgeList(object)))
})

#' @describeIn commonDisp-set Method for DGEList
#' @export
setReplaceMethod("commonDisp", c("DGEList", "numeric"), function(object, value) {
  object$common.dispersion <- value
  object@dgeList$trended.dispersion <- NULL
  object@dgeList$tagwise.dispersion <- NULL
  return(object)
})

#' @describeIn commonDisp-set Method for EdgeObject
#' @export
setReplaceMethod("commonDisp", c("EdgeObject", "numeric"), function(object, value) {
  object@dgeList$common.dispersion <- value
  object@dgeList$trended.dispersion <- NULL
  object@dgeList$tagwise.dispersion <- NULL
  return(object)
})

localSetCommonDispIfMissing <- function(object, common.disp) {
  if(!hasCommonDisp(object)) {
    commonDisp(object) <- common.disp
  }
  return(object)
}

#' @describeIn setCommonDispIfMissing Method for DGEList
#' @export
setMethod("setCommonDispIfMissing", c("DGEList","numeric"), function(object, value) {
  localSetCommonDispIfMissing(object, value)
})

#' @describeIn setCommonDispIfMissing Method for EdgeObject
#' @export
setMethod("setCommonDispIfMissing", c("EdgeObject","numeric"), function(object, value) {
  localSetCommonDispIfMissing(object, value)
})

##----------------------------------------##
## Features and sample names
##----------------------------------------##

#' Feature names
#' @param object An EdgeObject
#' @importMethodsFrom Biobase featureNames
#' @export
setMethod("featureNames", "EdgeObject", function(object) {
  return(rownames(dgeList(object)$counts))
})

#' Sample names
#' @param object An EdgeObject
#' @importMethodsFrom Biobase sampleNames
#' @export
setMethod("sampleNames", "EdgeObject", function(object) {
  return(colnames(dgeList(object)$counts))
})

#' Build design matrix from a DGEList object
#' 
#' @param object A DGEList object
#' @param formula Formula, passed to \code{\link{model.matrix}}
#'
#' Sample annotation is used to construct the formula
#'
#' @param ... Not used so far
#' @export model.DGEList
model.DGEList <- function(object, formula, ...) {
  model.matrix(formula, data=object$samples)
}

##----------------------------------------##
## SVA
##----------------------------------------##

#' @describeIn inferSV method for matrix as input
#' @export
setMethod("inferSV", c("matrix", "matrix"), function(object, design, ...) {
  svaRes <- sva::sva(dat=object, 
                     mod=design,
                     ...)
  sv <- as.matrix(svaRes$sv)
  colnames(sv) <- sprintf("sv%d", 1:ncol(sv))
  return(sv)
})

#' @describeIn inferSV method for voom-transformed DGEList and design matrix 
#'    as input
#' @export
setMethod("inferSV", c("DGEList", "matrix"), function(object, design, ...) {
  object <- calcNormFactorsIfNot(object)
  voomE <- limma::voom(object, design=design)$E
  inferSV(voomE, design)
})

#' @describeIn inferSV method for voom-transformed DGEList and formula
#'    as input
#' @export
setMethod("inferSV", c("DGEList", "formula"), function(object, design, ...) {
  designMatrix <- model.DGEList(object, design)
  inferSV(object, designMatrix)
})

#' @describeIn updateDesignMatrixBySVA for DGEList and formula
#' @export
setMethod("updateDesignMatrixBySVA", c("DGEList", "formula"), function(object, design, ...) {
  designMatrix <- model.DGEList(object, design)
  sv <- inferSV(object, design)
  res <- cbind(designMatrix, sv)
  return(res)
})

#' @describeIn voomSVA Method for count matrix and design matrix
#' @export
setMethod("voomSVA", c("matrix", "matrix"), function(object, design) {
  voomE <- voom(object, design=design)$E
  sv <- inferSV(voomE, design)
  return(sv)
})

#' @describeIn voomSVA Method for DGEList and design matrix
#' @export
setMethod("voomSVA", c("DGEList", "matrix"), function(object, design) {
  object <- calcNormFactorsIfNot(object)
  voomE <- voom(object, design=design)$E
  sv <- inferSV(voomE, design)
  object$voom <- voomE
  object$sv <- sv
  object$designMatrix <- design
  object$designMatrixWithSV <- cbind(design, sv)
  object$voomSVRemoved <- removeBatchEffect(voomE, covariates=sv, design=design)
  return(object)
})

#' @describeIn voomSVA Method for count matrix and design formula
#' @export
setMethod("voomSVA", c("DGEList", "formula"), function(object, design) {
  designMatrix <- model.DGEList(object, design)
  voomSVA(object, designMatrix)
})


##----------------------------------------##
## show
##----------------------------------------##

#' Show an SigFilter object
#' @param object An SigFilter object
#' @importMethodsFrom methods show
#' @export
setMethod("show", "SigFilter", function(object) {
  title <- "Significantly Differentially Expressed Genes Filter"
  msgs <- c()
  if(!isUnsetPosLogFC(object))
    msgs <- c(msgs,
                  sprintf("posLogFC filter set: logFC>=%f", posLogFC(object)))
  if(!isUnsetNegLogFC(object))
    msgs <- c(msgs,
                  sprintf("negLogFC filter set: logFC<=%f", negLogFC(object)))
  if(!isUnsetPValue(object))
    msgs <- c(msgs,
                  sprintf("pValue filter set: pValue<=%f", pValue(object)))
  if(!isUnsetFDR(object))
    msgs <- c(msgs,
                  sprintf("FDR filter set: FDR<=%f", FDR(object)))
  if(length(msgs)==0L)
    msgs <- c(msgs, "No active filter set")
  messages <- paste(paste(title,
                          paste("*", msgs, collapse="\n"),
                          sep="\n"), "\n", sep="")
  
  cat(messages)
  return(invisible(messages))
})

#' Show an EdgeSigFilter object
#' @param object An SigFilter object
#' @importFrom methods callNextMethod
#' @export
setMethod("show", "EdgeSigFilter", function(object) {
  msgs <- callNextMethod()
  if(!isUnsetLogCPM(object)) {
    msg <- sprintf("* logCPM filter set: logCPM>=%f", logCPM(object))
    cat(msg)
    msgs <- c(msgs, msg)
  }
  return(invisible(msgs))
})

#' Show an LimmaSigFilter object
#' @param object An LimmaSigFilter object
#' @export
setMethod("show", "LimmaSigFilter", function(object) {
  msgs <- callNextMethod()
  if(!isUnsetAveExpr(object)) {
    msg <- sprintf("* aveExpr filter set: aveExpr>=%f", aveExpr(object))
    cat(msg)
    msgs <- c(msgs, msg)
  }
  return(invisible(msgs))
})

#' Show an EdgeResult object
#' @param object An EdgeResult object
#' @export
setMethod("show", "EdgeResult", function(object) {
  summary <- sprintf("EdgeResult object: %d genes, %d samples, %d contrasts",
                     nrow(getCounts(dgeList(object))),
                     ncol(getCounts(dgeList(object))),
                     length(object@dgeTables))
  showBCV <- "Call plotBCV() to visualize biological coefficient of variance"
  sigFilterInfo <-  sprintf("* Significant DGE filter (call updateSigFilter() to update the settings): \n%s",
                            show(sigFilter(object)))
  ER_SHOW_SEP <- paste(rep("-", 40), collapse="")
  messages <- paste(summary, showBCV,
                    ER_SHOW_SEP, sigFilterInfo, "", sep="\n")
  cat(messages)
  return(invisible(messages))
})
