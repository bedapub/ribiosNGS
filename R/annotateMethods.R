#' @include AllClasses.R AllGenerics.R utils.R

isValidID <- function(featNames) {
  invalid <- is.na(featNames) || featNames=="" || featNames=="-"
  return(!invalid)
}
validIDs <- function(featNames) {
  return(featNames[isValidID(featNames)])
}
likeGeneID <- function(featNames) grepl("^[0-9]*$", featNames)
likeGeneSymbol <- function(featNames) grepl("^[A-Za-z][A-Za-z0-9]*$", featNames)
likeRefSeq <- function(featNames) grepl("^[N|X][M|R|G|P]_[0-9]+\\.?[0-9]*$", featNames)
likeEnsembl <- function(featNames) grepl("^ENS[T|G|P][0-9]+$", featNames)

#' Guess wether a vector of strings look like human gene symbols
#' @param x A vector of strings
#' @return Logical
#' \code{TRUE} is only returned if at least eighty percent features match the pattern
#' @export
likeHumanGeneSymbol <- function(x) mean(grepl("^[A-Z][A-Z0-9@orf]*$",
                                              subsetFeatures(x)), na.rm=TRUE)>=0.8


#' @describeIn sniffFeatures sniff features for EdgeObject
#' @export
setMethod("sniffFeatures", "EdgeObject", function(object) {
  featNames <- featureNames(object)
  positive.thr <- 0.5
  vnames <- validIDs(featNames)
  geneIdsLike <- likeGeneID(vnames)
  if(mean(geneIdsLike)>=positive.thr) {
    return("GeneID")
  }
  geneSymbolsLike <- likeGeneSymbol(vnames)
  if(mean(geneSymbolsLike)>=positive.thr) {
    return("GeneSymbol")
  }
  refseqLike <- likeRefSeq(vnames)
  if(mean(refseqLike)>=positive.thr) {
    return("RefSeq")
  }
  ensemblLike <- likeEnsembl(vnames)
  if(mean(ensemblLike)>=positive.thr) {
    return("EnsEMBL")
  }
  return("Unknown")
})

subsetFeatures <- function(x, n=100) {
    if(length(x)<=n) return(x)
    return(sample(x, n))
}
           
#' @importMethodsFrom ribiosExpression annotate
#' @importFrom ribiosAnnotation  annotateEnsembl annotateGeneIDs annotateGeneSymbols annotateRefSeqs
#' @export
setMethod("annotate", c("EdgeObject","character", "logical"),
          function(object, target, check.target) {
            target <- match.arg(target,
                                choices=c("GeneID", "GeneSymbol", "RefSeq", "EnsEMBL", "Automatic", "Unknown"))
            if(target=="Automatic") {
              target <- sniffFeatures(object)
            }
            if(target=="Unknown") {
              ## no annotations available
              object@dgeList$genes <- NULL
              object@dgeList$annotation <- NA
              return(object)
            }
            feats <- featureNames(object)
            ## only first 100 characters are used
            feats <- substr(feats, 1, 100)
            isValidFeat <- isValidID(feats)
            if(target=="GeneID") {
              anno <- annotateGeneIDs(feats,orthologue = TRUE)
            } else if (target=="GeneSymbol") {
                ## this is very slow because of the database table look up, but is working...
              organism <- ifelse(likeHumanGeneSymbol(feats), "human", "any")
              anno <- ribiosAnnotation::annotateGeneSymbols(feats,organism=organism, orthologue = TRUE)
            } else if (target=="RefSeq") {
              anno <- ribiosAnnotation::annotateRefSeqs(feats,orthologue = TRUE)
            } else if (target=="EnsEMBL") {
              anno <- ribiosAnnotation::annotateEnsembl(feats,orthologue = TRUE)
            }
            if(check.target) {
              positive.thr <- 0.5
              validFeatGeneID <- anno$GeneID[isValidFeat]
              isBadGuess <- mean(is.na(validFeatGeneID))>=positive.thr
              if(isBadGuess) {
                object@dgeList$annotation <- NULL
                return(object)
              }
            }

            object@dgeList$annotation <- target
            object@dgeList$genes <- anno
            return(object)
          })

#' @importMethodsFrom ribiosExpression annotate
#' @export
setMethod("annotate", c("EdgeObject","character", "missing"),
          function(object, target) {
            annotate(object, "Automatic", TRUE)
          })

#' @importMethodsFrom ribiosExpression annotate
#' @export
setMethod("annotate", c("EdgeObject","missing", "missing"),
          function(object) {
            annotate(object, "Automatic")
          })

#' @importMethodsFrom BiocGenerics annotation
#' @export
setMethod("annotation", "EdgeObject", function(object) {
  return(object@dgeList$annotation)
})

#' @importMethodsFrom Biobase fData fData<-
#' @export
setMethod("fData", "DGEList", function(object) object$genes)

#' @export
setMethod("fData<-", c("DGEList", "data.frame"), function(object, value) {
  object@genes <- value
  return(object)
})

#' @export
setMethod("fData", "EdgeObject", function(object) {
  return(object@dgeList$genes)
})

#' @export
setMethod("fData<-", c("EdgeObject", "data.frame"), function(object, value) {
  object@dgeList$genes <- value
  return(object)
})

#' @importMethodsFrom Biobase pData pData<-
#' @export
setMethod("pData", "DGEList", function(object) object$samples)

#' @export
setMethod("pData<-", c("DGEList", "data.frame"), function(object, value) {
  object@samples <- value
  return(object)
})

#' @export
setMethod("pData", "EdgeObject", function(object) {
  return(object@dgeList$samples)
})

#' @export
setMethod("pData<-", c("EdgeObject", "data.frame"), function(object, value) {
  object@dgeList$samples <- value
  return(object)
})

#' @describeIn isAnnotated Method for EdgeObject
#' @export
setMethod("isAnnotated", "EdgeObject", function(object) {
		  anno <- annotation(object)
		  res <- !is.null(anno) && !is.na(anno) && anno!=""
		  return(res)
})

