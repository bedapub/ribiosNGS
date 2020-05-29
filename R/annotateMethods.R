#' @include AllClasses.R AllGenerics.R utils.R
NULL

#' Annotate an EdgeObject
#' @param object An EdgeObject.
#' @param target Character, target of annotation.
#' @param check.target Logical, check whether the target is valid or not.
#' @importMethodsFrom ribiosExpression annotate
#' @importFrom ribiosAnnotation annotateEnsembl annotateGeneIDs annotateGeneSymbols annotateRefSeqs
#' @importFrom ribiosAnnotation isValidFeatureID likeHumanGeneSymbol
#' @importFrom ribiosExpression sniffFeatureType
#' @export
setMethod("annotate", c("EdgeObject","character", "logical"),
          function(object, target, check.target) {
            target <- match.arg(target,
                                choices=c("Automatic", "GeneID", "GeneSymbol", 
                                          "RefSeq", "Ensembl", "UniProt", 
                                          "Unknown"))
            if(target=="Automatic") {
              target <- ribiosExpression::sniffFeatureType(object)
            }
            if(target=="Unknown") {
              ## no annotations available
              object@dgeList$genes <- NULL
              object@dgeList$annotation <- NA
              return(object)
            }
            feats <- featureNames(object)
            ## only first 20 characters are used so that the query strings 
            ## are not too long
            feats <- substr(feats, 1, 20)
            if(target=="GeneID") {
              anno <- annotateGeneIDs(feats,orthologue = TRUE)
            } else if (target=="GeneSymbol") {
                ## this is very slow because of the database table look up, but is working...
              organism <- ifelse(likeHumanGeneSymbol(feats), "human", "any")
              anno <- ribiosAnnotation::annotateGeneSymbols(feats,organism=organism, orthologue = TRUE)
            } else if (target=="RefSeq") {
              anno <- ribiosAnnotation::annotateRefSeqs(feats,orthologue = TRUE)
            } else if (target=="Ensembl") {
              anno <- ribiosAnnotation::annotateEnsembl(feats,orthologue = TRUE)
            } else if (target=="UniProt") {
              anno <- ribiosAnnotation::annotateAnyIDs(feats, orthologue = TRUE)
            }
            if(check.target) {
              positive.thr <- 0.5
              isValidFeat <- isValidFeatureID(feats)
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

#' Annotate an EdgeObject, without checking the target
#' @param object An EdgeObject
#' @param target Character, target of annotation
#' @importMethodsFrom ribiosExpression annotate
#' @export
setMethod("annotate", c("EdgeObject","character", "missing"),
          function(object, target) {
            annotate(object, "Automatic", TRUE)
          })

#' Annotate an EdgeObject automatically without checking the target
#' @param object An EdgeObject
#' @importMethodsFrom ribiosExpression annotate
#' @export
setMethod("annotate", c("EdgeObject","missing", "missing"),
          function(object) {
            annotate(object, "Automatic")
          })

