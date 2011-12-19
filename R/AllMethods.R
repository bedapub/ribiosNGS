## helper functions
setGeneric("rowscale", function(object, center, scale) standardGeneric("rowscale"))
setMethod("rowscale", c("matrix","ANY", "ANY"), function(object, center, scale) {
  if(missing(center))
    center <- TRUE
  if(missing(scale))
    scale <- TRUE
  t(scale(t(object),center=center, scale=scale))
})

setMethod("rowscale", c("ExpressionSet","ANY", "ANY"), function(object, center, scale) {
  if(missing(center))
    center <- TRUE
  if(missing(scale))
    scale <- TRUE
  exprs(object) <- t(scale(t(exprs(object)),center=center, scale=scale))
  return(object)
})

## new generics
setGeneric("getUniqueFeatureLength", function(object) standardGeneric("getUniqueFeatureLength"))
setMethod("getUniqueFeatureLength", "ISAModules", function(object) {
  uniqueLength(unlist(getFeatureNames(object)))
})

setGeneric("getAnnotatedFeatureScores", function(eset, object, mods) standardGeneric("getAnnotatedFeatureScores"))
setMethod("getAnnotatedFeatureScores",
          c("ExpressionSet", "ISAModules"), function(eset, object, mods) {
            if(missing(mods))
              mods <- 1:length(object)
            lPerMode <- function(eSet, x, mod) {
              annotation <- fData(eset)[getFeatures(x)[[mod]],]
              score <- getFeatureScores(x, mod)[[1]]
              cbind(annotation, ISA.score=score)
            }
            lapply(mods, function(x) lPerMode(eset, object, x))
          })

setClass("dcISAModules",
         representation(group="factor"),
         contains="ISAModules")
setGeneric("group", function(object) standardGeneric("group"))
setGeneric("group<-", function(object, value) standardGeneric("group<-"))
setMethod("group", "dcISAModules", function(object) object@group)
setReplaceMethod("group", c("dcISAModules", "factor"),
                 function(object, value) {
                   object@group <- value
                   return(object)
                 })
setMethod("[[", c(x="dcISAModules", i="ANY", j="ANY"),
           function (x, i, j, ...) {
             .local <- function (x, i, j, ..., drop = FALSE) 
               {
                 if (!missing(i)) {
                   x@genes <- x@genes[, i, drop = FALSE]
                   x@conditions <- x@conditions[, i, drop = FALSE]
                   x@seeddata <- x@seeddata[i, ]
                   x@group <- x@group[i]
                 }
                 x
               }
             .local(x, i, j, ...)
           })

## repalce pData of ISAModules
setReplaceMethod("pData", c("ISAModules", "data.frame"), function(object, value) {
  object@rundata$pData <- value
  return(object)
})
