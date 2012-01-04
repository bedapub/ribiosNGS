setGeneric("annotate",
           function(object, target, ...) standardGeneric("annotate"))

annChar <- function(object, target, check.target=FALSE) {
  if(check.target) {
    has.target <- target %in% gtiChiptypes()
    if(!has.target)
      stop("Chip type ", target, " is not supported by GTI.\n",
           "Call 'gtiChiptypes()' to see supported types")
  }
  return(annotateProbesets(object, target))
}

setMethod("annotate",
          c("ExpressionSet", "character"),
          function(object, target, check.target=FALSE) {
            ann <- annChar(featureNames(object),
                            target,
                            check.target=check.target)
            fData(object) <- ann
            annotation(object) <- target
            return(object)
          })
setMethod("annotate",
          c("character", "character"),
          function(object, target, check.target=FALSE) {
            annChar(object, target, check.target)
          })


setGeneric("reannotate",
           function(object, ...) standardGeneric("reannotate"))
setMethod("reannotate",
          "ExpressionSet",
          function(object, check.target=FALSE) {
            old.ann <- annotation(object)
            new.ann <- bioc2gti(old.ann)
            if(check.target && is.na(new.ann)) {
              stop("Chip type '", old.ann, "' of Bioconductor ",
                   "has no corresponding annotation information ",
                   "in GTI.\n",
                   "Call 'gtiChiptypes()' to see supported types")
            }
            annotate(object, new.ann)
          })
