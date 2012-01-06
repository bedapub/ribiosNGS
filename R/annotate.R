setGeneric("annotate",
           function(object, target, check.target, ...) standardGeneric("annotate"))

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
          c("ExpressionSet", "character", "logical"),
          function(object, target, check.target) {
            ann <- annChar(featureNames(object),
                            target,
                            check.target=check.target)
            fData(object) <- ann
            annotation(object) <- target
            return(object)
          })
setMethod("annotate",
          c("ExpressionSet", "character","missing"),
          function(object, target) {
            annotate(object, target, check.target=FALSE)
          })
setMethod("annotate",
          c("character", "character", "logical"),
          function(object, target, check.target) {
            annChar(object, target, check.target)
          })
setMethod("annotate",
          c("character", "character", "missing"),
          function(object, target) {
            annChar(object, target, check.target=FALSE)
          })

setGeneric("reannotate",
           function(object, check.target,...) standardGeneric("reannotate"))
setMethod("reannotate",
          c("ExpressionSet", "logical"),
          function(object, check.target) {
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
setMethod("reannotate",
          c("ExpressionSet", "missing"),
          function(object) {
            reannotate(object, check.target=FALSE)
          })
