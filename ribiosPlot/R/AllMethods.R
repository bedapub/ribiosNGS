setGeneric("fcol", function(object, base) standardGeneric("fcol"))
#' Principal component (PC) labels
setGeneric("pcLabels", function(object, ...) standardGeneric("pcLabels"))


setGeneric("fcbase", function(object) standardGeneric("fcbase"))

setMethod("fcol", c("character", "character"), function(object, base) {
  new("fcol", object, base=base)
})
setMethod("fcbase", "fcol", function(object) return(object@base))

setMethod("show", "fcol", function(object) {
  acol <- as.character(object)
  bcol <- fcbase(object)
  cat("Factor-matching colors\n",
      "Colors: (", length(acol), "):", ribiosUtils::chosenFew(acol),"\n",
      "Base colors (", length(bcol), "):", ribiosUtils::chosenFew(fcbase(object)), "\n",
      sep="")
})

#' Labels of principal components
setMethod("pcLabels", "PCAScoreMatrix", function(object, variant=c("compact", "full")) {
  variant <- match.arg(variant)
  fmt <- ifelse(variant=="compact",
                "%s (%s)",
                "%s (%s variance explained)")
  res <- sprintf(fmt,
                 colnames(object),
                 ribiosUtils::percentage(object@expvar))
  return(res)
})

