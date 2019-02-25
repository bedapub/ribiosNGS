##---------##
## generics
##---------##
setGeneric("fcol", function(object, base) standardGeneric("fcol"))
setGeneric("fcbase", function(object) standardGeneric("fcbase"))

##---------##
## methods
##---------##
## methods
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
