setMethod("contrastNames", "DesignContrast", function(object)
          return(colnames(contrastMatrix(object))))

setMethod("exprsToLong", "matrix", function(x, idvar="illID",timevar="hybridID", valuevar="value", 
                                            ids=rownames(x), valueType="raw") {
  x <- as.data.frame(x)
  colnames(x) <- paste(valuevar, colnames(x), sep=".")
  va <- 1:ncol(x)
  x[,idvar] <- ids
  xLong <- reshape(x, idvar=idvar, varying=va, timevar=timevar, direction="long")
  rownames(xLong) <- NULL
  xLong$type <- valueType
  xLong <- xLong[,c(idvar, timevar, "type", valuevar)]
  return(xLong)
})
setMethod("exprsToLong", "ExpressionSet", function(x,...) {
  exprsToLong(exprs(x),...)
})

setMethod("rowscale", c("ExpressionSet","ANY", "ANY"), function(object, center, scale) {
  if(missing(center))
    center <- TRUE
  if(missing(scale))
    scale <- TRUE
  if(storageMode(object)!="lockedEnvironment")
    warning("The storageMode of the input object is not 'lockedEnvironment': exprs is replaced by row-scaled values\n",
            "To prevent damaging the data integrity, set the storageMode to 'lockedEnvironment'")
  
  exprs(object) <- t(scale(t(exprs(object)),
                           center=center, scale=scale))
  return(object)
})

## for limma::MArrayLM

#' Extract design matrix from MArrayLM
#'
#' @param object A MArrayLM object from the limma package
#' @return Design matrix
setMethod("designMatrix", "MArrayLM", function(object) {
    return(object$design)
})

#' Extract contrast names from MArrayLM
#'
#' @param object A MArrayLM object from the limma package
#' @return Character vector of contrast names
setMethod("contrastNames", "MArrayLM", function(object) {
    return(colnames(contrastMatrix(object)))
})
