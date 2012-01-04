setGeneric("exprsToLong", function(x,...) standardGeneric("exprsToLong"))


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
  exprs(object) <- t(scale(t(exprs(object)),center=center, scale=scale))
  return(object)
})
