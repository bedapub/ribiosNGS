setGeneric("rowscale", function(object, center, scale) standardGeneric("rowscale"))

setMethod("rowscale", c("matrix","ANY", "ANY"), function(object, center, scale) {
  if(missing(center))
    center <- TRUE
  if(missing(scale))
    scale <- TRUE
  t(scale(t(object),center=center, scale=scale))
})
