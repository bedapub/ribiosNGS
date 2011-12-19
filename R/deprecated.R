setGeneric("getFeatureLengths", function(object) standardGeneric("getFeatureLengths"))
setGeneric("getSampleLengths", function(object) standardGeneric("getSampleLengths"))
setMethod("getFeatureLengths", "ISAModules", function(object) {
  .Deprecated(new="getNoFeatures", package="eisa")
  sapply(getFeatureNames(object), length)
})
setMethod("getSampleLengths", "ISAModules", function(object) {
  .Deprecated(new="getNoSamples", package="eisa")
  sapply(getSampleNames(object), length)
})
