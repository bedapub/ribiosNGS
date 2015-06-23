remainHighestVarProbe <- function(eset, probe.index.name) {
  .Deprecated(new="keepMaxStatProbe", package="ribiosExpression")
  keepMaxStatProbe(eset, probe.index.name, stat=sd, na.rm=TRUE)
}
keepHighestVarProbe <- function(eset, probe.index.name) {
  .Deprecated(new="keepMaxStatProbe", package="ribiosExpression")
  keepMaxStatProbe(eset, probe.index.name, stat=sd, na.rm=TRUE)
}

## rank (better: sort) data frame by the order of one column
rankByCol <- function(data.frame, column, decreasing=TRUE) {
  .Deprecated(new="sortByCol", package="ribiosExpression")
  sortByCol(data.frame, column, decreasing=decreasing)
}

eset2gct <- function(eset, file=stdout(), feature.name.col) {
  .Deprecated("writeGct", "ribiosExpression")
  writeGct(eset, file, feature.name.col)
}
eset2cls <- function(eset, file=stdout(), sample.group.col) {
  .Deprecated("writeCls", "ribiosExpression")
  writeCls(eset, file, sample.group.col)
}



