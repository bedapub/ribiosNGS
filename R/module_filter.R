## filter modules based on feature and condition number in a module
feat.cond.threshold <- function(x, feat.thr=5, cond.thr=3) {
  stopifnot(length(feat.thr)==1 || length(feat.thr)==length(x))
  stopifnot(length(cond.thr)==1 || length(cond.thr)==length(x))
  feat.over <- getNoFeatures(x) >= feat.thr
  cond.over <- getNoSamples(x) >= cond.thr
  pass.filter <- feat.over & cond.over
  x[[pass.filter]]
}
