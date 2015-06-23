jitter.xyplot <- function(x,y, N=20, factor=1, ...) {
  if(!is.factor(x)) {
    xfac <- factor(x, levels=unique(x))
  } else {
    xfac <- x
  }
  facs.raw <- tapply(y,xfac, function(y0) {
    cuts <- cut(y0, breaks=pmin(as.integer(length(y)/5),N))
    clen <- ave(seq(along=cuts), cuts, FUN=length)
    clen/max(clen, na.rm=TRUE)*factor
  })
  facOrd <- match(1:length(y), unlist(split(1:length(y), xfac)))
  facs <- unlist(facs.raw)[facOrd]
  facs[is.na(facs)] <- 0
  if(!is.numeric(x))
    x <- as.numeric(x)
  xnew <- jitter(x, factor=facs)
  panel.xyplot(xnew, y, ...)
}
