## histogram with quantile
qHist <- function(x,quantiles=0.25, breaks=100,
                  qlty=2, qlwd=2, qcol="red", ...) {
  res <- hist(x, breaks=breaks, ...)
  if(!is.null(quantiles)) {
    qs <- quantile(x, quantiles, na.rm=TRUE)
    abline(v=qs, lty=qlty, lwd=qlwd, col=qcol)
    qsLabels <- sprintf("Q%d=%2.2f",
                        as.integer(quantiles*100),
                        qs)
    qsY <- par("usr")[4]
    text(qs+strwidth("M", "user"), qsY, qsLabels,
         srt=90, adj=c(1,1), col=qcol, font=2)
    res$quantiles <- qs
  }
  return(invisible(res))
}

## xclip hist: histogram with x-axis clipped with quantiles
qBreaks <- function(x,quantiles=c(0,0.99), breaks=100) {
  haltifnot(length(quantiles)==2 & quantiles[2]>quantiles[1],
            msg="quantiles must be a vector of two numbers with quantiles[2]>quantiles[1]")
  qts <- quantile(x, quantiles, na.rm=TRUE)
  subbreak <- as.integer((max(x, na.rm=TRUE)-min(x, na.rm=TRUE))/(qts[2]-qts[1]))*breaks
  return(subbreak)
}

xclipHist <- function(x, xclip=c(0.01, 0.99), breaks=100,
                     quantiles=0.25, qlty=2, qlwd=2, qcol="red",...) {
  haltifnot(length(xclip)==2 & xclip[2]>xclip[1],
            msg="xclip must be a vector of two numbers with xclip[2]>xclip[1]")
  xBreaks <- qBreaks(x, quantiles=xclip, breaks=breaks)
  xlim <- quantile(x, xclip, na.rm=TRUE)
  qHist(x, breaks=xBreaks, xlim=xlim, quantiles=quantiles,
        qlty=qlty, qlwd=qlwd, qcol=qcol, ...)
}
