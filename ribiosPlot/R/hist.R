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

## histogram of matrix
histMat <- function(mat,
                    linesOpt=list(lwd=NULL, col=NULL,lty=NULL, type=NULL, pch=NULL),
                    main=NULL, xlab=NULL, xlim=NULL,
                    ...) {

  if(!is.matrix(mat)) {
    stop("input must be a numeric matrix")
  }
  xrange <- symrange(mat, mid=0)
  mat.dens <- apply(mat, 2, density, na.rm=TRUE)
  
  if(missing(main)) 
    main <- as.character(substitute(mat))
  if(missing(xlab))
    xlab <- ""
  if(missing(xlim))
    xlim <- xrange
  
  oh <- hist(mat, freq=FALSE, xlab=xlab, main=main, xlim=xlim, ...)

  lines.col <- nonNull(linesOpt$col, palette(), ncol(mat))
  lines.lwd <- nonNull(linesOpt$lwd, 1L, ncol(mat))
  lines.type <- nonNull(linesOpt$type, "l", ncol(mat))
  lines.lty <- nonNull(linesOpt$lty, 1L, ncol(mat))
  lines.pch <- nonNull(linesOpt$pch, 1L, ncol(mat))
  
  for(i in 1:ncol(mat)) {
    lines(mat.dens[[i]], col=lines.col[i], lwd=lines.lwd[i],
          type=lines.type[i], lty=lines.lty[i], pch=lines.pch[i])
  }
  lopt <- list(col=lines.col, lwd=lines.lwd, type=lines.type,
               lty=lines.lty, pch=lines.pch)

  oh$xlim <- xlim
  oh$linesOpt <- lopt
  return(invisible(oh))
}
