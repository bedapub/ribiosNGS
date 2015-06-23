setOldClass("prcomp")

setClass("bidata",
         representation(x="matrix",
                        y="matrix",
                        var="numeric"))
setGeneric("expvar", function(x) standardGeneric("expvar"))
setGeneric("dexpvar", function(x) standardGeneric("dexpvar"))
setGeneric("pexpvar", function(x) standardGeneric("pexpvar"))
setGeneric("bidata", function(x,...) standardGeneric("bidata"))
setMethod("bidata", "prcomp", function(x,...) bidata.prcomp(x))
setMethod("expvar", "prcomp", function(x) (x$sdev)^2)
setMethod("dexpvar", "prcomp", function(x) (x$sdev^2)/sum(x$sdev^2))
setMethod("pexpvar", "prcomp", function(x) cumsum(x$sdev^2)/sum(x$sdev^2))
setMethod("expvar", "bidata", function(x) x@var)
setMethod("dexpvar", "bidata", function(x) (x@var)/sum(x@var))
setMethod("pexpvar", "bidata", function(x) cumsum(x@var)/sum(x@var))


## unsigned range
usrange <- function(x) {
   c(-abs(min(x, na.rm = TRUE)), abs(max(x, na.rm = TRUE)))
}
bidata.prcomp <- function(x, choices=1L:2L, scale=1L, rescaling=FALSE) {
  if (length(choices) != 2L) 
    stop("length of choices must be 2")
  if (!length(scores <- x$x)) 
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
         domain = NA)
  if (is.complex(scores)) 
    stop("biplots are not defined for complex PCA")
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  if (scale < 0 || scale > 1) 
    warning("'scale' is outside [0, 1]")
  if (scale != 0) 
    lam <- lam^scale
  else lam <- 1
  if (rescaling) 
    lam <- lam/sqrt(n)
  xx <- t(t(scores[, choices])/lam)
  yy <- t(t(x$rotation[,choices])*lam)
  return(new("bidata", x=xx, y=yy, var=(x$sdev)^2))
}

biplot.bidata <- function(bidata,
                          var.axes = TRUE,
                          xlabs=NULL, ylabs=NULL,
                          col, cex = rep(par("cex"), 2), 
                          expand = 1, xlim = NULL, ylim = NULL,
                          xlab=NULL, ylab=NULL,
                          arrow.len = 0.1,
                          ...) {
  x <- bidata@x
  y <- bidata@y
  n <- nrow(x)
  p <- nrow(y)
  if (missing(xlabs)) {
    xlabs <- dimnames(x)[[1L]]
    if (is.null(xlabs)) 
      xlabs <- 1L:n
  }
  ##dimnames(x) <- list(xlabs, dimnames(x)[[2L]])
  if (missing(ylabs)) {
    ylabs <- dimnames(y)[[1L]]
    if (is.null(ylabs)) 
      ylabs <- paste("Var", 1L:p)
  } else if (is.null(ylabs)) {
    ylabs <- rep("", nrow(y))
  }

  ##dimnames(y) <- list(ylabs, dimnames(y)[[2L]])
  if (length(cex) == 1L) 
    cex <- c(cex, cex)
  if (missing(col)) {
    col <- par("col")
    if (!is.numeric(col)) 
      col <- match(col, palette(), nomatch = 1L)
    col <- c(col, col + 1L)
  }
  else if (length(col) == 1L) 
    col <- c(col, col)
  rangx1 <- usrange(x[, 1L])
  rangx2 <- usrange(x[, 2L])
  rangy1 <- usrange(y[, 1L])
  rangy2 <- usrange(y[, 2L])
  if (missing(xlim) && missing(ylim)) 
    xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
  else if (missing(xlim)) 
    xlim <- rangx1
  else if (missing(ylim)) 
    ylim <- rangx2
  ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
  ## on.exit(par(op))
  ## op <- par(pty = "s")

  dvars <- dexpvar(bidata)*100
  if(missing(xlab))
    xlab <- sprintf("PC1 (%2.1f%%)", dvars[1])
  if(missing(ylab))
    ylab <- sprintf("PC2 (%2.1f%%)", dvars[2])
  
  plot(x, type = "n",
       xlim = xlim, ylim = ylim, col = col[1L],
       xlab=xlab, ylab=ylab,
       ...)
  if(!is.null(xlabs)) {
    xlabs <- as.character(xlabs)
    text(x, xlabs, cex = cex[1L], col = col[1L], ...)
  } else {
    points(x, cex=cex[1L], col=col[1L],...)
  }
  par(new = TRUE)
  dev.hold()
  on.exit(dev.flush())

  plot(y, axes = FALSE, type = "n", xlim = xlim * ratio, ylim = ylim * 
       ratio, xlab = "", ylab = "", col = col[1L], ...)
  axis(3, col = col[2L], ...)
  axis(4, col = col[2L], ...)
  box(col = col[1L])
  if(!is.null(ylabs)) {
    ylabs <- as.character(ylabs)
    text(y, labels = ylabs, cex = cex[2L], col = col[2L], ...)    
  } else {
    points(y, cex=cex[2L], col=col[2L],...)
  }

  if (var.axes) 
    arrows(0, 0, y[, 1L] * 0.8, y[, 2L] * 0.8, col = col[2L], 
           length = arrow.len)
  invisible()
}
