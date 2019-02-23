## PCA plot for samples of expression data
#' Retrieve PCA scores from prcomp objects
#'
#' @param x An object of prcomp
#' @param choices Integer vector, indices of principal components, default the first two PCs. If missing, \code{NULL} or \code{NA}, all PCs are returned.
#' @param offset Oither one or more rows's names in the loading matrix, or indices, or a logical vector. The average loading of the rows specified by offset is set to zero.
#' @param reverse Logical of length 2 or 1 (which will be repeated to 2), indicating whether the sign of values in the 1st/2nd axis should be reversed
#' @examples
#' testMatrix <- matrix(rnorm(1000), nrow=10)
#' testPCA <- prcomp(testMatrix)
#' testPCAscores <- pcaScores(testPCA)
#'
#' testPCAscores.withOffset <- pcaScores(testPCA, offset=c(1,3,5))
#' ## notice the average of offset-rows are near zero
#' colMeans(as.matrix(testPCAscores.withOffset)[c(1,3,5),])
#' 
#' testPCAscores.withReverse <- pcaScores(testPCA, reverse=c(TRUE, FALSE))
#' colMeans(as.matrix(testPCAscores.withReverse)[c(1,3,5),])

pcaScores <- function(x, choices, offset, reverse=c(FALSE, FALSE)) {
  stopifnot(all(is.logical(reverse)) & length(reverse)<=2)
  reverse <- rep(reverse, length.out=2)
  if(!is(x, "prcomp"))
    stop(sprintf("'%s' must be a prcomp object", deparse(substitute(x))))
  if (!length(scores <- x$x)) 
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
         domain = NA)
  if (is.complex(scores)) 
    stop("pcaScores is not defined for complex PCA")
  if(missing(choices) || is.null(choices) || is.na(choices)) {
    choices <- 1:ncol(scores)
  } else if (max(choices)>ncol(scores)) {
    stop("Input PCA has %d dimensions, the choices (%d, %d) are out of boundary",
         ncol(scores), choices[1], choices[2])
  }
  if(!missing(offset)) {
      if(!all(offset %in% rownames(x$x)) & !all(offset %in% 1:nrow(x$x)) & !(is.logical(offset) & length(offset)==nrow(x$x))) {
          stop("offset should be either one or more rows's names in x$x, or indices, or a logical vector" )
      }
  }
  
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  xx <- t(t(scores[, choices, drop=FALSE])/lam)
  if(!missing(offset)) {
      offsetMean <- colMeans(xx[offset,,drop=FALSE])
      xxOffset <- matrix(rep(offsetMean, nrow(xx)), ncol=ncol(xx), byrow=T)
      xx <- xx-xxOffset
  }
  reverse <- rep_len(reverse, length.out=length(choices))
  for(i in seq(along=reverse)) {
    if(reverse[i])
      xx[,i] <- -xx[,i]
  }
  browser()
  res <- PCAScoreMatrix(xx, expVar=expVar(x, choices))
  return(res)
}

#' S3 method plotPCA
#' @param x A prcomp object
#' @param choices Integer index, choices to plot
#' @param ... Other parameters
plotPCA <- function(x, choices, ...) UseMethod("plotPCA")

#' Visualise PCA results of expression data with the sample plot
#' 
#' \code{plotPCA} is designed to visualize sample relationships revealed
#' by PCA analysis of high-dimensional expression data. It is adapted
#' from the \code{biplot} function in the \code{stats} package, with
#' functionalities useful for sample visualization and labelling, and removing
#' the visualization of features (usually genes) in the input matrix. The
#' rationale is that in most cases there are too many features to provide
#' an informative visualization.
#' 
#' @param x \code{prcomp} object produced by the \code{prcomp} function
#' @param choices An integer vector of length 2, indicating which PCs to visualize. By default the first (X-axis) and second (Y-axis) are visualized
#' @param text A logical value or a list of options to label samples. See Details.
#' @param points A logical value or a list of options to pinpoint samples. See details.
#' @param arrows A logical or a list of options to draw arrows.
#' @param grid Logical value, indicating whether grid lines should be added to the plot.
#' @param abline A logical or a list of options to draw abline
#' @param xlim xlim of the plot. Automatically determined if missing.
#' @param ylim ylim of the plot. Automatically determined if missing.
#' @param xlab xlab of the plot. If missing, the PC and the explained variability are shown.
#' @param ylab ylab of the plot. If missing, the PC and the explained variability are shown.
#' @param offset Offset should be either one or more rows's names in the loading matrix, or indices, or a logical vector. The average loading of the rows specified by offset is set to zero.
#' @param main Title of the plot
#' @param reverse ogical of length 2 or 1 (which will be repeated to 2), indicating whether the sign of values in the 1st/2nd axis should be reversed.
#' @param ... Other parameters passed to \code{plot.window}
#' 
#' @details 
#'   The values for \code{text}, \code{points} and \code{arrows} can be
#'   \enumerate{
#'   \item Logical. If \code{FALSE}, no text or point is added.
#'   \item List. A list containing options passed to \code{text},  \code{points}, and \code{arrows} respectively, such as \code{col},
#'   \code{cex}, \code{lwd}, \code{lty}, \code{code}, \code{length},\code{angle}, and  \code{pos} (only for \code{text}). 
#'   \code{order} decides in what order are the points drawn, which can be useful when there are points to be drawn 'above' other points.
#'   \item A vector of character strigns (only for \code{text})
#'   }
#' See examples below.
#' 
#' @return   The value of the rotated data, namely the centered (and scaled if requested) data multiplied by the rotation matrix.
#' @note   \code{prcomp} should be called with \code{retx=TRUE}, which is the default behaviour.
#' @seealso   \code{prcomp} and \code{pcaScores}
#' @examples 
#' testVal <- matrix(rnorm(10000), nrow=500)
#' colnames(testVal) <- paste("Sample", 1:ncol(testVal), sep="")
#' rownames(testVal) <- paste("Gene",1:nrow(testVal), sep="")
#' 
#' testPCA <- prcomp(t(testVal), center=TRUE, scale=TRUE)
#' 
#' 
#' plotPCA(testPCA)
#' 
#' plotPCA(testPCA, points=FALSE, text=TRUE, grid=TRUE)
#' 
#' pointsList <- list(col=1:3, bg=21,pch=22, cex=4:1, lwd=1:2, lty=2:4)
#' textList <- list(col=c("orange", "royalblue"), cex=1.2, srt=15, pos=1)
#' 
#' plotPCA(testPCA, choices=c(1,2), grid=TRUE, points=pointsList,
#'         text=TRUE)
#' 
#' ## visualize dimension 1:3
#' rop <- par(mfrow=c(1,2), pty="s")
#' plotPCA(testPCA, choices=c(1,2), grid=TRUE, points=pointsList, text=textList)
#' plotPCA(testPCA, choices=c(2,3), grid=TRUE, points=pointsList, text=textList)
plotPCA.prcomp <- function(x,
                           choices=c(1,2),
                           text=FALSE,
                           points=list(pch=NULL, col=NULL, cex=NULL, bg=NULL, lwd=NULL, lty=NULL, order=NULL),
                           arrows=FALSE,
                           grid=FALSE, abline=FALSE,
                           xlim=NULL, ylim=NULL,
                           xlab=NULL, ylab=NULL,
                           offset,main=NULL, reverse=c(FALSE, FALSE), ...) {
    
    scores <- pcaScores(x, choices=choices,offset=offset, reverse=reverse)
    scoreMat <- as.matrix(scores)
    
    xind <- choices[1]
    yind <- choices[2]
    
    rangx1 <- range(scoreMat[, 1])
    rangx2 <- range(scoreMat[, 2])
    if(missing(xlim)) xlim <- rangx1
    if(missing(ylim)) ylim <- rangx2
    
    if(is.null(xlab))
      xlab <- expVarLabel(scores, choices=xind)
    if(is.null(ylab))
      ylab <- expVarLabel(scores, choices=yind)

    ## process text first because it may require adjusting xlim automatically
    plot.new()
    doText <- !is.null(text) && !(is.logical(text) && !text)
    
    if(doText) {
        text.col <- palette()[1]
        text.cex <- 1L
        text.font <- 1L
        text.adj <- NULL
        text.pos <- NULL
        text.offset <- 0.5
        text.vfont <- NULL
        text.srt <- 0
        text.family <- ""
        text.xpd <- FALSE
        labels <- NULL
        
        if (is.character(text) || is.factor(text) || is.numeric(text)) {
            labels <- text
        } else if (is.list(text)) {
            labels <- text$labels
            text.col <- nonNull(text$col, text.col)
            text.cex <- nonNull(text$cex, text.cex)
            text.font <- nonNull(text$font, text.font)
            text.adj <- nonNull(text$adj,text.adj, defaultNULL.ok=TRUE)
            text.pos <- nonNull(text$pos, text.pos, defaultNULL.ok=TRUE)
            text.offset <- nonNull(text$offset, text.offset)
            text.vfont <- nonNull(text$vfont, text.vfont, defaultNULL.ok=TRUE)
            text.srt <- nonNull(text$srt,text.srt)
            text.family <- nonNull(text$family,text.family)
            text.xpd <- nonNull(text$xpd,text.xpd, defaultNULL.ok=TRUE)
        }
        if (is.null(labels)) {
            labels <- dimnames(scoreMat)[[1L]]
            if (is.null(labels)) 
                labels <- 1L:nrow(scoreMat)
        }
        labels <- as.character(labels)
        
        labelWidth <- strwidth(labels, units="user", cex=text.cex,
                               font=text.font, vfont=text.vfont, family=text.family)
        isRightMost <- which.max(scoreMat[,1]+labelWidth)
        isLeftMost <- which.min(scoreMat[,1]-labelWidth)
        rightMostWidth <- labelWidth[isRightMost]
        leftMostWidth <- labelWidth[isLeftMost]
        singleCharWidth <- strwidth("M")
        
        leftAdj <- leftMostWidth+text.offset*singleCharWidth
        rightAdj <- rightMostWidth+text.offset*singleCharWidth
        heightAdj <- text.offset*singleCharWidth
        
        if(is.null(text.pos) || text.pos==1 || text.pos==3) {
            xlim <- c(xlim[1]-leftMostWidth,
                      xlim[2]+rightMostWidth)
            if(!is.null(text.pos)) {
                if(text.pos==1) {
                    ylim <- c(ylim[1]-heightAdj, ylim[2])
                } else if (text.pos==3) {
                    ylim <- c(ylim[1], ylim[2]+heightAdj)
                }
            }
        } else if (text.pos==4) {
            xlim <- c(xlim[1],
                      xlim[2]+rightAdj)
        } else if (text.pos==2) {
            xlim <- c(xlim[1]-leftAdj,
                      xlim[2]-leftAdj+rightAdj)
        } else {
            stop("text.pos: cannot happen")
        }
    }
    
    ##plot(scoreMat, type = "n", xlim = xlim, ylim = ylim, xlab=xlab, ylab=ylab, ...)
    plot.window(xlim=xlim, ylim=ylim,...)
    title(xlab=xlab, ylab=ylab, main=main)
    axis(1)
    axis(2)
    box()
    
    if(grid) grid(lty=1L)
    if(is.logical(abline) && abline) {
        abline(v=0, h=0)
    } else if (!is.logical(abline)) {
        do.call("abline", as.list(abline))
    }
    if(!is.null(points) && !(is.logical(points) && !points)) {
        pts.pch <- 1L
        pts.col <- palette()[1]
        pts.cex <- 1L
        pts.bg <- palette()[1]
        pts.lwd <- 1L
        pts.lty <- 1L
        pts.order <- 1L
        if (is.list(points)) {
            pts.pch <- nonNull(points$pch, pts.pch)
            pts.col <- nonNull(points$col,pts.col)
            pts.cex <- nonNull(points$cex, pts.cex)
            pts.bg <- nonNull(points$bg,pts.bg)
            pts.lwd <- nonNull(points$lwd, pts.lwd)
            pts.lty <- nonNull(points$lty,pts.lty)
            pts.order <- nonNull(points$order, pts.order)
        }
        pts.pch <- rep(pts.pch, length.out=nrow(scoreMat))
        pts.col <- rep(pts.col, length.out=nrow(scoreMat))
        pts.cex <- rep(pts.cex, length.out=nrow(scoreMat))
        pts.bg <- rep(pts.bg, length.out=nrow(scoreMat))
        pts.lwd <- rep(pts.lwd, length.out=nrow(scoreMat))
        pts.lty <- rep(pts.lty, length.out=nrow(scoreMat))
        uord <- sort(unique(pts.order), decreasing=FALSE)
        for(currOrd in uord) {
            isCurrOrd <- pts.order == currOrd
            points(scoreMat[isCurrOrd,],
                   pch=pts.pch[isCurrOrd],
                   col=pts.col[isCurrOrd],
                   cex=pts.cex[isCurrOrd],
                   bg=pts.bg[isCurrOrd],
                   lwd=pts.lwd[isCurrOrd],
                   lty=pts.lty[isCurrOrd])
        }
    }
    
    if(!is.null(arrows) && !(is.logical(arrows) && !arrows)) {
        if(is.logical(arrows)) arrows <- list()
        
        arrows.col <- palette()[1]
        arrows.lwd <- 1L
        arrows.lty <- 1L
        arrows.code <- 2
        arrows.length <- 0.1
        arrows.angle <- 30
        
        nout <- nrow(scoreMat)
        
        arrows.col <- nonNull(arrows$col, arrows.col, nout)
        arrows.lwd <- nonNull(arrows$lwd, arrows.lwd, nout)
        arrows.lty <- nonNull(arrows$lty, arrows.lty, nout)
        arrows.code <- nonNull(arrows$code, arrows.code, nout)
        arrows.length <- nonNull(arrows$length, arrows.length, nout)
        arrows.angle <- nonNull(arrows$angle, arrows.angle, nout)
        
        for(i in seq(along=1:nrow(scoreMat))) {
            arrows(0, 0, scoreMat[i,1]*0.95, scoreMat[i,2]*0.95,
                   col=arrows.col[i], lwd=arrows.lwd[i], lty=arrows.lty[i],
                   code=arrows.code[i], length=arrows.length[i], angle=arrows.angle[i])
        }
    }
    
    if(doText) {
        text(scoreMat, labels, col = text.col, cex = text.cex, font = text.font, 
             adj = text.adj, pos = text.pos, offset = text.offset, 
             vfont = text.vfont, srt = text.srt, family = text.family, 
             xpd = text.xpd)
    }
    return(invisible(as.data.frame(scoreMat)))
}

plotPCAloading <- function(loadings, x=1L, y=2L, circle=FALSE, title="", subtitle="",...) {
  plot(loadings[,x],loadings[,y],
       xlim=c(-1,1),ylim=c(-1,1),
       xlab=paste("PC ",x,sep=""),
       ylab=paste("PC ",y,sep=""),
       pch=15,cex=1.3,...)
  grid()
  abline(h=0)
  abline(v=0)
  arrows(0,0,loadings[,x],loadings[,y],lty=3)
  text(loadings[,x],loadings[,y],rownames(loadings),cex=1, adj=c(1.2,1), xpd=T)
  title(main=title,outer=TRUE)
  title(sub=subtitle,outer=TRUE,line=-0.5)
  if(circle) {
    plotrix::draw.circle(x=0,y=0,radius=1,border="grey",lwd=2)
  }
}

plotPCAscores <- function(scores, class, legendX, legendY, title="",...) {
  colbase <- brewer.pal.factorLevels(class, name="Set1")
  cols <- colbase[class]
  symbol <- rep(c(15:18,1:4),5L)
  vsym <- symbol[as.numeric(as.factor(class))]
  
  par(mfrow=c(1,2),oma=c(0,0,2,4))
  plot(scores$x[,1],scores$x[,2],
       pch=vsym,
       cex=2,
       col=cols,
       ylab=sprintf("PC2 (%2.1f%%)",summary(scores)$importance[2,2]*100),
       xlab=sprintf("PC1 (%2.1f%%)",summary(scores)$importance[2,1]*100),
       main="Front View",...)
  abline(h=0,v=0,lty=2)
  grid()
  
  par(mfrow=c(1,2),oma=c(0,4,2,0))
  plot(scores$x[,3],scores$x[,2],
       pch=vsym,
       cex=2,
       col=cols,
       ylab=sprintf("PC2 (%2.1f%%)",summary(scores)$importance[2,2]*100),
       xlab=sprintf("PC3 (%2.1f%%)",summary(scores)$importance[2,3]*100),
       main="Side View",...)
  abline(h=0,v=0,lty=2)
  grid()
  
  par(xpd=NA)   # This allows the legend to be printed outside the plot region
  legend(legendX,legendY,
         levels(class),
         pch=symbol[levels(class)],
         col=colbase,
         bty="n",
         cex=1, pt.cex=2,
         title="")
  title(title,outer=TRUE)
  par(xpd=F)
}

