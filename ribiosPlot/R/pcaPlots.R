## PCA plot for samples of expression data
plotPCA <- function(x,
                    choices=c(1,2),
                    text=FALSE,
                    points=list(pch=NULL, col=NULL, cex=NULL, bg=NULL, lwd=NULL, lty=NULL),
                    arrows=FALSE,
                    grid=FALSE,
                    xlim=NULL, ylim=NULL,
                    xlab=NULL, ylab=NULL, ...) {
  if(!is(x, "prcomp"))
    stop(sprintf("'%s' must be a prcomp object", deparse(substitute(x))))
  if (!length(scores <- x$x)) 
    stop(gettextf("object '%s' has no scores", deparse(substitute(x))), 
         domain = NA)
  if (is.complex(scores)) 
    stop("biplots are not defined for complex PCA")
  if(length(choices)!=2) {
    stop("choices must be a integer vector of length 2, indicating which components to visualize")
  } else if (max(choices)>ncol(scores)) {
    stop("Input PCA has %d dimensions, the choices (%d, %d) are out of boundary",
         ncol(scores), choices[1], choices[2])
  }

  xind <- choices[1]
  yind <- choices[2]
  
  lam <- x$sdev[choices]
  n <- NROW(scores)
  lam <- lam * sqrt(n)
  xx <- t(t(scores[, choices])/lam)

  rangx1 <- range(xx[, 1])
  rangx2 <- range(xx[, 2])
  if(missing(xlim)) xlim <- rangx1
  if(missing(ylim)) ylim <- rangx2

  expvar <- x$sdev^2/sum(x$sdev^2)
  if(is.null(xlab))
    xlab <- sprintf("Principal component %d (%2.1f%%)", xind, expvar[xind]*100)
  if(is.null(ylab))
    ylab <- sprintf("Principal component %d (%2.1f%%)", yind, expvar[yind]*100)

  ## process text first because it may require adjusting xlim automatically
  plot.new()
  
  if(!is.null(text) && !(is.logical(text) && !text)) {
      doText <- TRUE
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
          labels <- dimnames(xx)[[1L]]
          if (is.null(labels)) 
              labels <- 1L:n
      }
      labels <- as.character(labels)
      
      isRightMost <- which.max(xx[,1])
      isLeftMost <- which.min(xx[,1])
      rightMostWidth <- strwidth(labels[isRightMost], units="user", cex=text.cex,
                                 font=text.font, vfont=text.vfont, family=text.family)
      leftMostWidth <- strwidth(labels[isLeftMost], units="user", cex=text.cex,
                                font=text.font, vfont=text.vfont, family=text.family)
      singleCharWidth <- strwidth("M")

      leftAdj <- leftMostWidth+text.offset*singleCharWidth
      rightAdj <- rightMostWidth+text.offset*singleCharWidth
      heightAdj <- text.offset*singleCharWidth
      
      if(is.null(text.pos) || text.pos==1 || text.pos==3) {
          xlim <- c(xlim[1]-leftMostWidth/2,
                    xlim[2]+rightMostWidth/2)
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
  
  plot(xx, type = "n", xlim = xlim, ylim = ylim, xlab=xlab, ylab=ylab, ...)

  if(grid) grid(lty=1L)
  if(!is.null(points) && !(is.logical(points) && !points)) {
    pts.pch <- 1L
    pts.col <- palette()[1]
    pts.cex <- 1L
    pts.bg <- palette()[1]
    pts.lwd <- 1L
    pts.lty <- 1L
    if (is.list(points)) {
      pts.pch <- nonNull(points$pch, pts.pch)
      pts.col <- nonNull(points$col,pts.col)
      pts.cex <- nonNull(points$cex, pts.cex)
      pts.bg <- nonNull(points$bg,pts.bg)
      pts.lwd <- nonNull(points$lwd, pts.lwd)
      pts.lty <- nonNull(points$lty,pts.lty)
    }
    points(xx,
           pch=pts.pch, col=pts.col, cex=pts.cex,
           bg=pts.bg, lwd=pts.lwd, lty=pts.lty)
  }

  if(!is.null(arrows) && !(is.logical(arrows) && !arrows)) {
    if(is.logical(arrows)) arrows <- list()
    
    arrows.col <- palette()[1]
    arrows.lwd <- 1L
    arrows.lty <- 1L
    arrows.code <- 2
    arrows.length <- 0.1
    arrows.angle <- 30

    nout <- nrow(xx)

    arrows.col <- nonNull(arrows$col, arrows.col, nout)
    arrows.lwd <- nonNull(arrows$lwd, arrows.lwd, nout)
    arrows.lty <- nonNull(arrows$lty, arrows.lty, nout)
    arrows.code <- nonNull(arrows$code, arrows.code, nout)
    arrows.length <- nonNull(arrows$length, arrows.length, nout)
    arrows.angle <- nonNull(arrows$angle, arrows.angle, nout)

    for(i in seq(along=1:nrow(xx))) {
      arrows(0, 0, xx[i,1]*0.95, xx[i,2]*0.95,
             col=arrows.col[i], lwd=arrows.lwd[i], lty=arrows.lty[i],
             code=arrows.code[i], length=arrows.length[i], angle=arrows.angle[i])
    }
  }

  if(doText) {
      text(xx, labels, col = text.col, cex = text.cex, font = text.font, 
           adj = text.adj, pos = text.pos, offset = text.offset, 
           vfont = text.vfont, srt = text.srt, family = text.family, 
           xpd = text.xpd)
  }
  return(invisible(as.data.frame(xx)))
  
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

