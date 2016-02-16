## strbreak comes from Biobase (created by Wolfgang Huber)
strbreak <- function (x, width = getOption("width"), exdent = 2, collapse = "\n") {
    width <- as.integer(width)
    if (is.na(width) || width <= 1) 
        stop("invalid argument 'width'")
    exdent <- as.integer(exdent)
    if (is.na(exdent) || exdent > width) 
        stop("invalid argument 'exdent'")
    ww <- width - exdent
    lb <- paste(collapse, paste(rep(" ", exdent), collapse = ""), 
        sep = "")
    rv <- character(length(x))
    for (i in seq(along = x)) {
        first <- 1
        last <- width
        if (nchar(x[i]) > width) {
            f <- seq(width + 1, nchar(x[i]), ww)
            first <- c(first, f)
            last <- c(last, f + ww - 1)
        }
        rv[i] <- paste(substring(x[i], first = first, last = last), 
            collapse = lb)
    }
    return(rv)
}


guessWH <- function(nrow, ncol,
                    rownames, colnames,
                    cexRow, cexCol,
                    xlab, ylab,
                    width, height) {
  chr.wid <- 0.10
  chr.hei <- 0.12
  col.factor <- 1.02
  row.factor <- 1.02
  hm.min.wid <- 3
  hm.min.hei <- 3
  lgminw <- 1L ## minimum legend width
  lgminh <- 1L ## minimum legend height
  lgratio <- 0.8
  xlab.factor <- ylab.factor <- 1.25
  
  if(missing(cexRow) || is.na(cexRow)) cexRow <- 1L
  if(missing(cexCol) || is.na(cexCol)) cexCol <- 1L
  if(missing(rownames) || is.null(rownames)) rownames <- as.character(seq(1:nrow))
  if(missing(colnames) || is.null(colnames)) colnames <- as.character(seq(1:ncol))
  if(missing(xlab) || is.na(xlab) || xlab=="") {
    xlab.factor <- 0
  }
  if(missing(ylab) || is.na(ylab) || ylab=="") {
    ylab.factor <- 0
  }
  
  hmwidth <- pmax(hm.min.wid, ncol*chr.hei*col.factor*cexCol+max(chr.wid*cexRow*nchar(rownames), na.rm=TRUE)+chr.hei*col.factor*ylab.factor) ## columns+rowname+ylab
  lgwidth <- pmax(log10(ncol),lgminw)
  twidth <- hmwidth+lgwidth
  lg.wf <- lgwidth/twidth
  if(!missing(width) && !is.na(width)) {
    if(lg.wf*width<lgminw) {
      lgwidth <- lgminw
      hmwidth <- width-lgwidth
    }
  } else {
    width <- twidth
  }
  lwids <- c(lgwidth, hmwidth)

  hmheight <- pmax(hm.min.hei, nrow*chr.hei*row.factor*cexRow + max(chr.wid*cexCol*nchar(colnames), na.rm=TRUE)+chr.hei*row.factor*xlab.factor) ## rows + colname + xlab*2
  lgheight <- pmax(log10(nrow), lgminh)
  theight <- hmheight+lgheight
  if(missing(height) || is.na(height)) height <- theight
  lg.hf <- lgheight/theight
  ## the legend's height should not exceed 0.8 of its width for a nice visualization
  ## and if so, it is set to 0.8 of the width (or 1 if the result is smaller than 1)
  if (lg.hf*height>lgratio*lg.wf*width) {
    lgheight <- pmax(lgratio*lg.wf*width, lgminh)
  }
  hmheight <- height-lgheight
  lheis <- c(lgheight, hmheight)

  return(list(lwid=lwids, lhei=lheis, width=width, height=height))
}

biosHeatmap <- function (x,
                         ## dedrogram control
                         Rowv = TRUE,
                         Colv = if (symm) "Rowv" else TRUE,
                         distfun = dist,
                         hclustfun = hclust,
                         dendrogram = c("both","row", "column", "none"),
                         symm = FALSE,

                         ## data scaling
                         scale = c("none","row", "column"),
                         na.rm = TRUE,

                         ## impage plot
                         revC = identical(Colv, "Rowv"),
                         add.expr,

                         ## mapping data to colors
                         breaks,
                         symbreaks = min(x < 0, na.rm = TRUE) || scale != "none",

                         ## colors
                         col = "greenred",
                         na.color = "darkgray",
                         
                         ## block separation (not implemented in Rscript)
                         colsep,
                         rowsep, 
                         sepcolor = "white",
                         sepwidth = c(0.05, 0.05),

                         ## cell labeling (not implemented in Rscript)
                         cellnote,
                         notecex = 1, 
                         notecol = "cyan",

                         ## level trace (not implemented in Rscript)
                         trace = c("none", "column","row", "both"),
                         tracecol = "cyan",
                         hline = median(breaks), 
                         vline = median(breaks),
                         linecol = tracecol,

                         ## Row/Column Labeling 
                         margins = NULL,
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         labRow = NULL,
                         labCol = NULL,
                         cexMain=NULL,
                         cexRow = pmin(1, 0.2 + 1/log10(nr)), 
                         cexCol = pmin(1, 0.2 + 1/log10(nc)),
                         
                         ## Row/Column color (not implemented in Rscript)
                         ColSideColors, RowSideColors,
                         
                         ## color key (only title implemented)
                         color.key.title="Color Key",                         
                         key = TRUE,
                         keysize = 1.5,
                         density.info = c("none", "histogram","density"),
                         denscol = tracecol,
                         symkey = min(x < 0, na.rm = TRUE) || symbreaks,
                         densadj = 0.25,
                         zlim,

                         ## layout
                         lhei = c(1,7),
                         lwid = c(1,7),
                         lmat = NULL,

                         ...) 
{
  retval <- list()
  scale <- ifelse(symm && missing(scale), "none", match.arg(scale))
  dendrogram <- match.arg(dendrogram)
  trace <- match.arg(trace)
  density.info <- match.arg(density.info)

  ## parameter sanity check
  if (length(col) == 1 && is.character(col)) 
    col <- get(col, mode = "function")
  if (!missing(breaks) && (scale != "none")) 
    warning("Using scale=\"row\" or scale=\"column\" when breaks are", 
            "specified can produce unpredictable results.", "Please consider using only one or the other.")
  if (is.null(Rowv) || is.na(Rowv)) 
    Rowv <- FALSE
  if (is.null(Colv) || is.na(Colv)) 
    Colv <- FALSE
  else if (Colv == "Rowv" && !isTRUE(Rowv)) 
    Colv <- FALSE
  if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
    stop("'x' must be a numeric matrix")
  nr <- di[1];nc <- di[2]
  if (nr < 1 || nc < 1) 
    stop("'x' must have at least 2 rows and 2 columns")

  if (missing(cellnote)) 
    cellnote <- matrix("", ncol = ncol(x), nrow = nrow(x))
  if (!inherits(Rowv, "dendrogram")) {
    if (((!isTRUE(Rowv)) || (is.null(Rowv))) && (dendrogram %in% 
                                                 c("both", "row"))) {
      if (is.logical(Colv) && (Colv)) 
        dendrogram <- "column"
      else dedrogram <- "none"
      warning("Discrepancy: Rowv is FALSE, while dendrogram is '", 
              dendrogram, "'. Omitting row dendogram.")
    }
  }
  if (!inherits(Colv, "dendrogram")) {
    if (((!isTRUE(Colv)) || (is.null(Colv))) && (dendrogram %in% 
                                                 c("both", "column"))) {
      if (is.logical(Rowv) && (Rowv)) 
        dendrogram <- "row"
      else dendrogram <- "none"
      warning("Discrepancy: Colv is FALSE, while dendrogram is '", 
              dendrogram, "'. Omitting column dendogram.")
    }
  }
  if (inherits(Rowv, "dendrogram")) {
    ddr <- Rowv
    rowInd <- order.dendrogram(ddr)
  }
  else if (is.integer(Rowv)) {
    hcr <- hclustfun(distfun(x))
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd)) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Rowv)) {
    Rowv <- rowMeans(x, na.rm = na.rm)
    hcr <- hclustfun(distfun(x))
    ddr <- as.dendrogram(hcr)
    ddr <- reorder(ddr, Rowv)
    rowInd <- order.dendrogram(ddr)
    if (nr != length(rowInd)) 
      stop("row dendrogram ordering gave index of wrong length")
  }
  else {
    rowInd <- nr:1
  }
  if (inherits(Colv, "dendrogram")) {
    ddc <- Colv
    colInd <- order.dendrogram(ddc)
  }
  else if (identical(Colv, "Rowv")) {
    if (nr != nc) 
      stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
    if (exists("ddr")) {
      ddc <- ddr
      colInd <- order.dendrogram(ddc)
    }
    else colInd <- rowInd
  }
  else if (is.integer(Colv)) {
    hcc <- hclustfun(distfun(if (symm) 
                             x
    else t(x)))
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd)) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else if (isTRUE(Colv)) {
    Colv <- colMeans(x, na.rm = na.rm)
    hcc <- hclustfun(distfun(if (symm) 
                             x
    else t(x)))
    ddc <- as.dendrogram(hcc)
    ddc <- reorder(ddc, Colv)
    colInd <- order.dendrogram(ddc)
    if (nc != length(colInd)) 
      stop("column dendrogram ordering gave index of wrong length")
  }
  else {
    colInd <- 1:nc
  }
  retval$rowInd <- rowInd
  retval$colInd <- colInd
  retval$call <- match.call()
  x <- x[rowInd, colInd]
  x.unscaled <- x
  cellnote <- cellnote[rowInd, colInd]

  if (is.null(labRow)) 
    labRow <- if (is.null(rownames(x))) 
      (1:nr)[rowInd]
    else rownames(x)
  else labRow <- labRow[rowInd]
  if (is.null(labCol)) 
    labCol <- if (is.null(colnames(x))) 
      (1:nc)[colInd]
    else colnames(x)
  else labCol <- labCol[colInd]

  if (scale == "row") {
    retval$rowMeans <- rm <- rowMeans(x, na.rm = na.rm)
    x <- sweep(x, 1, rm)
    retval$rowSDs <- sx <- apply(x, 1, sd, na.rm = na.rm)
    x <- sweep(x, 1, sx, "/")
  }
  else if (scale == "column") {
    retval$colMeans <- rm <- colMeans(x, na.rm = na.rm)
    x <- sweep(x, 2, rm)
    retval$colSDs <- sx <- apply(x, 2, sd, na.rm = na.rm)
    x <- sweep(x, 2, sx, "/")
  }
  if (missing(breaks) || is.null(breaks) || length(breaks) < 1) {
    if (missing(col) || is.function(col)) 
      breaks <- 50
    else breaks <- length(col) + 1
  }

  zlim.missing <- missing(zlim) || (length(zlim)==2 && all(is.na(zlim)))
  if(!zlim.missing) {
    stopifnot(length(zlim)==2)
    if(is.na(zlim[1]))
      zlim[1] <- min(x, na.rm=TRUE)
    if(is.na(zlim[2]))
      zlim[2] <- max(x, na.rm=TRUE)
    if(zlim[2]<zlim[1]) {
      zlim <- rev(zlim)
    } else if (identical(zlim[1], zlim[2])) {
      zlim[2] <- zlim[1]+1
    }
  }
  if (length(breaks) == 1) {
    if(!zlim.missing) {
      breaks <- seq(zlim[1], zlim[2], length=breaks)
    } else {
      if (!symbreaks) 
        breaks <- seq(min(x, na.rm = na.rm), max(x, na.rm = na.rm), 
                      length = breaks)
      else {
        extreme <- max(abs(x), na.rm = TRUE)
        breaks <- seq(-extreme, extreme, length = breaks)
      }
    }
  }
  nbr <- length(breaks)
  ncol <- length(breaks) - 1
  if (class(col) == "function") 
    col <- col(ncol)
  min.breaks <- min(breaks)
  max.breaks <- max(breaks)
  x[x < min.breaks] <- min.breaks
  x[x > max.breaks] <- max.breaks
  if (missing(lhei) || is.null(lhei)) 
    lhei <- c(keysize, 6)
  if (missing(lwid) || is.null(lwid)) 
    lwid <- c(keysize, 6)
  if (missing(lmat) || is.null(lmat)) {
    lmat <- rbind(4:3, 2:1)
    if (!missing(ColSideColors) && !is.null(ColSideColors) && !is.na(ColSideColors)) {
      if (!is.character(ColSideColors) || length(ColSideColors) %% nc != 0) 
        stop("'ColSideColors' must be a character vector of length ncol(x)")
      lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 
                    1)
      lhei <- c(lhei[1], 0.2, lhei[2])
    }
    if (!missing(RowSideColors) && !is.null(RowSideColors) && !is.na(RowSideColors)) {
      if (!is.character(RowSideColors) || length(RowSideColors) %% nr != 0) 
        stop("'RowSideColors' must be a character vector of length nrow(x)")
      lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 
                                         1), 1), lmat[, 2] + 1)
      lwid <- c(lwid[1], 0.2, lwid[2])
    }
    lmat[is.na(lmat)] <- 0
  }
  if (length(lhei) != nrow(lmat)) 
    stop("lhei must have length = nrow(lmat) = ", nrow(lmat))
  if (length(lwid) != ncol(lmat)) 
    stop("lwid must have length = ncol(lmat) =", ncol(lmat))
  op <- par(no.readonly = TRUE)
  on.exit(par(op))

  ## check lhei[1]*par("din")[2] must be >=1, otherwise it reports error
  par.fin <- par("fin")
  estKeyHeight <- lhei[1]/(sum(lhei))*par.fin[2]
  if(estKeyHeight<1L) {
    adj.lhei.1 <- lhei[1]/estKeyHeight
    lhei.rest.coef <- (sum(lhei)-adj.lhei.1)/sum(lhei[-1])
    lhei <- c(adj.lhei.1, lhei[-1]*lhei.rest.coef)
  }
  layout(lmat, widths = lwid, heights = lhei, respect = FALSE)

  ## margins have to be determined now (after layout)
  if (is.null(margins) || !is.numeric(margins) || length(margins)!=2) {
    xlab.mar <- ifelse(!is.null(xlab) && length(xlab)==1,2,0.2)
    ylab.mar <- ifelse(!is.null(ylab) && length(ylab)==1,2,0.2)
    margins <- c(max(strwidth(labCol, units="inch", cex=cexCol),na.rm=TRUE)/par("csi")+xlab.mar,
                 max(strwidth(labRow, units="inch", cex=cexRow),na.rm=TRUE)/par("csi")+ylab.mar)
  }

  if (!missing(RowSideColors)) {
    par(mar = c(margins[1], 0, 0, 0.5))
    cs.color.round <- length(RowSideColors) %/% nr
    cs.matrix <- matrix(rep(1:nr, cs.color.round), nrow=cs.color.round)
    cs.color.order.base <- matrix(rep(0:(cs.color.round-1), each=nr), nrow=cs.color.round) * nr
    cs.color.ordered <- as.vector(cs.matrix[,rowInd]) + as.vector(t(cs.color.order.base))
    cs.matrix.ordered <- cs.matrix + as.vector(t(cs.color.order.base))
    image(cs.matrix.ordered, col = RowSideColors[cs.color.ordered], axes = FALSE)
  }
  if (!missing(ColSideColors)) {
    par(mar = c(0.5, 0, 0, margins[2]))
    cs.color.round <- length(ColSideColors) %/% nc
    cs.matrix <- matrix(rep(1:nc, cs.color.round), ncol=cs.color.round)
    cs.color.order.base <- matrix(rep(0:(cs.color.round-1), each=nc), ncol=cs.color.round) * nc
    cs.color.ordered <- as.vector(cs.matrix[colInd,]) + as.vector(cs.color.order.base)
    cs.matrix.ordered <- cs.matrix + as.vector(cs.color.order.base)
    image(cs.matrix.ordered, col = ColSideColors[cs.color.ordered], axes = FALSE)
  }
  par(mar = c(margins[1], 0, 0, margins[2]))
  x <- t(x)
  cellnote <- t(cellnote)
  if (revC) {
    iy <- nr:1
    if (exists("ddr")) 
      ddr <- rev(ddr)
    x <- x[, iy]
    cellnote <- cellnote[, iy]
  }
  else iy <- 1:nr

  image(1:nc, 1:nr, x,
        xlim = 0.5 + c(0, nc),
        ylim = 0.5 + c(0, nr),
        axes = FALSE, xlab = "", ylab = "",
        col = col, 
        breaks = breaks, zlim=zlim,...)
  retval$carpet <- x
  if (exists("ddr")) 
    retval$rowDendrogram <- ddr
  if (exists("ddc")) 
    retval$colDendrogram <- ddc
  retval$breaks <- breaks
  retval$col <- col
  if (!isInvalid(na.color) & any(is.na(x))) {
    mmat <- ifelse(is.na(x), 1, NA)
    image(1:nc, 1:nr, mmat, axes = FALSE, xlab = "", ylab = "", 
          col = na.color, add = TRUE)
  }
  if (symm) { ## if symmetric, hide upper-tri
      symmat <- x
      symmat[lower.tri(symmat, diag=FALSE)] <- 1L
      symmat[upper.tri(symmat, diag=TRUE)] <- NA
      symmat <- symmat[,ncol(symmat):1]
      image(1:nc, 1:nr, symmat, axes=FALSE, xlab="", ylab="",
             col="white", add=TRUE)
  }

  ## axis label
  axis(1, 1:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
       cex.axis = cexCol*par("cex"))
  if (!is.null(xlab)) 
    mtext(xlab, side = 1, line = margins[1] - 1.25, cex=par("cex"))
  axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
       cex.axis = cexRow*par("cex"))
  if (!is.null(ylab)) 
    mtext(ylab, side = 4, line = margins[2] - 1.25, cex=par("cex"))
  
  if (!missing(add.expr)) 
    eval(substitute(add.expr))
  if (!missing(colsep)) {
      if(symm) {
          cytop <- nrow(x)+0.5-colsep
      } else {
          cytop <- rep(ncol(x)+1, length(colsep))
      }
      rect(xleft=colsep+0.5,
           ybottom=rep(0, length(colsep)),
           xright=colsep+0.5+sepwidth[1],
           ytop=cytop, lty=1, lwd=1,
           col=sepcolor, border=sepcolor)
  }
  if (!missing(rowsep))  {
      if(symm) {
          rxright <- rowsep+0.5
      } else {
          rxright <- nrow(x)+1
      }
      rect(xleft=0.5,
           ybottom=ncol(x)+1-rowsep-0.5,
           xright=rxright,
           ytop=ncol(x)+1-rowsep-0.5-sepwidth[2],
           lty=1, lwd=1, col=sepcolor, border=sepcolor)
  }
  min.scale <- min(breaks)
  max.scale <- max(breaks)
  x.scaled <- boundNorm(t(x), min.scale, max.scale)
  if (trace %in% c("both", "column")) {
    retval$vline <- vline
    vline.vals <- boundNorm(vline, min.scale, max.scale)
    for (i in colInd) {
      if (!is.null(vline)) {
        abline(v = i - 0.5 + vline.vals, col = linecol, 
               lty = 2)
      }
      xv <- rep(i, nrow(x.scaled)) + x.scaled[, i] - 0.5
      xv <- c(xv[1], xv)
      yv <- 1:length(xv) - 0.5
      lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
    }
  }
  if (trace %in% c("both", "row")) {
    retval$hline <- hline
    hline.vals <- boundNorm(hline, min.scale, max.scale)
    for (i in rowInd) {
      if (!is.null(hline)) {
        abline(h = i + hline, col = linecol, lty = 2)
      }
      yv <- rep(i, ncol(x.scaled)) + x.scaled[i, ] - 0.5
      yv <- rev(c(yv[1], yv))
      xv <- length(yv):1 - 0.5
      lines(x = xv, y = yv, lwd = 1, col = tracecol, type = "s")
    }
  }
  if (!missing(cellnote)) 
    text(x = c(row(cellnote)), y = c(col(cellnote)), labels = c(cellnote), 
         col = notecol, cex = notecex)
  par(mar = c(margins[1], 0, 0, 0))
  if (dendrogram %in% c("both", "row")) {
    plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
  }
  else plot.new()
  par(mar = c(0.5, 0, if (!is.null(main)) 5 else 0, margins[2]))
  if (dendrogram %in% c("both", "column")) {
    plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
  }
  else plot.new()
  if (!is.null(main)) {
    ## adjust main size
    main <- strwrap(main, width=35L)
    if(length(main)==1) { ## wrapping not succeed
      main <- strbreak(main, 35L, exdent=0)
    } else {
      main <- paste(main, collapse="\n")
    }
    ## Todo: not quite sure whether the unit here is correct
    if(is.null(cexMain)) {
      main.width <- strwidth(main, units="figure", cex=op[["cex.main"]])    
      if(main.width<0.33) main.width <- 0.33
      cexMain <- op[["cex.main"]]/main.width*(lwid[length(lwid)]/sum(lwid))*0.8
    }
    title(main, cex.main=cexMain)
  }
  if (key) {
    par(mar = c(1.8, 1, 1, 1), cex = 0.75, mgp=c(1,0.5,0))
    tmpbreaks <- breaks
    if(!zlim.missing) {
      max.raw <- zlim[2]
      min.raw <- zlim[1]
    } else {
      if (symkey) {
        max.raw <- max(abs(c(x, breaks)), na.rm = TRUE)
        min.raw <- -max.raw
        tmpbreaks[1] <- -max(abs(x), na.rm=TRUE)
        tmpbreaks[length(tmpbreaks)] <- max(abs(x), na.rm=TRUE)
      }
      else {
        min.raw <- min(x, na.rm = TRUE)
        max.raw <- max(x, na.rm = TRUE)
      }
    }
    z <- seq(min.raw, max.raw, length = length(col))
    image(z = matrix(z, ncol = 1), col = col, breaks = tmpbreaks, 
          xaxt = "n", yaxt = "n")
    par(usr = c(0, 1, 0, 1))
    lv <- pretty(breaks)
    xv <- boundNorm(as.numeric(lv), min.raw, max.raw)
    axis(1, at = xv, labels = lv)
    if (scale == "row") 
      mtext(side = 1, "Row Z-Score", line = 1.2, cex=par("cex"))
    else if (scale == "column") 
      mtext(side = 1, "Column Z-Score", line = 1.2, cex=par("cex"))
    else mtext(side = 1, "", line = 2)
    if (density.info == "density") {
      dens <- density(x, adjust = densadj, na.rm = TRUE)
      omit <- dens$x < min(breaks) | dens$x > max(breaks)
      dens$x <- dens$x[-omit]
      dens$y <- dens$y[-omit]
      dens$x <- boundNorm(dens$x, min.raw, max.raw)
      lines(dens$x, dens$y/max(dens$y) * 0.95, col = denscol, 
            lwd = 1)
      axis(2, at = pretty(dens$y)/max(dens$y) * 0.95, pretty(dens$y))
      title("Color Key\nand Density Plot")
      par(cex = 0.5)
      mtext(side = 2, "Density", line = 2)
    }
    else if (density.info == "histogram") {
      h <- hist(x, plot = FALSE, breaks = breaks)
      hx <- boundNorm(breaks, min.raw, max.raw)
      hy <- c(h$counts, h$counts[length(h$counts)])
      lines(hx, hy/max(hy) * 0.95, lwd = 1, type = "s", 
            col = denscol)
      axis(2, at = pretty(hy)/max(hy) * 0.95, pretty(hy))
      title(sprintf("%s\nand Histogram", color.key.title))
      par(cex = 0.5)
      mtext(side = 2, "Count", line = 2)
    }
    else title(color.key.title)
  }
  else plot.new()
  retval$colorTable <- data.frame(low = retval$breaks[-length(retval$breaks)], 
        high = retval$breaks[-1], color = retval$col)
  invisible(retval)
}
