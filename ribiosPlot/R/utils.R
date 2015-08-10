## compop
compactPar<- function(mar=c(3,3,1.5,1.5), mgp=c(2,1,0),...) return(par(mar=mar, mgp=mgp, ...))

## get an matrix layout that is as near as a square
squareLayout <- function(n) {
  ncol <- ceiling(sqrt(n))
  nrow <- n %/% ncol+ ifelse(n %% ncol >0, 1, 0)
  return(c(nrow, ncol))
}

## symmetric range
symrange <- function(x, mid=0) {
  xrange <- range(x[!is.infinite(x)], na.rm=TRUE)
  maxabs <- max(abs(xrange-mid))
  return(c(mid-maxabs, mid+maxabs))
}

## non null value
nonNull <- function(x, default, length=NULL, defaultNULL.ok=FALSE) {
  if(is.null(default) & !defaultNULL.ok)
    stop("'default' is not allowed to be NULL")
  if(is.null(x)) {
    res <- default
  } else {
    res <- x
  }
  if(!missing(length))
    res <- rep(res, length.out=length)
  return(res)
}

bound <- function(x,low,high)  pmin(pmax(x, low),high)

boundNorm <- function(x,
                      low = min(x, na.rm=TRUE),
                      high = max(x, na.rm=TRUE)) {
  x <- (x - low)/(high - low)
  x
}

isInvalid <- function (x) 
{
    if (missing(x) || is.null(x) || length(x) == 0) 
        return(TRUE)
    if (is.list(x)) 
        return(all(sapply(x, isInvalid)))
    else if (is.vector(x)) 
        return(all(is.na(x)))
    else return(FALSE)
}

idev <- function(...) {
  if(interactive())
    dev.print(...)
}

ipdf <- function(file, ...) {
  if(interactive())
    dev.print(pdf, file=file, useDingbats=FALSE,...)
}
pdf2png <- function(..., convert="convert", density=300, outdir=NULL, outfile=NULL, wait=FALSE) {
  files <- unlist(list(...))
  assertFile(files)
  if(is.null(outdir)) outdir <- dirname(files)
  if(is.null(outfile)) {
    outfile <- file.path(outdir,
                         sprintf("%s.png", basefilename(files)))
  } else {
    haltifnot(length(outfile)==length(files))
  }

  ## mon udis machine, ghostscript has to be added to the path
  if(Sys.info()[["nodename"]]=="rbaus024.bas.roche.com") {
    convert <- "PATH=/apps64/ghostscript-9.10/bin/:/apps64/ImageMagick-6.7.5-4/bin/:${PATH} convert"
  }
  comms <- sprintf("%s -density %d %s %s", convert, density, files, outfile)
  for(i in seq(along=comms))
    system(comms[i], wait=wait)
  return(invisible(outfile))
}
intRange <- function(x, na.rm=TRUE) {
  range <- range(x, na.rm=na.rm)
  range[1] <- floor(range[1])
  range[2] <- ceiling(range[2])
  return(range)
}


#' get xlim/ylim ranges for plots from real values
#'
#' @param ... one or more vectors of real values
#' @param perc percentage of dynamic range that should be covered by the limits; if set to 1 the whole range is used.
#' @param symm logical value; if set to \code{TRUE}, the range will be symmetric around zero
#'
#' @examples
#' myX <- rnorm(100, mean=1)
#' myY <- rnorm(100)
#' myLim <- getLims(myX, myY, perc=0.99)
#' plot(myX, myY, xlim=myLim, ylim=myLim)
#' mySymmLim <- getLims(myX, myY, perc=0.99, symm=TRUE)
#' plot(myX, myY, xlim=myLim, ylim=mySymmLim)
#'
#' @export

getLims <- function(..., perc=0.99, symm=TRUE) {
    lower <- (1-perc)/2
    higher <- 1-lower
    allVal <- list(...)
    quant <- quantile(unlist(allVal), c(lower, higher), na.rm=TRUE)
    if(symm) {
        quantAbsMax <- max(abs(quant))
        quant <- c(-quantAbsMax, quantAbsMax)
    }
    return(quant)
}
