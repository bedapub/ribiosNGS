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
pdf2png <- function(..., convert="convert", density=300, outdir=NULL, wait=FALSE) {
  files <- unlist(list(...))
  assertFile(files)
  if(is.null(outdir)) outdir <- dirname(files)
  outfiles <- file.path(outdir,
                        sprintf("%s.png", basefilename(files)))
  comms <- sprintf("%s -density %d %s %s", convert, density, files, outfiles)
  for(i in seq(along=comms))
    system(comms[i], wait=wait)
}
