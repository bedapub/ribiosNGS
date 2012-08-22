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
