uniqueLength <- function(x, incomparables=FALSE)
  length(unique(x,incomparables=incomparables))

chosenFew <- function(vec, start=3, end=1, collapse=",") {
  if(length(vec)<start+end)
    return(paste(vec, collapse=collapse))
  vlen <- length(vec)
  return(paste(paste(vec[1:start], collapse=collapse),
               "...",
               paste(vec[(vlen-end+1):vlen], collapse=","),
               sep=collapse))
}

ulen <- uniqueLength

